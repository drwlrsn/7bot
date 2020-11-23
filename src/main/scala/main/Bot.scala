package main

import java.text.DateFormat
import java.time.Instant
import java.util.concurrent.{Executors, TimeUnit}
import java.util.{Date, Locale}

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import cats.Id
import cats.effect.{Blocker, ContextShift, IO}
import github4s.Github
import github4s.domain.{
  Issue,
  IssueStateOpen,
  IssueTypePullRequest,
  LabelParam,
  OwnerParamInRepository,
  PRFilterOpen,
  PRFilterSortPopularity,
  PRRStateApproved,
  PRRStateChangesRequested,
  PullRequest,
  PullRequestReview,
  SearchIn,
  SearchInTitle,
  User
}
import org.http4s.client.{Client, JavaNetClientBuilder}
import slack.api.SlackApiClient
import slack.rtm.SlackRtmClient

import scala.collection.mutable
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContextExecutor, Future}

object Bot {
  val dmChannels: mutable.MutableList[String] = mutable.MutableList[String]()
  val accessToken: Option[String]             = sys.env.get("BOT_GITHUB_TOKEN")
  val techLeads                               = List("padge", "unrolled", "raulchedrese")
  var prevMessages: Map[String, String]       = Map()
  implicit val system: ActorSystem            = ActorSystem("slack")
  implicit val ec: ExecutionContextExecutor   = system.dispatcher

  def main(args: Array[String]): Unit = {
    val slackToken = sys.env("BOT_SLACK_TOKEN")

    val client    = SlackRtmClient(slackToken, new FiniteDuration(30, TimeUnit.SECONDS))
    val apiClient = SlackApiClient(slackToken)
    val selfId    = client.state.self.id

    val httpClient: Client[IO] = {
      val blockingPool                  = Executors.newFixedThreadPool(5)
      val blocker                       = Blocker.liftExecutorService(blockingPool)
      implicit val cs: ContextShift[IO] = IO.contextShift(global)
      JavaNetClientBuilder[IO](blocker).create // use BlazeClientBuilder for production use
    }

    implicit val github: Github[IO] = Github[IO](httpClient, accessToken)
    client.onMessage { message =>
      val response = apiClient.postChatMessage(
        message.channel,
        callCommand(parseCommand(message.text))
      )

      if (message.user != selfId) {
        prevMessages get message.channel match {
          case Some(msgId) => deletePreviousMessage(apiClient, message.channel, msgId)
          case None =>
            () =>
              0
        }
      }

      response.map { res =>
        prevMessages = prevMessages + (message.channel -> res)
      }
    }

  }

  def deletePreviousMessage(client: SlackApiClient, channel: String, ts: String): Future[Unit] =
    client.deleteChat(channel, ts).map(res => println(res))

//  def getPullRequests(implicit client: Github[IO]): List[PullRequest] = {
//    val prFilters = List(PRFilterOpen, PRFilterSortPopularity)
//    val listPullRequests =
//      client.pullRequests.listPullRequests("7shifts", "webapp", prFilters)
//
//    val response = listPullRequests.unsafeRunSync()
//
//    response.result match {
//      case Left(exception) => throw exception
//      case Right(r)        => r
//    }
//  }

  def buildTechLeadApproval(approved: Boolean): List[String] =
    if (approved) {
      List.fill(1)("✨")
    } else {
      List.fill(1)("⚪")
    }

  def buildReviewStateStr(r: Review): String = {
    val slots = 2
    val qa: List[String] = (if (r.labels.map(_.name).contains("QA passed")) "🎨"
                            else if (r.labels.map(_.name).contains("QA not needed")) "🎨"
                            else if (r.labels.map(_.name).contains("QA fixes needed")) "❌"
                            else "⚪️") :: Nil
    val tla = buildTechLeadApproval(r.techLeadApproval)
    if (r.approved > slots - 1) {
      val approvals = List.fill(if (r.approved > slots) slots else r.approved)("✅")
      val changes   = List.fill(r.changes)("❌")
      (approvals ::: changes ::: tla ::: qa).mkString(" ")
    } else {
      val requiredReviews = List.fill(slots - r.approved - r.changes)("⬜")
      val approvals       = List.fill(r.approved)("✅")
      val changes         = List.fill(r.changes)("❌")
      (approvals ::: changes ::: requiredReviews ::: tla ::: qa).mkString(" ")
    }
  }

  def buildTitle(r: Review): String =
    if (r.labels.map(_.name).contains("PRIORITY")) "🚨 *" + r.title + "* 🚨" else r.title

  def buildUrl(u: String): String = "<" + u + "|PR #" + Uri(u).path.reverse.head.toString + ">"

  def buildDate(d: String): String = {
    val date = Date.from(Instant.parse(d))
    val df   = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, Locale.CANADA)
    " _<!date^" + (date.toInstant.toEpochMilli / 1000).toString + "^(opened {date_short_pretty})|" + df
      .format(date) + ">_"
  }

  def buildReviews(as: List[Issue])(implicit client: Github[IO]): List[String] =
    getReviews(as).foldLeft(List[String]())(
      (xs: List[String], r: Review) =>
        (buildReviewStateStr(r) :: buildUrl(r.url) :: buildDate(r.created_at) :: " – " + buildTitle(
          r) :: Nil)
          .mkString(" ") :: xs)

  private def getReviews(as: List[Issue])(implicit client: Github[IO]): List[Review] = {
    // I am pretty sure we could use fs2.async.parallelTraverse to do these concurrently
    val prLists = for (a: Issue <- as if a.state == "open")
      yield { // Factor this out into a different method and return `main.Review`
        val listReviews = client.pullRequests
          .listReviews("7shifts", "webapp", a.number)
        val response = listReviews.unsafeRunSync()
        response.result match {
          case Left(exception) => throw exception
          case Right(r) =>
            val filteredReviews = filterUserReviews(r)
            Review(
              a.html_url,
              a.title,
              filteredReviews.count(p => p.state == PRRStateApproved),
              filteredReviews.count(p => p.state == PRRStateChangesRequested),
              filteredReviews.exists(fr =>
                techLeads.contains(fr.user.get.login) && fr.state == PRRStateApproved),
              a.updated_at,
              a.created_at,
              a.closed_at,
              a.labels
            )
        }
      }

    prLists
  }

  def filterUserReviews(as: List[PullRequestReview]): List[PullRequestReview] = {
    val mappedByUser: Map[Option[User], List[PullRequestReview]] =
      as.filter(_.user.nonEmpty)
        .filter(p => p.state == PRRStateChangesRequested || p.state == PRRStateApproved)
        .groupBy(_.user)

    mappedByUser.foldLeft(List[PullRequestReview]())((xs, kv) => {
      kv._2.sortBy(_.id).reverse.head :: xs
    })
  }

  def buildPullRequests(as: List[Issue])(implicit client: Github[IO]): String =
    if (as.isEmpty) "Could not find any open PRs"
    else buildReviews(as).mkString("\n")

  def getPullRequests(team: String)(implicit client: Github[IO]): String = {
    val listPullRequests = client.issues.searchIssues(
      "",
      List(
        OwnerParamInRepository("47degrees/github4s"),
        IssueTypePullRequest,
        IssueStateOpen,
        LabelParam("enhancement"),
      )
    )

    val response = listPullRequests.unsafeRunSync()
    response.result match {
      case Left(exception) =>
        val message = exception.getMessage()
        val code    = response.statusCode
        s"Search failed for *$team* with the message `$message` and a HTTP status code of `$code`"
      case Right(r) => buildPullRequests(filterLabels(r.items))
    }
  }

  def parseCommand(message: String): List[String] = {
    val Pattern = "!([a-z]+)".r
    val command = Pattern.findFirstMatchIn(message)

    command match {
      case Some(m) => m.toString() :: (message.replace(m.toString(), "").trim() :: Nil)
      case None    => throw new IllegalArgumentException("No command found.")
    }
  }

  def callCommand(l: List[String])(implicit client: Github[IO]): String = l.head match {
    case "!prs" => parseTeam(l.tail)
    case _      => throw new IllegalArgumentException("Command not recognized.")
  }

  def parseTeam(l: List[String])(implicit client: Github[IO]): String = l.head match {
    case rest => getPullRequests(rest.split(" ")(0))
  }

  def isNotOnHold(pr: Issue): Boolean = !pr.labels.exists(_.name.equalsIgnoreCase("on hold"))

  def filterLabels(prs: List[Issue]): List[Issue] = prs.filter(isNotOnHold)

}
