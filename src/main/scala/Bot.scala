import akka.actor.ActorSystem
import github4s.Github
import github4s.Github._
import github4s.free.domain._
import github4s.jvm.Implicits._
import scalaj.http.HttpResponse
import slack.SlackUtil
import slack.rtm.SlackRtmClient

import scala.collection.mutable
import scala.concurrent.ExecutionContextExecutor

object Bot {
  val dmChannels: mutable.MutableList[String] = mutable.MutableList[String]()
  val accessToken = sys.env("7BOT_GITHUB_TOKEN")

  def main(args: Array[String]): Unit = {
    val slackToken = sys.env("7BOT_SLACK_TOKEN")
    implicit val system: ActorSystem = ActorSystem("slack")
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val client = SlackRtmClient(slackToken)
    val selfId = client.state.self.id

    client.onMessage { message =>
      val isDirectMessage: Boolean = SlackUtil.isDirectMsg(message)
      if (isDirectMessage && message.user != selfId)
        client.sendMessage(
          message.channel,
          callCommand(parseCommand(message.text))
        )
    }

  }

  def getPullRequests: List[PullRequest] = {
    val prFilters = List(PRFilterOpen, PRFilterSortPopularity)
    val listPullRequests =
      Github(Option(accessToken)).pullRequests.list("7shifts", "webapp", prFilters)

    listPullRequests.exec[cats.Id, HttpResponse[String]]() match {
      case Left(exception) => throw exception
      case Right(r) => r.result
    }
  }

  def buildPullRequests(prs: List[Issue]): String =
    if (prs.isEmpty) "Could not find any open PRs"
    else prs.foldLeft("")((titleStr, issue) => titleStr + issue.title + "\n")

  def getPullRequests2(team: String): String = {
    val listPullRequests = Github(Option(accessToken)).issues.searchIssues(
      "",
      List(
        OwnerParamInRepository("7shifts/webapp"),
        IssueTypePullRequest,
        IssueStateOpen,
        LabelParam(label = team, exclude = false),
        SearchIn(Set(SearchInTitle))
      )
    )

    listPullRequests.exec[cats.Id, HttpResponse[String]]() match {
      case Left(exception) => throw exception
      case Right(r) => buildPullRequests(r.result.items)
    }
  }

  def parseCommand(message: String): List[String] = {
    val Pattern = "!([a-z]+)".r
    val command = Pattern.findFirstMatchIn(message)

    command match {
      case Some(m) => m.toString() :: (message.replace(m.toString(), "").trim() :: Nil)
      case None => throw new IllegalArgumentException("No command found.")
    }
  }

  def callCommand(l: List[String]): String = l.head match {
    case "!prs" => parseTeam(l.tail)
    case _ => throw new IllegalArgumentException("Command not recognized.")
  }

  def parseTeam(l: List[String]): String = l.head match {
    case rest => getPullRequests2(rest.split(" ")(0))
    case _ => throw new IllegalArgumentException("Team missing from arguments")
  }

}
