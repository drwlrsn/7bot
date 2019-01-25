lazy val akkaHttpVersion = "10.0.11"
lazy val akkaVersion    = "2.5.11"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization    := "com.sevenshifts",
      scalaVersion    := "2.12.4"
    )),
    name := "7bot",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "com.typesafe.akka"     %% "akka-http"            % akkaHttpVersion,
      "com.typesafe.akka"     %% "akka-http-spray-json" % akkaHttpVersion,
      "com.typesafe.akka"     %% "akka-http-xml"        % akkaHttpVersion,
      "com.typesafe.akka"     %% "akka-stream"          % akkaVersion,
      "com.github.gilbertw1"  %% "slack-scala-client"   % "0.2.3",
      "com.47deg"             %% "github4s"             % "0.18.4",


      "com.typesafe.akka"     %% "akka-http-testkit"    % akkaHttpVersion % Test,
      "com.typesafe.akka"     %% "akka-testkit"         % akkaVersion     % Test,
      "com.typesafe.akka"     %% "akka-stream-testkit"  % akkaVersion     % Test,
      "org.scalatest"         %% "scalatest"            % "3.0.1"         % Test
    )
  )
