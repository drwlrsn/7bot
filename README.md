# 7bot
7bot is Slackbot that helps teams keep track of the state of their open pull requests.

## Getting started
`BOT_GITHUB_TOKEN=<token> BOT_SLACK_TOKEN=<token> ./7bot.jar`

### Prerequisites
1. Java 8
2. Scala v2.12.4
    `brew install scala`
3. scalafmt v1.4.0
    `brew install scalafmt`
4. Scala Build Tool v1.1.1
    `brew install sbt`

### Installing
1. `git clone https://github.com/drwlrsn/7bot.git`
2. `$ cd 7bot`
3. `$ sbt build`

### Running tests
`$ sbt test`