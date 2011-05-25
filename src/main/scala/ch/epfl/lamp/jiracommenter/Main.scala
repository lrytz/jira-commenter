package ch.epfl.lamp.jiracommenter

object Main extends CommitParser {
  val username = "anonymous"
  val password = "PassWord00"

  def main(args: Array[String]) {
//    val buildURL = "https://scala-webapps.epfl.ch/jenkins/job/scala-checkin/4112/api/json"
    val buildURL = args(0)

    val commits = Jenkins.getCommitData(buildURL)

    println(commits)

    val jiraSession = new AuthSession(username, password)
    val jira = new JIRA(jiraSession)


    for (commit <- commits) {
      val (closed, reopened, mentioned) = analyze(commit.message.toLowerCase)

      println(closed)
      println(reopened)
      println(mentioned)

      for (c <- closed) {
        println(c +", "+ commit.toString)
        jira.closeIssue(c, commit.toString)
      }
      for (r <- reopened) {
        println(r +", "+ commit.toString)
        jira.reopenIssue(r, commit.toString)
      }
      for (m <- mentioned) {
        println(m +", "+ commit.toString)
        jira.commentIssue(m, commit.toString)
      }
    }

/*
    val jiraSession = new AuthSession(username, password)
    val jira = new JIRA(jiraSession)
    println(jira.transitionsFor("TEST-1"))
    jira.closeIssue("TEST-1", "haha reopen :)")
*/
  }
}
