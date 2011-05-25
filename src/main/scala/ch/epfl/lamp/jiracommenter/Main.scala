package ch.epfl.lamp.jiracommenter

object Main {
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
      val (closed, reopened, mentioned) = analyze(commit)

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


  val closeCommand = List("closes", "closed", "close", "fixes", "fixed", "fix")
  val reopenCommand = List("reopens", "reopened", "reopen")

  val ticketReg = """(?:#|SI-|SI_|SI)(\d+)"""

  def analyze(commit: Jenkins.Commit) = {
    val msg = commit.message.toLowerCase

    val closeParts = parts(msg, closeCommand)
    val reopenParts = parts(msg, reopenCommand)

    val closed: Set[String] = closeParts.flatMap(ticketsIn(_))
    val reopened: Set[String] = reopenParts.flatMap(ticketsIn(_))

    val mentioned: Set[String] = (ticketsIn(msg) diff closed) diff reopened

    (closed, reopened, mentioned)
  }

  def parts(msg: String, commands: List[String]): Set[String] = {
    def sub(str: String): Set[String] = {
      val Reg = ("""\s+((?:"""+ ticketReg +""",? ?)*)(?:.|\s)*""").r
      str match {
        case Reg(s, _) => Set(s)
        case _ => Set()
      }
    }

    var p = Array(msg)
    for (c <- commands) {
      p = p.flatMap(_.split(c))
    }
    val set = p.tail.toSet
    set.flatMap(sub(_))
  }

  def ticketsIn(msg: String): Set[String] = {
    ticketReg.r.findAllIn(msg).matchData.map("SI-"+_.group(1)).toSet
  }
}
