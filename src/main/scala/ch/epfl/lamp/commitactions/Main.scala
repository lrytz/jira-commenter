package ch.epfl.lamp.commitactions

object Main {
  val username = "anonymous"
  val password = "PassWord00"

  def main(args: Array[String]) {
//    val buildURL = "https://scala-webapps.epfl.ch/jenkins/job/scala-checkin/4112/api/json"
    val buildURL = args(0)

    val commits = Jenkins.getCommitData(buildURL)

    println("commits:")
    println(commits)

    val jiraSession = new AuthSession(username, password)
    val jiraActions = new jira.JIRA(jiraSession)

    for (commit <- commits) {
      val (closed, reopened, mentioned) = jira.CommitParser.analyze(commit.message.toLowerCase)

      println("\ntickets to be closed, reopened and commented:")
      println(closed)
      println(reopened)
      println(mentioned)

      for (c <- closed) {
        println(c +", "+ commit.toString)
        jiraActions.closeIssue(c, commit.toString)
      }
      for (r <- reopened) {
        println(r +", "+ commit.toString)
        jiraActions.reopenIssue(r, commit.toString)
      }
      for (m <- mentioned) {
        println(m +", "+ commit.toString)
        jiraActions.commentIssue(m, commit.toString)
      }
    }
    
    val fecruSession = new AuthSession(username, password)
    val fecruActions = new fecru.Actions(fecruSession)
    
    for (commit <- commits) {
      if(fecru.Actions.needReview(commit.message)) {
        val (reviewers, comment, community) = fecruActions.parseMessage(commit.message, commit.user)
        println("\nparsed message:")
        println(reviewers)
        println(comment)
        println(community)
        val startReview = reviewers.nonEmpty || community

        val id = fecruActions.createReview(commit.user, commit.message, commit.revision, startReview)
        println("\ncreated review "+ id)

        if (comment.nonEmpty)
          fecruActions.addComment(id, comment)
        
        fecruActions.addReviewers(id, reviewers, community)
      }
      
    }
  }
}
