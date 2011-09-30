package ch.epfl.lamp.commitactions

object LocalTest {
  
  val localURL = "http://localhost:3990/fecru/"
  
  def main(args: Array[String]) {
    
    val commit = Jenkins.Commit("oburn", 2508, "lkjdflk slkdfjlksdjf sdfklj review with review by admin. slkdfjah.")
    testCodeReview(commit)
  }
  
  def testCodeReview(commit: Jenkins.Commit) {
    val fecruSession = new AuthSession("admin", "admin")
    val fecruActions = new fecru.Actions(fecruSession, localURL)

    if(fecru.Actions.needReview(commit.message)) {
        
      fecruActions.waitForCommit(commit.revision) {
        
        val (reviewers: scala.List[String], comment: String, community) = fecruActions.parseMessage(commit.message, commit.user)
        println("\nparsed message:")
        println(reviewers)
        println(comment)
        println(community)
        val startReview = reviewers.nonEmpty || community
        
        val id = fecruActions.createReview(commit.user, commit.message, commit.revision, startReview, community)
        println("\ncreated review "+ id)

        if (comment.nonEmpty)
          fecruActions.addComment(id, comment, commit.user)

        if (!startReview) {
          val txt = "{color:red}*Review creator error*{color}: no reviewers specified. Please add them manually and click \"Start Review\"."
          fecruActions.addComment(id, txt, commit.user)
        }
        
        fecruActions.addReviewers(id, reviewers)
      }
    }
  }
}