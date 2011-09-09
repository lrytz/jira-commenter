package ch.epfl.lamp.commitactions
package fecru

object Actions {
  val projectKey = "SR"
  val repository = "scala-svn"
    
  private val noReview = List("no review", "without review", "noreview")
  
  def needReview(commitMessage: String): Boolean = {
    val msg = commitMessage.toLowerCase
    noReview.exists(msg.contains(_))
  }

    
  private def parseReviewers(commitMessage: String): List[String] = {
    val init = "review by "
    val msg = commitMessage.toLowerCase
    val i = msg.indexOf(init)
    if (i < 0) Nil
    else {
      val rest = msg.substring(i + init.length, msg.length)
        // [\w-_]: word character or hyphen or underscore (todo: should allow . some users have dots, but not at the end of a name)
        // \s: whitespace (including line break)
        val names = """([\w-_]+),? ?"""
        // (?:   ): an ignored group
        val Reg = ("""((?:"""+ names +""")*)(?:.|\s)*""").r
        rest match {
          case Reg(s, _) =>
            // the second group is a repeated group, the variable only contains the last
            // matching substring, i.e. the last name. so we match again to get them all.
            val Names = names.r
            Names.findAllIn(s).matchData.map(_.group(1)).toList
          case _ => Nil
        }
    }
  }
  
  
}

class Actions(session: Session) {
  def createReview(authorCreator: String, commitMessage: String, changeset: Int, startReview: Boolean): String = {
/*      val nonEmptyLine =
      ((if (community) "[community] " else "") +
        changeset.getComment.split("\n").find(line => {
          org.apache.commons.lang.StringUtils.isNotBlank(line)
        }).getOrElse("")).take(200) // review name should not be longer than 255 (for the database)
*/
    ""
  }
  
  def addComment(reviewId: String, message: String): Unit = ()
  
  def addReviewers(reviewId: String, reviewers: List[String], community: Boolean): Unit = ()
  
  def userExists(user: String): Boolean = true

  def parseMessage(commitMessage: String, commiter: String): (List[String], String, Boolean) = {
    val comment = new StringBuffer()

    var community = false
    val reviewUsers = Actions.parseReviewers(commitMessage).flatMap(reviewer => {
      if (reviewer == "community") {
        community = true
        None
      } else if (reviewer == commiter) {
        println("\nignoring reviewer "+ reviewer +", as he is the committer.")
        None
      } else {
        if (userExists(reviewer)) {
          Some(reviewer)
        } else {
          val msg = "no curcible user found for reviewer %s.".format(reviewer)
          comment.append("{color:red}*review creator error*{color}: ").append(msg).append("\n")
          println("\n"+ msg)
          None
        }
      }
    })
    (reviewUsers, comment.toString(), community)
  }
}
