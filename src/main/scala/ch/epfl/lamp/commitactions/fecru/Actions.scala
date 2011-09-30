package ch.epfl.lamp.commitactions
package fecru

import java.net.{HttpURLConnection}
import xml.Node

object Actions {
  val projectKey = "SR"
  val repository = "scala-svn"

  // local testing
  //val projectKey = "CR"
  //val repository = "checkstyle"
    
  // val metricsVersion = 1
    
  val waitTimeoutSecs = 160

  // review name should not be longer than 255 (for the database)
  val reviewNameSize = 200

  private val noReview = List("no review", "without review", "noreview")

  def needReview(commitMessage: String): Boolean = {
    val msg = commitMessage.toLowerCase
    !noReview.exists(msg.contains(_))
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
      val Reg = ("""((?:""" + names + """)*)(?:.|\s)*""").r
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

class Actions(session: Session, crucibleBaseURL: String = "https://codereview.scala-lang.org/fisheye/") {
  def createReview(authorCreator: String, commitMessage: String, changeSet: Int, startReview: Boolean, community: Boolean): String = {

    val reviewName =
      ((if (community) "[community] " else "") +
        commitMessage.split("\n").find(line => {
          line.trim.length > 0
        }).getOrElse("Unnamed review! (Commit message was empty)")).take(Actions.reviewNameSize)

    /*
          <allowReviewersToJoin>true</allowReviewersToJoin>

          <createDate>
            {new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").format(new java.util.Date)}
          </createDate>

          <metricsVersion>{Actions.metricsVersion}</metricsVersion>
     */
        
    val requestXML =
      <createReview>
        <reviewData>
          <author>
            <userName>{authorCreator}</userName>
          </author>
          <creator>
            <userName>{authorCreator}</userName>
          </creator>
          <description>{commitMessage}</description>
          <name>{reviewName}</name>
          <projectKey>{Actions.projectKey}</projectKey>
          <state>{if(startReview) "Review" else "Draft"}</state>
          <type>REVIEW</type>
        </reviewData>
        <changesets>
          <changesetData>
            <id>{changeSet}</id>
          </changesetData>
          <repository>{Actions.repository}</repository>
        </changesets>
      </createReview>

    val (responseCode: Int, response) =
      session.doHttp(crucibleBaseURL + "rest-service/reviews-v1/", "POST", Some(getXMLString(requestXML)), "application/xml")

    if (Session.isSuccess(responseCode)) {
      // parse response
      val responseXML = scala.xml.XML.loadString(response)
      val id = (responseXML \ "permaId" \ "id").text

      if (id.trim.length == 0) throw new RuntimeException("Unable to locate id in: createReview response!!!")

      id
    } else {
      throw new RuntimeException(response)
    }
  }

  def addComment(reviewId: String, message: String, commenter: String) {
    val requestXML =
      <generalCommentData>
        <message>{message}</message>
        <user>
          <userName>{commenter}</userName>
        </user>
      </generalCommentData>

    val (responseCode: Int, response) =
      session.doHttp(crucibleBaseURL + "rest-service/reviews-v1/" + reviewId + "/comments", "POST", Some(getXMLString(requestXML)), "application/xml")

    checkResponse(responseCode, "while adding comment by "+ commenter +" to "+ reviewId +": "+ message)
  }

  def addReviewers(reviewId: String, reviewers: List[String]) {
    if (reviewers.nonEmpty) {
      val (responseCode: Int, data) = session.doHttp(crucibleBaseURL + "rest-service/reviews-v1/" + reviewId +"/reviewers", "POST", Some(reviewers.mkString(",")), "application/xml")
      checkResponse(responseCode, "while adding reviewers to "+ reviewId +": "+ reviewers)
    }
  }

  def userExists(user: String): Boolean =
    session.doHttp(crucibleBaseURL + "rest-service/users-v1/" + user)._1 == HttpURLConnection.HTTP_OK
    
  def changeSetExists(changeSet: Int, repository: String = Actions.repository) =
    session.doHttp(crucibleBaseURL +"rest-service-fe/revisionData-v1/changeset/"+ repository +"/"+ changeSet)._1 == HttpURLConnection.HTTP_OK

  def waitForCommit(changeSet: Int)(op: => Unit) {
    var timeout = 10
    while (!changeSetExists(changeSet) && timeout <= Actions.waitTimeoutSecs) {
      println("changeset not found, waiting for "+ timeout +" seconds")
      Thread.sleep(timeout * 1000)
      timeout *= 2
    }
    if (changeSetExists(changeSet)) op
    else throw new RuntimeException("Changeset "+ changeSet +" not found in crucible")
  }
    
  def parseMessage(commitMessage: String, commiter: String): (List[String], String, Boolean) = {
    var commentLines = List[String]()

    var community = false
    val reviewUsers = Actions.parseReviewers(commitMessage).flatMap(reviewer => {
      if (reviewer == "community") {
        community = true
        None
      } else if (reviewer == commiter) {
        println("\nignoring reviewer " + reviewer + ", as he is the committer.")
        None
      } else {
        if (userExists(reviewer)) {
          Some(reviewer)
        } else {
          val msg = "no curcible user found for reviewer %s.".format(reviewer)
          commentLines  = ("{color:red}*Review creator error*{color}: " + msg) :: commentLines
          None
        }
      }
    })
    (reviewUsers, commentLines.mkString("\n"), community)
  }

  private def checkResponse(responseCode: Int, details: String) {
    if (!Session.isSuccess(responseCode))
      throw new RuntimeException("Web service responded with error code " + responseCode +" ["+ details +"]")
  }

  private def getXMLString(node: Node): String = {
    val stringWriter = new java.io.StringWriter()
    scala.xml.XML.write(stringWriter, node, "UTF-8", true, null)
    stringWriter.toString
  }

}
