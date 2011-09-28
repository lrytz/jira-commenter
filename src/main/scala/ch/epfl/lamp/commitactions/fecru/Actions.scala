package ch.epfl.lamp.commitactions
package fecru

import java.net.{HttpURLConnection}
import xml.Node

object Actions {
  val projectKey = "SR"
  val repository = "scala-svn"
  val metricsVersion = 1

  // review name should not be longer than 255 (for the database)
  val reviewNameSize = 200

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

class Actions(session: Session, crucibleBaseURL: String) {
  def createReview(authorCreator: String, commitMessage: String, changeSet: Int, startReview: Boolean): String = {

    val reviewName =
      ((if (true) "[community] " else "") +
        commitMessage.split("\n").find(line => {
          line.trim.length > 0
        }).getOrElse("Unnamed review! (Commit message was empty)")).take(Actions.reviewNameSize)

    val requestXML =
      <createReview>
        <reviewData>
          <allowReviewersToJoin>true</allowReviewersToJoin>
          <author>
            <userName>
              {authorCreator}
            </userName>
          </author>
          <createDate>
            {new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").format(new java.util.Date)}
          </createDate>
          <creator>
            <userName>
              {authorCreator}
            </userName>
          </creator>
          <description>
            {commitMessage}
          </description>
          <metricsVersion>
            {Actions.metricsVersion}
          </metricsVersion>
          <name>
            {reviewName}
          </name>
          <projectKey>
            {Actions.projectKey}
          </projectKey>
          <type>REVIEW</type>
        </reviewData>
        <changesets>
          <changesetData>
            <id>
              {changeSet}
            </id>
          </changesetData>
          <repository>
            {Actions.repository}
          </repository>
        </changesets>
      </createReview>

    val (responseCode: Int, response) =
      session.doHttp(crucibleBaseURL + "/reviews-v1/", "POST", Some(getXMLString(requestXML)), "application/xml")

    // parse response
    val responseXML = scala.xml.XML.loadString(response)
    val id = (responseXML \ "permaId" \ "id").text

    if (id.trim.length == 0) throw new RuntimeException("Unable to locate id in: createReview response!!!")

    id
  }

  def addComment(reviewId: String, message: String) {
    val requestXML =
      <generalCommentData>
        <message>
          {message}
        </message>
      </generalCommentData>

    val (responseCode: Int, response) =
      session.doHttp(crucibleBaseURL + "/reviews-v1/" + reviewId + "/comments", "POST", Some(getXMLString(requestXML)), "application/xml")

    checkResponse(responseCode)
  }

  def addReviewers(reviewId: String, reviewers: List[String], community: Boolean) {
    val (responseCode: Int, data) = session.doHttp(crucibleBaseURL + "reviews-v1/" + reviewId, "POST", Some(reviewers.mkString(",")))
    checkResponse(responseCode)
  }

  def userExists(user: String): Boolean =
    session.doHttp(crucibleBaseURL + "users-v1/" + user)._1 == HttpURLConnection.HTTP_OK

  def parseMessage(commitMessage: String, commiter: String): (List[String], String, Boolean) = {
    val comment = new StringBuffer()

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
          comment.append("{color:red}*review creator error*{color}: ").append(msg).append("\n")
          println("\n" + msg)
          None
        }
      }
    })
    (reviewUsers, comment.toString(), community)
  }

  private def checkResponse(responseCode: Int) {
    if (responseCode != HttpURLConnection.HTTP_OK)
      throw new RuntimeException("Web service responded with error code " + responseCode)
  }

  private def getXMLString(node: Node): String = {
    val stringWriter = new java.io.StringWriter()
    scala.xml.XML.write(stringWriter, node, "UTF-8", true, null)
    stringWriter.toString
  }

}
