package ch.epfl.lamp.commitactions

import util.parsing.json._
import JSONTools._

object Jenkins {
  def getCommitData(url: String): List[Commit] = {
    val session = new Session()
    parseJson(session.doHttp(url)._2) match {
      case JSONObject(build) =>
        build("changeSet") match {
          case JSONObject(changes) =>
            changes("items") match {
              case JSONArray(changesets) =>
                changesets.map({
                  case JSONObject(changeset) =>
/* for git
                    val comitter = changeset("author") match {
                      // @TODO: instead of "fullname", take "absoluteURL/api/json", and get the real username from there
                      case JSONObject(author) => author("fullName").toString
                    }
                    Commit(comitter, changeset("id").toString, changeset("msg").toString)
*/
                    // somehow, the revision has the format "12345.0", so going via double
                    Commit(changeset("user").toString, changeset("revision").toString.toDouble.toInt, changeset("msg").toString)
                }).toList
            }
        }
    }
  }

  case class Commit(user: String, revision: Int, message: String) {
    override def toString = "("+ user + " in [r" + revision +
      "|https://codereview.scala-lang.org/fisheye/changelog/scala-svn?cs="+
      revision +"]) "+ message
  }
}
