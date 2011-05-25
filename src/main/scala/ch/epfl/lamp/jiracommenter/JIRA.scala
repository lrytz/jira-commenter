package ch.epfl.lamp.jiracommenter

import scala.util.parsing.json._
import JSONTools._

// cobbled together using the internets (mostly stackoverflow)
// http://stackoverflow.com/questions/5564074/scala-http-operations/5571782#5571782
// http://stackoverflow.com/questions/4883100/how-to-handle-http-authentication-using-httpurlconnection/4883890#4883890
// JIRA's REST docs: http://docs.atlassian.com/jira/REST/latest/ (note that I couldn't figure out how to login by posting application/json -- using crude https basic auth)

object JIRA {
  private def getRequiredFieldNames(in: Any) = in match {
    case JSONArray(fields) => fields flatMap {
      case JSONObject(obj) if obj("required").toString.toBoolean => Some(obj("id").toString)
      case _ => None
    }
  }

  def parsePossibleTransitions(in: JSONType): List[Transition] = {
    in match {
      case JSONObject(m) => (m map {
        case (id, JSONObject(trans)) =>
          new Transition(id.toInt, trans("name").toString, getRequiredFieldNames(trans("fields")))
      }).toList

      case _ =>
        List()
    }
  }

  case class Transition(id: Int, name: String, requiredFields: List[String]) {
    private def toJSON(comment:String, fieldValues: List[String]) = {
      assert(requiredFields.length == fieldValues.length)
      val commentData = if (comment.isEmpty) Nil else List("comment" -> comment)
      JO("transition" -> id.toString :: "fields" -> JO(requiredFields.zip(fieldValues) :_*) :: commentData :_*)
    }

    def isClose = name == "Close Issue"
    def mkClose(comment: String, resolution: String) = {
      assert(isClose)
      toJSON(comment, List(resolution))
    }

    def isReopen = name == "Reopen"
    def mkReopen(comment: String) = {
      assert(isReopen)
      toJSON(comment, List())
    }

    def isComment = name == "ID"
    def mkComment(comment: String) = {
      assert(isComment)
      toJSON(comment, List())
    }
  }
}

class JIRA(val session: AuthSession, val baseUrl: String = "https://issues.scala-lang.org/rest/api/2.0.alpha1/") {
  import JIRA._

  def transitionsFor(issue: String) = {
    parsePossibleTransitions(parseJson(session.doHttp(baseUrl+"issue/"+issue+"/transitions")))
  }

  private def updateIssue(issue: String, data: JSONObject) {
    session.doHttp(baseUrl+"issue/"+issue+"/transitions", "POST", Some(data.toString), "application/json")
  }

  def closeIssue(issue: String, comment: String, resolution: String = "Fixed") {
    transitionsFor(issue).find(_.isClose) match {
      case Some(trans) => updateIssue(issue, trans.mkClose(comment, resolution))
      case None => ()
    }
  }

  def reopenIssue(issue: String, comment: String = "") {
    transitionsFor(issue).find(_.isReopen) match {
      case Some(trans) => updateIssue(issue, trans.mkReopen(comment))
      case None => ()
    }
  }

  def commentIssue(issue: String, comment: String) {
    transitionsFor(issue).find(_.isComment) match {
      case Some(trans) => updateIssue(issue, trans.mkComment(comment))
      case None => ()
    }
  }
}
