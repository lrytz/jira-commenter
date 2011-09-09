package ch.epfl.lamp.commitactions

import scala.util.parsing.json._

object JSONTools {
  def JO(elems: (String, Any)*) = new JSONObject(Map(elems :_*))

  def parseJson(str: String): JSONType = {
    val p = new Parser()
    p.root(new p.lexical.Scanner(str)) map {case jo: JSONType => jo } getOrElse JSONObject(Map())
  }
}
