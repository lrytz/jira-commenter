package ch.epfl.lamp.jiracommenter

trait CommitParser {
  val closeCommand = List("closes", "closed", "close", "fixes", "fixed", "fix")
  val reopenCommand = List("reopens", "reopened", "reopen")

  val ticketReg = """(?:#|si-|si_|si)(\d+)"""

  def analyze(msg: String) = {
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

object TestCommitParser extends App with CommitParser {
  lazy val commits = {
    // the file with the commit messages is not included in the repo, 
    // as it is huge and can easily be generated: `git log --format="%B##################################################" > /tmp/msgs`
    val iter = io.Source.fromFile("/tmp/msgs").getLines()
    var res = List[String]()
    while(iter.hasNext) res = iter.takeWhile(!_.startsWith("##################################################")).mkString("\n") :: res
    res.view.reverse
  }

  for (msg <- commits) {
    val (closed, reopened, mentioned) = analyze(msg)

    println("##################################################")
    println(msg)
    println("##################################################")
    def str(s: Set[String]) = if(s isEmpty) "/" else s.mkString(", ")
    println((str(closed), str(reopened), str(mentioned))+"<-- (closed, reopened, mentioned)")
  }
}