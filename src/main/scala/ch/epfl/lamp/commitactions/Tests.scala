package ch.epfl.lamp.commitactions

object Tests {
  import Main.{username, password}
  
  val user = "habasch"
  
  def main(args: Array[String]) {
    val fecruSession = new AuthSession(username, password)
    val fecruActions = new fecru.Actions(fecruSession)
    println(fecruSession.doHttp("https://codereview.scala-lang.org/fisheye/rest-service/users-v1/" + user))
    println(fecruActions.userExists(user))
  }
}
