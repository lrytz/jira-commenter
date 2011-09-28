package ch.epfl.lamp.commitactions

import java.net.{HttpURLConnection, URL, URLConnection}
import java.io.OutputStreamWriter
import collection.JavaConversions._
import io.Source

class Session(val userAgent: String = "", val encoding: String = "UTF-8", val requestTimeout: Int = 15000) {
  var cookies = Map[String, String]()

  def loadCookies(conn: URLConnection) = {
    for ((name, value) <- cookies) conn.setRequestProperty("Cookie", name + "=" + value)
  }

  def saveCookies(conn: URLConnection) = {
    conn.getHeaderFields.lift("Set-Cookie") match {
      case Some(cList) => cList foreach { c =>
        val (name,value) = c span { _ != '=' }
        cookies += name -> (value drop 1)
      }
      case None =>
    }
  }

  def setupConnection(conn: URLConnection) = {
    conn.setConnectTimeout(requestTimeout)
    conn.setRequestProperty("User-Agent", userAgent)
  // conn.setChunkedStreamingMode(0)
  }

  def doHttp(url: String, method: String = "GET", outputData: Option[String] = None, contentType: String = ""): (Int, String) = {
    val conn = (new URL(url)).openConnection.asInstanceOf[HttpURLConnection]

    setupConnection(conn)
    conn.setRequestMethod(method)
    if(contentType nonEmpty) conn.setRequestProperty("Content-Type", contentType)

    loadCookies(conn)

    conn.setDoOutput(outputData nonEmpty) // doInput is true by default

    conn.connect()

    for(data <- outputData) yield {
      val wr = new OutputStreamWriter(conn.getOutputStream())
      wr.write(data)
      wr.flush
    }

    saveCookies(conn)
    val responseCode = conn.getResponseCode
    val res = Source.fromInputStream(conn.getInputStream).getLines().mkString
    conn.disconnect()

    (responseCode, res)
  }
}

class AuthSession(val username: String, val password: String, userAgent: String = "", encoding: String = "UTF-8", requestTimeout: Int = 15000)
extends Session(userAgent, encoding, requestTimeout) {
  override def setupConnection(conn: URLConnection) = {
    super.setupConnection(conn)
    val userPassword = username + ":" + password
    conn.setRequestProperty("Authorization", "Basic " + (new sun.misc.BASE64Encoder()).encode(userPassword.getBytes()))
  }
}
