package firesword.scalajsdomext

import firesword.app.Editor.failOnUnsuccessfulResponse
import org.scalajs.dom.experimental.Fetch.fetch
import org.scalajs.dom.experimental.Response

import scala.concurrent.Future
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global

object Fetch {

  private def failOnUnsuccessfulResponse(response: Response): Response = {
    if (response.ok) {
      response
    } else {
      throw new Exception("Fetch error")
    }
  }

 private def fetchResponse(path: String): Future[Response] = {
    fetch(path).toFuture.map(failOnUnsuccessfulResponse)
  }

  def fetchArrayBuffer(path: String): Future[ArrayBuffer] = {
    for (
      response <- fetchResponse(path);
      arrayBuffer <- response.arrayBuffer().toFuture
    ) yield arrayBuffer
  }

  def fetchJson(path: String): Future[Any] = {
    for (
      response <- fetchResponse(path);
      json <- response.json().toFuture
    ) yield json
  }
}
