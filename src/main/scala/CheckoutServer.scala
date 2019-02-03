import cats.effect.IO
import fs2.StreamApp
import org.http4s.server.blaze.BlazeBuilder

import scala.concurrent.ExecutionContext

object CheckoutServer extends StreamApp[IO] {

  import scala.concurrent.ExecutionContext.Implicits.global

  def stream(args: List[String], requestShutdown: IO[Unit]) = ServerStream.stream

}

object ServerStream {
  def stream(implicit ex: ExecutionContext) =
    BlazeBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .mountService(CheckoutService.service(), "/")
      .serve
}
