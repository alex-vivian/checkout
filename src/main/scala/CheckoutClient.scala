import cats.effect._
import io.circe.Encoder
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveEncoder
import org.http4s.Status.{NotFound, Successful}
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.blaze._
import io.circe.syntax._
import org.http4s.circe._

object CheckoutClient {
  val httpClient: Client[IO] = Http1Client[IO]().unsafeRunSync

  case class Embedded(programmes: List[ProgrammeID])
  case class ProgrammeID(programmeId: String)
  case class MostPopular(_embedded: Embedded)
  implicit val Encoder: Encoder[MostPopular] = deriveEncoder[MostPopular]

  def thing(client: Client[IO]): IO[String] = IO {

    val otherThing = client.get(Uri.uri("http://pedro.prd.hubsvc.itv.com/popularity/itvonline/programmes")) {
      case Successful(resp) =>
        resp.decodeJson[MostPopular].map(x => x.asJson.toString())
      case NotFound(_) =>
         IO.pure("Not Found")
      case resp => IO.pure("Failed: " + resp.status)
    }

    otherThing.unsafeRunSync()
  }

}
