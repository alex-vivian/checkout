import cats.effect.IO
import org.http4s.HttpService
import org.http4s.dsl.Http4sDsl
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._

object CheckoutService extends Http4sDsl[IO] {

  val basket = Basket.apply()

  case class Item(SKU: String)
  implicit val itemDecoder = jsonOf[IO, Item]

  case class dbSKU(SKU: String)
  implicit val dbSKUDecoder = jsonOf[IO, dbSKU]

  implicit val priceDecoder = jsonOf[IO, Price]

  case class oUpdate(SKU: String, offer: Offer)
  implicit val oUpdateDecoder = jsonOf[IO, oUpdate]
  implicit val offerDecoder   = jsonOf[IO, Offer]

  case class pUpdate(SKU: String, cost: Double)
  implicit val pUpdateDecoder = jsonOf[IO, pUpdate]

  def service(): HttpService[IO] =
    HttpService[IO] {
      case GET -> Root / "menu" =>
        Ok().withBody(basket.availableItems().asJson.noSpaces)

      case request @ POST -> Root / "addItem" =>
        for {
          item <- request.as[Item]
          resp <- Ok(addToBasket(item.SKU))
        } yield (resp)

      case request @ POST -> Root / "removeItem" =>
        for {
          item <- request.as[Item]
          resp <- Ok(removeFromBasket(item.SKU))
        } yield (resp)

      case GET -> Root / "totalPrice" =>
        Ok(totalPrice())

      case GET -> Root / "basket" =>
        Ok(basketContents())

      case DELETE -> Root / "clearBasket" =>
        Ok(clearBasket())

      case request @ POST -> root / "deleteItemDB" =>
        for {
          sku  <- request.as[dbSKU]
          resp <- Ok(deleteItemDB(sku.SKU))
        } yield (resp)

      case request @ POST -> root / "addItemDB" =>
        for {
          price <- request.as[Price]
          resp  <- Ok(addItemDB(price))
        } yield (resp)

      case request @ POST -> root / "removeOfferDB" =>
        for {
          sku  <- request.as[dbSKU]
          resp <- Ok(removeOfferDB(sku.SKU))
        } yield (resp)

      case request @ POST -> root / "addOfferDB" =>
        for {
          offer <- request.as[oUpdate]
          resp  <- Ok(addOfferDB(offer.SKU, offer.offer))
        } yield (resp)

      case request @ POST -> root / "updatePriceDB" =>
        for {
          price <- request.as[pUpdate]
          resp  <- Ok(updatePriceDB(price.SKU, price.cost))
        } yield (resp)

    }

  def addToBasket(item: String): String =
    if (basket.itemExists(item)) {
      basket.addToBasket(item)
      basketContents()
    } else "Invalid Item"

  def removeFromBasket(item: String): String = {
    basket.removeFromBasket(item)
    basketContents()
  }

  def totalPrice(): String =
    basket.totalPrice().asJson.noSpaces

  def basketContents() = {
    val c = basket.contents()

    if (c.isEmpty) "Empty Basket".asJson.noSpaces
    else c.asJson.noSpaces
  }

  def clearBasket(): String = {
    basket.clearBasket()
    basketContents()
  }

  def availableItems(): String =
    basket.availableItems().asJson.noSpaces

  def deleteItemDB(sku: String): String = {
    basket.removeAvailableItem(sku)
    availableItems
  }

  def addItemDB(item: Price): String = {
    basket.addAvailableItem(item)
    availableItems
  }

  def removeOfferDB(sku: String): String = {
    basket.removeOffer(sku)
    availableItems
  }

  def addOfferDB(sku: String, offer: Offer): String = {
    basket.addOffer(sku, offer)
    availableItems()
  }

  def updatePriceDB(sku: String, cost: Double): String = {
    basket.updatePrice(sku, cost)
    availableItems()
  }

}
