import cats.effect.Effect
import org.http4s.HttpService
import org.http4s.dsl.Http4sDsl



class CheckoutService[F[_]: Effect] extends Http4sDsl[F] {

  val basket = Basket.apply()

  def service(): HttpService[F] =
    HttpService[F] {
      case GET -> Root / "menu" =>
        Ok(basket.availableItems)
      case GET -> Root / "addItem" / itemName =>
        Ok(addToBasket(itemName))
      case GET -> Root / "removeItem" / itemName =>
        Ok(removeFromBasket(itemName))
      case GET -> Root / "totalPrice" =>
        Ok(totalPrice())
      case GET -> Root / "basket" =>
        Ok(
          basketContents())
      case GET -> Root / "clearBasket" =>
        Ok(clearBasket())

    }

  def addToBasket(item: String): String = {
    if(basket.itemExists(item)) {
      basket.addToBasket(item)
      basketContents()
    }
    else "Invalid Item"

  }

  def removeFromBasket(item: String): String = {
    basket.removeFromBasket(item)
    basketContents()
  }

  def totalPrice(): String =
    "Total cost: " + basket.totalPrice().toString

  def basketContents(): String =
    basket.contents()

  def clearBasket() = {
    basket.clearBasket()
    basketContents()
  }

}
