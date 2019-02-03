import com.github.benmanes.caffeine.cache.Caffeine
import doobie.imports._
import io.circe._
import scalacache._
import scalacache.caffeine.CaffeineCache
import scalacache.modes.sync._


case class Price(SKU: String, price: Double, offerPrice: Option[Offer])

object Price {
  import io.circe.generic.semiauto.deriveEncoder
  implicit val priceEncoder: Encoder[Price] = deriveEncoder[Price]
}

case class Offer(units: Int, price: Double)

object Offer {
  import io.circe.generic.semiauto.deriveEncoder

  implicit val offerEncoder: Encoder[Offer] = deriveEncoder[Offer]
}
case class Purchase(SKU: String, quant: Int)

object Purchase {
  import io.circe.generic.semiauto.deriveEncoder

  implicit val offerEncoder: Encoder[Purchase] = deriveEncoder[Purchase]
}

case class Basket(price: Vector[Price]) {

  //val prices = if (price == Vector()) Basket.getPrices() else price

  val purchases: Cache[Vector[Purchase]] = CaffeineCache(Caffeine.newBuilder.build[String, Entry[Vector[Purchase]]])

  def addToBasket(item: String) =
    Basket.addToBasket(item, purchases, Basket.getPrices())

  def itemExists(item: String): Boolean =
    Basket.itemExists(item, Basket.getPrices())

  def removeFromBasket(item: String) =
    Basket.removeFromBasket(item, purchases)

  def availableItems(): List[Price] =
    Basket.availableItems(Basket.getPrices(): Vector[Price])

  def clearBasket() =
    Basket.clearBasket(purchases)

  def contents() =
    Basket.contents(purchases)

  def totalPrice(): Double =
    Basket.totalPrice(Basket.getPrices(), purchases)

  def addAvailableItem(p: Price): Int = {
    Basket.addAvailableItem(p)
  }

  def removeAvailableItem(p: String): Int = {
    Basket.removeAvailableItem(p)
  }

  def addOffer(sku: String, o: Offer): Int = {
     Basket.addOffer(sku, o)
  }

  def removeOffer(sku: String): Int = {
    Basket.removeOffer(sku)
  }

  def updatePrice(sku: String, p: Double): Int = {
    Basket.updatePrice(sku, p)
  }

}

object Basket {
  def apply(): Basket = new Basket(Vector())

  //removes entry for basket from cache
  def clearBasket(purchases: Cache[Vector[Purchase]]) = {
    val inBasket = purchases.get("basket")
    inBasket match {
      case None           => {}
      case Some(allItems) => purchases.remove("basket")
    }
  }

  //combines price strings for all items
  def availableItems(prices: Vector[Price]): List[Price] =
    prices.toList

  //returns string containing all items in purchased along with their quantity
  def contents(purchases: Cache[Vector[Purchase]]): List[Purchase] = {
    val inBasket = purchases.get("basket")
    inBasket match {
      case None           => List()
      case Some(allItems) => allItems.toList

    }
  }

  //returns the price object for a given item
  def getPrice(SKU: String, prices: Vector[Price]): Option[Price] = {

    def loop(i: Int): Option[Price] =
      if (i == prices.length) None
      else {
        prices(i).SKU match {
          case x if x.equals(SKU) => Some(prices(i))
          case _                  => loop(i + 1)
        }
      }
    loop(0)
  }

  //returns total price for items selected
  def totalPrice(prices: Vector[Price], purchases: Cache[Vector[Purchase]]): Double = {

    val inBasket = purchases.get("basket")

    def loop(i: Int, allItems: Vector[Purchase]): Double =
      if (i == allItems.size) 0
      else {
        val itemPrice = getPrice(allItems(i).SKU, prices)
        itemPrice match {
          case Some(Price(x, y, None)) =>
            allItems(i).quant * y + loop(i + 1, allItems)

          case Some(Price(x, y, Some(Offer(a, b)))) =>
            offerPrice(allItems(i).quant, y, Offer(a, b)) + loop(i + 1, allItems)

          case None => loop(i + 1, allItems)
        }
      }

    inBasket match {
      case None           => 0
      case Some(allItems) => loop(0, allItems)
    }
  }

  //calculates the total cst for a single item type including any offers
  def offerPrice(units: Int, basePrice: Double, offer: Offer): Double = {
    val remainder  = units % offer.units
    val offerUnits = (units - remainder) / offer.units
    (remainder * basePrice) + (offerUnits * offer.price)
  }

  def removeFromExistingBasket(allItems: Vector[Purchase], Item: String): Vector[Purchase] = {

    def loop(i: Int): Vector[Purchase] =
      if (i == allItems.size) allItems
      else {
        allItems(i) match {
          case Purchase(Item, x) =>
            if (x == 1) allItems.filterNot(_.equals(Purchase(Item, 1)))
            else allItems.updated(i, Purchase(Item, x - 1))
          case _ => loop(i + 1)
        }
      }
    loop(0)
  }

  def removeFromBasket(item: String, purchases: Cache[Vector[Purchase]]) = {
    val inBasket = purchases.get("basket")

    inBasket match {
      case None => purchases
      case Some(allItems) =>
        purchases.remove("basket")
        purchases.put("basket")(removeFromExistingBasket(allItems, item), None)
    }
  }

  //returns true if item is present in prices and false if not
  def itemExists(item: String, prices: Vector[Price]): Boolean = {

    def loop(i: Int): Boolean =
      if (i == prices.size) false
      else {
        prices(i).SKU match {
          case x if x.equals(item) => true
          case _                   => loop(i + 1)
        }
      }
    loop(0)
  }

  def addToBasket(item: String, purchases: Cache[Vector[Purchase]], prices: Vector[Price]) = {

    val inBasket = purchases.get("basket")

    if (itemExists(item, prices)) {
      inBasket match {
        case None =>
          purchases.put("basket")(Vector(Purchase(item, 1)), None)

        case Some(allItems) =>
          purchases.put("basket")(addToExistingBasket(allItems, item), None)
      }
    }

  }

  def addToExistingBasket(allItems: Vector[Purchase], Item: String): Vector[Purchase] = {

    def loop(i: Int): Vector[Purchase] =
      if (i == allItems.size) allItems :+ Purchase(Item, 1)
      else {
        allItems(i) match {
          case Purchase(Item, x) => allItems.updated(i, Purchase(Item, x + 1))
          case _                 => loop(i + 1)
        }
      }

    loop(0)
  }

  def getPrices(): Vector[Price] = {

    val xa = DriverManagerTransactor[IOLite]("org.postgresql.Driver", "jdbc:postgresql:checkout", "alexvivi", "")

    val priceRule = sql"SELECT * FROM items"
      .query[(Option[String], Option[Double], Option[Int], Option[Double])]
      .list
      .transact(xa)
      .unsafePerformIO

    def loop(i: Int): Vector[Price] =
      if (i < priceRule.size) {
        priceRule(i) match {
          case (Some(w), Some(x), Some(y), Some(z)) =>
            Price(w, x, Some(Offer(y, z))) +: loop(i + 1)
          case (Some(w), Some(x), None, None) =>
            Price(w, x, None) +: loop(i + 1)
          case (_, _, _, _) =>
            loop(i + 1)
        }
      } else Vector()

    loop(0)
  }

  def addAvailableItem(p: Price): Int =
  {
    val xa = DriverManagerTransactor[IOLite]("org.postgresql.Driver", "jdbc:postgresql:checkout", "alexvivi", "")

    p match {
      case Price(x, y, None) =>
        sql"INSERT INTO items VALUES($x, $y)".update.run.transact(xa).unsafePerformIO
      case Price(x, y , Some(Offer(a, b))) =>
        sql"INSERT INTO items VALUES($x, $y, $a, $b)".update.run.transact(xa).unsafePerformIO
    }


  }

  def removeAvailableItem(p: String): Int =
  {
    val xa = DriverManagerTransactor[IOLite]("org.postgresql.Driver", "jdbc:postgresql:checkout", "alexvivi", "")
    sql"DELETE FROM items where sku = $p".update.run.transact(xa).unsafePerformIO
  }

  def addOffer(sku: String, o: Offer): Int = {
    val xa = DriverManagerTransactor[IOLite]("org.postgresql.Driver", "jdbc:postgresql:checkout", "alexvivi", "")
    sql"UPDATE items SET offer_units =${o.units}, offer_price = ${o.price} WHERE sku = $sku".update.run.transact(xa).unsafePerformIO
  }

  def removeOffer(sku: String): Int = {
    val xa = DriverManagerTransactor[IOLite]("org.postgresql.Driver", "jdbc:postgresql:checkout", "alexvivi", "")
    sql"UPDATE items SET offer_units = null, offer_price =  null WHERE sku = $sku".update.run.transact(xa).unsafePerformIO
  }


  def updatePrice(sku: String, p: Double): Int = {
    val xa = DriverManagerTransactor[IOLite]("org.postgresql.Driver", "jdbc:postgresql:checkout", "alexvivi", "")
    sql"UPDATE items SET price = $p WHERE sku = $sku".update.run.transact(xa).unsafePerformIO
  }

}
