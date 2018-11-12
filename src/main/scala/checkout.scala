import java.nio.file.{Files, Paths}
import scalacache._
import scalacache.modes.sync._
import scalacache.caffeine.CaffeineCache
import com.github.benmanes.caffeine.cache.Caffeine

case class Basket(price: Vector[Price]) {

  val prices = if (price == Vector()) Basket.getPrices() else price

  val purchases: Cache[Vector[Purchase]] = CaffeineCache(Caffeine.newBuilder.build[String, Entry[Vector[Purchase]]])

  def addToBasket(item: String) =
    Basket.addToBasket(item, purchases, prices)

  def itemExists(item: String): Boolean =
    Basket.itemExists(item, prices)

  def removeFromBasket(item: String) =
    Basket.removeFromBasket(item, purchases)

  def availableItems: String =
    Basket.availableItems(prices: Vector[Price])

  def clearBasket() =
    Basket.clearBasket(purchases)

  def contents() =
    Basket.contents(purchases)

  def totalPrice(): Double =
    Basket.totalPrice(prices, purchases)
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

  //returns String for price object, including offer is one is available
  def priceString(p: Price): String =
    p match {
      case Price(a, b, None)              => a + ": " + b
      case Price(a, b, Some(Offer(c, d))) => a + ": " + b + " or " + c + " for " + d
    }

  //combines price strings for all items
  def availableItems(prices: Vector[Price]): String = {

    def loop(i: Int): String =
      if (i == prices.length) ""
      else priceString(prices(i)) + "\n" + loop(i + 1)
    loop(0)
  }

  //returns string containing all items in purchased along with their quantity
  def contents(purchases: Cache[Vector[Purchase]]): String = {
    val inBasket = purchases.get("basket")

    def loop(i: Int, allItems: Vector[Purchase]): String =
      if (i == allItems.size) ""
      else allItems(i).SKU + ": " + allItems(i).quant + "\n" + loop(i + 1, allItems)

    inBasket match {
      case None => "Basket Empty"
      case Some(allItems) =>
        val itemList = loop(0, allItems)
        if (itemList.length == 0) "Empty Basket" else itemList
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
          case Some(Price(x, y, None)) => allItems(i).quant * y + loop(i + 1, allItems)
          case Some(Price(x, y, Some(Offer(a, b)))) =>
            offerPrice(allItems(i).quant, y, Offer(a, b)) + loop(i + 1, allItems)
          case None => loop(i + 1, allItems)
        }
      }
    inBasket match {
      case None             => 0
      case (Some(allItems)) => loop(0, allItems)
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
        case None => {
          purchases.put("basket")(Vector(Purchase(item, 1)), None)
        }
        case Some(allItems) => purchases.put("basket")(addToExistingBasket(allItems, item), None)
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

  //creates a vector of Price objects from price file
  def getPrices(): Vector[Price] = {
    //pricing rules are gotten from text file and each item is an entry in an array
    val priceRule = new String(Files.readAllBytes(Paths.get("rules.txt"))).split("\\n")

    def loop(i: Int): Vector[Price] =
      if (i < priceRule.size) {
        val p = priceRule(i).split(" ").toList
        p match {
          case List(a, b) if b.toDouble > 0 => Price(a, b.toDouble, None) +: loop(i + 1)
          case List(a, b, c, d) if b.toDouble > 0 && c.toInt > 0 && d.toDouble > 0 =>
            Price(a, b.toDouble, Some(Offer(c.toInt, d.toDouble))) +: loop(i + 1)
          case _ => loop(i + 1)

        }
      } else Vector()

    loop(0)
  }
}

case class Price(SKU: String, price: Double, offerPrice: Option[Offer])

case class Offer(units: Int, price: Double)

case class Purchase(SKU: String, quant: Int)
