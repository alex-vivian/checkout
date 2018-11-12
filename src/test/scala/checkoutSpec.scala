import org.scalatest.{FunSpec, Matchers}
import scalacache._
import scalacache.modes.sync._
import scalacache.caffeine.CaffeineCache
import com.github.benmanes.caffeine.cache.Caffeine

class checkoutSpec extends FunSpec with Matchers {

  describe("returns string containing menu") {
    it("should return a string containing the menu") {
      val basket = Basket.apply()
      val m      = basket.availableItems()
      println(m)
    }
  }

  describe("getting prices") {
    it("should create a vector containing the price rules") {
      val basket = Basket.apply()
      val prices = basket.prices
      prices(0) shouldEqual Price("A", 50.0, Some(Offer(3, 130.0)))
      prices(2) shouldEqual Price("C", 20, None)
    }
  }

  describe("adding items to basket") {
    it("should add A to the basket") {
      val basket = Basket.apply()
      basket.addToBasket("A")
      basket.purchases.get("basket") shouldEqual Some(Vector(Purchase("A", 1)))
    }

    it("should add 2 units of A to the basket") {
      val basket = Basket.apply()
      basket.addToBasket("A")
      basket.addToBasket("A")
      basket.purchases.get("basket") shouldEqual Some(Vector(Purchase("A", 2)))
    }
  }

  describe("removing items from basket") {
    it("shouldn't change purchases item is not present") {
      val basket = Basket.apply()
      basket.addToBasket("A")
      basket.removeFromBasket("B")

      basket.purchases.get("basket") shouldEqual Some(Vector(Purchase("A", 1)))
    }

    it("should reduce the number of units of a given item in the basket by 1") {
      val basket = Basket.apply()
      basket.addToBasket("A")
      basket.addToBasket("A")
      basket.removeFromBasket("A")
      basket.purchases.get("basket") shouldEqual Some(Vector(Purchase("A", 1)))
    }
  }

  describe("total cost of basket") {
    it("should return the total cost of all items in the basket, including any offer prices") {
      val basket = Basket.apply()
      basket.addToBasket("A")
      basket.addToBasket("B")
      basket.addToBasket("A")
      basket.addToBasket("C")
      basket.addToBasket("B")
      basket.addToBasket("A")
      basket.addToBasket("A")
      basket.addToBasket("B")
      basket.addToBasket("B")
      basket.addToBasket("D")
      basket.totalPrice() shouldEqual 305.0
    }
  }

}
