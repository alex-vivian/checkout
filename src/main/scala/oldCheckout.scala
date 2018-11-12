import java.nio.file.{Files, Paths}
import scala.io.StdIn.readLine


object checkout
{
  val rules = getRules
  val sepRules = separateRules
  printMenu()
  val items = purchaseItems(Map[String, Int]())
  for((k,v)<- items) println(k + ":" + v)
  val price = calculateTotal(items)
  println("Total Cost:" + price)

  /*rules are stored in a text file where each line contains one product and is in the format:
   SKU IndividualPrice NumberOfUnitsForSpecial SpecialCost
   */
  def getRules() : String =
  {
    new String(Files.readAllBytes(Paths.get("rules.txt")))
  }

  //builds map of sku and prices
  def separateRules() :Map[String, String] =
  {
    //splits string at line break
    val splitStr = rules.split("\\n")
    //calls recursive function createMap to build price map
    createMap(Map[String, String](), splitStr, 0)

  }

  //uses price rules to print product menu
  def printMenu() : Unit =
  {
    println(("Menu"))

    for((k,v) <- sepRules)
    {
      val temp = v.split(" ")

      //if no specials print regular price
      if(temp.length == 1)
      {
        println(k + " - " + v)
      }
      //if there are specials print regualr price follwed by offer
      else
      {
        println(k + " - " + temp(0) + " or " + temp(1) + " for " + temp(2))
      }
    }
  }

  //called recursively to add items purchased to a map until no more items are selected
  def purchaseItems(m : Map[String, Int]) : Map[String, Int] =
  {
    print("Select Item to purchase or select N to exit ")
    var input = readLine().toUpperCase

    //if input is valud
    if(rules contains (input))
    {
      //if item type is already in map
      if(m contains input)
      {
        //increment existing value in map
        val res = m + (input -> (m(input)+1))
        purchaseItems(res)
      }
      else
      {
        //add one unit to map
        val res = m + (input -> 1)
        purchaseItems(res)
      }

    }
    //ignore incorrect input
    else if(input != "N")
    {
      purchaseItems(m)
    }
    //exit purchase
    else
    {
      m
    }

  }

  //called recursively to split each string in the array into the SKU (key) and the pricing rules (value)
  def createMap(m : Map[String, String], str : Array[String], index : Int): Map[String, String] =
  {
    val curr = str(index)

    if(index < str.length-1)
    {
      //key is SKU
      val key = curr.substring(0, curr.indexOf(" "))
      //value is rules
      val value = curr.substring(curr.indexOf(" ")+1)
      val res = m + (key -> value)
      createMap(res, str, index+1)
    }
    else
    {
      val key = curr.substring(0, curr.indexOf(" "))
      val value = curr.substring(curr.indexOf(" ")+1)

      m + (key -> value)
    }
  }


  def calculateTotal(items : Map[String, Int]) : Double =
  {
    var total = 0.0
    //for each key value in the pair
    for((k,v) <- items)
    {
      //get the corresponding value from the rules map and split it at each white space
      val splitRule = sepRules(k).split(" ")

      //if there are no specials on the product multiple the price by the number of unites
      if(splitRule.length == 1)
      {
        total = total + (v* splitRule(0).toDouble)
      }
      /*if there is a special on, calculate how many times to apply the special and apply the regular price on the remainder
       e.g if 3 for 45 on a product you have 5 of, apply special once and regular twice
       */
      else
      {
        val remainder = v % splitRule(1).toInt
        val inOffer = (v-remainder)/splitRule(1).toInt
        total = total + (remainder*splitRule(0).toDouble) + (inOffer*splitRule(2).toDouble)
      }
    }
    total
  }

}