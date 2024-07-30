object InventoryManagement {

  // Sample inventories
  val inventory1: Map[Int, (String, Int, Double)] = Map(
    101 -> ("ProductA", 10, 25.0),
    102 -> ("ProductB", 5, 30.0),
    103 -> ("ProductC", 8, 22.0)
  )

  val inventory2: Map[Int, (String, Int, Double)] = Map(
    102 -> ("ProductB", 7, 35.0),
    104 -> ("ProductD", 4, 50.0)
  )

  // I. Retrieve all product names from inventory1
  def getAllProductNames(inventory: Map[Int, (String, Int, Double)]): Set[String] = {
    inventory.values.map(_._1).toSet
  }

  // II. Calculate the total value of all products in inventory1
  def calculateTotalValue(inventory: Map[Int, (String, Int, Double)]): Double = {
    inventory.values.map { case (_, quantity, price) => quantity * price }.sum
  }

  // III. Check if inventory1 is empty
  def isInventoryEmpty(inventory: Map[Int, (String, Int, Double)]): Boolean = {
    inventory.isEmpty
  }

  // IV. Merge inventory1 and inventory2, updating quantities and retaining the highest price
  def mergeInventories(inv1: Map[Int, (String, Int, Double)], inv2: Map[Int, (String, Int, Double)]): Map[Int, (String, Int, Double)] = {
    (inv1 ++ inv2).foldLeft(Map.empty[Int, (String, Int, Double)]) {
      case (acc, (id, (name, quantity, price))) =>
        acc.get(id) match {
          case Some((_, existingQuantity, existingPrice)) =>
            val newQuantity = existingQuantity + quantity
            val newPrice = Math.max(existingPrice, price)
            acc + (id -> (name, newQuantity, newPrice))
          case None =>
            acc + (id -> (name, quantity, price))
        }
    }
  }

  // V. Check if a product with a specific ID (e.g., 102) exists and print its details
  def printProductDetails(inventory: Map[Int, (String, Int, Double)], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some((name, quantity, price)) =>
        println(s"Product ID: $productId, Name: $name, Quantity: $quantity, Price: $price")
      case None =>
        println(s"Product ID: $productId does not exist in the inventory.")
    }
  }

  def main(args: Array[String]): Unit = {
    // Retrieve all product names from inventory1
    println("Product Names: " + getAllProductNames(inventory1).mkString(", "))

    // Calculate the total value of all products in inventory1
    println("Total Value: " + calculateTotalValue(inventory1))

    // Check if inventory1 is empty
    println("Is Inventory1 Empty: " + isInventoryEmpty(inventory1))

    // Merge inventory1 and inventory2
    val mergedInventory = mergeInventories(inventory1, inventory2)
    println("Merged Inventory: " + mergedInventory)

    // Check if a product with ID 102 exists and print its details
    printProductDetails(inventory1, 102)
  }
}
