// IBM -- Exercise 2 -- 08/02/2016 -- Thomas Hood
// Scala Worksheet

// Recipe contains a list of ingredients and name of recipe
class Recipe(x: String, y: List[String]) {
  val name = x
  var ingredients = y

  // Adds ingredient to recipe
  def add(x: String) = {
    ingredients :: y
  }

  def getIngredients: List[String] = {
    ingredients
  }

  def getName: String = {
    name
  }

  override def toString = {
    name.toString + "\n" +
    ingredients.toString
  }
}

// Create some recipes
var chocolatecake = new Recipe("chocolatecake", List("eggs", "sugar", "cream", "sponge"))
var pizza = new Recipe("pizza", List("dough", "cheese"))
var applepie = new Recipe("applepie", List("butter", "eggs", "sugar", "cream"))
var breadstick = new Recipe("breadstick", List("dough", "cheese"))

// List of recipes
var recipes = List(chocolatecake, pizza, applepie, breadstick)

// Function to search for recipes with list of ingredients and list of recipes
def findrecipe(search: List[String], r: List[Recipe]):List[Recipe] = {
  val recipes = r
  var list: List[Recipe] = List()
  // For each recipe, check our list of recipes for those ingredients,
  // if it contains all ingredients then add it to our results
  recipes.foreach { recipe => if(recipe.getIngredients.equals(search)) list = recipe::list }
  // return results
  list
}

// Testing -- return a list of recipes that match our list of ingredients
val results = findrecipe(List("dough", "cheese"), recipes)
