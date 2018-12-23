package ch.kufi.aoc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day14 extends Challenge[String, Int] {
  override def part1(): String = {
    val numberOfRecipes = 47801

    val finalRecipe = bakeRecipes
      .find(_.recipes.size >= numberOfRecipes + 10)
      .get

    finalRecipe.recipes.drop(numberOfRecipes).foldLeft("")(_ + _)
  }


  override def part2(): Int = {
    val patternToFind = List(0, 4, 7, 8, 0, 1).zipWithIndex
    val size = patternToFind.size

    val finalRecipe = bakeRecipes
      .map(_.recipes)
      .find(recipes => {
        val recipeSize = recipes.size
        if (recipeSize < size + 1) {
          false
        } else {
          patternToFind.forall(digit => recipes(recipeSize - size + digit._2) == digit._1) ||
            patternToFind.forall(digit => recipes(recipeSize - size + digit._2 - 1) == digit._1)
        }
      })
      .get

    if(finalRecipe(finalRecipe.size - size) == patternToFind.head._1) {
      finalRecipe.size - patternToFind.size
    } else {
      finalRecipe.size - patternToFind.size - 1
    }
  }

  private def bakeRecipes = {
    Iterator
      .iterate(Recipes(new ArrayBuffer(400000000) ++= List(3, 7), List(0, 1))) { recipes =>
        recipes.bake()
      }
  }

  case class Recipes(recipes: mutable.ArrayBuffer[Byte], elves: List[Int]) {
    def bake(): Recipes = {
      val chosenRecipes = elves.map(recipes(_)).sum
      val createdRecipes = chosenRecipes.toString.toCharArray.map(_.asDigit.toByte)
      recipes ++= createdRecipes

      Recipes(recipes, elves.map(elve => (elve + recipes(elve) + 1) % recipes.size))
    }
  }

}
