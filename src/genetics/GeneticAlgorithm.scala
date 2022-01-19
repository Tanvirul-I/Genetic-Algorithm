package genetics

import scala.annotation.tailrec

object GeneticAlgorithm {

  /**
   * Uses a genetic algorithm to optimize a generic problem
   *
   * @param incubator Determines how instances of type T are created from a List of Doubles (genes)
   * @param costFunction Determines the cost for a given instance of T
   * @param numberOfGenes The size of the List expected by the incubator
   * @tparam T The type to be optimized
   * @return An instance of T with minimal cost
   */
  def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {
    val populationSize: Int = 500
    val population: List[List[Double]] = createPopulation(numberOfGenes, populationSize, 100, -100)
    val fittest: List[Double] = getFittest(incubator, costFunction, numberOfGenes, population, populationSize)
    incubator(fittest)
  }

  def getFittest[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int, population: List[List[Double]], populationSize: Int): List[Double] = {
    //Survival of the fittest - 5% Left / 25
    val fittest: List[(List[Double], Double)] = getCostOfPopulation(incubator,costFunction, population).slice(0, (populationSize * .05).toInt)
    //Mutate - Add 25% / 125 + 25 = 150
    val mutatedPopulation: List[List[Double]] = mutate(fittest, numberOfGenes, (populationSize * .25).toInt, 0, List()) ::: fittest.map(x => x._1)
    //Crossover - Add 25% / 125 + 125 + 25 = 275
    val crossPopulation: List[List[Double]] = crossOver(fittest, numberOfGenes, (populationSize * .25).toInt, 0, List()) ::: mutatedPopulation
    //Random - Add 45% / 225 + 125 + 125 + 25 = 500
    val finalPopulation: List[List[Double]] = createPopulation(numberOfGenes, (populationSize * .45).toInt, 100, -100) ::: crossPopulation
    //Check cost of new population
    val finalPopulationCost: List[(List[Double], Double)] = getCostOfPopulation(incubator, costFunction, finalPopulation)
    //Average costs
    val averageCosts: Double = getAverage(finalPopulationCost.slice(0, 10), 0, 0)
    if(finalPopulationCost.head._2 <= .05 || math.abs(finalPopulationCost.head._2 - averageCosts) < .001) {
      finalPopulationCost.head._1
    } else {
      getFittest(incubator, costFunction, numberOfGenes, finalPopulation, populationSize)
    }
  }

  def createPopulation[T](numberOfGenes: Int, populationSize: Int, upperBound: Int, lowerBound: Int): List[List[Double]] = {
    List.fill(populationSize)(List.fill(numberOfGenes)(Math.random() * (math.abs(lowerBound) + upperBound) - upperBound))
  }

  def iterateOverPopulation[T](returnVal: List[(List[Double],Double)], position: Int, list: List[List[Double]], costFunction: T => Double, incubator: List[Double] => T): List[(List[Double],Double)] = {
    if(returnVal.size == list.length) {
      returnVal
    } else {
      val distance: Double = costFunction(incubator(list(position)))
      val appendVal = (list(position), distance)
      iterateOverPopulation(returnVal :+ appendVal, position + 1, list, costFunction, incubator)
    }
  }

  def getCostOfPopulation[T](incubator: List[Double] => T, costFunction: T => Double, population: List[List[Double]]): List[(List[Double], Double)] = {
    iterateOverPopulation(List(), 0, population, costFunction, incubator).sortBy(_._2)
  }

  def mutate(fittest: List[(List[Double], Double)], numberOfGenes: Int, amountToAdd: Int, position: Int, newPopulation: List[List[Double]]): List[List[Double]] = {
    if(position == amountToAdd) {
      newPopulation
    } else {
      val randomParent: List[Double] = fittest(math.floor(math.random() * fittest.length).toInt)._1
      val mutatedAnimal: List[Double] = mutateHelper(randomParent, numberOfGenes, 0, List())
      mutate(fittest, numberOfGenes, amountToAdd, position + 1, newPopulation :+ mutatedAnimal)
    }
  }

  def mutateHelper(parent: List[Double], numberOfGenes: Int, position: Int, newAnimal: List[Double]): List[Double] = {
    if(position == numberOfGenes) {
      newAnimal
    } else {
      mutateHelper(parent, numberOfGenes, position + 1, newAnimal :+ parent(position) * math.random())
    }
  }

  def crossOver(fittest: List[(List[Double], Double)], numberOfGenes: Int, amountToAdd: Int, position: Int, newPopulation: List[List[Double]]): List[List[Double]] = {
    if(position == amountToAdd) {
      newPopulation
    } else {
      val mutatedAnimal: List[Double] = crossOverHelper(fittest(math.floor(math.random() * fittest.length).toInt)._1, fittest(math.floor(math.random() * fittest.length).toInt)._1, numberOfGenes, 0, List())
      crossOver(fittest, numberOfGenes, amountToAdd, position + 1, newPopulation :+ mutatedAnimal)
    }
  }

  def crossOverHelper(parent1: List[Double], parent2: List[Double], numberOfGenes: Int, position: Int, newAnimal: List[Double]): List[Double] = {
    if(position == numberOfGenes) {
      newAnimal
    } else {
      val randomParent: List[Double] = List(parent1, parent2)(math.round(math.random()).toInt)
      crossOverHelper(parent1, parent2, numberOfGenes, position + 1, newAnimal :+ randomParent(position) * math.random())
    }
  }

  def getAverage(population: List[(List[Double], Double)], position: Int, average: Double): Double = {
    if(position == population.length) {
      average / population.length
    } else {
      getAverage(population, position + 1, population(position)._2 + average)
    }
  }

}