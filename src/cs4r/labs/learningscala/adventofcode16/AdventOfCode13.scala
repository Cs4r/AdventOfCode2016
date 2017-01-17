package cs4r.labs.learningscala.adventofcode16

import scala.collection.immutable.Queue


/**
  * Created by cs4r on 12/01/17.
  */
object AdventOfCode13 extends App {

  val favoriteNumber = 1364
  val initialLocation = (1, 1)
  val partADestination = (31, 39)

  trait CoordinateType

  case object WALL extends CoordinateType {
    override def toString: String = "#"
  }

  case object OPEN_SPACE extends CoordinateType {
    override def toString: String = "."
  }

  val computation: (((Int, Int)) => Long) = (point) => (point._1 * point._1 + 3 * point._1 + 2 * point._1 * point._2 + point._2 + point._2 * point._2)

  val sum = computation andThen (favoriteNumber + _)

  val binaryRepresentation: (Long => String) = (number) => number.toBinaryString

  val countOnes: (String => Int) = (input) => input.count(_ == '1')

  val numberOfOnesToCoordinateType: Int => CoordinateType = (input) => if (input % 2 == 0) OPEN_SPACE else WALL

  val coordinateType = sum andThen binaryRepresentation andThen countOnes andThen numberOfOnesToCoordinateType

  val endToRemove = partADestination

  def findPath(visited: List[(Int, Int)], queue: Queue[List[(Int, Int)]], end: (Int, Int)): List[(Int, Int)] = {
    if (!queue.isEmpty) {
      val (path, newQueue) = queue.dequeue
      val current: (Int, Int) = path.head
      if (current == end) {
        return path
      } else {
        if (visited.contains(current)) {
          return findPath(visited, newQueue, end)
        } else {
          val (x, y) = current

          val neighbours = (x + 1, y) :: (x - 1, y) :: (x, y + 1) :: (x, y - 1) :: Nil

          val validNeighbours: List[(Int, Int)] = neighbours.filter(n => coordinateType(n._1, n._2) == OPEN_SPACE && n._1 >= 0 && n._2 >= 0)

          val newQueue2 = validNeighbours.foldLeft(newQueue)((acc, n) => {
            acc :+ (n :: path)
          })

          return findPath(current :: visited, newQueue2, end)
        }
      }
    }
    throw new IllegalStateException("Path not found")
  }

  private val initialPaths: Queue[List[(Int, Int)]] = Queue.empty :+ List(initialLocation)

  var partA = findPath(List(), initialPaths, partADestination).init

  println(partA.size)


  def differentLocations(visited: List[(Int, Int)], queue: Queue[List[(Int, Int)]], unique: Set[(Int, Int)], end: Int): Set[(Int, Int)] = {

    if (!queue.isEmpty) {
      val (path, newQueue) = queue.dequeue
      val current: (Int, Int) = path.head
      if (path.size <= end) {
        if (visited.contains(current)) {
          return differentLocations(visited, newQueue, unique, end)
        } else {
          val (x, y) = current

          val neighbours = (x + 1, y) :: (x - 1, y) :: (x, y + 1) :: (x, y - 1) :: Nil

          val validNeighbours: List[(Int, Int)] = neighbours.filter(n => coordinateType(n._1, n._2) == OPEN_SPACE && n._1 >= 0 && n._2 >= 0)

          val newQueue2 = validNeighbours.foldLeft(newQueue)((acc, n) => {
            acc :+ (n :: path)
          })

          return differentLocations(current :: visited, newQueue2, unique ++ path + current, end)
        }
      } else {
        return differentLocations(visited, newQueue, unique + current, end)
      }
    }
    return unique
  }


  var partB = differentLocations(List(), initialPaths, Set(), 50)

  println(partB.size)
}