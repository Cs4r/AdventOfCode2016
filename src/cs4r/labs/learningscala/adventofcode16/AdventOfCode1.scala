package cs4r.labs.learningscala.adventofcode16

object AdventOfCode1 extends App {

  val puzzleInput =
    """L1, L3, L5, L3, R1, L4, L5, R1, R3, L5, R1, L3, L2, L3, R2, R2, L3, L3, R1, L2, R1, L3, L2, R4, R2, L5, R4, L5,
      | R4, L2, R3, L2, R4, R1, L5, L4, R1, L2, R3, R1, R2, L4, R1, L2, R3, L2, L3, R5, L192, R4, L5, R4, L1, R4, L4,
      |  R2, L5, R45, L2, L5, R4, R5, L3, R5, R77, R2, R5, L5, R1, R4, L4, L4, R2, L4, L1, R191, R1, L1, L2, L2, L4, L3,
      |  R1, L3, R1, R5, R3, L1, L4, L2, L3, L1, L1, R5, L4, R1, L3, R1, L2, R1, R4, R5, L4, L2, R4, R5, L1, L2, R3, L4,
      |  R2, R2, R3, L2, L3, L5, R3, R1, L4, L3, R4, R2, R2, R2, R1, L4, R4, R1, R2, R1, L2, L2, R4, L1, L2, R3, L3, L5,
      |  L4, R4, L3, L1, L5, L3, L5, R5, L5, L4, L2, R1, L2, L4, L2, L4, L1, R4, R4, R5, R1, L4, R2, L4, L2, L4, R2, L4,
      |  L1, L2, R1, R4, R3, R2, R2, R5, L1, L2""".stripMargin

  val movementPattern = "([RL])(\\d+)".r

  val movements: Array[(String, Int)] = puzzleInput.split("\\s*,\\s*").map(s => {
    val movementPattern(movement, blocks) = s
    (movement, blocks.toInt)
  })

  def nextDirection(prev: String, next: String): String = prev match {
    case "N" => next match {
      case "L" => "W"
      case "R" => "E"
    }
    case "S" => next match {
      case "L" => "E"
      case "R" => "W"
    }
    case "E" => next match {
      case "L" => "N"
      case "R" => "S"
    }
    case "W" => next match {
      case "L" => "S"
      case "R" => "N"
    }
  }

  val initialPos = (0, 0)
  val initialDirection = "N"

  def nextPosition(pos: (Int, Int), direction: String, blocks: Int): (Int, Int) = direction match {
    case "N" => (pos._1, pos._2 + blocks)
    case "S" => (pos._1, pos._2 - blocks)
    case "E" => (pos._1 + blocks, pos._2)
    case "W" => (pos._1 - blocks, pos._2)
  }

  def nextPositions(pos: (Int, Int), direction: String, blocks: Int): List[(Int, Int)] = {
    val newPos = nextPosition(pos, direction, blocks)
    if (blocks == 1) List(newPos) else List(newPos) ++ nextPositions(pos, direction, blocks - 1)
  }

  val positions = movements.foldLeft((List(initialPos), initialDirection))((curr, next) => {

    val pos = curr._1.head

    val direction = nextDirection(curr._2, next._1)

    val blocks = next._2

    val newPos = nextPositions(pos, direction, blocks)

    (newPos ++ curr._1, direction)

  })._1

  def taxicabDistance(a: (Int, Int), b: (Int, Int)) = {
    math.abs(a._1 - b._1) + math.abs(a._2 - b._2)
  }

  val finalPos = positions.head

  val partA = taxicabDistance(initialPos, finalPos)

  println(partA)

  val positionsInVisitingOrder =  positions.reverse

  var visitedTwice = positionsInVisitingOrder.find(e => positionsInVisitingOrder.filter(x => x == e ).size > 1).get

  val partB = taxicabDistance(initialPos, visitedTwice)

  println(partB)

}
