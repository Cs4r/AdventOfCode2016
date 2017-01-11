package cs4r.labs.learningscala.adventofcode16

import scala.io.Source

/**
  * Created by cs4r on 11/01/17.
  */
object AdventOfCode12 extends App {


  trait Instruction {
    val register: String
  }

  case class Cpy(val value: String, val register: String) extends Instruction

  case class Inc(val register: String) extends Instruction

  case class Dec(val register: String) extends Instruction

  case class Jnz(val register: String, val destination: Int) extends Instruction

  val instructions = Source.fromFile("input12.txt").getLines().map(line => {

    val cpyPattern = "cpy (\\p{Alnum}+) ([a-z])".r
    val incPattern = "inc ([a-z])".r
    val decPattern = "dec ([a-z])".r
    val jnzPattern = "jnz (\\p{Alnum}+) (-?[0-9]+)".r

    line match {
      case cpyPattern(value, register) => Cpy(value, register)
      case incPattern(register) => Inc(register)
      case decPattern(register) => Dec(register)
      case jnzPattern(register, destination) => Jnz(register, destination.toInt)
    }
  }).toIndexedSeq


  def processInstructions(instructionPointer: Int, instructions: IndexedSeq[Instruction],
                          registers: Map[String, Int]): Map[String, Int] = {

    if (instructionPointer >= instructions.length) {
      registers
    } else {
      val currentInstruction = instructions(instructionPointer)

      currentInstruction match {
        case Cpy(value, register) => processInstructions(instructionPointer + 1, instructions,
          registers + (register -> registers.getOrElse(value, value.toInt)))
        case Inc(register) => processInstructions(instructionPointer + 1, instructions,
          registers + (register -> (registers(register) + 1)))
        case Dec(register) => processInstructions(instructionPointer + 1, instructions,
          registers + (register -> (registers(register) - 1)))
        case Jnz(register, destination) => {
          val value: Int = registers.getOrElse(register, register.toInt)

          if (value != 0) {
            processInstructions(instructionPointer + destination, instructions, registers)
          } else {
            processInstructions(instructionPointer + 1, instructions, registers)
          }
        }
      }
    }
  }

  val partARegisters = processInstructions(0, instructions, Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0))

  val partA = partARegisters("a")

  println(partA)

  val partBRegisters = processInstructions(0, instructions, Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0))

  val partB = partBRegisters("a")

  println(partB)

}


