package cs4r.labs.learningscala.adventofcode16


import java.security.MessageDigest

object AdventOfCode5 extends App {

  val puzzleInput: String = "cxdnnyjw"

  def md5(s: String): Array[Byte] = {
    val instance: MessageDigest = MessageDigest.getInstance("MD5")
    instance.digest(s.getBytes)
  }

  def startWithNZeros(n: Int)(input: Array[Byte]) = {
    arrayToString(input).take(n).forall(_ == '0')
  }

  def startWithFiveZeros(input: Array[Byte]) = {
    startWithNZeros(5)(input)
  }

  def startWithSixZeros(input: Array[Byte]) = {
    startWithNZeros(6)(input)
  }

  def arrayToString(input: Array[Byte]): String = {
    input.map("%02x".format(_)).reduce(_ + _)
  }

  val hashes = Stream.from(1).map(e => md5(puzzleInput + e)).filter(startWithFiveZeros(_)).map(arrayToString)

  val partA  = hashes.take(8).map(_(5)).mkString

  println(partA)

  var positions: Set[Int] = Set()


  // TODO: fix part B

  val partB = hashes.filter(e => {
      val pos = e(5).toInt
      if(pos >=0 && pos <= 7) { print(pos); true }
      else false
  }).takeWhile(e => {
    positions += e(5).toInt
    positions.size < 8
  }).foldLeft(Array.fill(8){'0'})( (acc, curr) => {
      val pos = curr(5).toInt
      val char = curr(6)
      acc(pos) = char
      acc
  }).mkString

  println(partB)

}
