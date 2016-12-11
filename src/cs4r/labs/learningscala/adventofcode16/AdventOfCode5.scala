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

  def hashWithValidPosition(hash: String) = {
    val pos = hexDigitToInt(hash(5))
    pos >= 0 && pos <= 7
  }

  def hexDigitToInt(c: Char) = {
    Integer.parseInt(c.toString, 16)
  }

  val partB = hashes.filter(hashWithValidPosition).map(e => {
    val pos = hexDigitToInt(e(5))
    val char = e(6)
    (pos, char)
  }).take(25).foldLeft(Map[Int, Char]())((acc, curr) => {
    if (!acc.contains(curr._1)) acc + curr
    else acc
  }).foldLeft(Array.fill(8) {'0'})((acc, curr) => {
    val pos = curr._1
    val char = curr._2
    acc(pos) = char
    acc
  }).mkString

  println(partB)
}
