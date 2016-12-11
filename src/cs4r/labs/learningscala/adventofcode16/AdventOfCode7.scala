package cs4r.labs.learningscala.adventofcode16

import scala.io.Source

/**
  * Created by cs4r on 11/12/16.
  */
object AdventOfCode7 extends App {

  val puzzleInput = Source.fromFile("input7.txt").getLines.toList

  val ips = puzzleInput

  val isABBA: (String => Boolean) = (input) => input.sliding(4).exists(e => e(0) != e(1) && e(0) == e(3) && e(1) == e(2))

  val hipertextPattern: String = "\\[[a-z]+\\]"
  val supernetPattern: String = "^[a-z]+\\[|\\][a-z]++|\\[|\\]"

  val supportTLS = ips.map(e => {
    e.split(hipertextPattern).exists(isABBA) && !e.split(supernetPattern).exists(isABBA)
  }).count(_ == true)

  println(supportTLS)

  val isABA: (String => Boolean) = (input) => input(0) != input(1) && input(0) == input(2)

  val getBAB: (String => String) = (input) => s"${input(1)}${input(0)}${input(1)}"

  val supportSSL = ips.map(e => {
    val abasInSuperNet = e.split(hipertextPattern).flatMap(_.sliding(3).filter(isABA))
    val possibleBabs = abasInSuperNet.map(getBAB)
    e.split(supernetPattern).flatMap(_.sliding(3)).exists(possibleBabs.contains(_))
  }).count(_ == true)

  println(supportSSL)

}
