package cs4r.labs.learningscala.adventofcode16

import java.util

import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by cs4r on 8/01/17.
  */
object AdventOfCode9 extends App {

  val puzzleInput = Source.fromFile("input9.txt").getLines().map(l => l.replaceAll("\\s+", "")).mkString("")

  val markerPattern = """\((\d+)x(\d+)\)""".r

  def decompress(input: String): String = {


    var processingMarker = false
    var buffer = ""

    val result = new StringBuilder

    var i = 0

    while (i < input.length) {
      val c: Char = input(i)

      if (c == '(') {
        processingMarker = true
        buffer += c
      } else if (processingMarker) {
        buffer += c

        if (c == ')') {
          processingMarker = false

          if (buffer.matches(markerPattern.regex)) {
            val markerPattern(take, times) = buffer
            val decompressed = input.substring((i + 1), (i + 1) + take.toInt) * times.toInt
            result.append(decompressed)
            i += take.toInt
          } else {
            result.append(buffer)
          }

          buffer = ""
        }
      } else {
        result.append(c)
      }

      i += 1
    }


    result.toString()
  }


  val partA = decompress(puzzleInput).length

  println(partA)


  /* Part B */

  def computeDecompressedLegth(input: String): Long = {

    var length = 0L
    val weights = List.fill(input.length)(List(1)).flatten.toArray

    var i = 0
    var processingMarker = false
    var buffer = ""

    while (i < input.length) {
      val c: Char = input(i)
      if (c == '(') {
        processingMarker = true
        buffer += c
      } else if (processingMarker) {
        buffer += c
        if (c == ')') {
          processingMarker = false

          if (buffer.matches(markerPattern.regex)) {
            val markerPattern(take, times) = buffer

            for (j <- i + 1 until i+1+take.toInt) {
              weights(j) *= times.toInt
            }

          } else {
            length += buffer.length
          }
          buffer = ""
        }
      } else {
        length += weights(i)
      }


      i += 1

    }

    length
  }


  var partB = computeDecompressedLenght(puzzleInput)

  println(partB)

}
