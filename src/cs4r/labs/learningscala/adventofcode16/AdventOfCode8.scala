package cs4r.labs.learningscala.adventofcode16

import scala.io.Source
import scala.reflect.ClassTag

/**
  * Created by cs4r on 11/12/16.
  */
object AdventOfCode8 extends App {


  val screen: Array[Array[Char]] = Array.ofDim[Char](6, 50)

  for (i <- screen.indices) {
    for (j <- screen(i).indices) {
      screen(i)(j) = '.'
    }
  }

  def rect(arr: Array[Array[Char]], a: Int, b: Int) = {

    for (i <- 0 until b) {
      for (j <- 0 until a) {
        arr(i)(j) = '#'
      }
    }

  }


  def rotateColumn[T: ClassTag](arr: Array[Array[T]], column: Int, order: Int) = {
    val columnLength = arr.length


    val copyOfColumn = Array.ofDim[T](columnLength)

    for (i <- 0 until columnLength) {
      copyOfColumn(i) = arr(i)(column)
    }

    rotate(copyOfColumn, order)

    for (i <- 0 until columnLength) {
      arr(i)(column) = copyOfColumn(i)
    }
  }

  def rotateRow[T](arr: Array[Array[T]], row: Int, order: Int) = {
    rotate(arr(row), order)
  }


  private def rotate[T](arr: Array[T], order: Int) = {
    val l1 = arr.length - order

    reverse(arr, 0, l1 - 1)
    reverse(arr, l1, arr.length - 1)
    reverse(arr, 0, arr.length - 1)

  }

  private def reverse[T](arr: Array[T], left: Int, right: Int) {
    if (arr != null && arr.length > 1) {
      var l = left
      var r = right
      while (l < r) {
        val temp = arr(l)
        arr(l) = arr(r)
        arr(r) = temp
        l += 1
        r -= 1
      }
    }
  }


  def printScreen[T](arr: Array[Array[T]]) = {
    for (i <- screen.indices) {
      for (j <- screen(i).indices) {
        print(screen(i)(j))
      }
      println
    }
  }

  Source.fromFile("input8.txt").getLines().foreach(l => {

    val rectPattern = """rect\s+(\d+)x(\d+)\s*""".r
    val rowPattern = """rotate\s+row\s+y=(\d+)\s+by\s+(\d+)\s*""".r
    val columnPattern = """rotate\s+column\s+x=(\d+)\s+by\s+(\d+)\s*""".r

    if (l.matches(rectPattern.regex)) {
      val rectPattern(a, b) = l
      rect(screen, a.toInt, b.toInt)
    } else if (l.matches(rowPattern.regex)) {
      val rowPattern(row, order) = l
      rotateRow(screen, row.toInt, order.toInt)
    } else if (l.matches(columnPattern.regex)) {
      val columnPattern(column, order) = l
      rotateColumn(screen, column.toInt, order.toInt)
    }
  })


  val partA = screen.flatten.count(_ == '#')

  println(partA)

  printScreen(screen)

}
