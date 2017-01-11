package cs4r.labs.learningscala.adventofcode16

import scala.io.Source

/**
  * Created by cs4r on 10/01/17.
  */
object AdventOfCode10 extends App {

  trait Sink {
    def id: Int

    def sendValue(value: Int)
  }

  class Bot(val theId: Int) extends Sink {
    private var allSent: Boolean = false
    private var lowTo: Sink = _
    private var highTo: Sink = _
    private var values: List[Int] = List()

    override def id: Int = theId

    override def sendValue(value: Int): Unit = values = value :: values

    def lowDestination(to: Sink): Unit = lowTo = to

    def highDestination(to: Sink): Unit = highTo = to

    def highDestination = highTo

    def lowDestination = lowTo

    def highValue = if (values.isEmpty) -1 else values.max

    def lowValue = if (values.isEmpty) -1 else values.min

    def receivedAllValues: Boolean = values.size == 2

    def allValuesSent(allSent: Boolean) = this.allSent = allSent

    def allValuesSent: Boolean = allSent
  }

  class Output(val theId: Int) extends Sink {

    private var value: Int = 0

    override def id: Int = theId

    override def sendValue(received: Int): Unit = value = received

    def valueReceived = value
  }


  val transferPattern = "bot ([0-9]+) gives low to ([a-z]+) ([0-9]+) and high to ([a-z]+) ([0-9]+)".r

  val valueSendingPattern = "value ([0-9]+) goes to bot ([0-9]+)".r

  val OUTPUT_TOKEN: String = "output"

  // TODO: Do this in a more functional way...
  val bots = scala.collection.mutable.HashMap.empty[Int, Bot]

  val outputs = scala.collection.mutable.HashMap.empty[Int, Output]

  Source.fromFile("input10.txt").getLines().foreach(line => {
    if (line.matches(transferPattern.regex)) {
      val transferPattern(id, type1, low, type2, high) = line

      val bot = addBot(id)


      val sink1: Sink = if (type1 == OUTPUT_TOKEN) addOutput(low) else addBot(low)
      val sink2: Sink = if (type2 == OUTPUT_TOKEN) addOutput(high) else addBot(high)

      bot.lowDestination(sink1)
      bot.highDestination(sink2)

    } else if (line.matches(valueSendingPattern.regex)) {
      val valueSendingPattern(value, id) = line
      addBot(id).sendValue(value.toInt)
    }
  })

  def addOutput(id: String) = {
    if (!outputs.contains(id.toInt)) {
      outputs += (id.toInt -> new Output(id.toInt))
    }
    outputs(id.toInt)
  }

  def addBot(id: String): Bot = {
    if (!bots.contains(id.toInt)) {
      bots += (id.toInt -> new Bot(id.toInt))
    }
    bots(id.toInt)
  }

  var valuesToReceive = true

  while (valuesToReceive) {

    valuesToReceive = false

    for (id <- 0 until bots.size) {
      val bot: Bot = bots(id)
      if (bot.receivedAllValues && !bot.allValuesSent) {
        bot.lowDestination.sendValue(bot.lowValue)
        bot.highDestination.sendValue(bot.highValue)
        bot.allValuesSent(true)
        valuesToReceive = true
      }
    }

  }

  val partA = bots.find(p => p._2.lowValue == 17 && p._2.highValue == 61).get._2

  println(partA.id)

  val partB = outputs.filter(p => List(0, 1, 2).contains(p._1)).values.map(_.valueReceived).product

  println(partB)
}
