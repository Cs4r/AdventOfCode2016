package cs4r.labs.learningscala.adventofcode16


object AdventOfCode2 extends App {

  val puzzleInput =
    """LLRRLLRLDDUURLLRDUUUDULUDLUULDRDDDULLLRDDLLLRRDDRRUDDURDURLRDDULRRRLLULLULLRUULDLDDDUUURRRRURURDUDLLRRLDLLRRDRDLLLDDRRLUDDLDDLRDRDRDDRUDDRUUURLDUDRRLULLLDRDRRDLLRRLDLDRRRRLURLLURLRDLLRUDDRLRDRRURLDULURDLUUDURLDRURDRDLULLLLDUDRLLURRLRURUURDRRRULLRULLDRRDDDULDURDRDDRDUDUDRURRRRUUURRDUUDUDDDLRRUUDDUUDDDUDLDRDLRDUULLRUUDRRRDURLDDDLDLUULUDLLRDUDDDDLDURRRDRLLRUUUUDRLULLUUDRLLRDLURLURUDURULUDULUDURUDDULDLDLRRUUDRDDDRLLRRRRLDRRRD
      |DRRRDULLRURUDRLRDLRULRRLRLDLUDLUURUUURURULRLRUDRURRRLLUDRLLDUDULLUUDLLUUUDDRLRUDDDDLDDUUDULDRRRDULUULDULDRUUULRUDDDUDRRLRLUDDURLLDRLUDUDURUUDRLUURRLUUUDUURUDURLUUUDRDRRRDRDRULLUURURDLUULLDUULUUDULLLDURLUDRURULDLDLRDRLRLUURDDRLDDLRRURUDLUDDDLDRLULLDRLLLURULLUURLUDDURRDDLDDDDRDUUULURDLUUULRRLRDLDRDDDRLLRUDULRRRUDRRLDRRUULUDDLLDUDDRLRRDLDDULLLRDURRURLLULURRLUULULRDLULLUUULRRRLRUDLRUUDDRLLLLLLLURLDRRUURLDULDLDDRLLLRDLLLDLRUUDRURDRDLUULDDRLLRRURRDULLULURRDULRUDUDRLUUDDDDUULDDDUUDURLRUDDULDDDDRUULUUDLUDDRDRD
      |RRRULLRULDRDLDUDRRDULLRLUUDLULLRUULULURDDDLLLULRURLLURUDLRDLURRRLRLDLLRRURUDLDLRULDDULLLUUDLDULLDRDLRUULDRLURRRRUDDLUDLDDRUDDUULLRLUUDLUDUDRLRUULURUDULDLUUDDRLLUUURRURUDDRURDLDRRDRULRRRRUUUDRDLUUDDDUDRLRLDRRRRUDDRLLRDRLUDRURDULUUURUULLRDUUULRULRULLRULRLUDUDDULURDDLLURRRULDRULDUUDDULDULDRLRUULDRDLDUDRDUDLURLLURRDLLDULLDRULDLLRDULLRURRDULUDLULRRUDDULRLDLDLLLDUDLURURRLUDRRURLDDURULDURRDUDUURURULLLUDDLDURURRURDDDRRDRURRUURRLDDLRRLDDULRLLLDDUDRULUULLULUULDRLURRRLRRRLDRRLULRLRLURDUULDDUDLLLUURRRLDLUDRLLLRRUU
      |URLDDDLDRDDDURRRLURRRRLULURLDDUDRDUDDLURURLLRDURDDRLRUURLDLLRDLRUUURLRLDLDRUDDDULLDULLDUULURLDRDUDRRLRRLULRDDULUDULDULLULDLRRLRRLLULRULDLLDULRRLDURRRRDLURDLUDUUUDLURRRRRUDDUDUUDULDLURRDRLRLUDUDUUDULDDURUDDRDRUDLRRUDRULDULRDRLDRUDRLLRUUDDRLURURDRRLRURULLDUUDRDLULRUULUDURRULLRLUUUUUDULRLUUDRDUUULLULUDUDDLLRRLDURRDDDLUDLUUDULUUULDLLLLUUDURRUDUDLULDRRRULLLURDURDDLRRULURUDURULRDRULLRURURRUDUULRULUUDDUDDUURLRLURRRRDLULRRLDRRDURUDURULULLRUURLLDRDRURLLLUUURUUDDDLDURRLLUUUUURLLDUDLRURUUUDLRLRRLRLDURURRURLULDLRDLUDDULLDUDLULLUUUDLRLDUURRR
      |RLLDRDURRUDULLURLRLLURUDDLULUULRRRDRLULDDRLUDRDURLUULDUDDDDDUDDDDLDUDRDRRLRLRLURDURRURDLURDURRUUULULLUURDLURDUURRDLDLDDUURDDURLDDDRUURLDURRURULURLRRLUDDUDDDLLULUDUUUDRULLLLULLRDDRDLRDRRDRRDLDLDDUURRRDDULRUUURUDRDDLRLRLRRDLDRDLLDRRDLLUULUDLLUDUUDRDLURRRRULDRDRUDULRLLLLRRULDLDUUUURLDULDDLLDDRLLURLUDULURRRUULURDRUDLRLLLRDDLULLDRURDDLLDUDRUDRLRRLULLDRRDULDLRDDRDUURDRRRLRDLDUDDDLLUDURRUUULLDRLUDLDRRRRDDDLLRRDUURURLRURRDUDUURRDRRUDRLURLUDDDLUDUDRDRURRDDDDRDLRUDRDRLLDULRURULULDRLRLRRLDURRRUL""".stripMargin


  val movements = puzzleInput.split("\n").toList

  def obtainCode(movements: List[String])(f: (Char, Char) => Char): String = {
    movements.foldLeft(('5', List[Char]()))((curr, acc) => {
      val button = acc.foldLeft(curr._1)(f)
      (button, button :: curr._2)
    })._2.reverse.mkString
  }

  def simpleKeyPad(current: Char, movement: Char): Char = current match {
    case '1' => movement match {
      case 'U' => '1'
      case 'D' => '4'
      case 'L' => '1'
      case 'R' => '2'
    }

    case '2' => movement match {
      case 'U' => '2'
      case 'D' => '5'
      case 'L' => '1'
      case 'R' => '3'
    }

    case '3' => movement match {
      case 'U' => '3'
      case 'D' => '6'
      case 'L' => '2'
      case 'R' => '3'
    }


    case '4' => movement match {
      case 'U' => '1'
      case 'D' => '7'
      case 'L' => '4'
      case 'R' => '5'
    }

    case '5' => movement match {
      case 'U' => '2'
      case 'D' => '8'
      case 'L' => '4'
      case 'R' => '6'
    }

    case '6' => movement match {
      case 'U' => '3'
      case 'D' => '9'
      case 'L' => '5'
      case 'R' => '6'
    }

    case '7' => movement match {
      case 'U' => '4'
      case 'D' => '7'
      case 'L' => '7'
      case 'R' => '8'
    }

    case '8' => movement match {
      case 'U' => '5'
      case 'D' => '8'
      case 'L' => '7'
      case 'R' => '9'
    }

    case '9' => movement match {
      case 'U' => '6'
      case 'D' => '9'
      case 'L' => '8'
      case 'R' => '9'
    }

  }


  val partA = obtainCode(movements)(simpleKeyPad)

  println(partA)


  def complexKeyPad(current: Char, movement: Char): Char = current match {
    case '1' => movement match {
      case 'U' => '1'
      case 'D' => '3'
      case 'L' => '1'
      case 'R' => '1'
    }

    case '2' => movement match {
      case 'U' => '2'
      case 'D' => '6'
      case 'L' => '2'
      case 'R' => '3'
    }

    case '3' => movement match {
      case 'U' => '1'
      case 'D' => '7'
      case 'L' => '2'
      case 'R' => '4'
    }


    case '4' => movement match {
      case 'U' => '4'
      case 'D' => '8'
      case 'L' => '3'
      case 'R' => '4'
    }

    case '5' => movement match {
      case 'R' => '6'
      case _ => '5'
    }

    case '6' => movement match {
      case 'U' => '2'
      case 'D' => 'A'
      case 'L' => '5'
      case 'R' => '7'
    }

    case '7' => movement match {
      case 'U' => '3'
      case 'D' => 'B'
      case 'L' => '6'
      case 'R' => '8'
    }

    case '8' => movement match {
      case 'U' => '4'
      case 'D' => 'C'
      case 'L' => '7'
      case 'R' => '9'
    }

    case '9' => movement match {
      case 'L' => '8'
      case _ => '9'
    }

    case 'A' => movement match {
      case 'U' => '6'
      case 'D' => 'A'
      case 'L' => 'A'
      case 'R' => 'B'
    }

    case 'B' => movement match {
      case 'U' => '7'
      case 'D' => 'D'
      case 'L' => 'A'
      case 'R' => 'C'
    }

    case 'C' => movement match {
      case 'U' => '8'
      case 'D' => 'C'
      case 'L' => 'B'
      case 'R' => 'C'
    }

    case 'D' => movement match {
      case 'U' => 'B'
      case _ => 'D'
    }

  }

  val partB = obtainCode(movements)(complexKeyPad)

  println(partB)
}
