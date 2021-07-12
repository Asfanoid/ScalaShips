package blah

import java.io.{File, PrintWriter}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object PuzzleSolver extends App{


  // Endre din path her fjerne home/asfand/ideaprojects til hva nå enn dere har

  val unsolved_puzzle_path = "C:/Users/eivin/OneDrive/Desktop/New folder/eivine16/ScalaAssignment/src/puzzles_unsolved.txt"

  val lines = scala.io.Source.fromFile(unsolved_puzzle_path).mkString.split("\n")
  val lines2 = scala.io.Source.fromFile(unsolved_puzzle_path).getLines.toList

  val one = removeSpace(lines(6))
  val two = removeSpace(lines(7))
  val three = removeSpace(lines(8))
  val four = removeSpace(lines(9))
  val five = removeSpace(lines(10))
  val six = removeSpace(lines(11))
  val seven = removeSpace(lines(12))
  val hints = removeSpace(lines(3).drop(11)) // drops horizontal if you use the drop(11)


  val puzzleList = for(i<-List.range(0,lines.size)) yield {removeSpace(lines(i))}

  val splitList = puzzleList.span(_ == puzzleList.startsWith("size"))
  val test = lines2.partition(_=="puzzles")
  val herre = lines2.splitAt(4)

  //println(splitPuzzles(lines2))



  /*
    def testeSplit(sl:List[String]):List[Board] = {
      val puzzlesAmount = sl.head(9).toInt
      sl.filterNot(_ == "hints")

      for(i<-List.range(0,puzzlesAmount-1)) yield {
        convertToPuzzle(i,sl)
      }

      /*
      while(i< puzzlesAmount*puzzleSize) {
        convertToPuzzle(i,sl)
        i = i+puzzleSize+4
      }*/

    }*/

  def convertToPuzzle(sl:List[String]):Board = {
    if(sl.contains("hints")) {
      val horiHints = convertHintValuesToInt(removeSpace(sl(sl.indexWhere(_.contains("horizontal")))).drop(1))
      val vertHints = convertHintValuesToInt(removeSpace(sl(sl.indexWhere(_.contains("vertical")))).drop(1))
      // to get the board
      val input = sl.splitAt(sl.indexWhere(_.contains("hints"))+1)
      val initialStringBoard = input._2
      val nonSpacedBoard = for(i<-initialStringBoard) yield removeSpace(i)
      val finalBoard = for(i<-nonSpacedBoard)yield convertBoardValueToInt(i)
      return (vertHints,horiHints,finalBoard)
    }
    (List(0,0,0),List(0,0,0),List(List(0,0,0),List(0,0,0),List(0,0,0)))
  }




  def recursionPuzzleMaker(sl:List[String],lb:ListBuffer[List[String]]):List[List[String]] = {
    val splitter = sl.lastIndexOf("hints")
    val puzzle = sl.splitAt(splitter-4)
    retList += puzzle._2
    println(puzzle._2)
    if(splitter != -1) {
      recursionPuzzleMaker(puzzle._1,retList)
    }
    retList.toList
  }



  var retList = new ListBuffer[List[String]]
  val puzzles = recursionPuzzleMaker(lines2,retList)

  val puzzletoBoard = for(i<-puzzles)yield{convertToPuzzle(i)}

  val finalpuzzles = puzzletoBoard.reverse.drop(1)


  for(i<-finalpuzzles) yield {
    printBoard(i)
    println("--------------")
  }


  def removeSpace(s:String):List[String] = {
    s.split("\\s").toList
  }



  /* convertBoardValueToInt
  This is the conversion from the value of a square to its number which we use in our logic to
  solve the puzzle.

  TODO: remove logicList as a value to increase performance (less use of memory) - Done 9/13/2018 4PM
  TODO: add all the values to the board - Done 9/15/2018 6PM
 */

  def convertBoardValueToInt(sl:List[String]):List[Int] = {
    for (i <- sl) yield {
      i match {
        case "?" => 0
        case "-" => 1
        case "S" => 3 // 2 is final ship
        case "A" => 4
        case "V" => 5
        case "<" => 6
        case ">" => 7
        case "+" => 8
        case "*" => 9
        case _ => -999
      }
    }
  }



  /*
  def returnBoard(puzzles:String):List[Board] = {

  for(puzzles.splitfunkjson.removespace.converthint table på hintsa / convertboardvalue på board values)
  yield List[Board]

  finner hvor det står horizontal i index 0
  dropright(1)

   */

  /*

  TODO: Fix so that we can have first value of horizontal or vertical to become either -1 or -2
  TODO: Add this feature to convertValue to int, to reduce functions
   */

  def convertHintValuesToInt(sl:List[String]):List[Int] = {
    for (i <- sl) yield {
      i.toInt
    }
  }

  val x1 = convertBoardValueToInt(one)
  val x2 = convertBoardValueToInt(two)
  val x3 = convertBoardValueToInt(three)

  val x4 = convertBoardValueToInt(four)
  val x5 = convertBoardValueToInt(five)
  val x6 = convertBoardValueToInt(six)
  val x7 = convertBoardValueToInt(seven)





  val y = convertHintValuesToInt(hints)
  val x = convertHintValuesToInt(removeSpace(lines(4).drop(9)))


  //@tailrec
  def alterBoard(b:Board):Board = {
    println("-----------------------------------------")
    printBoard(b)
    println("-----------------------------------------")
    for (i<-List.range(0, b._1.length)) {
      val board_transposed = b._3.transpose
      if (b._1(i) == 0) {
        return alterBoard(b._1
          .updated(i,-1),b._2,b._3
          .updated(i,List.fill(b._3(i).length)(1)))
      }
      if (b._2(i) == 0) {
        return alterBoard(b._1,b._2
          .updated(i,-1),b._3.transpose
          .updated(i,List.fill(b._3(i).length)(1)).transpose)
        /*
        Grunnen til at det er så mange transpose. Første transpose transpose er for å komme i riktig linje.
        den andre transposen etter fyllingen er for å sette verdien i riktig linje. Dette gjør det mulig for å
        lagre alle de gamle verdiene som var der, ellers blir de overskrevet.

        Listen med -1 som hint betyr at hint listen er fullført.
         */
      }
      if(b._1(i)==b._3(i).count(_ > 1)) {
        return alterBoard(b._1
          .updated(i,-1),b._2,b._3
          .updated(i,b._3(i).map(x => if(x==0) 1 else x)))
      }
      if(b._2(i)==board_transposed(i).count(_ > 1)) {
        return alterBoard(b._1,b._2
          .updated(i,-1),board_transposed
          .updated(i,board_transposed(i).map(x => if(x==0) 1 else x)).transpose)
      }
      if(b._3(i).exists(_ > 2) ) {
        return alterBoard(b._1,b._2,shipSquareAlteration(i,b._3))
      }
      if(b._1(i)==b._3(i).count(_ != 1)) {
        return alterBoard(b._1
          .updated(i,-1),b._2,b._3
          .updated(i,b._3(i).map(x => if(x==0) 3 else x)))
      }
      if(b._2(i)==board_transposed(i).count(_ != 1)) {
        return alterBoard(b._1,b._2
          .updated(i,-1),board_transposed
          .updated(i,board_transposed(i).map(x => if(x==0) 3 else x)).transpose)
      }
    }
    b
  }

  def shipSquareAlteration(index:Int,values:List[List[Int]]):List[List[Int]] = {
    for (lane <-values) {
      for(laneIndex<-List.range(0,lane.size)) {
        if(lane(laneIndex)==3) {return removeDiagonal(index,laneIndex,values)}
        if(lane(laneIndex)==4) {return removeDiagonal(index,laneIndex,shipTop(index,laneIndex,values))}
        if(lane(laneIndex)==5) {return removeDiagonal(index,laneIndex,shipBottom(index,laneIndex,values))}
        if(lane(laneIndex)==6) {return removeDiagonal(index,laneIndex,shipLeft(index,laneIndex,values))}
        if(lane(laneIndex)==7) {return removeDiagonal(index,laneIndex,shipRight(index,laneIndex,values))}
        if(lane(laneIndex)==8) {return removeDiagonal(index,laneIndex,values)}
        if(lane(laneIndex)==9) {return removeDiagonal(index,laneIndex,shipSingle(index,laneIndex,values))}
      }
    }
    values
  }

  def shipLeft(index:Int,indexOfIndex:Int,values:List[List[Int]]):List[List[Int]] = {
    if(indexOfIndex == 0) {
      values
        .updated(index,values(index)
          .updated(indexOfIndex,2) // finalize itself to ship
          .updated(indexOfIndex+1,3)) // converts right side to an unknown ship
    }else {
      values
        .updated(index,values(index)
          .updated(indexOfIndex,2) // finalize itself to ship
          .updated(indexOfIndex+1,3) // converts right side to an unknown ship
          .updated(indexOfIndex-1,1)) // converts left side to water
    }
  }

  def shipRight(index:Int,indexOfIndex:Int,values:List[List[Int]]):List[List[Int]] = {
    if(indexOfIndex == values(index).size-1) {
      values
        .updated(index,values(index)
          .updated(indexOfIndex,2) // finalize itself to ship
          .updated(indexOfIndex-1,3)) // converts left side to an unknown ship
    }else {
      values
        .updated(index,values(index)
          .updated(indexOfIndex,2) // finalize itself to ship
          .updated(indexOfIndex-1,3) // converts left side to an unknown ship
          .updated(indexOfIndex+1,1)) // converts right side to water
    }

  }

  def shipTop(index:Int,indexOfIndex:Int,values:List[List[Int]]):List[List[Int]] = {
    if(index == 0) {
      values
        .updated(index,values(index)
          .updated(indexOfIndex,2))
        .updated(index+1,values(index+1)
          .updated(indexOfIndex,3))
    }else {
      values
        .updated(index,values(index)
          .updated(indexOfIndex,2))
        .updated(index+1,values(index+1)
          .updated(indexOfIndex,3))
        .updated(index-1,values(index)
          .updated(indexOfIndex,1))
    }
  }

  def shipBottom(index:Int,indexOfIndex:Int,values:List[List[Int]]):List[List[Int]] = {
    if(index == values(index).size-1) {
      values
        .updated(index,values(index)
          .updated(indexOfIndex,2))
        .updated(index-1,values(index-1)
          .updated(indexOfIndex,3))
    }else {
      values
        .updated(index,values(index)
          .updated(indexOfIndex,2))
        .updated(index-1,values(index-1)
          .updated(indexOfIndex,3))
        .updated(index+1,values(index)
          .updated(indexOfIndex,1))
    }

  }

  def removeDiagonal(index:Int,indexOfIndex:Int,values:List[List[Int]]):List[List[Int]] = {


    if(index == 0) {
      if(indexOfIndex == 0) {
        return values
          .updated(index+1,values(index+1) // lower lane
            .updated(indexOfIndex+1,1)) // converts bottom right to water
          .updated(index,values(index)
          .updated(indexOfIndex,2)) // finalizes the ship to a final ship
      }
      if (indexOfIndex == values(index).size-1) {
        return values
          .updated(index+1,values(index+1) // lower lane
            .updated(indexOfIndex-1,1)) // converts bottom left to water
          .updated(index,values(index)
          .updated(indexOfIndex,2)) // finalizes the ship to a final ship
      }else {
        return values
          .updated(index+1,values(index+1) // lower lane
            .updated(indexOfIndex+1,1) // converts bottom right to water
            .updated(indexOfIndex-1,1)) // converts bottom left to water
          .updated(index,values(index)
          .updated(indexOfIndex,2)) // finalizes the ship to a final ship
      }
    }
    println(index)
    println(values(index).size-1)

    if(index == values(index).size-1) {
      if(indexOfIndex == 0) {
        return values
          .updated(index-1,values(index-1) // upper lane
            .updated(indexOfIndex+1,1)) // converts top right to water
          .updated(index,values(index)
          .updated(indexOfIndex,2)) // finalizes the ship to a final ship
      }
      if (indexOfIndex == values(index).size-1) {
        return values
          .updated(index-1,values(index-1) // upper lane
            .updated(indexOfIndex-1,1)) // converts top right to water
          .updated(index,values(index)
          .updated(indexOfIndex,2)) // finalizes the ship to a final ship
      }else {
        return values
          .updated(index-1,values(index-1) // upper lane
            .updated(indexOfIndex+1,1) // converts top right to water
            .updated(indexOfIndex-1,1)) // converts top left to water
          .updated(index,values(index)
          .updated(indexOfIndex,2)) // finalizes the ship to a final ship
      }
    }



    if(indexOfIndex == 0 ) { // update
      return values
        .updated(index-1,values(index-1) // upper lane
          .updated(indexOfIndex+1,1)) // converts top right to water
        .updated(index+1,values(index+1) // lower lane
        .updated(indexOfIndex+1,1)) // converts bottom right to water
        .updated(index,values(index)
        .updated(indexOfIndex,2)) // finalizes the ship to a final ship
    }
    if(indexOfIndex == values(index).size-1) {
      return values
        .updated(index-1,values(index-1) // upper lane
          .updated(indexOfIndex-1,1)) // converts top left to water
        .updated(index+1,values(index+1) // lower lane
        .updated(indexOfIndex-1,1)) // converts bottom left to water
        .updated(index,values(index)
        .updated(indexOfIndex,2)) // finalizes the ship to a final ship
    }else{
      values
        .updated(index-1,values(index-1) // upper lane
          .updated(indexOfIndex+1,1) // converts top right to water
          .updated(indexOfIndex-1,1)) // converts top left to water
        .updated(index+1,values(index+1) // lower lane
        .updated(indexOfIndex+1,1) // converts bottom right to water
        .updated(indexOfIndex-1,1)) // converts bottom left to water
        .updated(index,values(index)
        .updated(indexOfIndex,2)) // finalizes the ship to a final ship
    }
  }

  def shipSingle(index:Int,indexOfIndex:Int,values:List[List[Int]]):List[List[Int]] = {


    if(index == 0) {
      if(indexOfIndex == 0) {
        return values
          .updated(index,values(index) // current lane, change it with same lane but sides ==1
            .updated(indexOfIndex+1,1) // right
            .updated(indexOfIndex,2)) // finalizes the ship to a final ship
          .updated(index+1,values(index+1)  // lower lane, take the current lower lane and convert the following
          .updated(indexOfIndex,1))
      }
      if(indexOfIndex == values(index).size-1) {
        return values
          .updated(index,values(index) // current lane, change it with same lane but sides ==1
            .updated(indexOfIndex-1,1) // left
            .updated(indexOfIndex,2)) // finalizes the ship to a final ship
          .updated(index+1,values(index+1)  // lower lane, take the current lower lane and convert the following
          .updated(indexOfIndex,1))
      }else {
        return values
          .updated(index,values(index) // current lane, change it with same lane but sides ==1
            .updated(indexOfIndex+1,1) // right
            .updated(indexOfIndex-1,1) // left
            .updated(indexOfIndex,2)) // finalizes the ship to a final ship
          .updated(index+1,values(index+1)  // lower lane, take the current lower lane and convert the following
          .updated(indexOfIndex,1))
      }
    }

    if(index == values(index).size-1) {
      if(indexOfIndex == 0) {
        return values
          .updated(index,values(index) // current lane, change it with same lane but sides ==1
            .updated(indexOfIndex+1,1) // right
            .updated(indexOfIndex,2)) // finalizes the ship to a final ship
          .updated(index-1,values(index-1)  // upper lane, take the current lower lane and convert the following
          .updated(indexOfIndex,1))
      }
      if(indexOfIndex == values(index).size-1) {
        return values
          .updated(index,values(index) // current lane, change it with same lane but sides ==1
            .updated(indexOfIndex-1,1) // left
            .updated(indexOfIndex,2)) // finalizes the ship to a final ship
          .updated(index-1,values(index-1)  // upper lane, take the current lower lane and convert the following
          .updated(indexOfIndex,1))
      }else {
        return values
          .updated(index,values(index) // current lane, change it with same lane but sides ==1
            .updated(indexOfIndex+1,1) // right
            .updated(indexOfIndex-1,1) // left
            .updated(indexOfIndex,2)) // finalizes the ship to a final ship
          .updated(index-1,values(index-1)  // upper lane, take the current lower lane and convert the following
          .updated(indexOfIndex,1))
      }
    }

    if(indexOfIndex==0) {
      return values
        .updated(index,values(index) // current lane, change it with same lane but sides ==1
          .updated(indexOfIndex+1,1) // right
          .updated(indexOfIndex,2)) // finalizes the ship to a final ship
        .updated(index-1,values(index-1)  // upper lane, take the current lower lane and convert the following
        .updated(indexOfIndex,1))
        .updated(index+1,values(index+1)  // lower lane, take the current lower lane and convert the following
          .updated(indexOfIndex,1))
    }

    if(indexOfIndex==values(index).size-1) {
      return values
        .updated(index,values(index) // current lane, change it with same lane but sides ==1
          .updated(indexOfIndex-1,1) // left
          .updated(indexOfIndex,2)) // finalizes the ship to a final ship
        .updated(index-1,values(index-1)  // upper lane, take the current lower lane and convert the following
        .updated(indexOfIndex,1))
        .updated(index+1,values(index+1)  // lower lane, take the current lower lane and convert the following
          .updated(indexOfIndex,1))
    }



    values
      .updated(index,values(index) // current lane, change it with same lane but sides ==1
        .updated(indexOfIndex+1,1)
        .updated(indexOfIndex-1,1)
        .updated(indexOfIndex,2)) // finalizes the ship to a final ship
      .updated(index+1,values(index+1)  // lower lane, take the current lower lane and convert the following
      .updated(indexOfIndex,1))
      .updated(index-1,values(index-1) // upper lane
        .updated(indexOfIndex,1))

  }



  def printBoard(b:Board) = {
    for (shipLane<-b._3) {
      println(shipLane)
    }
  }

  def bruteForce(b:Board):Board = {

    val r = new scala.util.Random

    val board_transposed = b._3.transpose
    for (i <- List.range(0, b._3.length-1))yield
      {
        for (j <- List.range(0, b._3(i).length-1))yield
          {
            if (b._3(i)(j) == 0) {
              b._3(i).updated(j, 2)
            }
          }
      }
    b
    }

  type Board = (List[Int], List[Int], List[List[Int]]);

  // fra stackoverflow, bare for testing.
  def printType[T](x:T) :Unit = {println(x.getClass.toString())}

  //type Board = (List[Int],List[Int],List[List[Int]])

  type BattleshipsGame =(Board,List[Int])


  def ConvertManyToOutput (b: List[Board]) = {
    for (j <- 0 to b.length-1) yield {
      val r = new scala.util.Random
      val flatList = b(j)._3.flatten;

      for (i <- flatList) yield
        i match {
          case 1 => "-"
          case 2 => "S"
          case 0 => "S"
          case _ => "S"
        }
    }
  }
  //flatten board to make it easier to iterate through



  def printMulti(l:IndexedSeq[List[String]]) = {

    val temp = l.length;

    val writer = new PrintWriter(new File("puzzle_solved.txt"))

    writer.write("puzzles " + temp + "\r\n")

    for (i <- 0 until temp) {

      val size = l(i).grouped(scala.math.sqrt(l(i).length).toInt).toList.length
      val test = l(i).grouped(scala.math.sqrt(l(i).length).toInt).toList
      val sizestr = size + "x" + size;

      writer.write("size " + sizestr + "\r\n")
      for (y <- 0 until size) {
        val values = for (x <- 0 until size) writer.write(test(y)(x) + " ")
        writer.write("\n")
      }
    }

    writer.close();

  }

  //l.length denotes the amount of solved puzzles, size denotes the length of the squared list(i)
  // and test is a grouped iterator that takes in the data from one instance of l(i) and turns it into a
  //sqrt*sqrt list

  //Still not checked for a list of different sized puzzles, and var size/test are to be replaced with val List(size)
  // and val List(test) if time allows it

  //printSingle((x,y,f), ConvertToOutput(alterBoard(x,y,f)));

  for(i<-finalpuzzles) yield {
    alterBoard(i)

    printMulti(ConvertManyToOutput((for(i<-finalpuzzles)yield alterBoard((i)))));
  }
}




// Source for git ignore: https://github.com/chronodm/scala-project-template/blob/master/.gitignore