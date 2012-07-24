import annotation.tailrec
import java.io.{File, RandomAccessFile}

/**
 * Created by IntelliJ IDEA.
 * User: Randy
 * Date: 7/23/12
 * Time: 9:11 PM
 * To change this template use File | Settings | File Templates.
 */

class BinarySeeker(val path:String) extends Seeker{

  case class DataColumn(col:Int)
  case class Chrom() extends DataColumn(0)
  case class Start() extends DataColumn(1)
  case class End() extends DataColumn(2)
  case class Data() extends DataColumn(3)

  private val sourcePath = path + """/snippet.txt"""
  private val sourceFile = new RandomAccessFile(new File(sourcePath), "r")

  def index(){} //Does not index

  def query(terms:String){
    val range:ChromoRange = parseTerms(terms)
//    val chrom = seek()
    val start = seek(Start(), range.start.index, sourceFile.length/2, (0, sourceFile.length))
    val end = seek(End(), range.end.index, sourceFile.length/2, (0, sourceFile.length))

//    println(start)
//    println(end)

    for(s <- start; e <- end) printPointsFrom(s._2, e._2)
  }

  def printPointsFrom(start:Long, end:Long) = {
//    println(start + " " + end)
    println("Result:\n")
    sourceFile.seek(start)
    while(sourceFile.getFilePointer <= end) {
      sourceFile.readLine()
      val line = sourceFile.readLine()
      println(line.split("\t")(Data().col))
    }
  }

  @tailrec
  private final def seek(pos:DataColumn, target:Long, startPos:Long, prev:(Long, Long) = (0, sourceFile.length)): Option[(String,Long)] = {
    var hadProblem = false  //mutable allows us to keep tailrec

    val currentLine = {
      sourceFile.seek(startPos)
      sourceFile.readLine
      sourceFile.readLine //get next FULL line
    }

    val currentIndex = try {
      val currentIndexPart = currentLine.split("\t")(pos.col) //There's a \ u in the data!
      currentIndexPart.toLong
    } catch {
        case n:NumberFormatException => {
          n.printStackTrace()
          hadProblem = true
          0
        }
        case n: Exception => {
          n.printStackTrace()
          hadProblem = true
          0
        }
    }

//    println(currentIndex)
    if (hadProblem) seek(pos, target, startPos + 1, prev) //skip ahead
    else if(prev == (0,0) || prev == (sourceFile.length, sourceFile.length)) None  //EOF
    else if ((currentIndex == target) //direct hit
      || (prev._1 - prev._2 == 1 || prev._1 - prev._2 == -1 )) {  //Between 2 points
      Some((currentLine, startPos)) //Can improve win condition
    }//Some(prev._1)
    else if(target > currentIndex) seek(pos, target, (prev._2 + startPos)/2, (startPos, prev._2))
    else if(target < currentIndex) seek(pos, target, (prev._1 + startPos)/2, (prev._1, startPos))
    else None
//    Option(0)
  }
}

