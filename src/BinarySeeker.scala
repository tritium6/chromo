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

  private val sourcePath = """C:\Users\Randy\IdeaProjects\BioDiscovery\snippet.txt"""
  private val sourceFile = new RandomAccessFile(new File(sourcePath), "r")

  def index(){} //Does not index

  def query(terms:String){
    val range:ChromoRange = parseTerms(terms)
//    val chrom = seek()
    val start = seek(Start(), range.start.index, sourceFile.length/2, (0, sourceFile.length))
    val end = seek(End(), range.end.index, sourceFile.length/2, (0, sourceFile.length))

    println(start)
    println(end)

  }

    //  @tailrec
  def seek(pos:DataColumn, target:Long, startPos:Long, prev:(Long, Long) = (0, sourceFile.length)): Option[String] = {
    val currentLine = {
      sourceFile.seek(startPos)
      sourceFile.readLine
      sourceFile.readLine //get next FULL line
    }

    val currentIndex = currentLine.split("\t")(pos.col).toLong

//    println(currentIndex)

    if(prev == (0,0) || prev == (sourceFile.length, sourceFile.length)) None
    else if(prev._1 - prev._2 == 1 || prev._1 - prev._2 == -1 ) { //Can improve win condition
      Some(currentLine)
    }//Some(prev._1)

    else if(target > currentIndex) seek(pos, target, (prev._2 + startPos)/2, (startPos, prev._2))
    else if(target < currentIndex) seek(pos, target, (prev._1 + startPos)/2, (prev._1, startPos))
    else None
//    Option(0)
  }
}

