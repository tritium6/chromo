import io.Source
import java.io.PrintWriter

/**
 * Created by IntelliJ IDEA.
 * User: Randy
 * Date: 7/23/12
 * Time: 7:05 PM
 * To change this template use File | Settings | File Templates.
 */

trait Seeker {
  val path:String

  def index()

  def query(terms:String)

  protected def parseTerms(terms:String) = {
    val q = """chr\s*(\d+)\s*:(\d+)\s*-\s*chr\s*(\d+)\s*:\s*(\d+)\s*""".r
    val q(c1,i1,c2,i2) = terms
    ChromoRange(ChromoCoords(c1.toInt, i1.toInt), ChromoCoords(c2.toInt, i2.toInt))
  }

    def snippet(){
    println("Loading " + path)
    var lc = 0

    val sb = new StringBuilder

    val s = Source.fromFile(path)
    s.getLines.next
    s.getLines.take(1000 * 10).takeWhile(s=>s.startsWith("chr1")).foreach(line => {
      println(line)
      sb.append(line).append("\r\n")
    })

    val outPath = """C:\Users\Randy\IdeaProjects\BioDiscovery\snippet.txt"""
    val out = new PrintWriter(outPath)
    try{ out.print( sb.toString ) }
    finally{ out.close }
//    while(lc < 1000)
  }
}

case class ChromoCoords(chromosome:Int, index:Int)
case class ChromoRange(start:ChromoCoords, end:ChromoCoords)