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

  def snippet(size:Int){
    println("Snipping " + path)
    val sb = new StringBuilder
    var i=0
    val s = Source.fromFile(path + """/probes.txt""")
    s.getLines().next()
    s.getLines().take(size).foreach(line => {              //.takeWhile(s=>s.startsWith("chr1"))
      i +=1
      println( i + "::" + line)
      sb.append(line).append("\r\n")
    })

    val outPath = path + """/snippet.txt"""
    val out = new PrintWriter(outPath)
    try{ out.print( sb.toString() ) }
    finally{ out.close() }
  }
}

case class ChromoCoords(chromosome:Int, index:Int)
case class ChromoRange(start:ChromoCoords, end:ChromoCoords)