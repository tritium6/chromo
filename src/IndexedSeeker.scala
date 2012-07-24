import io.Source
import java.io.{PrintWriter, File, RandomAccessFile}

/**
 * Created by IntelliJ IDEA.
 * User: Randy
 * Date: 7/23/12
 * Time: 7:03 PM
 * To change this template use File | Settings | File Templates.
 */

class IndexedSeeker(val path:String) extends Seeker{

  private val ixPath = path + """/index.txt"""
  private val ixFile = new RandomAccessFile(new File(ixPath), "rw")

  private val sourcePath = path + """/snippet.txt"""
  private val sourceFile = new RandomAccessFile(new File(sourcePath), "r")

  private val lineLengthInBytes = 41

  def index(){
//    val s = Source.fromFile(path)
//    s.getLines.next
//    s.getLines.take(10).foreach(line => {
    var line = ""
    while(line!=null){
      line = sourceFile.readLine
      if(line !=null ){
        val parts = line.split("\t")
        val ix = parts(1)
        println(parts(1) + "::" + sourceFile.getFilePointer + "::" + line)
        ixFile.seek(ix.toLong)

        val existing = ixFile.read
        if(existing == 00) {
          ixFile.write(Array(sourceFile.getFilePointer.toByte))
        } //this is a problem for legit 00 data. should pad.
      }
    }//)

  }

  def query(terms:String) = {
    val range:ChromoRange = parseTerms(terms)

    println(findPoint(range.start.index))
    println(findPoint(range.end.index))
  }

  def findPoint(ix:Int) = {
    var bArr = Array[Byte]()
    ixFile.seek(ix)
    var sourceIx = ixFile.read
    while(sourceIx == 0) {
      sourceIx = ixFile.read  //should speed this up instead of reading one byte at a time
    }
    while(sourceIx !=0) {
      bArr = bArr :+ sourceIx.toByte
      sourceIx = ixFile.read
    } //this is a problem for legit 00 data. should pad.

//      println(ixFile.getFilePointer)

//    println(sourceIx)
    val spot = bArr.foldLeft(0)((z,y) => (z << 8) + y)  //todo: I think this is wrong

    sourceFile.seek(spot - lineLengthInBytes)
    var content = sourceFile.readLine
    if(!content.contains("chr")) content = sourceFile.readLine
    println(content)
    content
  }

}