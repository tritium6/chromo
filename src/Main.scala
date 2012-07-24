import annotation.tailrec
import java.io.{File, RandomAccessFile}
import java.nio.channels.FileChannel.MapMode
import java.nio.charset.Charset

/**
 * Created by IntelliJ IDEA.
 * User: Randy
 * Date: 7/20/12
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */


//Don't know enough about data to optimize. Are chromosomes same size? Can regions overlap?

object Main {
  def main(args:Array[String]) {
    val options = nextOption(Map(), args.toList)
    val mode = options.getOrElse(Symbol("mode"), "1").asInstanceOf[String]
    val dataPath = options.getOrElse(Symbol("path"), """C:\Users\Randy\IdeaProjects\BioDiscovery\probes.txt""" ).asInstanceOf[String]

    println(mode + dataPath)

//    val s = new Seeker(dataPath)
//
//    s.seek(3545000, 8527000)
  }
}

class Seeker (path:String) {
  val file = new RandomAccessFile(new File(path), "r")

  def seek(targetL:Long, targetH:Long): Option[Long] = {
    val low = seek(true, targetL, file.length/2)
    val hi = seek(false, targetH, file.length/2, (low.get, file.length))
    Option(0)

  }

//  @tailrec
  def seek(low: Boolean, target:Long, i:Long, prev:(Long, Long) = (0, file.length)): Option[Long] = {
    val currentLine = {
      file.seek(i)
      file.readLine
      file.readLine //get next FULL line
    }

    val currentIndex = {
      val l = currentLine.split("\t")
      if(low) l(1) else l(2)
    }.toLong

    println(currentIndex)

    if(prev == (0,0) || prev == (file.length, file.length)) None
    else if(prev._1 - prev._2 == 1 || prev._1 - prev._2 == -1 ) Some(prev._1)

    else if(target > currentIndex) seek(low, target, (prev._2 + i)/2, (i, prev._2))
    else if(target < currentIndex) seek(low, target, (prev._1 + i)/2, (prev._1, i))
    else None
//    Option(0)
  }
}

//Based on http://jectbd.com/?p=1557
type OptionMap = Map[Option[String]]

//class OptionParser(){

  // Recursively parse the arguments provided in remainingArguments
// adding them to the parsedArguments map and returning the
// completed map when done.
def nextOption(parsedArguments: OptionMap,
               remainingArguments: List[String]): OptionMap = {
  // Does a string look like it could be an option?
  def isOption(s: String) = s.startsWith("--")
  // Match the remaining arguments.
  remainingArguments match {
  // Nothing left so just return the parsed arguments
    case Nil => parsedArguments

    case "--path" :: value :: tail =>
      nextOption(parsedArguments ++ Map(Symbol("path") -> value),
      tail)

    case "--mode" :: value :: tail =>
      nextOption(parsedArguments ++ Map(Symbol("mode") -> value),
      tail)

    case unknownOption :: tail =>
      error("Unknown option " + unknownOption)
      exit(1)
    }
    // Option defining the port to listen on. Use the value after the
    // --port option as the number and continue parsing with the
    // remainder of the list.
//    case "--port" :: value :: tail =>
//      nextOption(parsedArguments ++ Map(Symbol("port") -> value.toInt),
//        tail)
    // The data directory. This case matches if the directory comes
    // before the port option, the directory doesn't look like an
    // option (doesn't start with --) and the string after it
    // does. Here parsing needs to continue with tail of the
    // arguments provided to this call as the next
    // iteration must consider possibleOption.
//    case dir :: possibleOption :: tail
//      if !isOption(dir) && isOption(possibleOption) =>
//      nextOption(parsedArguments ++ Map(Symbol("dir") -> dir),
//        remainingArguments.tail)
    // Data directory. This matches the last element in the list if it
    // doesn't look like an option. As we know there is nothing
    // left in the list use Nil for the remainingArguments passed
    // to the next iteration.
//    case dir :: Nil
//      if !isOption(dir) =>
//      nextOption(parsedArguments ++ Map(Symbol("dir") -> dir),
//        Nil)
    // Nothing else matched so this must be an unknown option.

}