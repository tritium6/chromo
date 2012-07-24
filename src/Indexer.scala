/**
 * Created by IntelliJ IDEA.
 * User: Randy
 * Date: 7/23/12
 * Time: 6:51 PM
 * To change this template use File | Settings | File Templates.
 */

object Indexer{
  def main(args:Array[String]) {
    val options = nextOption(Map(), args.toList)
    val mode = options.getOrElse("mode", "binary")
    val dataPath = options.getOrElse("path", """C:\Users\Randy\IdeaProjects\BioDiscovery\""" )

    val seeker = mode match {
      case "binary" => new BinarySeeker(dataPath).asInstanceOf[Seeker]
      case "indexed" => new IndexedSeeker(dataPath).asInstanceOf[Seeker]
    }

    options.get("snippet") foreach( size => seeker.snippet(size.toInt) )  //Cheat by shrinking dataset

    if(options.contains("index")) seeker.index()

    val startTime = System.currentTimeMillis

    options.get("query").foreach(term => {
      println("Querying... " + term)
      seeker.query(term)
    })

    val endTime = System.currentTimeMillis

    println("QueryTime: " + (endTime - startTime ) + "ms")
  }

  //Based on http://jectbd.com/?p=1557
  type OptionMap = Map[String, String]

  def nextOption(parsedArguments: OptionMap,
               remainingArguments: List[String]): OptionMap = {
      // Does a string look like it could be an option?
    def isOption(s: String) = s.startsWith("--")
    // Match the remaining arguments.
    remainingArguments match {
    // Nothing left so just return the parsed arguments
      case Nil => parsedArguments

      case "--path" :: value :: tail =>
        nextOption(parsedArguments ++ Map("path" -> value),
        tail)

      case "--mode" :: value :: tail =>
        nextOption(parsedArguments ++ Map("mode" -> value),
        tail)

      case "--query" :: value :: tail =>
        nextOption(parsedArguments ++ Map("query" -> value),
        tail)

      case "--index" :: tail =>
        nextOption(parsedArguments ++ Map("index" -> ""),
        tail)

      case "--snippet" :: value :: tail =>
        nextOption(parsedArguments ++ Map("snippet" -> value),
        tail)

      case unknownOption :: tail =>
        sys.error("Unknown option " + unknownOption)
        sys.exit(1)
    }
  }
}

