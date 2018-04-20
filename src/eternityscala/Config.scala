package eternityscala

object Config {
    //import scala.io.Source
    import java.util.Properties
    import java.io.FileInputStream

    // list.get(0).asInstanceOf[Person]

    //val configFile = "C:/Users/dene/NetBeansProjects/EternityScala/src/eternityscala/config.properties"
    val configFile = "src/eternityscala/config.properties"
    val props = new Properties
    val fis = new FileInputStream(configFile)
    props.load(fis)

    def property(key: String) = {
        props.getProperty(key)
    }
    
    implicit def string2Int(str: String): Int =
        try {
            Integer.parseInt(str)
        } catch {
            case nfe: NumberFormatException => -1
        }

    implicit def string2ByteArray(str: String): Array[Byte] = {
        val parts = str.split(",")
        val out = new Array[Byte](parts.length)
        var index = 0
        parts.foreach((part: String) => {
                out.update(index, string2Int(part).toByte)
                index = index + 1
            })
        out
    }

    implicit def string2Boolean(str: String): Boolean =
        str.trim.toLowerCase == "true"


//    def property(key: String) {
//        def processLine(line: String) {
//            if (line.startsWith(key)) {
//                val parts = line.split("=")
//                val str: String = parts(1)
//                println(str)
//                return str.trim
//            }
//        }
//
//        val source = Source.fromFile(configFile)
//        for (line <- source.getLines)
//            processLine(line)
//    }

            /**
        * Save the pieces list at points of interest in case we need to
        * revert and try again from that point with a less strict matcher
        */
        //def cache(highWater: Short, pieces: List[Array[Byte]]) = {
            //import scala.collection.mutable
            //val keep = mutable.Map.empty[Short, List[Array[Byte]]]
            //if (keep.contains(highWater)) {
            //  keep(highWater)
            //}
            //else
            //  keep + (highWater -> pieces)
        //}
}
