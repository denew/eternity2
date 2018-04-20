package eternityscala.pieces

/**
* For efficiency, each piece is pre-computed as 4 groups of 4 bytes,
* representing the pattern indexes of all 4 piece rotations. Clients will
* therefore read the piece array from indexes 0, 4, 8 and 12 to simulate
* rotation. The '1' based piece identifier is found from the piece's index
* in the containing array (with 1 added) as this structure is immutable.
* <p/>
* Accordingly, any re-ordering or shuffling does not use the actual 'pieces'
* structure. The 'shuffle' list is used instead, with its elements being
* used as indexes into the pieces array
* <p/>
* Any fixed piece is treated as a special case here to avoid having to
* handle its logic differently in the main flow--it is specified with all 4
* rotations the same. Its orientation is therefore as specified in the rules,
* not necessarily its 'raw' orientation
* <p/>
* Almost all numbering is byte-based and therefore requires offsets/corrections
* to produce readable results. This is an optimisation attempt, but with no real
* measurements to support it, just 'gut feel'
*/
abstract class Pieces {
    // dimension of the 'real' board, i.e. without dummy border
    val boardDimension: Int
    
    // number of different patterns/colours, not including the border (0)
    val patternCount: Int

    // the list of pieces to place
    def pieces: List[Array[Byte]]

    /**
    * Auditing
    */
    def countPatterns(pieces: List[Array[Byte]], multiplier: Byte) = {
        import scala.collection.mutable
        val map = mutable.Map.empty[Byte, Byte]
        // data collection...
        pieces.foreach(_.slice(0, 4).foreach((byte: Byte) => {
                val count = map.get(byte)
                count match {
                    case Some(value) => map.put(byte, (value + 1).toByte)
                    case None => map.put(byte, 1)
                }
        }))

        // ...and results
        List.range(0, (patternCount + 1) * multiplier).foreach((pattern: Int) => {
        val c = map.get(pattern.toByte)
        c match {
            case Some(value) => println(pattern + " -> " + value)
            case None => null   // of no interest
        }})
    }
}

object Pieces {
    val rand = new java.util.Random
    
    def shuffle(xs: List[Array[Byte]]): List[Array[Byte]] = xs match {
      case List() => List()
      case xs => {
        val i = rand.nextInt(xs.size);
        xs(i) :: shuffle(xs.take(i) ++ xs.drop(i+1))
      }
    }

    /**
    * Hard-coded for very slight performance improvement. Note that the 5th
    * array slot (piece number/identifier) is not touched. This is clockwise--
    * direction can be significant
    * <p/>
    * Note also that a piece retains its rotation(s) if going back up the
    * recursive stack
    */
    def rotate(piece: Array[Byte]) {
        val tmp = piece(0)
        piece(0) = piece(3)
        piece(3) = piece(2)
        piece(2) = piece(1)
        piece(1) = tmp
    }

    def rotateAntiClockwise(piece: Array[Byte]) {
        val tmp = piece(0)
        piece(0) = piece(1)
        piece(1) = piece(2)
        piece(2) = piece(3)
        piece(3) = tmp
    }
}

