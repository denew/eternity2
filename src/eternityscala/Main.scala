package eternityscala

/**
 * Using the supplied tour stops (see Tour), continue to place matching
 * pieces, removing them from the collection of remaining pieces.
 * <p/>
 * When none of the remaining pieces can be matched/placed, backtrack to the
 * first point where a piece can be placed and try again.
 * <p/>
 * Selection and backtracking is based on tail-recursive calls to avoid
 * the inevitable memory exhaustion that a full-size board would incur.
 * <p/>
 * For simplicity, any fixed piece (e.g. 139 in the full board) is placed
 * at grid initialisation. The piece is not included in the pieces list and the
 * tour never visits that cell. Accordingly, no program logic is required.
 * <p/>
 * If no progress beyond a highwater mark seems possible ('x' recurrences
 * of the same cell/highwater mark), reduce the matching edge requirement
 * and continue. In addition, the number of backtrack steps allowed can
 * be configured.
 * <p/>
 * Count the matching edges at the conclusion of the run and display the
 * graphical board representation.
 */
object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) = {
    import eternityscala.pieces._
    import eternityscala.tour._
    import eternityscala.matcher._
    import Config._

    // piece and cells constants
    val North: Byte = 0
    val East: Byte = 1
    val South: Byte = 2
    val West: Byte = 3
    val PieceNum: Byte = 4
    val Row: Byte = 0
    val Col: Byte = 1

    // other constants
    val MaxRotations: Byte = 3
    val ByteOffset: Short = 129

    // properties from configuration file
    val selectedPieces = property("pieces")
    val selectedTour = property("tour")
    val reportLowerLimit: Int = string2Int(property("reportLowerLimit"))
    val highWaterMark: Int = string2Int(property("highWaterMark"))
    val reportFrequency: Int = string2Int(property("reportFrequency"))
    val multiplier = string2Int(property("variationMultiplier")).toByte
    val selectedVariationPatterns = string2ByteArray(property("selectedVariationPatterns"))
    val loopLimit: Int = string2Int(property("loopLimit"))
    val shuffle = string2Boolean(property("shuffle"))
    val reportDetail = string2Boolean(property("reportDetail"))
    val outputDir = property("outputDir")
    val totalRuns: Int = string2Int(property("totalRuns"))

    // TODO--this should be abstract
    val t = new Tour
    t.init

    //def init {
    def tour: Array[Array[Short]] = selectedTour match {
      // large board
      case "LargeScanlineTour" => LargeScanlineTour.tour
      case "LargeCircularTour" => LargeCircularTour.tour
      case "LargeCornersTour" => LargeCornersTour.tour
      case "AssignmentTour" => AssignmentTour.tour
      case "LargeScanBorderTour" => LargeScanBorderTour.tour
      case "LargeChessTour" => LargeChessTour.tour
        // small board
      case "Small4CornerTour" => Small4CornerTour.tour
      case "SmallScanlineTour" => SmallScanlineTour.tour
      case _ => Array()
    }

    def pieces: List[Array[Byte]] = selectedPieces match {
      case "PiecesLarge" => PiecesLarge.pieces
      case "PiecesSmall" => PiecesSmall.pieces
      case "PiecesLargePartSort" => PiecesLargePartSort.pieces
      case _ => List()
    }
    //}
    //init

    // adjust for 'hard-coded' fixed pieces, pre-rotated to their correct
    // orientation
    tour.size match {
      // all large boards, piece 139
      case 255 => t.cells(9)(8) = Array((18 * multiplier).toByte,
                                        (6 * multiplier).toByte,
                                        (6 * multiplier).toByte,
                                        (11 * multiplier).toByte,
                                        10)
        // all small boards, piece 10 (even though no fixed piece in demo
        // version)
      case 15 => t.cells(1)(2) = Array(0,
                                       (1 * multiplier).toByte,
                                       (3 * multiplier).toByte,
                                       (1 * multiplier).toByte,
                                       -119)
        // for the small board, we can provide a tour with all 16 cells
        // clear--in this case no adjustment is made
      case _ =>
    }

    // TODO - could change this to an array of pieces based on above
    //val fixedPiecesCount: Byte = 1
        
    // keep a track of recursion progress...
    var highWaterCount: Short = 0
    // ...and iterations
    var loopCount: Int = 0

    val matchers: Array[Matcher] = Array(Match4Sides(),
                                         Match3Sides(),
                                         Match2SidesAdjacent(),
                                         Match2SidesParallel(),
                                         Match1Side(),
                                         Match0Sides())

    val startTime = System.currentTimeMillis

    /**
     * Change the mapping of patterns to add more complexity. For a multiplier
     * of 2, a selected pattern, e.g. pattern 1, will now be represented by 1
     * and 2. Non-selected patterns will double their pattern value, but
     * retain their ratio with others. For a multiplier of 2 and selected
     * patterns of 2 and 4, the patterns will therefore now be:
     * <p/>
     * 0,2,3,4,6,7,8,10,12...44
     * <p/>
     * Pieces are assigned one of the pattern possibilities
     * predictably (using a simple round-robin selection), but this could be
     * random in future. The edge pattern (0) does not change
     */
    def addVariation(selectedPatterns: Array[Byte]) = {
      var offset: Byte = 0
      def newPattern(currentPattern: Byte): Byte = {
        val pattern: Byte = ((currentPattern * multiplier) - offset).toByte
        if (offset < multiplier - 1)
          offset = (offset + 1).toByte
        else
          offset = 0
        pattern
      }

      def isInSelected(pattern: Byte) = {
        val find = selectedPatterns.find(_ == pattern * multiplier)
        find match {
          case Some(v) => true
          case _ => false
        }
      }

      pieces.foreach((piece: Array[Byte]) => {
          var count: Byte = 0
          piece.slice(0, 4).foreach((pattern: Byte) => {
              if (isInSelected(pattern))
                piece.update(count, newPattern(pattern))
              else
                piece.update(count, (pattern * multiplier).toByte)
              count = (count + 1).toByte
            }
          )
        })
    }

    // NB - for simplicity do not use 18, 6 or 11 (used
    // in the fixed piece). Zero (edge) is never included
    //if (selectedVariationPatterns.length > 0)
      addVariation(selectedVariationPatterns)

    if (reportDetail)
      selectedPieces match {
        case "PiecesLarge" => PiecesLarge.countPatterns(pieces, multiplier)
        case "PiecesSmall" => PiecesSmall.countPatterns(pieces, multiplier)
        case _ =>
      }

    // TODO-config values need dblng also--wrong
    def timeBlock(block: => Unit) {
      val start = System.currentTimeMillis
      block
      printf("Block took {0} milliseconds\n", System.currentTimeMillis - start)
    }

    List.range(0, totalRuns).foreach((counter: Int) => {
        try {
          println("run " + counter)
          //Thread.sleep(10000)
          loopCount = 0
          val tourStop: Short = 0
          tour
          pieces
          if (shuffle)
            place(tourStop, Pieces.shuffle(pieces))
          else
            place(tourStop, pieces)
        } catch {
          case e: RuntimeException => {
              println(e.getMessage)
            }
        }
      })

    // forward declarations required here

    def writeGrid(pieces: Array[Array[Array[Byte]]], remainder: List[Array[Byte]], showQuadrants: Boolean, count: Short) = {
      val str = printGrid(pieces, remainder, showQuadrants)
      import java.io.{File, FileWriter}
      import java.util.Date
      val d = new Date
      val date_str = d.getMonth + "_"
      + d.getDay + "_" + d.getHours + "_"
      + d.getMinutes + "_" + d.getSeconds
      val f = new File(outputDir, count + "_" + date_str + ".txt")
      val fw = new FileWriter(f)
      try {
        fw.write(str)
      }
      finally {
        fw.close
      }

    }

    /**
     * Assumes up to 3 figure numbers for the piece identifier,
     * 2 figures for the patterns
     */
    def printGrid(pieces: Array[Array[Array[Byte]]], remainder: List[Array[Byte]], showQuadrants: Boolean) = {
      var str = new StringBuilder

      // includes the dummy border
      val dimension = pieces.length

      def pad(num: Short) = {
        if (num == -1)
          " " + num
        else if (num < (10))
          "  " + num
        else if (num < (100))
          " " + num
        else num
      }
      /**
       * Convert byte range of -128 to +127 to actual piece number (1-
       * based) form
       */
      def pieceIdentifier(b: Byte) = {
        (ByteOffset + b).toShort
      }
            
      def topRow(row: Array[Array[Byte]]) {
        // subArray--don't show the dummy border columns
        row.slice(1,dimension - 1).foreach((piece: Array[Byte]) =>
          rowFormat(piece, "   \\" + pad(piece(North)) + "/    "))
      }
      def middleRow(row: Array[Array[Byte]]) {
        row.slice(1,dimension - 1).foreach((piece: Array[Byte]) => {
            if (showQuadrants)
              rowFormat(piece, pad(piece(West)) + "|" + pad(pieceIdentifier(piece(PieceNum))) + "|" + pad(piece(East)) + " ")
            else
              rowFormat(piece, "  " + pad(pieceIdentifier(piece(PieceNum))))
          })
      }
      def bottomRow(row: Array[Array[Byte]]) {
        row.slice(1,dimension - 1).foreach((piece: Array[Byte]) =>
          rowFormat(piece, "   /" + pad(piece(South)) + "\\    "))
      }
      def spaces(count: Byte) {
        for (i <- 0 to count)
          str.append(" ")
      }
      // don't show empty cells
      def rowFormat(piece: Array[Byte], func: String) = {
        if (!isBlank(piece))
          str.append(func)
        else
          // for current layout
        if (showQuadrants) spaces(11) else spaces(4)
      }

      /**
       * Using Byte offsets, a zero identifier is a legitimate piece.
       * Therefore we can only identify 'blank' pieces in concert with the
       * quadrants (a pattern of -1 being blank)
       */
      def isBlank(piece: Array[Byte]) = {
        var sum = 0
        piece.slice(0, 4).foreach(sum += _)
        sum == -4 && piece(4) == 0
      }

      def piecesToCsv() {
        pieces.slice(1,dimension - 1).foreach(_.slice(1,dimension - 1).foreach((piece: Array[Byte]) =>
            if (!isBlank(piece)) str.append(piece(4) + ByteOffset + ",")))
      }

      // don't show the dummy border rows
      for (i <- 1 until dimension - 1) {
        if (showQuadrants)
          topRow(pieces(i)); str.append("\n")
        middleRow(pieces(i)); if (showQuadrants) str.append("\n")
        if (showQuadrants)
          bottomRow(pieces(i)); str.append("\n")
      }

      // these are the placed pieces...
      piecesToCsv()
      str.append("\n\n")
      // ... and those remaining
      remainder.foreach((piece: Array[Byte]) => str.append(piece(4) + ByteOffset + ","))

      str.toString
    }

    def printPieces(ps: List[Array[Byte]]) = {
      ps.foreach((piece: Array[Byte]) => printPiece(piece))
      println("")
    }

    /**
     * Each time a piece is printed, it is the current orientation, not
     * its 'out-of-the-box' orientation
     */
    def printPiece(piece: Array[Byte]) = {
      piece.slice(0, 4).foreach((x: Byte) => {
          if (reportDetail)
            print(x + ".")
        })
      if (reportDetail)
        print("*")
      print(piece(4) + ByteOffset + "  ")
    }

    /**
     * Recursively attempt to place a piece from the list
     */
    def place(tourStop: Short, pieces: List[Array[Byte]]): Unit = {
      //printPieces(pieces)
      if (tourStop > highWaterCount) {
        highWaterCount = tourStop
        if (highWaterCount >= reportLowerLimit)
          println("highWater=" + highWaterCount)
        if (highWaterCount >= highWaterMark ) {     // equals for small board, always solved
          // write out the solution to file
          //println(printGrid(t.cells, reportDetail))
          writeGrid(t.cells, pieces, reportDetail, highWaterCount)
          println("new best, placed=" + t.placedPieces + ", edge count=" + t.matchedEdges + "/" + t.matchableEdges + ", verify=" + t.check + "\n")
        }
      }
      loopCount += 1
      if (loopCount % reportFrequency == 0) println(loopCount + " tourstop=" + tourStop)

      if (loopCount == loopLimit) {         // 188 best, scan 165, circ 99, corner 43
        println("stop limit hit " + loopCount)
        println("Run complete in " + (System.currentTimeMillis - startTime) / 1000 + " second(s)")
        throw new RuntimeException("No improvement--highWater=" + highWaterCount)
        //return
        tour
        pieces
      }
 
      val coords = tour(tourStop)
      val template = t.template(coords(Row),(coords(Col)))

      // TODO - hide the rotations in the matcher?
      def canFit(piece: Array[Byte]): Boolean = {
        // only rotate 3 times, but any of the 4 orientations could match
        for (i <- 0 to MaxRotations) {
          if (Match4Sides().matcher(template, piece, multiplier))
            return true
          else
            Pieces.rotate(piece)
        }
        false
      }

      /**
       * Reload the board from the best (cached) position in this run
       */
      def fillFromCache() {

      }

      /**
       * from the best point on, which we can't improve on (entry in cache), try
       * the next piece with a 3 sides matcher. If no success, try 2, 1, and 0 until
       * a piece is placed. Then revert to the 'match 4' for the next piece and
       * continue until the board is filled
       */
      def fillStrategy() {

      }

      /**
       * This function will continue to bounce back up the call stack and
       * find more solutions, because there appears to be no simple way of
       * exiting a 'foreach'. We could replace the iteration with the
       * following imperative, which does exit early, if required:
       *
       * <pre>
       * val els = pieces.elements
       * while (els.hasNext && t.placedPieces < tour.length) {
       *       val piece = els.next
       *       ...
       * </pre>
       *
       * Note that the 'canFit' calls are negative case first to ensure
       * that the recursive call is optimisable (i.e. on the last line)
       * <p/>
       * When all possibilities in the list of pieces have been tried and
       * nothing can fit, try a 'lesser' matcher
       */
      pieces.foreach((piece: Array[Byte]) => {
          if (!canFit(piece)) {
            // blank out/reset the current cell
            for (j <- 0 until 4)    // quadrants only
              t.cells(coords(Row)) (coords(Col)).update(j, -1)
            t.cells(coords(Row))(coords(Col))(PieceNum) = 0     // 'blank' identifier
          } else {
            for (j <- 0 to 4)   // quadrant + piece identifier
              t.cells(coords(Row)) (coords(Col)).update(j, piece(j))
            val remaining = pieces.remove(_(PieceNum) == piece(PieceNum))
            place((tourStop + 1).toShort, remaining)
          }
        })
    }
    println("finish")
  }
}
