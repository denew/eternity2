package eternityscala.tour

/**
 * A tour represents a fixed order in which the board cells are visited. Each
 * tour is essentially hard-coded and therefore requires no program logic. Any
 * complex pattern of cell visits can be used--circular, scans, diagonals, etc.
 * <p/>
 * There is an 'invisible' border one piece wide around the board that is added
 * at initialisation to simplify the algorithms used to match with neighbouring
 * pieces. This border contains pieces with all edge quadrants. The tour stops
 * only consider the real board cells.
 * <p/>
 * Accordingly, we never have to consider during a tour if a real piece contains
 * an edge or consider 'array out of bounds' issues--it can simply be matched in
 * the same way as an internal piece.
 */
class Tour {
    //import eternityscala.pieces.Pieces

    val North: Byte = 0
    val East: Byte = 1
    val South: Byte = 2
    val West: Byte = 3
    val PieceNum: Byte = 4
    val dimension: Byte = 16    // can be overridden
    val Quadrants: Byte = 4
    val Wildcard: Byte = -1

    // NB--the +1 is for the piece identifier (last slot in array)
    val cells = new Array[Array[Array[Byte]]](dimension + 2, dimension + 2, Quadrants + 1)

    /**
     * Note--this produces a count of 544 (for a large board) against
     * the 'official' count of 480, which does not include the edge. There is
     * therefore an assumption that even a partial result <em>must</em> include
     * a complete border. This counts against 'scanline' tours, or, at worst,
     * all edge pieces must be placed even if they do not match.
     */
    val matchableEdges = ((dimension + 1) * dimension) * 2

    /**
     * Leave the border cells with their initialised values (0 or edge colour);
     * change internal cells (the real board) to all wildcard (-1) values for
     * each quadrant. The piece identifier, 5th array slot, also stays at 0.
     * Note that a byte value of zero represents a legal piece--in this case
     * we need to check the quadrants also
     */
    def init {
        cells.slice(1, dimension + 1)
        .foreach(_.slice(1, dimension + 1)
                 .foreach((bytes: Array[Byte]) => {for (i <- 0 until Quadrants) bytes.update(i, Wildcard)}))
    }

    /**
     * At the end of a run, count the number of matched edges. Eliminate
     * wildcard matches (pieces not placed)
     */
    def matchedEdges = {
        var sum = 0

        // first the down edges...
        for (i <- 1 to dimension + 1; j <- 1 to dimension) {
            val tmp = cells(i)(j)(North)
            if (tmp == cells(i - 1)(j)(South) && tmp != Wildcard)
            sum += 1
        }
        
        // ... then the across edges
        for (i <- 1 to dimension; j <- 1 to dimension + 1) {
            val tmp = cells(i)(j)(West)
            if (tmp == cells(i)(j - 1)(East) && tmp != Wildcard)
            sum += 1
        }
        sum
    }

    def placedPieces = {
        var sum = 0
        for (i <- 1 to dimension + 1; j <- 1 to dimension)
        if (cells(i)(j)(4 /* identifier */) != 0) sum += 1
        sum
    }

    /**
     * Ensure that each piece is only placed once--zero is a legal piece, so
     * adjust only where the quadrants are legal also
     */
    def check = {
        def isBlank(piece: Array[Byte]) = {
            var sum = 0
            piece.slice(0, 4).foreach(sum += _)
            sum == -4 && piece(4) == 0
        }
        import scala.collection.mutable
        val set = mutable.Set.empty[Byte]
        for (i <- 1 to dimension + 1; j <- 1 to dimension) {
            val cell = cells(i)(j)
            if (!isBlank(cell))
                set += cell(4)
        }
        // take out any zeros (unplaced pieces)
        set -= 0
        set.size == placedPieces
    }

    /**
     * Create a structure that represents all edges surrounding the current
     * cell as it is visited by a tour.
     */
    def template(row: Short, col: Short) = {
        val template: Array[Byte] = Array(South, West, North, East)
        // the piece to the North of this (previous row), South quadrant
        template(North) = cells(row - 1)(col)(South)

        // the piece to the East of this (next col), West quadrant
        template(East) = cells(row)(col + 1)(West)

        // the piece to the South of this (next row), North quadrant
        template(South) = cells(row + 1)(col)(North)

        // the piece to the West of this (previous column), East quadrant
        template(West) = cells(row)(col - 1)(East)

        template
    }

    /**
     * Capture any result of interest for later re-population of cells
     */
    def freeze = {
        val str = new StringBuilder
        cells.slice(1, dimension + 1)
        .foreach(_.slice(1, dimension + 1)
                 .foreach((bytes: Array[Byte]) => {
                    for (i <- 0 until Quadrants)
                    str.append(bytes(i) + ".")
                    str.append(bytes(PieceNum) + "|")
                }))
        str.toString
    }

    def freeze2 = {
        val str = new StringBuilder
        cells.slice(1, dimension + 1)
        .foreach(_.slice(1, dimension + 1)
                 .foreach((bytes: Array[Byte]) => {
                    for (i <- 0 until Quadrants) {
                        if (bytes(i) >= 0 && bytes(i) <= 9)
                            str.append("0")
                        else
                            str.append("")
                        str.append(bytes(i) + ".")
                        
                     }
                     str.append("\n")
                }))
        str.toString
    }

    /**
     * String input not checked--assumed to be of the form '1.3.2.0.11|3.4.4.3.7|...', i.e. from
     * a freeze method
     */
    def thaw(frozen: String) = {
        // temp--start with a 'copy' of the grid
        val dummy = new Array[Array[Array[Byte]]](dimension + 2, dimension + 2, Quadrants + 1)

        frozen.split("[|]").foreach((item: String) => item.split("[.]").foreach(println(_)))
    }
}

object Small4CornerTour extends Tour {

    override val dimension: Byte = 4

    def tour = {
        val tour: Array[Array[Short]] = new Array((dimension * dimension) - 1)

        tour(0) = Array(1, 1)
        tour(4) = Array(2,1)
        tour(7) = Array(3, 1)
        tour(1) = Array(4, 1)
        //tour(7) = Array(1, 2) // fixed position removed
        tour(11) = Array(2, 2)
        tour(12) = Array(3, 2)
        tour(5) = Array(4, 2)
        tour(10) = Array(1, 3)
        tour(14) = Array(2, 3)
        tour(13) = Array(3, 3)
        tour(8) = Array(4, 3)
        tour(3) = Array(1, 4)
        tour(6) = Array(2, 4)
        tour(9) = Array(3, 4)
        tour(2) = Array(4, 4)

        tour
    }
}

object SmallScanlineTour extends Tour {

    override val dimension: Byte = 4

    def tour = {
        val tour: Array[Array[Short]] = new Array((dimension * dimension) - 1)

        tour(0) = Array(1, 1)
        tour(3) = Array(2,1)
        tour(7) = Array(3, 1)
        tour(11) = Array(4, 1)

        tour(4) = Array(2, 2)
        tour(8) = Array(3, 2)
        tour(12) = Array(4, 2)
        tour(1) = Array(1, 3)
        tour(5) = Array(2, 3)
        tour(9) = Array(3, 3)
        tour(13) = Array(4, 3)
        tour(2) = Array(1, 4)
        tour(6) = Array(2, 4)
        tour(10) = Array(3, 4)
        tour(14) = Array(4, 4)

        tour
    }
}





