package eternityscala

object DiamondSolver {
    import scala.collection.mutable._

    /*
    0 -> 64
    1 -> 24
    2 -> 48
    3 -> 50
    4 -> 50
    5 -> 24
    6 -> 48
    7 -> 50
    8 -> 50
    9 -> 24
    10 -> 48
    11 -> 50
    12 -> 50
    13 -> 24
    14 -> 48
    15 -> 50
    16 -> 50
    17 -> 24
    18 -> 48
    19 -> 50
    20 -> 50
    21 -> 50
    22 -> 50

    */


    val edgePatterns4 = Array(
                            Array(1, 6),
                            Array(2, 6)
                       )

    // The 'internal' arrays are the pattern number and the starting count.
    // Note that counts are adjusted for the fixed piece
    val centrePatterns4 = Array(
                            Array(3, 6),
                            Array(4, 6)
                         )


    val edgePatterns = Array(
                            Array(1, 12),
                            Array(5, 12),
                            Array(9, 12),
                            Array(13, 12),
                            Array(17, 12)
                       )

    val centrePatterns = Array(
                            Array(2, 24),
                            Array(3, 25),
                            Array(4, 25),
                            Array(6, 24),   // adj for 2 in fixed piece
                            Array(7, 25),
                            Array(8, 25),
                            Array(10, 24),
                            Array(11, 25),  // adjust for 1 in fixed
                            Array(12, 25),
                            Array(14, 24),
                            Array(15, 25),
                            Array(16, 25),
                            Array(18, 24),  // adjust for 1 in fixed
                            Array(19, 25),
                            Array(20, 25),
                            Array(21, 25),
                            Array(22, 25)
                       )

    val board: Byte = 16

    val dimension = 32  //(((board + 1) * 2) - 1)

    // will all have edge colour zero by default
    val diamondCells = new Array[Array[Byte]](dimension, dimension)

    // used to randomly pull a pattern from the relevant array, reducing the
    // balance, until all used up
    val rand = new java.util.Random

    var psum = 0

    def main(args: Array[String]) = {
        fillDiamond
        addFixed
        printDiamondCells
        println(check)
        //hashCheck
        println("psum=" + psum)
        quad
    }

    def hashCheck = {
        println(Array(1,2,3,4))
        println(Array(1,2,3,4))
        println(Array(4,1,2,3))
        println(Array(3,4,1,2))
        println(Array(2,3,4,1))
        println(Array(5,1,2,3))
        println(Array(1,2,3,5))
    }

    def check = {
        def format(arr: Array[Int]) = {
            println(arr(0) + "->" + arr(1))
        }
        var sum = 0
        edgePatterns.foreach(sum += _(1))
        edgePatterns.foreach((arr: Array[Int]) => format(arr))
        println("*")
        centrePatterns.foreach(sum += _(1))
        centrePatterns.foreach((arr: Array[Int]) => format(arr))
        sum == 0
    }

    def addFixed = {
        diamondCells(15)(15) = 11
        diamondCells(15)(14) = 18
        diamondCells(16)(15) = 6
        diamondCells(16)(14) = 6
    }

    def printDiamondCells = {
        // first 'foreach' is the rows
        diamondCells.foreach((columns: Array[Byte]) => {
                columns.foreach((pattern: Byte) => {
                        if (pattern <= 9)
                            print(" ")
                        print(if (pattern == 0) " " else pattern)
                        print(" ")
                })
                print("\n")
            })
    }

    def quad = {
        import eternityscala.pieces._
        var matches4 = 0
        var matches3 = 0
        var matches2 = 0
        var r = 0
        // real cells in 'dimension - 1', but we use '+ 1' in the coordinates
        for (row <- diamondCells.slice(1, dimension)) {
            var c = 0
            for (cell <- row.slice(1, dimension)) {
                //println(
                    // maximum of 2 zeros (edges) allowed for a real piece, therefore if
                    // any 3 edges sum to 0 it must be outside the diamond
                    val tl = diamondCells(r)(c)
                    val tr = diamondCells(r)(c + 1)
                    val ll = diamondCells(r + 1)(c)
                    val lr = diamondCells(r + 1)(c + 1)
                    if (!(tl + tr + ll == 0 || tl + tr + lr == 0 || tr + ll + lr == 0 || tl + ll + lr == 0)) {
                        println(tl + " " + tr + " " + ll + " " + lr)
                        PiecesLarge.pieces.foreach((piece: Array[Byte]) => {
                                
                                for (i <- 0 to 3) {
                                    if (tl == piece(0) && tr == piece(1) && ll == piece(2) && lr == piece(3)) {
                                        matches4 += 1
                                        println("matched 4 " + (piece(4)))
                                    } else if (tl == piece(0) && tr == piece(1) && ll == piece(2)) {
                                        matches3 += 1
                                        println("matched 3 " + (piece(4)))
                                    } else if (tl == piece(0) && tr == piece(1)) {
                                        matches2 += 1
                                        println("matched 2 " + (piece(4)))
                                    }
                                    Pieces.rotate(piece)
                                }
                            })
                        //Pieces.rotate(array)
                    }
                    //diamondCells(r)(c) + " " + diamondCells(r)(c + 1) + " " + diamondCells(r + 1)(c) + " " + diamondCells(r + 1)(c + 1)
                //)
                c += 1
            }
            r += 1
        }
        println("matches4=" + matches4)
        println("matches3=" + matches3)
        println("matches2=" + matches2)
    }

    /**
    * For even count diamonds only
    */
    def fillDiamond = {
        val row = List.range((dimension / 2) - 1, 0, -1)
        val col = List.range(dimension / 2, dimension - 1, 1)
        val rowAll = row ::: row.reverse

        val colAll = col ::: col.reverse
        val pairs = rowAll.zip(colAll)

        var rowCount = 0
        def handle(bytes: Array[Byte]) = {
            val start = pairs(rowCount)._1.toByte
            val end = (pairs(rowCount)._2).toByte
            var colCount = 0
            for (byte <- bytes) {
                // skip fixed positions previously set
                if (bytes(colCount) == 0) {
                    if (colCount == start || colCount == end)
                        bytes.update(colCount, edgePattern)
                    else if (colCount > start && colCount < end)
                        bytes.update(colCount, centrePattern)
                }
                colCount += 1
            }
            rowCount += 1
        }
        diamondCells.slice(1, dimension -1).foreach(handle(_))
    }

    def edgePattern: Byte = pattern(edgePatterns)

    def centrePattern: Byte = pattern(centrePatterns)

    def pattern(arr: Array[Array[Int]]): Byte = {
        val i = rand.nextInt(arr.length)

        if (arr(i)(1) != 0) {
            arr(i).update(1, arr(i)(1) - 1)
            psum += 1
            arr(i)(0).toByte
        } else
            pattern(arr)
    }

}

/*

(1) create a random diamond
(2) 'reserve' any full matches, using a set to exclude duplicates
(3) use assignment to swap in pieces for 3 matches, taking from 0's only
(4) then 2 matches from 0's only
(5) assign the balance

*/
