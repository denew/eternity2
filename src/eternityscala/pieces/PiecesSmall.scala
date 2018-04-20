package eternityscala.pieces

object PiecesSmall extends Pieces {
    override val boardDimension = 4
    override val patternCount = 4

    override def pieces = {
        val pieces: List[Array[Byte]] = List(
            Array(0,1,2,0,-128),
            Array(0,0,2,1,-127),
            Array(1,0,0,1,-126),
            Array(2,0,0,2,-125),
            Array(4,3,4,4,-124),
            Array(3,3,3,4,-123),
            Array(3,3,4,4,-122),
            Array(3,3,4,4,-121),
            Array(2,3,2,0,-120),
            //Array(1,3,1,0,-119), // fixed for demo
            Array(1,3,2,0,-118),
            Array(2,3,1,0,-117),
            Array(2,4,2,0,-116),
            Array(1,4,1,0,-115),
            Array(2,4,1,0,-114),
            Array(1,4,2,0,-113)
        )
        pieces
    }
}
