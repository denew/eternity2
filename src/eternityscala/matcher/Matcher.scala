package eternityscala.matcher

/**
* A template is an artificial structure representing the edge colours that abut
* all compass points of the cell to be populated. In some cases, these will be
* real colours from other pieces already placed (including the false border);
* in other cases these will be wildcard (-1) values from the initialisation of
* the board. A candidate piece for the cell can be rotated up to 3 times (the
* 4th would simply return to an already rejected start position) to attempt
* a match. Note however that all 4 orientations must be checked.
* <p/>
* Note also that the direction of rotation for anything other than a full
* match can be significant
* <p/>
* If a match is found, the (possibly rotated) piece is assigned to the board
* cell and removed from the collection of remaining pieces. Later backtracking
* may reverse this, removing the piece from the board and adding it back to the
* collection.
* <p/>
* The main objective is to match all sides, but a 'fallback' position may be
* useful (refer Strategy). Matching 3 sides, right down to matching no sides,
* should allow the complete board to be filled and at least add some matching
* edges to the solution.
* <p/>
* Wwe also ensure that no edge pieces are placed internally which would contradict
* the rules
* <p/>
* The multiplier parameter is an experimental idea to introduce more complexity
* into the pieces, which paradoxically can improve the chances of finding a
* real solution
*/
sealed abstract class Matcher {
    def matcher(template: Array[Byte], piece: Array[Byte], multiplier: Byte) = {
        def matchLogic(index: Byte) = {
            // match exact or wildcard (-1), but not if this piece is an edge (an edge is always
            // 0 regardless of the multiplier)
            template(index) == piece(index) || (template(index) == -1 && piece(index) / multiplier != 0)
        }

        def `0Sides` = true

        def `1Side`= matchLogic(0);// || matchLogic(1) || matchLogic(2) || matchLogic(3)

        def `2SidesAdjacent`= //(matchLogic(0) && matchLogic(1)) ||
                                //(matchLogic(1) && matchLogic(2)) ||
                                //(matchLogic(2) && matchLogic(3)) ||
                                (matchLogic(3) && matchLogic(0))

        def `2SidesParallel`= (matchLogic(0) && matchLogic(2)) //||
                                //(matchLogic(1) && matchLogic(3))

        def `3Sides` = (matchLogic(0) && matchLogic(1) && matchLogic(3)) //||
                                //(matchLogic(0) && matchLogic(1) && matchLogic(2)) ||
                                //(matchLogic(1) && matchLogic(2) && matchLogic(3)) ||
                                //(matchLogic(0) && matchLogic(2) && matchLogic(3))

        def `4Sides` = matchLogic(0) && matchLogic(1) && matchLogic(2) && matchLogic(3)

        this match {
            case Match4Sides() => `4Sides`
            case Match3Sides() => `3Sides`
            case Match2SidesAdjacent() => `2SidesAdjacent`
            case Match2SidesParallel() => `2SidesParallel`
            case Match1Side() => `1Side`
            case Match0Sides() => `0Sides`
        }
    }
}

final case class Match4Sides() extends Matcher {
    override def toString = "All Sides"
}

final case class Match3Sides() extends Matcher {
    override def toString = "3 Sides"
}

final case class Match2SidesAdjacent() extends Matcher {
    override def toString = "2 Sides Adjacent"
}

final case class Match2SidesParallel() extends Matcher {
    override def toString = "2 Sides Parallel"
}

final case class Match1Side() extends Matcher {
    override def toString = "1 Side"
}

final case class Match0Sides() extends Matcher {
    override def toString = "No Sides"
}
