package eternityscala.pieces

/**
 * Based on a placement of 197 pieces (best to 20/3/09)
 */
object PiecesLargePartSort extends Pieces {
    override val boardDimension = 16
    override val patternCount = 22

    override def pieces = {
        val partPieces: List[Array[Byte]] = List(
            Array(1,5,0,0,-127),
              Array(16,1,0,13,-72),
              Array(7,13,0,17,-99),
              Array(18,17,0,9,-113),
              Array(10,9,0,1,-123),
              Array(10,1,0,13,-79),
              Array(16,13,0,17,-94),
              Array(2,17,0,17,-102),
              Array(20,17,0,17,-97),
              Array(22,17,0,13,-69),
              Array(8,13,0,1,-116),
              Array(2,1,0,1,-124),
              Array(15,1,0,5,-87),
              Array(20,5,0,5,-83),
              Array(8,5,0,17,-95),
              Array(9,17,0,0,-126),
              Array(11,13,0,5,-90),
              Array(11,16,6,21,52),
              Array(21,11,6,7,43),
              Array(21,21,18,6,15),
              Array(14,6,10,16,-2),
              Array(22,16,10,11,-12),
              Array(16,15,11,11,96),
              Array(19,15,2,19,-52),
              Array(21,15,19,20,111),
              Array(21,22,14,12,66),
              Array(19,3,14,8,71),
              Array(4,19,2,22,-32),
              Array(22,15,14,12,67),
              Array(20,20,14,20,70),
              Array(20,8,2,22,-31),
              Array(2,9,0,17,-103),
              Array(8,17,0,13,-73),
              Array(21,21,18,8,31),
              Array(11,4,3,21,90),
              Array(4,21,19,8,113),
              Array(3,19,14,7,60),
              Array(7,22,11,20,100),
              Array(12,22,11,11,94),
              Array(12,19,14,21,74),
              Array(14,15,2,19,-53),
              Array(6,14,2,12,-44),
              Array(3,8,10,6,-23),
              Array(4,21,2,8,-35),
              Array(12,14,10,21,3),
              Array(14,20,18,16,33),
              Array(18,22,18,3,18),
              Array(18,17,0,17,-100),
              Array(22,5,0,17,-93),
              Array(4,22,18,19,22),
              Array(3,21,10,19,-11),
              Array(21,8,14,16,72),
              Array(22,7,14,3,55),
              Array(22,20,7,4,116),
              Array(22,16,18,7,25),
              Array(16,21,15,20,121),
              Array(19,18,2,15,-48),
              Array(3,4,18,14,16),
              Array(4,3,10,16,0),
              Array(4,16,2,20,-39),
              Array(16,20,10,22,7),
              Array(16,22,20,22,126),
              Array(19,22,3,11,79),
              Array(11,17,0,1,-120),
              Array(19,9,0,5,-89),
              Array(19,4,2,8,-36),
              Array(10,3,2,10,-66),
              Array(16,19,10,10,-26),
              Array(16,4,19,7,105),
              Array(4,12,3,16,89),
              Array(18,12,18,3,17),
              Array(20,19,10,3,-17),
              Array(18,19,2,12,-45),
              Array(8,12,4,12,123),
              Array(4,22,3,12,86),
              Array(4,3,10,22,6),
              Array(3,16,2,7,-51),
              Array(6,22,2,20,-40),
              Array(19,21,18,6,12),
              Array(21,1,0,9,-104),
              Array(10,1,0,9,-114),
              Array(21,15,10,8,-3),
              Array(21,21,3,22,92),
              Array(8,22,10,15,-9),
              Array(12,4,15,4,119),
              Array(3,20,3,12,85),
              Array(6,20,18,16,32),
              Array(20,16,10,3,-16),
              Array(6,3,18,6,9),
              Array(8,15,6,8,50),
              Array(8,8,3,4,84),
              Array(11,4,10,22,5),
              Array(22,7,3,3,78),
              Array(11,16,3,22,91),
              Array(11,11,18,7,23),
              Array(7,9,0,9,-110),
              Array(6,13,0,1,-121),
              Array(15,15,6,15,44),
              Array(21,22,15,15,118),
              Array(21,22,8,22,127),
              Array(4,16,18,22,35),
              Array(3,16,3,15,83),
              Array(15,6,18,6,13),
              Array(8,6,18,20,30),
              Array(4,8,6,14,37),
              Array(15,12,10,14,-19),
              Array(12,8,14,21,75),
              Array(19,12,14,11,56),
              Array(11,19,3,7,81),
              Array(16,19,14,7,61),
              Array(11,15,6,19,39),
              Array(15,9,0,9,-109),
              Array(7,5,0,13,-76),
              Array(20,8,7,15,115),
              Array(20,15,2,11,-55),
              Array(2,21,2,20,-41),
              Array(14,14,2,18,-63),
              Array(3,4,6,14,36),
              Array(14,4,6,20,48),
              Array(20,6,2,18,-61),
              Array(11,4,2,4,-46),
              Array(16,11,10,6,-20),
              Array(7,11,6,21,53),
              Array(7,12,2,3,-58),
              Array(8,3,2,11,-54),
              Array(19,8,14,20,69),
              Array(12,20,6,15,45),
              Array(15,9,0,5,-86),
              Array(4,1,0,5,-84),
              Array(4,8,6,7,41),
              Array(6,11,18,19,21),
              Array(20,7,18,18,8),
              Array(22,7,14,16,73),
              Array(12,16,6,8,49),
              Array(8,8,14,11,57),
              Array(6,3,18,6,9),
              Array(7,21,6,4,46),
              Array(16,15,3,7,82),
              Array(15,11,18,22,34),
              Array(3,14,18,3,19),
              Array(3,3,6,11,38),
              Array(19,21,6,19,40),
              Array(4,16,19,12,109),
              Array(4,5,0,9,-108),
              Array(6,1,0,1,-122),
              Array(20,16,6,7,42),
              Array(7,20,19,4,107),
              Array(18,18,2,4,-47),
              Array(22,8,2,18,-60),
              Array(12,20,11,8,102),
              Array(20,8,10,16,1),
              Array(4,15,10,6,-21),
              Array(4,21,15,12,120),
              Array(3,3,14,15,62),
              Array(21,22,3,22,93),
              Array(22,21,14,21,77),
              Array(21,11,14,12,65),
              Array(14,21,14,7,59),
              Array(14,16,10,22,4),
              Array(10,9,0,13,-78),
              Array(7,5,0,1,-119),
              Array(16,21,2,7,-49),
              Array(21,7,14,19,58),
              Array(12,14,2,8,-34),
              Array(2,8,2,18,-65),
              Array(11,10,2,18,-62),
              Array(19,21,10,16,-1),
              Array(19,15,2,8,-37),
              Array(8,12,2,12,-43),
              Array(19,8,14,14,54),
              Array(14,22,10,4,-8),
              Array(22,6,10,10,-25),
              Array(7,19,6,12,47),
              Array(7,7,18,4,26),
              Array(18,22,2,18,-64),
              Array(2,13,0,13,-80),
              Array(3,13,0,5,-91),
              Array(20,3,2,7,-50),
              Array(19,15,11,7,97),
              Array(15,12,18,3,20),
              Array(6,18,2,2,-68),
              Array(14,7,2,2,-67),
              Array(16,3,14,21,76),
              Array(16,8,11,20,101),
              Array(12,11,10,11,-13),
              Array(11,19,18,7,24),
              Array(4,15,10,18,-24),
              Array(20,15,10,20,-5),
              Array(19,20,19,19,104),
              Array(12,15,19,4,108),
              Array(7,12,18,4,27),
              Array(4,13,0,13,-74),
              Array(16,13,0,13,-71),
              Array(16,20,2,12,-42),
              Array(15,4,2,11,-56),
              Array(19,15,3,20,88),
              Array(6,16,10,20,-7),
              Array(7,16,6,16,51)
            )

        val shufflePieces: List[Array[Byte]] = List(

              // don't shuffle above'


            Array(1,17,0,0,-128),


              Array(17,9,0,0,-125),






              Array(15,9,0,1,-118),
              Array(8,5,0,1,-117),

              Array(21,5,0,1,-115),


              Array(14,13,0,9,-112),
              Array(19,13,0,9,-111),



              Array(12,1,0,9,-107),
              Array(12,13,0,9,-106),
              Array(20,1,0,9,-105),



              Array(10,17,0,17,-101),


              Array(15,9,0,17,-98),

              Array(8,9,0,17,-96),



              Array(18,1,0,5,-92),



              Array(19,17,0,5,-88),


              Array(15,17,0,5,-85),


              Array(8,5,0,5,-82),
              Array(16,5,0,5,-81),



              Array(6,1,0,13,-77),

              Array(4,5,0,13,-75),




              Array(21,9,0,13,-70),










              Array(3,7,2,3,-59),

              Array(14,18,2,11,-57),


















              Array(11,12,2,8,-38),




              Array(21,3,2,21,-33),


              Array(21,6,2,22,-30),
              Array(22,21,2,22,-29),
              Array(12,15,10,10,-28),
              Array(12,16,10,10,-27),




              Array(19,8,10,6,-22),



              Array(12,15,10,14,-18),


              Array(14,4,10,11,-15),
              Array(7,12,10,11,-14),



              Array(16,12,10,7,-10),



              Array(14,19,10,20,-6),

              Array(12,22,10,8,-4),





              Array(6,20,10,21,2),







              Array(6,12,18,6,11),


              Array(16,12,18,6,14),













              Array(22,7,18,4,28),
              Array(7,16,18,20,29),

































              Array(15,20,14,15,63),
              Array(11,7,14,4,64),



              Array(11,22,14,20,68),











              Array(8,15,3,11,80),






              Array(22,21,3,12,87),







              Array(20,7,11,11,95),


              Array(12,12,11,7,98),
              Array(19,8,11,4,99),



              Array(12,21,11,8,103),


              Array(7,4,19,4,106),



              Array(15,22,19,20,110),

              Array(7,21,19,8,112),

              Array(15,12,7,15,114),


              Array(16,22,7,21,117),




              Array(22,8,4,4,122),

              Array(16,20,12,8,124),
              Array(21,16,20,16,125)
        )
        partPieces ::: Pieces.shuffle(shufflePieces)
    }
}
