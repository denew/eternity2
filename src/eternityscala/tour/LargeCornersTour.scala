package eternityscala.tour

object LargeCornersTour extends Tour {

    override val dimension: Byte = 16

    def tour = {
        val tour: Array[Array[Short]] = new Array((dimension * dimension) - 1)

        tour(0) = Array(1, 1)
        tour(4) = Array(2, 1)
        tour(12) = Array(3, 1)
        tour(24) = Array(4, 1)
        tour(40) = Array(5, 1)
        tour(60) = Array(6, 1)
        tour(84) = Array(7, 1)
        tour(112) = Array(8, 1)
        tour(143) = Array(9, 1)
        tour(111) = Array(10, 1)
        tour(83) = Array(11, 1)
        tour(59) = Array(12, 1)
        tour(39) = Array(13, 1)
        tour(23) = Array(14, 1)
        tour(11) = Array(15, 1)
        tour(3) = Array(16, 1)
        tour(5) = Array(1, 2)
        tour(13) = Array(2, 2)
        tour(25) = Array(3, 2)
        tour(41) = Array(4, 2)
        tour(61) = Array(5, 2)
        tour(85) = Array(6, 2)
        tour(113) = Array(7, 2)
        tour(144) = Array(8, 2)
        tour(171) = Array(9, 2)
        tour(142) = Array(10, 2)
        tour(110) = Array(11, 2)
        tour(82) = Array(12, 2)
        tour(58) = Array(13, 2)
        tour(38) = Array(14, 2)
        tour(22) = Array(15, 2)
        tour(10) = Array(16, 2)
        tour(14) = Array(1, 3)
        tour(26) = Array(2, 3)
        tour(42) = Array(3, 3)
        tour(62) = Array(4, 3)
        tour(86) = Array(5, 3)
        tour(114) = Array(6, 3)
        tour(145) = Array(7, 3)
        tour(172) = Array(8, 3)
        tour(195) = Array(9, 3)
        tour(170) = Array(10, 3)
        tour(141) = Array(11, 3)
        tour(109) = Array(12, 3)
        tour(81) = Array(13, 3)
        tour(57) = Array(14, 3)
        tour(37) = Array(15, 3)
        tour(21) = Array(16, 3)
        tour(27) = Array(1, 4)
        tour(43) = Array(2, 4)
        tour(63) = Array(3, 4)
        tour(87) = Array(4, 4)
        tour(115) = Array(5, 4)
        tour(146) = Array(6, 4)
        tour(173) = Array(7, 4)
        tour(196) = Array(8, 4)
        tour(215) = Array(9, 4)
        tour(194) = Array(10, 4)
        tour(169) = Array(11, 4)
        tour(140) = Array(12, 4)
        tour(108) = Array(13, 4)
        tour(80) = Array(14, 4)
        tour(56) = Array(15, 4)
        tour(36) = Array(16, 4)
        tour(44) = Array(1, 5)
        tour(64) = Array(2, 5)
        tour(88) = Array(3, 5)
        tour(116) = Array(4, 5)
        tour(147) = Array(5, 5)
        tour(174) = Array(6, 5)
        tour(197) = Array(7, 5)
        tour(216) = Array(8, 5)
        tour(231) = Array(9, 5)
        tour(214) = Array(10, 5)
        tour(193) = Array(11, 5)
        tour(168) = Array(12, 5)
        tour(139) = Array(13, 5)
        tour(107) = Array(14, 5)
        tour(79) = Array(15, 5)
        tour(55) = Array(16, 5)
        tour(65) = Array(1, 6)
        tour(89) = Array(2, 6)
        tour(117) = Array(3, 6)
        tour(148) = Array(4, 6)
        tour(175) = Array(5, 6)
        tour(198) = Array(6, 6)
        tour(217) = Array(7, 6)
        tour(232) = Array(8, 6)
        tour(243) = Array(9, 6)
        tour(230) = Array(10, 6)
        tour(213) = Array(11, 6)
        tour(192) = Array(12, 6)
        tour(167) = Array(13, 6)
        tour(138) = Array(14, 6)
        tour(106) = Array(15, 6)
        tour(78) = Array(16, 6)
        tour(90) = Array(1, 7)
        tour(118) = Array(2, 7)
        tour(149) = Array(3, 7)
        tour(176) = Array(4, 7)
        tour(199) = Array(5, 7)
        tour(218) = Array(6, 7)
        tour(233) = Array(7, 7)
        tour(244) = Array(8, 7)
        tour(251) = Array(9, 7)
        tour(242) = Array(10, 7)
        tour(229) = Array(11, 7)
        tour(212) = Array(12, 7)
        tour(191) = Array(13, 7)
        tour(166) = Array(14, 7)
        tour(137) = Array(15, 7)
        tour(105) = Array(16, 7)
        tour(119) = Array(1, 8)
        tour(150) = Array(2, 8)
        tour(177) = Array(3, 8)
        tour(200) = Array(4, 8)
        tour(219) = Array(5, 8)
        tour(234) = Array(6, 8)
        tour(245) = Array(7, 8)
        tour(252) = Array(8, 8)

        tour(250) = Array(10, 8)
        tour(241) = Array(11, 8)
        tour(228) = Array(12, 8)
        tour(211) = Array(13, 8)
        tour(190) = Array(14, 8)
        tour(165) = Array(15, 8)
        tour(136) = Array(16, 8)
        tour(120) = Array(1, 9)
        tour(151) = Array(2, 9)
        tour(178) = Array(3, 9)
        tour(201) = Array(4, 9)
        tour(220) = Array(5, 9)
        tour(235) = Array(6, 9)
        tour(246) = Array(7, 9)
        tour(253) = Array(8, 9)
        tour(254) = Array(9, 9)
        tour(249) = Array(10, 9)
        tour(240) = Array(11, 9)
        tour(227) = Array(12, 9)
        tour(210) = Array(13, 9)
        tour(189) = Array(14, 9)
        tour(164) = Array(15, 9)
        tour(135) = Array(16, 9)
        tour(91) = Array(1, 10)
        tour(121) = Array(2, 10)
        tour(152) = Array(3, 10)
        tour(179) = Array(4, 10)
        tour(202) = Array(5, 10)
        tour(221) = Array(6, 10)
        tour(236) = Array(7, 10)
        tour(247) = Array(8, 10)
        tour(248) = Array(9, 10)
        tour(239) = Array(10, 10)
        tour(226) = Array(11, 10)
        tour(209) = Array(12, 10)
        tour(188) = Array(13, 10)
        tour(163) = Array(14, 10)
        tour(134) = Array(15, 10)
        tour(104) = Array(16, 10)
        tour(66) = Array(1, 11)
        tour(92) = Array(2, 11)
        tour(122) = Array(3, 11)
        tour(153) = Array(4, 11)
        tour(180) = Array(5, 11)
        tour(203) = Array(6, 11)
        tour(222) = Array(7, 11)
        tour(237) = Array(8, 11)
        tour(238) = Array(9, 11)
        tour(225) = Array(10, 11)
        tour(208) = Array(11, 11)
        tour(187) = Array(12, 11)
        tour(162) = Array(13, 11)
        tour(133) = Array(14, 11)
        tour(103) = Array(15, 11)
        tour(77) = Array(16, 11)
        tour(45) = Array(1, 12)
        tour(67) = Array(2, 12)
        tour(93) = Array(3, 12)
        tour(123) = Array(4, 12)
        tour(154) = Array(5, 12)
        tour(181) = Array(6, 12)
        tour(204) = Array(7, 12)
        tour(223) = Array(8, 12)
        tour(224) = Array(9, 12)
        tour(207) = Array(10, 12)
        tour(186) = Array(11, 12)
        tour(161) = Array(12, 12)
        tour(132) = Array(13, 12)
        tour(102) = Array(14, 12)
        tour(76) = Array(15, 12)
        tour(54) = Array(16, 12)
        tour(28) = Array(1, 13)
        tour(46) = Array(2, 13)
        tour(68) = Array(3, 13)
        tour(94) = Array(4, 13)
        tour(124) = Array(5, 13)
        tour(155) = Array(6, 13)
        tour(182) = Array(7, 13)
        tour(205) = Array(8, 13)
        tour(206) = Array(9, 13)
        tour(185) = Array(10, 13)
        tour(160) = Array(11, 13)
        tour(131) = Array(12, 13)
        tour(101) = Array(13, 13)
        tour(75) = Array(14, 13)
        tour(53) = Array(15, 13)
        tour(35) = Array(16, 13)
        tour(15) = Array(1, 14)
        tour(29) = Array(2, 14)
        tour(47) = Array(3, 14)
        tour(69) = Array(4, 14)
        tour(95) = Array(5, 14)
        tour(125) = Array(6, 14)
        tour(156) = Array(7, 14)
        tour(183) = Array(8, 14)
        tour(184) = Array(9, 14)
        tour(159) = Array(10, 14)
        tour(130) = Array(11, 14)
        tour(100) = Array(12, 14)
        tour(74) = Array(13, 14)
        tour(52) = Array(14, 14)
        tour(34) = Array(15, 14)
        tour(20) = Array(16, 14)
        tour(6) = Array(1, 15)
        tour(16) = Array(2, 15)
        tour(30) = Array(3, 15)
        tour(48) = Array(4, 15)
        tour(70) = Array(5, 15)
        tour(96) = Array(6, 15)
        tour(126) = Array(7, 15)
        tour(157) = Array(8, 15)
        tour(158) = Array(9, 15)
        tour(129) = Array(10, 15)
        tour(99) = Array(11, 15)
        tour(73) = Array(12, 15)
        tour(51) = Array(13, 15)
        tour(33) = Array(14, 15)
        tour(19) = Array(15, 15)
        tour(9) = Array(16, 15)
        tour(1) = Array(1, 16)
        tour(7) = Array(2, 16)
        tour(17) = Array(3, 16)
        tour(31) = Array(4, 16)
        tour(49) = Array(5, 16)
        tour(71) = Array(6, 16)
        tour(97) = Array(7, 16)
        tour(127) = Array(8, 16)
        tour(128) = Array(9, 16)
        tour(98) = Array(10, 16)
        tour(72) = Array(11, 16)
        tour(50) = Array(12, 16)
        tour(32) = Array(13, 16)
        tour(18) = Array(14, 16)
        tour(8) = Array(15, 16)
        tour(2) = Array(16, 16)

        tour
    }
}
