import scala.annotation.tailrec

object d06 extends App {

  val input =
    """VM4)6C4
      |7KN)17S
      |GJY)8S9
      |78B)DLT
      |LSK)L97
      |JB7)P2M
      |ZT2)64G
      |YVF)FCB
      |1L9)C2Q
      |1TH)V5G
      |Q5K)2W5
      |23Y)25Z
      |XTW)MYZ
      |C8S)8S4
      |2MF)Z9X
      |4TV)P9Q
      |N6P)349
      |4HY)17M
      |HVY)DRY
      |P31)5V1
      |F2W)7S8
      |W84)XKM
      |D49)SHK
      |3G5)6G7
      |QGF)FF7
      |HCH)X94
      |MCB)J58
      |YCV)XGD
      |W4P)DYF
      |LSW)PKQ
      |LNV)42V
      |CVV)BRX
      |1B3)419
      |TZT)RH6
      |9VJ)CFF
      |BC8)H4V
      |JVZ)XJL
      |BGN)PQ3
      |3WR)69K
      |VK9)BD9
      |MCN)9FW
      |DRY)S82
      |3F8)D4G
      |GW9)ZZF
      |QYC)HKT
      |1L5)1TH
      |LJV)BP3
      |5W4)518
      |B85)WTD
      |LNS)2XZ
      |X5V)22Y
      |ZCN)M73
      |W6H)RNH
      |SN3)FYN
      |VKR)LQ4
      |8DZ)KW8
      |S82)P5J
      |J59)ZMM
      |861)FH8
      |V5G)KGC
      |N92)CHB
      |6FM)2XG
      |ZGM)QGF
      |16Q)7BP
      |639)9KP
      |5P1)296
      |49M)J91
      |YVH)D1L
      |SGJ)KT4
      |PRD)B7D
      |6XM)VVZ
      |L38)VY6
      |T5J)R7D
      |9T8)KV5
      |ZQG)KQ3
      |P9Q)NMZ
      |V5F)YG4
      |6TK)845
      |P6M)6SQ
      |XZK)K4P
      |NZX)49W
      |S1P)X5B
      |KXG)DFN
      |M8C)DSX
      |DHQ)SZT
      |1TN)W9C
      |99B)Q5K
      |5L3)HQP
      |NZV)N1W
      |HW5)RFF
      |Z77)LLK
      |Q5V)6RR
      |MP8)QH1
      |PS4)W8Z
      |69M)S1H
      |BRX)97C
      |8S8)5W3
      |CK9)5L3
      |6MB)2XP
      |MBN)1WR
      |WWV)XR5
      |ND3)VZN
      |KQ5)HVF
      |DJ6)SWQ
      |JKK)6XF
      |8B9)J4L
      |T11)NZX
      |Q6R)QS5
      |B3F)XJ1
      |WTD)XYM
      |YCK)VN4
      |8C8)RZK
      |DQP)LBL
      |845)XY9
      |PRL)4CG
      |H9K)2SC
      |SHZ)G6X
      |1HY)S1Z
      |5VG)C29
      |QC7)ZST
      |CHM)6M8
      |PW5)Q7Z
      |CDT)6MB
      |RH7)J7G
      |CT4)CXS
      |JWC)8SZ
      |MQ9)N7R
      |61P)QTC
      |8RL)1B3
      |LZS)K81
      |2W4)FTD
      |TDC)78B
      |3QZ)5X9
      |QPS)44C
      |BFS)CPP
      |1G3)ZGM
      |BJD)QYC
      |YYT)52V
      |49W)GF6
      |4WL)ZW9
      |Z6Z)JVZ
      |9PR)NCC
      |54X)V1C
      |8N6)WKX
      |SWQ)WF5
      |KMN)F3Z
      |ZW2)F3L
      |V3X)VRK
      |KPT)2NT
      |F4S)5VG
      |R3C)74T
      |NCJ)Y3P
      |LMF)FMB
      |J56)KKT
      |CNR)WH2
      |1NC)7VS
      |CVL)FQ6
      |2PG)K1W
      |YPV)SS8
      |SS8)46V
      |95H)TQP
      |JDZ)VFW
      |58T)GHL
      |1QN)GXG
      |H1C)LPV
      |Y67)5S5
      |ZZQ)SCR
      |YM3)2PG
      |7TQ)T11
      |2QR)9DZ
      |MG1)7RW
      |2NP)WHD
      |VLY)TZT
      |4JT)MX2
      |XFL)WNL
      |BDV)SZ4
      |WK9)NDR
      |GP7)LD1
      |YV8)STD
      |ZHD)SGJ
      |ZC2)B3S
      |SQZ)6SM
      |6VH)7K7
      |4P9)6DB
      |K39)KDR
      |ZW9)XVL
      |VMZ)2NP
      |Y3P)6XM
      |LWW)7KN
      |YGH)6CH
      |B6X)TRG
      |KQ3)7H6
      |22X)GQ9
      |43G)WTY
      |1JV)SBS
      |9QB)9RV
      |YLQ)LJB
      |MYZ)ZL9
      |LNF)72M
      |WKP)22X
      |H4C)HLD
      |TN5)3XH
      |F3L)963
      |G3Q)8WJ
      |1DK)89D
      |LZ2)J3G
      |B68)81Z
      |576)S6F
      |ZGC)68G
      |GHL)5P1
      |89M)WL3
      |VJH)PSH
      |55J)DGT
      |B9S)3BS
      |LF3)TBN
      |2XL)89M
      |LPG)TC8
      |B9N)2ZV
      |R3S)2T4
      |FZL)C3F
      |ZMM)C7D
      |7SK)XQP
      |6GH)KFV
      |C7B)V93
      |G51)2DN
      |FYN)6GH
      |5HF)7BB
      |D47)4GK
      |NDR)TBB
      |Z41)BN4
      |BLK)5HL
      |5MZ)MM5
      |NWN)6KR
      |8SD)B1B
      |9L7)Y4W
      |VYW)RHQ
      |TC3)QBL
      |LBL)8D5
      |44B)J51
      |BNV)J5V
      |BD9)DM4
      |G84)TN5
      |935)KXG
      |NWL)8MY
      |HQT)NTZ
      |KW8)14D
      |NZ5)V5N
      |CPP)M6M
      |1TH)X36
      |D4G)VKR
      |WH2)HT8
      |BC2)Z41
      |MSJ)9BV
      |B8T)CDC
      |BC8)BWR
      |QSX)VM4
      |8BY)T2B
      |VJF)WFZ
      |4JM)N3R
      |2GH)CLL
      |VZS)2XL
      |8YZ)149
      |M19)GPY
      |P14)XDH
      |69M)YVF
      |LGW)DHQ
      |4DB)M5S
      |FR7)L3B
      |2R5)LXT
      |HHX)X5P
      |L97)V8M
      |H2Y)WB7
      |ZCP)YG1
      |2K8)SQ9
      |GXN)ZDN
      |ZGN)K42
      |TXX)S1P
      |NXW)9MX
      |M1C)JS4
      |G4Y)G9J
      |89D)QV6
      |4GH)3VY
      |KQQ)B9S
      |WG6)3X2
      |QBL)54X
      |RJB)R4S
      |N77)GKV
      |XDH)ZVN
      |DW1)9RZ
      |M1L)VN3
      |GQX)M3N
      |5FY)4JM
      |9WY)RNK
      |ZVM)GFK
      |6R4)QQW
      |518)MPL
      |GMY)589
      |68B)FTK
      |WR3)2HC
      |85G)2JB
      |5B5)LYP
      |8J2)LNP
      |WK1)MXP
      |CXS)LV6
      |DDF)TJJ
      |H8H)BTV
      |FP2)BLK
      |LD1)HWN
      |YV5)8PP
      |4CG)WR9
      |S6F)3J3
      |GDQ)MB2
      |3YL)RCH
      |RF4)GQX
      |3ZD)SBD
      |WB7)HDZ
      |WX7)Z9Y
      |5RT)LLW
      |8S9)YHY
      |FKW)W84
      |FQF)2BC
      |68G)FKW
      |6Z4)BMP
      |YZ4)G51
      |XSY)SC7
      |X5B)8R3
      |D2C)FMS
      |LSW)W97
      |Q2G)W3F
      |KFV)XWR
      |FQ9)SHF
      |88P)JH5
      |7BP)4F8
      |8S7)GKN
      |23G)GZM
      |V8N)X96
      |BMP)KKP
      |LXK)3Z1
      |NTQ)68R
      |Z78)3G8
      |BDQ)TM3
      |R4P)5SP
      |NR3)D2Z
      |9FW)RWT
      |FRJ)6BF
      |KFV)79N
      |CW2)K6F
      |9RZ)WK3
      |MZN)1M3
      |GCQ)615
      |8D5)WGF
      |JNF)N9F
      |JS7)Y13
      |G5Z)SLN
      |DTX)N4J
      |64G)2TQ
      |6GH)GFP
      |HV2)JWB
      |RYV)9T8
      |P49)B88
      |KND)7QV
      |HDZ)1TN
      |F96)3FM
      |KB1)BF1
      |HBC)2Z7
      |BXC)82G
      |C2Q)TH1
      |4GK)HMT
      |CKJ)XX4
      |L6V)49R
      |2ZV)MTY
      |MRZ)S9K
      |W5S)71M
      |8XD)H2R
      |NMZ)B41
      |TTW)7RV
      |Z6S)XBS
      |M8B)GP7
      |R8J)8WM
      |NNV)LT9
      |4QY)7LW
      |YL3)82P
      |Q8G)5SQ
      |NPQ)81X
      |P6G)7NK
      |HZY)1SH
      |8C1)SGX
      |SQ9)KMN
      |2X2)2CQ
      |TQP)GMQ
      |B6S)V7K
      |YG1)8W2
      |FWV)32G
      |83C)QTY
      |9K5)4YP
      |LV6)KXW
      |X8Z)F2W
      |4K8)HJW
      |PG2)YNP
      |NTZ)RLK
      |CB5)G2Y
      |78F)X1V
      |LY8)PL3
      |W9K)3CK
      |TPK)2Y2
      |RH6)YLJ
      |6DY)77G
      |59F)RY1
      |W2C)1HY
      |YK9)Y9K
      |HJK)WT4
      |BF1)Q26
      |HVY)F5Y
      |1V3)TJ2
      |K77)1G3
      |FKN)R3S
      |HHJ)CFZ
      |SCR)YBL
      |VKP)YQJ
      |JHT)TVH
      |N4J)6HF
      |Q1S)SN3
      |8PP)9NL
      |G4Y)86S
      |JB1)4DB
      |57Y)ZGC
      |K1L)B4R
      |CCP)F4Q
      |M6M)TLJ
      |Q7Z)H3N
      |ZHF)YZ9
      |LGW)68B
      |MP8)PWD
      |GMQ)P8Y
      |LPK)PV7
      |QR6)QGT
      |1JM)PDT
      |852)JXR
      |C25)XVW
      |52V)27F
      |8LF)LJP
      |7S8)7SK
      |WFZ)W5S
      |4FQ)R5F
      |78B)624
      |XK6)YHL
      |37Q)PWP
      |DMG)FBD
      |8C1)FY7
      |XQW)7YZ
      |MB2)FKD
      |84P)C7B
      |X3S)R8J
      |9QB)RXL
      |CJJ)16Q
      |RKF)8S7
      |1DP)FX2
      |J2G)JK2
      |7HY)B6N
      |XJL)Y67
      |B4L)2JC
      |WJZ)8MJ
      |DBB)RBG
      |6WR)XFP
      |YHY)CNW
      |ZVN)PR2
      |H5V)C9B
      |YQJ)5C7
      |G76)7HQ
      |4B6)GWS
      |LS8)3DQ
      |HX1)1N4
      |8Q9)DTX
      |4NL)YY7
      |VN3)S6B
      |MM5)QHC
      |GXJ)PGV
      |6SQ)LZ2
      |82G)VYW
      |KPV)CK9
      |314)CK2
      |1TJ)GFW
      |3XR)VT1
      |2SC)XCV
      |3J3)W9R
      |8NX)CFH
      |VRK)YPV
      |8N7)LHK
      |PV7)N77
      |V53)852
      |JZJ)8N6
      |NX8)HZC
      |TGK)2PV
      |RHS)M34
      |L8J)KVT
      |721)GXN
      |95Q)HQT
      |7VS)Z1V
      |HG8)6N7
      |8SZ)JKK
      |FV6)RYV
      |B41)RCK
      |TNX)3JQ
      |F6S)M1L
      |8MJ)N61
      |4B6)ZHF
      |S2V)KQQ
      |X91)5LR
      |2XZ)3J6
      |F8W)1QN
      |1CK)FSN
      |B5Z)2K8
      |P2M)NPQ
      |59Y)YD8
      |1WR)JB7
      |3YR)3N8
      |FBR)M8B
      |VHW)8G1
      |5KS)C7N
      |WMS)8Q9
      |5NY)WR3
      |4T1)WB4
      |PHB)S2V
      |VYT)35X
      |FWF)K7K
      |9K5)V3B
      |V5N)JWC
      |T5D)SMP
      |TM3)R4N
      |17M)7B5
      |1DW)3F6
      |RF3)M1P
      |767)FPB
      |B4W)VMZ
      |YJ2)8BD
      |CNK)R83
      |2TQ)7XK
      |H5P)FV6
      |SB5)MW2
      |DNP)NCJ
      |ZDN)LWR
      |LNV)Q1N
      |9RV)8XS
      |55K)44B
      |BDY)FY8
      |NHZ)NR3
      |9WY)L5B
      |GKV)LZS
      |GGJ)LB9
      |K26)NNK
      |MYZ)WC2
      |G7S)2QW
      |N1V)XC7
      |WT4)PLG
      |PYL)62L
      |P7P)6W7
      |SXJ)RF3
      |2W5)DF6
      |Z7B)WRL
      |M5H)N6G
      |1T3)HG6
      |VCD)8D4
      |KM6)NFF
      |1QT)899
      |9X2)7JV
      |WJ2)93L
      |3GM)BCX
      |6BF)5CX
      |HF2)4NT
      |TXX)2VT
      |7H6)JK1
      |KBF)87T
      |FGS)6P5
      |GTR)JS7
      |B2H)GJ3
      |NCC)HHJ
      |MTY)QHW
      |TRG)QNK
      |QDG)XKR
      |VT1)4KQ
      |X96)LZC
      |XR9)69S
      |Z75)DBB
      |CFS)DCM
      |HCH)8SD
      |TH1)YZN
      |HWN)91L
      |F1H)8MS
      |329)BZD
      |319)XLL
      |6KF)ZBN
      |ZCN)ZZ9
      |B3S)ZLJ
      |5PV)ZM9
      |RZB)7GG
      |WB4)9JT
      |1Q7)VB7
      |QTY)J84
      |XQP)134
      |4BY)FS3
      |L7J)95G
      |Z1V)ZQB
      |2Y2)X6K
      |8BN)MMB
      |ZFB)T9R
      |B4J)BXC
      |9XV)4X4
      |CDC)TLK
      |K6J)HD8
      |JW8)T76
      |3G8)RHS
      |TQ6)7VN
      |2R9)HQX
      |KKT)CCP
      |8XX)LXK
      |2VQ)3P2
      |BRX)4HY
      |5PV)NQC
      |V2D)WG6
      |HKT)41S
      |WTY)FSG
      |LYP)JPJ
      |6N7)XTW
      |NWN)1XP
      |ZMB)J4S
      |MT4)63J
      |CLL)JHT
      |6FM)R3N
      |RV6)M83
      |KBZ)FB2
      |6FZ)56T
      |LJH)8C1
      |5CX)P49
      |HQX)3XZ
      |TXJ)69M
      |9P3)ZWG
      |NNK)PRD
      |LXT)B4W
      |2VT)QR6
      |1WR)C75
      |GW2)P7P
      |6P5)YJ2
      |BHS)1JB
      |PR2)GBB
      |X6H)TPK
      |K7P)1L5
      |SZT)FWV
      |C8P)4P9
      |39Q)R4P
      |WNZ)D1H
      |KVD)LYL
      |YBL)378
      |XFP)2ZW
      |R8C)1GD
      |71T)6DW
      |78G)BJD
      |2Z7)3M8
      |KZV)FH1
      |C71)SCG
      |TNN)5M4
      |DSX)MCB
      |FTQ)RGM
      |5VW)NVV
      |JXK)2FF
      |28V)16Z
      |XMC)YZ4
      |3DP)SLM
      |P12)2JW
      |LD5)HB4
      |HGL)1D9
      |L1Q)4B6
      |F5W)L29
      |J3G)ZZY
      |PZK)M4N
      |7JV)ZBT
      |9RM)DT7
      |XC7)YK9
      |K79)9VJ
      |Q1N)ZBD
      |Z1W)T9C
      |K6B)N4F
      |68M)B8M
      |QGY)5ZG
      |6TS)TQ6
      |F5M)DNL
      |WR9)FY4
      |3PY)Z8V
      |637)FBR
      |YQR)RF4
      |LG1)PRL
      |BXW)2HZ
      |TCK)HW5
      |P5J)XB4
      |YZ2)PYL
      |GCT)9P3
      |8D4)NBM
      |2TQ)GW3
      |WGF)NZ5
      |6YX)C25
      |F7C)J6R
      |7HQ)68M
      |52V)XC5
      |7YZ)CDT
      |GV5)9QB
      |963)VK9
      |HTQ)R7J
      |28V)JSY
      |M3F)H8H
      |N7J)KBZ
      |61P)79R
      |ZWG)31C
      |F2N)J9D
      |GWK)B3V
      |X47)H7F
      |2W2)ZVM
      |1ZP)H1T
      |WL3)1T7
      |1ZS)TRV
      |W5Y)4C1
      |9D5)8K3
      |NZG)YQR
      |J9D)V53
      |GFP)5HF
      |V2P)VHW
      |BK5)T4V
      |FDY)1JM
      |HCN)XJJ
      |FBD)P5W
      |J7G)YR5
      |CR7)1RG
      |LPB)95P
      |CXQ)HG8
      |49R)7HH
      |K9X)DMG
      |JPJ)SYY
      |RZK)BM4
      |XKR)BC2
      |J8B)TXD
      |H4V)Z1W
      |RFF)K6J
      |8HL)F3X
      |FPB)LWW
      |QV7)B6S
      |M5S)FTQ
      |B4T)QG4
      |F9V)9K5
      |YR5)KTZ
      |J4L)MMM
      |Q26)9D1
      |JBJ)3T4
      |NNS)ZCP
      |L29)NQH
      |QLQ)BC8
      |FTK)P4N
      |XYW)QLQ
      |RMC)Z3B
      |C9B)6HL
      |R1Z)KFR
      |HLD)K6W
      |FSG)DRQ
      |539)WJ2
      |J5X)7BY
      |P4X)H4C
      |K6W)MVQ
      |TK4)RZG
      |RWT)T5J
      |MJK)RFK
      |3S5)779
      |3CK)NWY
      |JVK)DKR
      |272)2MF
      |4X6)Q3T
      |ST2)GCQ
      |JVX)R1K
      |2H8)XR9
      |2CX)K8H
      |SD8)NWL
      |TLY)XK6
      |RRV)KCT
      |F3X)539
      |PGV)6DY
      |K1W)2TY
      |X56)MYW
      |KP5)6YH
      |RHZ)P14
      |JC9)3ZD
      |2T4)H1K
      |4BY)PPZ
      |BP3)C8S
      |QHW)HM3
      |RK8)FRJ
      |BND)ZV5
      |RJ8)XQ8
      |PDT)6FZ
      |2HL)9Q3
      |JXR)18G
      |3Z1)61P
      |5B9)4LN
      |378)4TV
      |SLM)G37
      |PWP)W2C
      |9PG)7FK
      |HC8)ZFT
      |T1S)KBF
      |K4P)SQZ
      |C7D)Y5C
      |3C6)F8W
      |LT9)X47
      |BDK)KPQ
      |DWM)MQ9
      |6SX)X3S
      |QC1)7RC
      |BZ4)P31
      |XRX)7TF
      |35X)9XY
      |XHW)SHZ
      |Y54)3CJ
      |QFR)KZL
      |FY7)4LD
      |Y5T)MT4
      |TJJ)TW3
      |4PF)KR5
      |2W2)YVH
      |KJY)1JV
      |D1V)8WF
      |J91)WW5
      |HZC)K77
      |PLG)YM3
      |B8H)N1V
      |2DN)TVD
      |WKX)YBK
      |27F)2QR
      |9NL)B47
      |D2Z)S78
      |FKD)J92
      |1TN)329
      |GFK)F5M
      |JS5)6GM
      |K81)RV6
      |6FX)585
      |5LR)MG1
      |LPV)LVV
      |XBX)R9L
      |2GB)6ZN
      |WFS)HX8
      |5SQ)H5P
      |3DR)PZH
      |L45)3SP
      |ZK8)TDV
      |LWH)L45
      |M1P)3P1
      |MV5)2HL
      |229)5RK
      |6M8)NHZ
      |H55)V3G
      |DNL)57Y
      |C1K)BXW
      |FMB)X7W
      |7DC)4T1
      |BN4)Q17
      |JXB)XBX
      |W9C)37Q
      |86S)89B
      |VZN)83C
      |XWB)L5H
      |7BB)SB5
      |S4M)V3X
      |4F3)4G7
      |Q3T)11J
      |4X4)3WS
      |9BV)LF3
      |M6T)D1V
      |ZYS)BBW
      |KFR)3C3
      |79R)Z28
      |P4N)BHS
      |TRV)GPP
      |M3N)N92
      |LVM)WJ7
      |FTD)3PX
      |D85)FCW
      |L5H)ZFB
      |HN1)LJH
      |JN4)TR2
      |7VN)MQ2
      |N2Y)WPW
      |5S5)XP3
      |S1Z)YNF
      |3M8)ZDD
      |5KH)2ZQ
      |CHB)F85
      |2QW)78F
      |32G)YCP
      |XKM)W54
      |56C)VJ1
      |6D6)PQH
      |3FM)W9K
      |M34)TNN
      |JHC)KB1
      |JLS)HC8
      |KCT)QV7
      |W8G)3B7
      |4L8)N6P
      |W97)D49
      |LQ4)Z78
      |JCW)RZB
      |VK9)PF7
      |WW5)G28
      |PLG)ZGN
      |KR5)G4Y
      |S6F)5MZ
      |V2C)2R9
      |4G6)YLQ
      |4C2)D47
      |Q2F)B4J
      |8BD)MRZ
      |R83)YCK
      |GQ9)44N
      |9QK)HCN
      |22Z)JJ1
      |LRK)6FX
      |68R)TK4
      |DZ2)FKN
      |2LM)J2G
      |G4Q)YCH
      |PQ7)Z7B
      |SNW)WFX
      |7B5)TTS
      |P2Y)5Z3
      |3P1)RHZ
      |3BS)GMY
      |V8R)8ZF
      |9KY)MTN
      |FF7)V9M
      |Z28)4G5
      |QNK)XZK
      |PQH)ZT2
      |W54)YOU
      |Y2Y)34X
      |G74)M4M
      |KC1)JKD
      |SWQ)PG2
      |1JM)RH7
      |LJP)ZMB
      |LHK)1CK
      |5YV)GTR
      |44C)BYH
      |H2R)971
      |W8Z)KJY
      |FH8)ZF9
      |8R3)RLL
      |RY1)2B1
      |C7N)4QY
      |6HF)M1C
      |2BC)8LF
      |2JB)SAN
      |55P)5YV
      |XYM)CVV
      |71M)VXN
      |69X)1TJ
      |11J)2CX
      |RKF)WC6
      |B4J)XM1
      |M7W)QJ5
      |GKN)LPK
      |7D6)JCT
      |D1D)RKF
      |J1P)2GB
      |ZZ9)YWJ
      |16Z)WK9
      |589)3Q2
      |K9X)QS1
      |XB4)9RH
      |ZZQ)B2S
      |QXR)5MW
      |6SM)PMB
      |1W5)9XR
      |V49)WPB
      |X94)C9F
      |14D)QFR
      |6G7)F9W
      |CNJ)8P3
      |KT4)HF2
      |7HH)5KK
      |7RC)W8G
      |5C7)R4Z
      |9FZ)DN9
      |8ZF)RK8
      |GZM)B9N
      |Q2J)JPH
      |R5F)L8J
      |XYW)6KN
      |R3N)P2Y
      |GW2)639
      |42V)KPV
      |3XZ)TN6
      |WJ7)3XF
      |JSY)8HL
      |J4S)2LM
      |XQ8)QR8
      |Z83)LYV
      |ZM9)W7G
      |FSN)DQH
      |KMN)18X
      |V3B)BNV
      |TDJ)9G7
      |VYT)CSH
      |CYZ)TNX
      |RCH)V8G
      |3SP)PHB
      |DM4)QDG
      |34X)25T
      |XLL)43G
      |9RH)B5Z
      |8P3)KLB
      |WD2)4MH
      |QGT)6VH
      |9XR)C71
      |S35)1TW
      |8QD)62J
      |25Z)JSV
      |X6H)7FJ
      |HBL)5W4
      |3HN)DBQ
      |59F)1W5
      |DRQ)HQJ
      |TMX)NZV
      |GGP)J59
      |GF6)4G6
      |N6G)1NC
      |WC6)3C6
      |95G)CXQ
      |VJX)QPS
      |MMB)V8R
      |4W2)B4L
      |44N)CY6
      |K7T)LV5
      |JB7)NML
      |M4N)85R
      |FMS)K6B
      |3PX)L15
      |PSH)ZK8
      |8XS)K7P
      |ZFT)GWK
      |HD8)M7W
      |SNP)D85
      |3LP)VJH
      |1YC)G4Q
      |JN4)NWN
      |ZZF)CFS
      |YY7)9LP
      |FP1)PZY
      |K6N)YYT
      |WFX)XWB
      |8CJ)2PT
      |LZ4)JKR
      |J6R)VJX
      |7BY)CB5
      |S1H)LVM
      |9XY)V2C
      |ZBT)G3Q
      |4RV)6WR
      |COM)SNP
      |YHL)HBH
      |G6X)637
      |NJ9)LKV
      |YG4)W1Z
      |GLW)P6M
      |HDF)725
      |Y9K)9T4
      |74T)Q2G
      |DLF)229
      |KBW)43T
      |85R)QSX
      |4G5)2VQ
      |4KQ)84P
      |FCB)MBN
      |8G1)HY7
      |TJ2)935
      |TC6)3FF
      |MJ2)Y6S
      |H3N)CC1
      |VXN)6TS
      |R4N)Q2F
      |DQK)D2H
      |JCT)6TK
      |4MH)SNW
      |2HZ)2BT
      |B7D)KM6
      |PZH)HGL
      |3B7)LS8
      |LQV)XWX
      |TBN)TTW
      |2FH)VPC
      |4KJ)3BN
      |961)FQF
      |G37)1R7
      |V4K)LNV
      |N1W)JS5
      |HQP)JNF
      |3Q2)KPT
      |DF6)CKJ
      |HQJ)1PR
      |YJK)Y2Y
      |82P)141
      |WJ7)TLY
      |C29)17T
      |L97)R1Z
      |CFZ)8T2
      |ZQB)CJJ
      |H39)J56
      |5ZG)1ZS
      |TS8)1P5
      |5ZF)45C
      |3SY)THK
      |85Y)LD5
      |8XS)L38
      |ZDZ)TS8
      |RCK)WK8
      |MMB)DMM
      |4GH)X6H
      |XCV)42H
      |M9Z)2BF
      |QGT)95Q
      |WMG)5ZF
      |PMB)5KS
      |YTB)CJD
      |SHF)ZKH
      |558)8RJ
      |V33)9QK
      |18G)KND
      |ZDD)X8Z
      |FV3)NTC
      |95G)1QT
      |9T4)DQP
      |7RW)C1K
      |RJ8)PW5
      |4C2)3SY
      |TLJ)RN8
      |1R7)M5H
      |L6L)CH2
      |MH4)TGK
      |GNH)GJY
      |QQW)YL3
      |9BG)VZS
      |149)HVY
      |P6M)FCG
      |NJJ)FGS
      |7FJ)NPN
      |2TY)59F
      |Y13)ZW2
      |NNV)SND
      |YWJ)BDV
      |KJ7)F7C
      |DGT)319
      |CC1)767
      |RD7)YCV
      |PMB)GV8
      |DH3)N8M
      |PQ3)MJ2
      |YCF)27B
      |675)J5C
      |6SQ)2C8
      |97C)K9Q
      |4C6)RD7
      |SGL)6Z4
      |ZQR)6FM
      |SYY)8QD
      |4G7)1KW
      |HT8)LDB
      |ZT1)7HD
      |8MS)5B9
      |BZK)861
      |G28)JVX
      |81Z)RJB
      |RGM)FBV
      |8T2)MQB
      |BJT)GW2
      |349)PQ9
      |N61)2X2
      |56T)H5V
      |S9K)HBL
      |6ZN)MYB
      |MPL)PF9
      |7FK)7HW
      |1D9)NX8
      |SMP)85T
      |PZY)ZNW
      |7MQ)J1M
      |6XM)K4H
      |3F6)2RG
      |LJB)Z7C
      |6HL)Q6R
      |9J4)N56
      |6R4)56C
      |2XG)H93
      |FR4)G74
      |TSJ)4C2
      |HB4)CNR
      |9D1)2W4
      |F4Q)CW2
      |G7X)Z7X
      |FH1)RMC
      |T9R)L7J
      |KKP)3XR
      |2ZQ)PQ7
      |LWR)V33
      |63J)L6V
      |ZBN)1N5
      |WPB)3S5
      |FKN)V5F
      |L1Q)9N9
      |7RV)ZT1
      |B1B)VJF
      |KMY)BND
      |C9F)NNV
      |RY1)B68
      |5Z3)KC1
      |R4S)J1P
      |39V)H9K
      |1JB)H2Y
      |3XW)K39
      |9W9)8B9
      |PQ9)ST2
      |9PB)Y54
      |X14)4C6
      |JG9)ZHD
      |5M5)DLF
      |81X)HN1
      |3XH)2R5
      |8LJ)NTQ
      |GW3)3YR
      |TVD)LZ4
      |DMM)9X2
      |J19)3LP
      |95P)JB1
      |17T)FDY
      |LZC)4KJ
      |FY4)TNQ
      |DZV)G76
      |725)Z6T
      |2XP)FR4
      |CFF)WMS
      |WRL)STF
      |3T4)6YX
      |NTC)V8N
      |Z3B)ZCN
      |1ML)WKB
      |VY6)DZV
      |JH5)K4J
      |TVH)QPW
      |624)9XV
      |5F5)4F3
      |8K3)FCF
      |9QL)HFF
      |69S)GZ5
      |5V1)23Y
      |P48)BZ4
      |BS2)4JT
      |N4F)F9V
      |GPS)1YC
      |T9C)Z2K
      |Z7X)WFS
      |Z2K)HBC
      |7XK)ZRR
      |VHW)J5X
      |V9M)ZKP
      |K75)JG9
      |GXJ)LRK
      |TW3)G7S
      |31C)LCW
      |4T1)8J2
      |MBB)2HV
      |LPN)CHM
      |3J6)KMY
      |5X9)P91
      |JKR)ND3
      |L15)WWV
      |SZ4)7DC
      |G9J)DC8
      |BY5)DNP
      |LLW)VKX
      |TDV)ZNR
      |JFX)3HR
      |LKV)P6G
      |L5B)2GH
      |BTV)JC9
      |TVT)HD9
      |ZV5)CYZ
      |NQH)D1D
      |2PV)WKP
      |BCX)X14
      |6W7)R28
      |W52)ZNK
      |79R)F6S
      |ZKH)VYS
      |5Q7)182
      |7QV)2W2
      |6YH)8NX
      |MQ2)95H
      |GCQ)9W1
      |2HV)XKN
      |6XF)4L8
      |SND)8XD
      |GPP)LSW
      |RL2)3Q4
      |QNK)S35
      |GCS)NRS
      |SC7)1ZP
      |ZNK)DW1
      |NPN)W4P
      |D6L)R3C
      |86T)B4T
      |L33)GCS
      |ZPG)HV2
      |1PR)6J7
      |2ZW)LMF
      |5M4)1B4
      |B88)3DP
      |WSJ)49M
      |8WF)99B
      |8S4)N3B
      |6CH)MSJ
      |D49)V49
      |9DZ)576
      |3XF)6D6
      |R1K)LWH
      |7TF)LQV
      |V1C)SB9
      |D1L)3GM
      |GFW)5M5
      |5RK)85G
      |ZL9)DJ6
      |SGX)558
      |28P)9J4
      |GJ3)9L7
      |JK2)Z6S
      |6J7)J19
      |WNL)FP2
      |N9F)93B
      |6DW)39V
      |FWF)JCW
      |W7G)59Y
      |F9W)6SX
      |H6C)YJK
      |YQ9)F2N
      |RNK)BDK
      |C4L)VKP
      |QPW)9QL
      |FZL)LPN
      |38F)3DR
      |GV5)272
      |WK8)7TQ
      |K4J)JMW
      |QV6)4GH
      |2B1)BDQ
      |55P)9QD
      |9QD)7D6
      |S78)LY8
      |7SF)GSK
      |YNP)ZC2
      |R28)YCF
      |69K)G16
      |3Q2)3HN
      |J6C)22Z
      |Q2G)LMM
      |FCW)JN4
      |3WS)SD8
      |QWB)4PF
      |1P4)R8C
      |TNQ)NZG
      |YR5)B85
      |K7K)GLW
      |P8Y)G5Z
      |ZNR)NJJ
      |SBS)B8T
      |2HC)6YS
      |9MX)MM7
      |ZQM)WD2
      |93B)XMC
      |N3P)TMX
      |JK1)9RM
      |W6F)W52
      |144)86T
      |9VJ)6KF
      |1B4)PS4
      |Z7C)B7P
      |W97)Y5T
      |MVQ)8N7
      |WC2)55J
      |GXG)K7T
      |DC8)YGH
      |BYH)KJ4
      |6RR)LPG
      |Y5G)M9Q
      |77G)M9Z
      |2RG)4W2
      |FY8)3PY
      |G37)K9X
      |5SP)LGW
      |2HV)BFS
      |3N8)KQ5
      |62L)TVF
      |971)KVD
      |182)CNK
      |DBQ)P12
      |BWR)9WY
      |JQV)4RV
      |QS1)X56
      |B2S)8LJ
      |2CQ)CCQ
      |2B1)GCT
      |ZDN)B2H
      |FS3)HSL
      |SYY)S4M
      |79N)JLS
      |847)9LB
      |7TQ)BS2
      |9BG)7VB
      |6KN)X11
      |85T)XRX
      |HMT)L1Q
      |DRV)Q5V
      |QHC)F1H
      |1MS)H6C
      |7J2)38F
      |TVF)39Q
      |Z4F)4BY
      |8RJ)8LM
      |2Q6)9W9
      |DQH)LG1
      |FF6)M19
      |GBB)TCK
      |TRG)8XX
      |25T)X5V
      |LNP)1ML
      |8LM)FZL
      |3P2)FW5
      |QC7)DRV
      |PKQ)N3P
      |HM3)PZK
      |WKB)3QZ
      |9W1)QC7
      |V8M)HHX
      |Z9Y)5NY
      |L2G)5PV
      |H1T)GV5
      |BBW)VLY
      |DN9)K26
      |2FF)M3F
      |MCB)XQW
      |D1H)G7X
      |K8H)DQK
      |6D6)W5Y
      |G2Y)BY5
      |ZST)3F8
      |VZM)B8H
      |FCF)SXJ
      |8W2)BJT
      |KZL)F4S
      |RRV)8CJ
      |KBZ)YXP
      |P91)MFD
      |Y4W)85Y
      |419)LNS
      |63J)SGL
      |WT4)P56
      |JNF)8YZ
      |F5Y)GW9
      |XX4)HJK
      |K42)TC3
      |899)2Q6
      |2BF)8S8
      |NML)LJV
      |D2H)YZ2
      |62J)YND
      |ZF9)M6T
      |4LN)314
      |PPZ)4NL
      |NRS)4X6
      |VVZ)W6H
      |R4Z)9PB
      |YDC)WK1
      |BP3)9FZ
      |P8V)M2D
      |DT7)QYD
      |1N4)L2G
      |1RG)NXW
      |34X)ZQG
      |MYW)721
      |2NT)FV3
      |M9Q)JN6
      |33R)ZQM
      |3SY)L84
      |PF7)F96
      |YLJ)XSY
      |3HR)TC6
      |F3Z)M8C
      |QJG)FF6
      |NVV)4K8
      |MM7)8BY
      |G16)1T3
      |CH2)4WF
      |PZH)WQJ
      |KJ7)WJZ
      |MB2)T5D
      |YND)TLF
      |JPJ)847
      |FBV)H55
      |5SQ)G4V
      |4KJ)5KH
      |RLK)NNS
      |QJG)KBW
      |FX2)4WL
      |585)RJ8
      |7NK)HZY
      |NWY)K1L
      |93L)N2Y
      |8H5)X2K
      |CNW)5FY
      |RFK)M3P
      |NVV)DWM
      |HSL)FP1
      |QG4)Z4F
      |7HW)J9P
      |3Q4)GPS
      |WW5)1DP
      |PF9)MQC
      |SLZ)3YL
      |JZJ)GXJ
      |HBH)MSQ
      |MSQ)JW8
      |6YS)K79
      |1N5)DZ2
      |LMM)LNF
      |LKW)5F5
      |G4V)B6X
      |TR2)Y5G
      |2V2)KJ7
      |3DQ)BZK
      |JS4)SLZ
      |H93)88P
      |L3B)CVL
      |J92)7HY
      |7HD)7FQ
      |NQC)GNR
      |79N)TDJ
      |VJ1)2FH
      |9G7)BGN
      |3C3)78G
      |XWR)C4L
      |M1P)1DW
      |XJ1)JQV
      |KTZ)FQ9
      |1XP)5B5
      |B3F)GDQ
      |SNP)Q8G
      |4LD)TRL
      |615)BDY
      |F9V)9KY
      |CCQ)P48
      |WTD)MCN
      |RFV)8BN
      |TXD)RFV
      |C4L)YTB
      |FBR)97L
      |1HN)KCX
      |V8G)B3F
      |MG1)LKW
      |V93)T48
      |R7D)MV5
      |M83)X91
      |42H)TMZ
      |YD8)675
      |WF5)1HN
      |M2D)Z83
      |GNR)Z6Z
      |CJD)VM6
      |F1H)KZV
      |MQC)J8B
      |45C)RD1
      |46V)3QM
      |ZBD)23G
      |22Y)LMC
      |KPQ)NGV
      |TBB)Q2J
      |91L)R2C
      |BM4)CT4
      |2JC)MP8
      |3VY)LSK
      |5HL)1Q7
      |LDB)XW5
      |JKD)2V2
      |LMC)HDF
      |3X2)7J2
      |SN3)Z75
      |HD9)KP5
      |K6F)JCH
      |RXL)JDZ
      |B7P)1V3
      |LYV)HX1
      |9CV)1L9
      |W1Z)ZDZ
      |5KK)144
      |KVT)LV1
      |KGC)GGJ
      |NQC)28V
      |ZNW)D2C
      |JN6)MZN
      |SBD)HCH
      |2BT)CR7
      |YZ9)WMG
      |1DP)5Q7
      |89B)JFX
      |JJ1)QQX
      |1TW)9PG
      |LLK)BK5
      |BZD)55P
      |7K7)28P
      |Z6T)QWB
      |X1V)BM1
      |QDG)D6L
      |Y5C)XYW
      |XW5)VZM
      |2H8)NJ9
      |QH1)5VW
      |9KP)QC1
      |KDR)9PR
      |Q17)JXK
      |9X2)XFL
      |RN8)9CV
      |LB9)LPB
      |FB2)5RT
      |J5V)W6F
      |4YP)HTQ
      |FCG)JBJ
      |XBS)DH3
      |NWY)QGY
      |XM1)WSJ
      |XR5)H39
      |WQJ)V2P
      |RNH)TDC
      |MX2)P8V
      |J84)P4X
      |RD1)7SF
      |SCG)3XW
      |TTS)JHC
      |NDR)GNH
      |97L)23X
      |XVL)DDF
      |F85)YQ9
      |XKN)FWF
      |BM1)GGP
      |ZRR)71T
      |B6N)YDC
      |TM3)ZYS
      |XVW)QGK
      |Y5D)55K
      |C75)JVK
      |N56)MJK
      |1M3)C8P
      |SB5)9BG
      |GPY)YV8
      |VN4)TVT
      |W7G)Y5D
      |141)YMP
      |6KR)MBB
      |9XY)8C8
      |TLK)F5W
      |X6K)L6L
      |1D9)WX7
      |P56)XHW
      |LZ2)1DK
      |MTN)ZPG
      |1KW)Q1S
      |K4H)TSJ
      |9N9)N7J
      |6C4)MH4
      |X11)8RL
      |N8M)PDR
      |X7W)ZZQ
      |J5C)XS4
      |JSV)K75
      |T2B)69X
      |SLN)VYT
      |2PT)4FQ
      |J1M)TXJ
      |QS5)YRV
      |RHQ)58T
      |CFH)1P4
      |S6B)H1C
      |J51)V4K
      |VKX)9D5
      |N7R)Z77
      |MQB)961
      |CSH)7MQ
      |T76)KGH
      |3JQ)YV5
      |L84)WNZ
      |VZM)G84
      |VFW)8H5
      |779)6SJ
      |7VB)QJG
      |72M)CNJ
      |W9R)3WR
      |7LW)6R4
      |6SJ)VCD
      |K9Q)3G5
      |R2C)RL2
      |TLF)T1S
      |RLL)Z7J
      |P5W)FR7
      |QJ5)RRV
      |Z6T)V2D
      |PDR)L33
      |1T7)J6C
      |YZN)TXX
      |DKR)ZQR
      |QV6)QXR
      |B47)8DZ
      |YXP)2H8
      |Z8V)K6N
      |2C8)JZJ
      |JCH)33R
      |4NT)1MS
      |HX8)F4H
      |9LP)JXB""".stripMargin.split("\n").map(_.trim)

  val test1 = """COM)B
                    |B)C
                    |C)D
                    |D)E
                    |E)F
                    |B)G
                    |G)H
                    |D)I
                    |E)J
                    |J)K
                    |K)L""".stripMargin.split("\n").map(_.trim)

  val test2 = """COM)B
                |B)C
                |C)D
                |D)E
                |E)F
                |B)G
                |G)H
                |D)I
                |E)J
                |J)K
                |K)L
                |K)YOU
                |I)SAN""".stripMargin.split("\n").map(_.trim)

  val orbits = input.foldLeft(Map.empty[String, String]) { case (m, s) =>
    val Array(l, r) = s.split("\\)")
    m.updated(r, l)
  }

  @tailrec
  def countOrbits(plob: String, acc: Int): Int = {
    if (plob == "COM") acc else countOrbits(orbits(plob), acc + 1)
  }

  @tailrec
  def pathToCom(from: String, acc: List[String]): List[String] = {
    if (from == "COM") acc.reverse else pathToCom(orbits(from), orbits(from) +: acc)
  }

  val fromMe = pathToCom("YOU", Nil)
  val fromSan = pathToCom("SAN", Nil)

  val target = fromMe.dropWhile(s => !fromSan.contains(s)).head

  val meToTarget = fromMe.takeWhile(_ != target).length
  val sanToTarget = fromSan.takeWhile(_ != target).length

  println(meToTarget + sanToTarget)
}
