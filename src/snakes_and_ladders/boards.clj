(ns snakes-and-ladders.boards)

(def national-archive-1901
  {:filename "snakes-ladders_1901.jpg"
   :size [700 700]
   :middle-of-corner-squares [40 40 660 660]
   :columns 10
   :rows 10
   :finish 100
   :snakes-and-ladders-map {;; Snakes
                            44 22
                            46 5
                            48 9
                            52 11
                            55 7
                            59 17
                            64 36
                            69 33
                            73 1
                            83 19
                            92 51
                            95 24
                            98 28
                            ;; Ladders
                            8 26
                            21 82
                            43 77
                            50 91
                            54 93
                            62 96
                            66 87
                            80 100}})

;; Miaou Miaou
;; This is a 1900s french print.  At one point it was for sale at:
;; http://www.art.com/products/p10098547-sa-i1288768/miaou-miaou-c-1900-.htm?sorig=sch&ui=e5343aeae7db4e13ac8ded4d5a387103
(def cats-board-details
  {:filename  "resources/cats-and-ladders-1024x734.jpg"
   :size [1024 734]
   :middle-of-corner-squares [48 45  973 689]
   :columns 12
   :rows 10
   :finish 120
   :width 12
   :height 10
   :top 44
   :bottom 686
   :left 49
   :right 972
   :snakes-and-ladders-map {;; "Snakes"
                            18 8
                            19 3
                            45 27
                            63 38
                            104 21
                            108 80
                            118 95
                            ;; Ladders
                            11 36
                            16 34
                            24 51
                            32 93
                            57 65
                            61 83
                            69 75
                            71 96
                            101 115
                            106 110}})

(def board cats-board-details)
;;(def board national-archive-1901)

(def GJHayter-Board
  {:snakes-and-ladders-map { ;; Snakes
                            21 3
                            24 7
                            35 9
                            50 11
                            53 15
                            60 23
                            75 44
                            89 48
                            93 25
                            97 65
                            99 58
                            ;; Ladders
                            4 16
                            12 33
                            18 22
                            26 37
                            42 61
                            49 51
                            55 74
                            82 98
                            85 95
                            88 92}})
