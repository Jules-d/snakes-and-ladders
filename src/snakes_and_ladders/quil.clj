(ns snakes-and-ladders.quil
  (:use quil.core)
  (:use clojure.test)
  (:require [snakes-and-ladders.boards :as boards]))

(def img (ref nil))
(def animation-duration 750)

(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 60)                   ;;Set framerate to 1 FPS
  (background 0)                    ;;Black background
  (dosync (ref-set img (load-image (:filename boards/board))))
  (set-state! :mouse-position (atom [0 0])))

(def token-size 27)

(defn player-token-color [player]
  ([[255 0 0]
    [0 255 0]
    [0 0 255]
    [255 255 0]
    [255 0 255]
    [0 255 255]] player))

(defn  squarenum->column-row [square-number]
  (let [board boards/board
        n (- square-number 1)
        row (int  (/ n (board :columns)))
        columns (board :columns)
        initial-column (rem n columns)
        column (if (even? row)
                 initial-column
                 (- columns initial-column 1))]
    [column row]))

(defn column-row-to-screen-x-y [[column row]]
  (let [board boards/board
        [left top right bottom] (board :middle-of-corner-squares)
        width (- right left)
        height (- bottom top)
        square-width (/ width (-  (board :columns) 1))
        square-height (/ height (-  (board :rows) 1))
        horizontal-offset (* square-width column)]
    [(int (+ left horizontal-offset))
     (int  (- bottom (* square-height row)))]))

(is (= (squarenum->column-row 1) [0 0]))

(def player-1-location [2 2])

(defn timer [seconds]
  (let [factor (* 1000 seconds)]
    (/ (rem (millis) factor) factor)))

(defn wiggle
  [n] (+ n (* 50  (sin (* PI 2 (timer 2))))))

(defn position-to-screen-x-y [position]
  (column-row-to-screen-x-y (squarenum->column-row position)))

(defn draw-player [player player-positions]
  (let [[x y] ( position-to-screen-x-y (player-positions player))]
    (do
      (apply fill (player-token-color player))
      (apply ellipse (concat [x y] [token-size token-size])))))

(def wip-origin [0 0])
(def wip-destination [0 0])

(defn mouse-moved [& args]
  (let [x (mouse-x)  y (mouse-y)]
    (reset! (state :mouse-position) [x y])))

;; Starting to work on animation
(defn progress [[start-time end-time] current-time]
  (let [duration (max 0 (- end-time start-time))
        progress (max 0 (- current-time start-time))]
    (if (or (= duration 0)
            (<= progress 0))
      0
      (min  (/ progress duration) 1))))

(def another-atom (atom 0))

(def animation-atom (atom {:origin [0 0]
                           :destination [150 150]
                           :start 0
                           :duration animation-duration}))

(def animation-function-atom (atom (fn [] [100 100])))

;; Usage:
;;(update-animation @animation-atom :start 12 :origin [42 2] :destination [6 7])
;;{:duration 0, :destination [6 7], :start 12, :origin [42 2]}
(declare generate-dynamic-position)
(declare generate-composable-dynamic-position)
(defn update-animation []
  (let [time (millis)
        old-destination (@animation-atom :destination)
        new-x (mouse-x)
        new-y (mouse-y)
        new-destination [new-x new-y]]
    (do
      (swap! animation-atom assoc
             :origin old-destination
             :destination new-destination
             :start time)
      (compare-and-set! animation-function-atom
                        @animation-function-atom
                        (generate-composable-dynamic-position {:start-time time
                                                    :end-time (+ time 1250)
                                                    :origin old-destination
                                                    :destination new-destination}
                                                   )))))

(defn do-animation []
;  (println "do-animationing at " (millis))
  (compare-and-set! another-atom @another-atom (millis))
  (update-animation))

(defn pairwise [& args]
  (apply map vector args))

(defn interpolate [progress [a b]]
  (let [ diff (- b a)]
       (+ a (* progress diff))))

(defn interpolate-vectors [progress origin destination]
  ;; Convert [[x1 y1] [x2 y2]] to [[x1 x2] [y1 y2]]
  (let [pairs (pairwise origin destination)]
    (vec (map (partial interpolate progress) (pairwise origin destination)))))

(is (= (interpolate-vectors 0.5 [0 0] [10 10]) [5.0 5.0]))
(is (= (interpolate-vectors 1 [0 0] [10 10]) [10 10]))

; An "animation" is just a value that varies by time: a function.
(defn example-dynamic-position [time]
  (let [start-time 0
        minutes 1
        end-time 3000
        clamped-time (min end-time (max time start-time))
        start-position 0
        end-position 1000
        current-progress (progress [start-time end-time] clamped-time)
        result (interpolate current-progress [start-position end-position])]
    result))

(defn generate-dynamic-position [args]
  (let [*start-time (args :start-time)
        *end-time (args :end-time)
        origin (args :origin)
        destination (args :destination)]
    (fn [time]
      (let [start-time *start-time
            end-time *end-time
            clamped-time (min end-time (max time start-time))
            start-position origin
            end-position destination
            current-progress (progress [start-time end-time] clamped-time)
            result (interpolate current-progress
                                [start-position end-position])]
        result)
      )))

(def example-generated-dynamic-position
  (generate-dynamic-position {:start-time 2000
                              :end-time 7000
                              :origin 100
                              :destination 800}))

(defn generate-composable-dynamic-position [args]
  (let [*start-time (args :start-time)
        *end-time (args :end-time)
        origin (args :origin)
        destination (args :destination)]
    (fn [[time position]]
      (let [start-time *start-time
            end-time *end-time
            clamped-time (min end-time (max time start-time))
            start-position origin
            end-position destination
            current-progress (progress [start-time end-time] clamped-time)]
        (if (or (< time start-time)
                (> time end-time)) [time position]
                [time (interpolate-vectors current-progress
                                           start-position end-position)])))))


(def example-composable-generated-dynamic-position
  (generate-composable-dynamic-position {:start-time 1000
                                         :end-time 11000
                                         :origin [100 100]
                                         :destination [1100 2100]}))


(is (= (example-composable-generated-dynamic-position [0 [1 1]]) [0 [1 1]]))
(is (= (example-composable-generated-dynamic-position [999 [2 2]]) [999 [2 2]]))
(is (= (example-composable-generated-dynamic-position [1000 [2 2]]) [1000 [100 100]]))
(is (= (example-composable-generated-dynamic-position [11000 [2 2]]) [11000 [1100 2100]]))
(is (= (example-composable-generated-dynamic-position [11001 [2 2]]) [11001 [2 2]]))
(is (= (example-composable-generated-dynamic-position [4000 [2 2]]) [4000 [400N 700N]]))
(is (= (example-composable-generated-dynamic-position [7800 [2 2]]) [7800 [780N 1460N]]))



(defn progress [[start end] value]
  (let [span (- end start)
        clamped-value (min (max value start) end)
        elapsed (- clamped-value start)]
    (if (= span 0)
      0
      (/ elapsed span))))

(is (= (progress [0 100] 0) 0))
(is (= (progress [0 100] 50) 1/2))
(is (= (progress [0 100] 100) 1))

(defn draw-animated-piece []
  (fill 255 0 255)
  (let [time (millis)
        start (@animation-atom :start)
        duration (max (@animation-atom :duration) 1)
        origin (@animation-atom :origin)
        destination (@animation-atom :destination)
        animation-function @animation-function-atom
        elapsed (max 0 (- time start))
        progress (progress [start (+ start duration)] time)
        [x y] (interpolate-vectors progress origin destination),
        [returned-time dynamic-position]
        (example-composable-generated-dynamic-position [time [destination]])]
    (do
      ;;(println progress " * " origin " -> " destination " : " position)
      (ellipse
       ;;(example-generated-dynamic-position time)
       x
        y token-size token-size)
      ;(println (concat dynamic-position [60 60]))
      ;;(apply ellipse (concat dynamic-position [60 60]))
      )))

(defn draw-players-dynamic [game-state]
  (let [player-positions (:positions game-state)
        animations (:animations game-state)
        time (quil.core/millis)]
    (dorun (for [i (range (count player-positions))]
             (let [player i
                   position (player-positions i)
                   animation (animations i)
                   [x* y*] (position-to-screen-x-y position)
                   [time2 [x y]] (animation [time [x* y*]])]
               (do
                 (apply fill (player-token-color i))
                 (ellipse x  y token-size token-size))))))
  )

(comment
(defn draw-player [player player-positions]
  (let [[x y] ( position-to-screen-x-y (player-positions player))]
    (do
      (apply fill (player-token-color player))
      (apply ellipse (concat [x y] [token-size token-size])))))
  )


(defn draw [game-state]
  (image @img 0 0)
  (stroke 0 0 0)
  (stroke-weight 2)
  (let [board boards/board
        player-positions (game-state :positions)
        animations (:animations game-state)
        time (quil.core/millis)]

    (dorun (for [i (range (count player-positions))]
             (do
               (apply fill (player-token-color i))
               (comment (draw-player i player-positions))))))
  (draw-animated-piece)
  (draw-players-dynamic game-state))
