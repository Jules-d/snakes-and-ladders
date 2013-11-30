(ns snakes-and-ladders.quil
  (:use quil.core)
  (:use clojure.test)
  (:require [snakes-and-ladders.boards :as boards]))

(def img (ref nil))

(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 30)                   ;;Set framerate to 1 FPS
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

(defn draw-player [player player-positions]
  (let [[x y] ( column-row-to-screen-x-y (squarenum->column-row (player-positions player)))]
    (do
      (apply fill (player-token-color player))
      (apply ellipse (concat [x y] [token-size token-size])))))

(def wip-origin [0 0])
(def wip-destination [0 0])

(defn draw [game-state]
  (image @img 0 0)
  (stroke 0 0 0)             ;;Set the stroke colour to a random grey
  (stroke-weight 1)       ;;Set the stroke thickness randomly
  (let [board boards/board
        player-positions (game-state 0)]
    (dorun (for [i (range (count player-positions))]
             (do
               (apply fill (player-token-color i))
               (draw-player i player-positions))))))

(defn mouse-moved [& args]
  (let [x (mouse-x)  y (mouse-y)]
    (reset! (state :mouse-position) [x y])))

;; Starting to work on animation
(defn progress [[start-time end-time] current-time]
  (let [duration (max 0 (- end-time start-time))
        progress (max 0 (- current-time start-time))]
    (if (<= progress 0)
      0
      (min  (/ progress duration) 1))))

(is (= (progress [100 200] 100) 0 ))
(is (= (progress [100 200] 150) 1/2 ))
(is (= (progress [100 200] 200) 1 ))
(is (= (progress [100 200] 300) 1 ))
