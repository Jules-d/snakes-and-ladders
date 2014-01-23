(ns snakes-and-ladders.quil
  (:use quil.core)
  (:use clojure.test)
  (:require [snakes-and-ladders.boards :as boards]))

(def img (ref nil))

(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 60)                   ;;Set framerate to 1 FPS
  (background 0)                    ;;Black background
  (dosync (ref-set img (load-image (:filename boards/board))))
  (set-state! :mouse-position (atom [0 0])))

(def token-size 27)

(defn player-token-color [player]
  ([[180 0 180]
    [0 255 0]
    [0 0 255]
    [255 255 0]
    [255 0 255]
    [0 255 255]] player))

(defn squarenum->column-row [square-number]
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

(defn position-to-screen-x-y [position]
  (column-row-to-screen-x-y (squarenum->column-row position)))

(defn draw-player [player player-positions]
  (let [[x y] ( position-to-screen-x-y (player-positions player))]
    (do
      (apply fill (player-token-color player))
      (apply ellipse (concat [x y] [token-size token-size])))))

(defn mouse-moved [& args]
  (let [x (mouse-x)  y (mouse-y)]
    (reset! (state :mouse-position) [x y])))


(defn draw-players [game-state]
  (let [player-positions (:positions game-state)
        animations (:animations game-state)
        time (System/currentTimeMillis)]
    (dorun (for [i (range (count player-positions))]
             (let [player i
                   position (player-positions i)
                   animation (animations i)
                   [x* y*] (position-to-screen-x-y position)
                   [time2 [x y]] (animation [time [x* y*]])]
               (do
                 (apply fill (player-token-color i))
                 (ellipse x  y token-size token-size)))))))

(defn draw-die [roll]
  (stroke 0 0 0)
  (fill 255 255 255)
  (stroke-weight 2)
  (let [x 1
        y 1
        size 13
        spot-size 8
        middleh (+ x (* 2 size))
        middlev (+ y (* 2 size))
        left (+ x size)
        top size
        right (+ x (* 3 size))
        bottom (+ y (* 3 size))]
    (do
      (rect x y (+ x (* 4 size)) (+ y (* 4 size)))
      (fill 0 0 0)
      (if (odd? roll)
        (ellipse middleh middlev spot-size spot-size))
      (if (> roll 1)
        (do
          (ellipse left top spot-size spot-size)
          (ellipse right bottom spot-size spot-size)))
      (if (> roll 3)
        (do
          (ellipse right top spot-size spot-size)
          (ellipse left bottom spot-size spot-size)))
      (if (= roll 6)
        (do
          (ellipse right middlev spot-size spot-size)
          (ellipse left middlev spot-size spot-size))))))

(defn draw [game-state]
  (image @img 0 0)
  (stroke 0 0 0)
  (stroke-weight 2)
  (let [board boards/board
        player-positions (game-state :positions)
        time (System/currentTimeMillis)]
  (draw-players game-state)
  (draw-die (:current-roll game-state))))
