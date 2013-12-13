(ns snakes-and-ladders.core
  (:require [quil.core :as quil])
  (:require [snakes-and-ladders.quil :as drawing])
  (:require [snakes-and-ladders.boards :as boards])
  (:use clojure.repl)
  (:gen-class)
  (:use clojure.test))

;; State

;; Snakes and Ladders does not need much state:
;; position for each players, and a turn counter.
;; The state also includes the last die rolled, so the UI can render it
;; You need a number of players to start the game, too
(defn new-game
  ([] (new-game 2))
  ([number-of-players]
     (let [positions (vec (repeat number-of-players 1))
           state  {:positions positions
                   :previous-positions positions
                   :move-times positions
                   :animations (vec (repeat number-of-players (fn [args] args)))
                   :turn-counter 0
                   :player-turn 0}]
       state)))

(def state-atom (atom (new-game 2)))

(defn reset-game [players]
  (swap! state-atom (fn [old-state] (new-game players))))

 ;; 3 players at positions 7 13 and 1, with player 3 about to move
(def example-state [[7 13 4] 2 6])

(def board boards/board)

(defn shifted-position [position]
  (if-let [shifted-position ((board :snakes-and-ladders-map) position)]
    (do
      (println "Shifted from" position "to" shifted-position)
      shifted-position)
    position))

(defn roll-die []
  (let [ current-roll (+ 1 (rand-int 6))]
       (println "Rolled a" current-roll)
       current-roll))

(defn next-state [state]
;;  (println "millis " (quil/millis))
  (let[positions (state :positions)
       current-player-id (state :player-turn)
       turn-counter (state :turn-counter)
       move-times (state :move-times)
       animations (state :animations)
       current-player-position (positions current-player-id)
       current-roll (roll-die)
       finish (board :finish)
       time (quil/millis)
       old-screen-position (drawing/position-to-screen-x-y current-player-position)
       rolled-position (+ current-player-position current-roll)
       new-position (min finish (shifted-position rolled-position))
       new-screen-x-y (drawing/position-to-screen-x-y new-position)
       new-animation (drawing/generate-composable-dynamic-position
                              {:start-time time
                               :end-time (+ time 1250)
                               :origin old-screen-position
                               :destination new-screen-x-y})
       new-positions (assoc positions current-player-id new-position)
       new-move-times (assoc move-times current-player-id time)
       new-animations (assoc animations current-player-id new-animation)
       ]
    (if (= new-position finish)
      (do (println "Winner player" (inc current-player-id) "!!!")
          [new-positions current-player-id turn-counter "Winner!"])
      (do ;;(println new-move-times)
          {:positions new-positions
           :previous-positions positions
           :player-turn (rem (inc current-player-id) (count positions))
           :turn-counter (inc turn-counter)
           :move-times new-move-times
           :animations new-animations
           }))))

(defn take-turn []
  (swap! state-atom next-state)
  (drawing/do-animation))

(defn draw []
  (drawing/draw @state-atom))

;; Alternating control.  You can use these functions in defsketch.
(defn key-turn []
  (println (@state-atom 1) (= 0 (@state-atom 1)))
  (when (= 0 (@state-atom 1)) (take-turn)))

(defn mouse-turn []
  (println (@state-atom 1) (= 0 (@state-atom 1)))
  (when-not (= 0 (@state-atom 1)) (take-turn)))

(quil/defsketch snakes-and-ladders-sketch
  :title "Snakes and Ladders"
  :setup drawing/setup
  :draw  draw
  :size (:size boards/board)
  :mouse-pressed take-turn
  :key-pressed take-turn)

(defn -main [& args]
  (println "args - " args))

;; Control expressions
(reset-game 2)

;; (Take-turn)
