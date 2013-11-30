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
     (let [state  [(vec (repeat number-of-players 1)) 0 0]]
       state)))

(def state-atom (atom (new-game 2)))


(defn reset-game [players] (swap! state-atom (fn [old-state] (new-game players))))

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

(defn next-state [[positions current-player-id turn-counter]]
  (let[current-player-position (positions current-player-id)
       current-roll (roll-die)
       finish (board :finish)
       rolled-position (+ current-player-position current-roll)
       new-position (min finish (shifted-position rolled-position))
       new-positions (assoc positions current-player-id new-position)]
    (if (= new-position finish)
      (do ( println "Winner player" (inc current-player-id) "!!!")
          [new-positions current-player-id turn-counter "Winner!"])
      [new-positions (rem
                      (inc current-player-id)
                      (count positions))
       (inc turn-counter)])))

(defn take-turn []
  (swap! state-atom next-state))

(defn draw []
  ( drawing/draw @state-atom))

(quil/defsketch snakes-and-ladders-sketch
  :title "Snakes and Ladders"
  :setup drawing/setup
  :draw  draw
  :size (:size boards/board) ;;TODO
  :mouse-clicked take-turn)

(defn -main [& args]
  (println "args - " args))

;; Control expressions
(reset-game 2)

(take-turn)
