(ns snakes-and-ladders.core
  (:require [quil.core :as quil])
  (:require [snakes-and-ladders.quil :as drawing])
  (:require [snakes-and-ladders.boards :as boards])
  (:use snakes-and-ladders.animation)
  (:use clojure.repl)
  (:gen-class)
  (:use clojure.test))

;; State

;; :positions - a vector with the position for each player in order
;; :animations - animations for rendering each player (see the drawing code)
;; :turn-counter - number of turns since the game started
;; :player-turn - the current player index (technically redundant, but handy)
;; :current-roll - the die value generated
(defn new-game
  ([] (new-game 2))
  ([number-of-players]
     (let [positions (vec (repeat number-of-players 1))
           state  {:positions positions
                   :move-times positions
                   :animations (vec (repeat number-of-players (fn [args] args)))
                   :turn-counter 0
                   :player-turn 0
                   :current-roll 6}]
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

(defn generate-animation-from-positions [start-position positions]
  ;; We using count in the speed factor gives us a uniform
  ;; animation duration, and the 1.5 makes that duration 2/3rds
  ;; of a second
  (let [speed-factor (* (count positions) 1.5)]
    (apply comp-l-to-r (concat
                        [(start-now)
                         (start-pos
                          (drawing/position-to-screen-x-y start-position))]
                        [(apply (partial with-speed speed-factor)
                                (map move-to
                                     (map drawing/position-to-screen-x-y
                                          positions)))]))))

(defn next-state [state]
  (let[positions (state :positions)
       current-player-id (state :player-turn)
       turn-counter (state :turn-counter)
       move-times (state :move-times)
       animations (state :animations)
       current-player-position (positions current-player-id)
       current-roll (roll-die) ; This generates the random number
       finish (board :finish)
       time (System/currentTimeMillis)
       old-screen-x-y (drawing/position-to-screen-x-y current-player-position)
       rolled-position (min finish (+ current-player-position current-roll))
       new-position (shifted-position rolled-position)
       is-shifted (not (= rolled-position new-position))
       intermediate-positions (concat (range
                                       (inc current-player-position)
                                       rolled-position)
                                      ;; Always include the rolled position.
                                      [rolled-position]
                                      (if is-shifted [new-position] []))
       new-animation (generate-animation-from-positions current-player-position intermediate-positions)

       new-positions (assoc positions current-player-id new-position)
       new-move-times (assoc move-times current-player-id time)
       new-animations (assoc animations current-player-id new-animation)
       ]
    (if (= new-position finish)
      (do (println "Winner player" (inc current-player-id) "!!!")
          (assoc state :positions new-positions
                 :move-times new-move-times
                 :animations new-animations))
      {:positions new-positions
       :player-turn (rem (inc current-player-id) (count positions))
       :turn-counter (inc turn-counter)
       :move-times new-move-times
       :animations new-animations
       :current-roll current-roll})))

(defn take-turn []
  (swap! state-atom next-state))

(defn draw []
  (drawing/draw @state-atom))

;; Alternating control means that you have to click the mouse and then press
;; a key.
;; Handy for playing with little ones.
(def alternate-turns
  ;true
  nil
  )

(defn key-turn []
  (when (= 0 (:player-turn @state-atom)) (take-turn)))

(defn mouse-turn []
  (when-not (= 0 (:player-turn @state-atom)) (take-turn)))

(quil/defsketch snakes-and-ladders-sketch
  :title "Snakes and Ladders"
  :setup drawing/setup
  :draw  draw
  :size (:size boards/board)
  :mouse-pressed (if alternate-turns mouse-turn take-turn)
  :key-pressed (if alternate-turns key-turn take-turn))

(defn -main [& args]
  (println "args - " args))

;; Control expressions
(reset-game 2)
;;(System/exit 0)


;; (take-turn)
