(ns snakes-and-ladders.animation
  (:use clojure.test))

;; An "animation" here is just a function that modifies an x,y position with
;; respect to time.

;; This file describes set of higher-order functions to create and manipulate
;; functions that modify x,y coordinates with respect to time (animations).

;; The functions all expect to be working with functions with both input and
;; output of the following form:
;; [time [x y]]

;; Time is assumed to be in milliseconds, and x and y in pixels, but it
;; doesn't actually matter.

;; By convention, the generated functions last 1000 units, and decrement the time value by this amount.  If the time is less than 0, they typically make no change to the position, and if the time is greater than 1000 they apply the full change, and if the time is between 0 and 1000, they perform a linear interpolation between their input values.

;; The "move" animation is a simple example of this convention.

;;
(defn comp-l-to-r
  "Compose left-to-right, so animations read in the same order they are applied"
  ([f] f)
  ([f g]
     (fn
       ([] (g (f)))
       ([x] (g (f x)))
       ([x y] (g (f x y)))
       ([x y z] (g (f x y z)))
       ([x y z & args] (g (apply f x y z args)))))
  ([f1 f2 & fs]
     (let [fs (list* f1 f2 fs)]
       (fn [& args]
         (loop [ret (apply (first fs) args) fs (next fs)]
           (if fs
             (recur ((first fs) ret) (next fs))
             ret))))))

(let [square #(* % %)]
  (is (= 2 ((comp-l-to-r square square inc) 1)))
  (is (= 2 ((comp-l-to-r square square square square inc) 1)))
  (is (= (square (square (square (square (inc 1)))))
         ((comp-l-to-r inc square square square square) 1))))


(def null-state [0 [0 0]])

(defn start-time [time*]
  (fn [[time [x y]]]
    [(- time time*) [x y]]))

;; Set an animation to start "now"
;; This should work mid-stream too, as a sort of variable-length wait.
(defn start-now []
  (start-time (System/currentTimeMillis)))

(is (= [0 [0 0]] ((start-time 100) [100 [0 0]])))
(is (= [100 [0 0]] ((start-time 100) [200 [0 0]])))

(defn start-pos [[x* y*]]
  "If time is not negative, sets the pos to the arguments"
  (fn [[time [x y]]]
    [time (if (>= time 0)
            [x* y*]
            [x y])]))

(let [test-start-pos (start-pos [102 -3])]
  (is (= (test-start-pos [0 [0 0]])
         [0 [102 -3]]))
  (is (= (test-start-pos [1 [0 0]])
         [1 [102 -3]]))
  (is (= (test-start-pos [-1 [0 0]])
         [-1 [0 0]])))

(defn interpolate [progress [a b]]
  (let [ diff (- b a)]
       (+ a (* progress diff))))

(defn set-position [pos]
  (fn [time _]
    [time pos]))

(defn lerpk [time args-array]
  "performs a linear interpolation of each element in the second argument, using the first argument as a progress value out of 1000"
  (let [progress (/ time 1000)]
    (map (partial * progress) args-array)))

(is (= (lerpk 0 [1 2 3 1000 4000])
       [0 0 0 0 0]))
(is (= (lerpk 500 [2 4 8 1000])
       [1 2 4 500]))
(is (= (lerpk 1000 [12])
       [12]))

;;
(defn move [[dx dy]]
  "Move by the given amount (relative to the input position"
  (fn [[time [x y]]]
    (cond (>= time 1000) [(- time 1000) [(+ x dx) (+ y dy)]]
          (<= time 0) [(- time 1000) [x y]]
          :else (let [movement (lerpk time [dx dy])
                      new-position (map + [x y] movement)]
                  [(- time 1000) new-position]))))

(is (= ((move [100 100]) [1000 [0 0]])
       [0 [100 100]]))
(is (= ((move [100 100]) [0 [0 0]])
       [-1000 [0 0]]))
(is (= ((move [100 100])
        [500 [50 50]])))

(let [start (start-time 2500)
      move1 (move [50 0])
      move2 (move [100 100])
      test-animation (apply comp (reverse [start move1 move2]))]
  (is (= (test-animation [2000 [0 0]])
         [-2500 [0 0]]))
  (is (= (test-animation [2500 [0 0]])
         [-2000 [0 0]]))
  (is (= (test-animation [3500 [0 0]])
         [-1000 [50 0]]))
  (is (= (test-animation [4500 [0 0]])
           [0 [150 100]])))

(defn move-to [[to-x to-y]]
  "Move to a fixed position"
  (fn [[time [x y]]]
    (let [new-time (- time 1000)]
      (cond (>= time 1000) [new-time [to-x to-y]]
            (<= time 0) [new-time [x y]]
            :true (let [dx (- to-x x)
                        dy (- to-y y)]
                    [new-time (map + [x y] (lerpk time [dx dy]))])))))

(let [test-move-to (move-to [100 100])]
  (is (= (test-move-to [0 [0 0]])
         [-1000 [0 0]]))
  (is (= (test-move-to [-10 [0 0]]) [-1010 [0 0]]))
  (is (= (test-move-to [500 [0 0]]) [-500 [50 50]]))
  (is (= (test-move-to [500 [50 50]])
         [-500 [75N 75N]])))

;; You can manipulate time too.  If you want to speed up or slow down later
;; animations, you can multiply the time-stream by n before you compose the
;; other animations.
;; You need to slow it down again afterwards if you want later animations to
;; run at "normal" speed, or you can use the with-speed convenience function
;; to do that for you.
(defn double-speed []
  (fn [[time pos]]
    [(* time 2) pos]))

(defn alter-speed [factor]
  (fn [[time pos]]
    [(* factor time) pos]))

(defn with-speed [factor & functions]
  "Compose one or more functions with speed modified to factor.  Restore previous speed afterwards."
  {:pre  [(not (= factor 0))]}
  (let [result-function (apply comp-l-to-r
                                (concat [(alter-speed factor)]
                                        functions
                                        [(alter-speed (/ 1 factor))]))]
    (fn [& args]
      (apply result-function args))))

(let [test-anim (with-speed 4
                    (move [0 50])
                    (move [0 50])
                    (move [50 0])
                    (move [50 0]))]
  (is (= (test-anim [0 [0 0]])
         [-1000 [0 0]]))
  (is (= (test-anim [250 [0 0]])
         [-750 [0 50]]))
  (is (= (test-anim [1000 [0 0]])
         [0 [100 100]])))

(let [test-anim (comp-l-to-r (move [50 0])
                             (with-speed 4
                               (move [0 50])
                               (move [0 50])
                               (move [50 0])
                               (move [50 0]))
                             (move [0 50]))]
  (is (= (test-anim [0 [0 0]])
         [-3000 [0 0]]))
  (is (= (test-anim [500 [0 0]])
         [-2500 [25 0]]))
  (is (= (test-anim [1000 [0 0]])
         [-2000 [50 0]]))
  (is (= (test-anim [1250 [0 0]])
         [-1750 [50 50]]))
  (is (= (test-anim [1500 [0 0]])
         [-1500 [50 100]]))
  (is (= (test-anim [1625 [0 0]])
         [-1375 [75 100]]))
  (is (= (test-anim [3000 [0 0]])
         [0 [150 150]])))

(is (= ((comp-l-to-r (double-speed) (move [50 50])) [500 [0 0]])
       [0 [50 50]]))

(is (= ((comp-l-to-r (alter-speed 5) (move [50 50])) [100 [0 0]])
       [-500 [25 25]]))
(is (= ((comp-l-to-r (alter-speed 10) (move [50 50])) [100 [0 0]])
       [0 [50 50]]))

;; Check that we can apply and reverse speed changes.
;; This test does a y animation at 10 times speed and then restores
;; normal speed to get half way through another animation.
(is (= ((comp-l-to-r (alter-speed 10)
                     (move [50 0])
                     (alter-speed (/ 1 10))
                     (move [100 100])) [600 [0 0]])
       [-500 [100 50]]))
