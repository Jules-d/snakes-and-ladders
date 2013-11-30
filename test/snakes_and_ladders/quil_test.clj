(ns snakes-and-ladders.core-test
  (:require [clojure.test :refer :all]
            [snakes-and-ladders.quil :refer :all]))

(is (= (progress [100 200] 100) 0 ))
(is (= (progress [100 200] 150) 1/2 ))
(is (= (progress [100 200] 200) 1 ))
(is (= (progress [100 200] 300) 1 ))
