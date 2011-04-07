(ns pushcollider
  (:require [clojush :exclude (mutate crossover report)] [clojure.contrib.math])
  (:use [clojush :exclude (mutate crossover report)] [clojure.contrib.math]))

(def succeeded (atom false)) ;; global flag to indicate success

(defrecord individual [genome error totalerror ancestor]) ;; data structure for individuals


(defn report-success
  "Sets the global success flag and also prints a success message."
  [e g]
  (reset! succeeded true)
  (println "Success! Error: " e ", Genome: " g))

(defn evaluate
  "Silly example evaluator for a binary genome: error = #1s."
  [g]
  (let [e (apply + (for [input (range 10)]
                     (let [state (run-push g 
                                   (push-item input :auxiliary 
                                     (push-item input :integer 
                                       (make-push-state))))
                           top-int (top-item :integer state)]
                       (if (number? top-int)
                         (abs (- top-int 
                                (- (* input input input) 
                                  (* 2 input input) input)))
                         1000))))]
    (when (zero? e) (report-success e g))
    e))

(define-registered in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(def pushcollider-atom-generators (list (fn [] (rand-int 10))
                                    'in
                                    'integer_div
                                    'integer_mult
                                    'integer_add
                                    'integer_sub))


(defn new-individual
  "Returns a new, evaluated individual. "
  [& {:keys [genome error totalerror ancestor]
      :or {genome (random-code 50 pushcollider-atom-generators)
           error 0
           totalerror 0
           ancestor ()}}]
  (individual. genome (evaluate genome) totalerror ancestor))


(defn pmutate
  "Returns an evaluated individual resulting from the point mutation of i."
  [i]
  (let [old-genome (:genome i)
        new-genome (insert-code-at-point old-genome 
                     (select-node-index old-genome)
                     (random-code 20 pushcollider-atom-generators))]
    (if (> (count-points new-genome) 50)
      i
      (individual. new-genome (evaluate new-genome) 0 0))))



  
(defn autoconstruct
  [pgm]
  (top-item :code 
    (run-push pgm 
      (push-item 0 :auxiliary 
        (push-item pgm :code
          (make-push-state))))))



(defn mutate
  "Returns an evaluated individual resulting from the autoconstructive mutation of i."
  [i]
  (let [old-genome (:genome i)
        randomx (random-code 50 pushcollider-atom-generators)
        descendant (autoconstruct (:genome i))
        sibling (autoconstruct (:genome i))
        child-error (evaluate descendant)
        failed (or (= descendant :no-stack-item)
                 (= descendant sibling)
                 (= descendant old-genome)
                 (>= child-error 20000000)
                 (> (count-points descendant) 50)
                 )]
    (if failed
      (new-individual :genome randomx :error (evaluate randomx) :totalerror 0 :ancestor ())  
      (new-individual
        :genome descendant
        :error child-error
        :totalerror (+ child-error (get i :totalerror ()))
        :ancestor (cons old-genome (get i :ancestor ()))))))
       

(defn crossover
  "Returns an evaluated individual resulting from the recombination 
of i1 and i2."
  [i1 i2]
  (let [new-genome (insert-code-at-point (:genome i1) 
                     (select-node-index (:genome i1))
                     (code-at-point (:genome i2)
                       (select-node-index (:genome i2))))]
    (if (> (count-points new-genome) 50)
      i1
      (new-individual
        :genome new-genome 
        :error (evaluate new-genome)
        :totalerror 0 ;(evaluate new-genome)
        :ancestor 0))));;(cons (:genome i1) (:genome i2))))))

(defn constructive-collision 
  "Takes a pair of individuals and returns a vector of individuals."
  [[i1 i2]]
  (vector i1 i2 (mutate i1) (mutate i2) (crossover i1 i2) (crossover i2 i1)))

(defn constructive-collisions
  "Splits the population into pairs, collides the pairs constructively
(on multiple threads) and returns a vector of all of the resulting
individuals."
  [population]
  (let [pairs (partition 2 2 population population)]
    (apply concat (pmap constructive-collision pairs))))

(defn destructive-collision
  "Takes a pair of individuals and single individual."
  [[i1 i2]]
  (if (< (:error i1) (:error i2)) i1 i2))

(defn destructive-collisions
  "Splits the population into pairs, collides the pairs destructively
and returns a vector of all of the resulting individuals."
  [population base-pop-size]
  (if (< (count population) base-pop-size)
    population
    (recur 
      (map destructive-collision 
        (partition 2 2 population population))
      base-pop-size)))

(defn report
  "Report on the given population at the given generation and return
the population."
  [generation population]
  (println "Generation: " generation 
    ", lowest error: " (apply min (map :error population)))
  population)

(defn evolve
  "Top-level call to the collider evolutionary computation system."
  [base-pop-size max-gen]
  (reset! succeeded false) ;; set global success flag to false
  (loop [generation 0 
         population (repeatedly base-pop-size new-individual)]
    (when (not (or @succeeded (>= generation max-gen)))
      (let [next-gen (report generation
                       (destructive-collisions 
                         (shuffle
                           (constructive-collisions population))
                         base-pop-size))]
        (recur (inc generation) next-gen)))))
      
  