;--------------------------------------------------------------------------
; Modification of pushcollider.clj by Lee Spector (lspector@hampshire.edu)
; Hampshire College AutoEvolution Group Spring 2011
;--------------------------------------------------------------------------
;
; To do and change log are located at the bottom.
;
;----------------------
; namespace and import
;----------------------
(ns moddedpushcollider
  (:require [clojush :exclude (mutate crossover report)] [clojure.contrib.math])
  (:use [clojush :exclude (mutate crossover report)] [clojure.contrib.math]))

;----------------
; core functions
;----------------
(def succeeded (atom false)) 
  ;global success flag

(defrecord individual [genome error totalerror ancestor]) 
  ;data structure for individuals

(defn report-success [e g]
  ;Sets the global success flag and also prints a success message.
  (reset! succeeded true)
  (println "Successful| Error:"e". Genome:" g"|"))

(defn evaluate [g]
  ;An example evaluator for a binary genome: error = #1s.
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

(def pushcollider-atom-tors (list (fn [] (rand-int 10))
                                    'in
                                    'integer_div
                                    'integer_mult
                                    'integer_add
                                    'integer_sub))

(defn new-individual
  ;Returns a new, evaluated individual. In this example a genome is a sequence of 50 random zeros or ones.
  [& {:keys [genome error totalerror ancestor]
                          :or {genome (random-code 50 pushcollider-atom-tors)
                               error nil
                               totalerror nil
                               ancestor nil}}]
  (individual. genome error totalerror ancestor))

;------------------------
; evolutionary functions
;------------------------
 (defn autoconstruct [pgm]
  (top-item :code (run-push pgm (push-item 0 :auxiliary (make-push-state)))))

(defn mutate [i]
  ;Returns an evaluated individual resulting from the autoconstructive mutation of i.
  (let [old-genome (:genome i)
        random (random-code 50 pushcollider-atom-tors)
        descendant (autoconstruct (:genome i))
        sibling (autoconstruct (:genome i))
        child-error (evaluate descendant)
        failed (or 
                 (= descendant :no-stack-item)
                 (= descendant sibling)
                 (>= child-error 2000)
                 (> (count-points descendant) 50))
        old-mutate (insert-code-at-point old-genome 
                     (select-node-index old-genome)
                     (random-code 20 pushcollider-atom-tors))]  
      (new-individual
        :genome (if failed
                  random
                  descendant)
        :error (if failed
                (evaluate random)
                child-error)
        :totalerror (if failed
                       nil
                       (+ child-error (:totalerror i)))
        :ancestor (if failed
                       ()
                       (cons old-genome (get i :ancestor ()))))))

;-------------------------------------------------------------------
; original mutate code
(comment
(defn mutate [i]
 ;Returns an evaluated individual resulting from the mutation of i.
  (let [old-genome (:genome i)
        new-genome (insert-code-at-point old-genome 
                     (select-node-index old-genome)
                     (random-code 20 pushcollider-atom-tors))]
    (if (> (count-points new-genome) 50)
      i
      (individual. new-genome (evaluate new-genome) nil nil))))
)
;-------------------------------------------------------------------

(defn crossover [i1 i2]
  ;Returns an evaluated individual resulting from the recombination of i1 and i2.
  (let [new-genome (insert-code-at-point (:genome i1) 
                     (select-node-index (:genome i1))
                     (code-at-point (:genome i2)
                       (select-node-index (:genome i2))))]
    (if (> (count-points new-genome) 50)
      i1
      (new-individual
        :genome new-genome 
        :error (evaluate new-genome)
        :totalerror (evaluate new-genome)
        :ancestor (cons (:genome i1) (:genome i2))))))

(defn col-constr [[i1 i2]]
  ;Takes a pair of individuals and returns a vector of individuals.
  (vector i1 i2 (mutate i1) (mutate i2) (crossover i1 i2) (crossover i2 i1)))

(defn col-constr-multi [population]
  ;Splits the population into pairs, collides the pairs constructively (on multiple threads) and returns a vector of all of the resulting individuals.
  (let [pairs (partition 2 2 population population)]
    (apply concat (pmap col-constr pairs))))

(defn col-destr [[i1 i2]]
  ;Takes a pair of individuals and single individual.
  (if (< (:error i1) (:error i2)) i1 i2))

(defn col-destr-multi [population base-pop-size]
  ;Splits the population into pairs, collides the pairs destructively and returns a vector of all of the resulting individuals.
  (if (< (count population) base-pop-size)
    population
    (recur 
      (map col-destr
        (partition 2 2 population population))
      base-pop-size)))

;----------------
; user interface
;----------------
(defn report [generation population]
  ;Report on the given population at the given generation and return the population.
  (println "Generation:" generation "| Lowest error:" (apply min (map :error population)))
  population)

(defn help []
  (println \newline "Modified pushcollider.clj."\newline\newline"To initialize the program: (evolve [initial/base population size] [maximum generations] [genome sequence size] [evolutionary method])"\newline\tab"Evolutionary methods: 'col-constr', 'col-constr-multi', 'col-destr', 'col-destr-multi'"\newline))

;--------------------------------------------------------------------------------------------------
; beta evolve code
(defn evolve [base-pop-size max-gen] ;(evolve 100 100 50 col-constr)
  ;Top-level call to the collider evolutionary computation system.
  (reset! succeeded false) ;set global success flag to false
  (loop [generation 0 
         population (repeatedly base-pop-size new-individual)]
    (when (not (or @succeeded (>= generation max-gen)))
      (let [next-gen (report generation
                       (col-destr-multi 
                         (shuffle
                           (col-constr population))
                         base-pop-size))]
        (recur (inc generation) next-gen)))))
;--------------------------------------------------------------------------------------------------

;------------------------------------------------------------------
; original evolve code
(comment
(defn evolve [base-pop-size max-gen] ;(evolve 100 100)
  ;Top-level call to the collider evolutionary computation system.
  (reset! succeeded false) ;set global success flag to false
  (loop [generation 0 
         population (repeatedly base-pop-size new-individual)]
    (when (not (or @succeeded (>= generation max-gen)))
      (let [next-gen (report generation
                       (col-destr-multi 
                         (shuffle
                           (col-constr population))
                         base-pop-size))]
        (recur (inc generation) next-gen)))))
)
;------------------------------------------------------------------

;-------------------------------------------------------------------------------------------------------
; To do:
;  -Impliment user interface to allow the user to select which evolutionary process they wish to utilize
;    (evolve [base-pop-size max-gen genome-seq-size evolve-method]) 
;    ~ line 165
;  -More improvements and mutation functions
;-------------------------------------------------------------------------------------------------------
; Recent changelog:
;  -Renamed evolutionary functions
;  -Cleaned up comments
;  -Added basic "help" function (line 155)
;-------------------------------------------------------------------------------------------------------