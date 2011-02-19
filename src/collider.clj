;; collider.clj
;; Lee Spector (lspector@hampshire.edu), 20110217
;;
;; A simple evolutionary computing system based on the idea of constructive
;; and destructive collisions. We start with a population of size 
;; base-pop-size and then iteratively conduct rounds of "constructive 
;; collisions" (in which pairs interact to produce new, evaluated
;; individuals) and "destructive collisions" (in which pairs interact 
;; and the weaker of each pair is eliminiated, until the population is 
;; reduced to the base size or smaller).
;;
;; The code here uses a binary genome (a sequence of 1s and 0s) and 
;; it attempts to minimize the number of 1s (which is very easy!). Any  
;; serious application will probably use a different genome representation
;; and/or different genetic operators and/or a different error function.
;; The functions to consider altering include new-individual, mutate,
;; crossover, and evaluate.
;;
;; The top-level function is evolve, and it should be called with two
;; arguments: the base population size and the maximum number of 
;; generations to run, for example (evolve 20 20).
;;
;; Note that constructive collisions (which include error testing of the
;; new individuals) are dispatched with pmap, so this is multithreaded.
;;
;; Revision History
;; 20110217: - fixed ns, digest -> report, max-pop -> base-pop-size
;;           - pmap -> map in destructive collisions
;;           - improved documentation

(ns collider)

(def succeeded (atom false)) ;; global flag to indicate success

(defrecord individual [genome error]) ;; data structure for individuals

(defn report-success
  "Sets the global success flag and also prints a success message."
  [e g]
  (reset! succeeded true)
  (println "Success! Error: " e ", Genome: " g))

(defn evaluate
  "Silly example evaluator for a binary genome: error = #1s."
  [g]
  (let [e (apply + g)]
    (when (zero? e) (report-success e g))
    e))

(defn new-individual
  "Returns a new, evaluated individual. In this simple example a
a genome is a sequence of 20 random zeros or ones."
  []
  (let [genome (repeatedly 20 #(rand-int 2))]
    (individual. genome (evaluate genome))))

(defn mutate
  "Returns an evaluated individual resulting from the mutation of i."
  [i]
  (let [old-genome (:genome i)
        flip-loc (rand-int (count old-genome))
        new-genome (concat (take flip-loc old-genome)
                     (vector (- 1 (nth old-genome flip-loc)))
                     (drop (inc flip-loc) old-genome))]
    (individual. new-genome (evaluate new-genome))))

(defn crossover
  "Returns an evaluated individual resulting from the recombination 
of i1 and i2."
  [i1 i2]
  (let [cross-loc (rand-int (count (:genome i1)))
        new-genome (concat (take cross-loc (:genome i1))
                     (drop cross-loc (:genome i2)))]
    (individual. new-genome (evaluate new-genome))))

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
      
  