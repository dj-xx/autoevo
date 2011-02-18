;; collider.clj
;; Lee Spector (lspector@hampshire.edu), 20110217
;;
;; A simple evolutionary algorithm system based on the idea of constructive
;; and destructive collisions. 
;;
;; The code here uses a simple binary genome (a sequence of 1s and 0s) and it
;; attempts to minimize the number of 1s (which is very easy!). Presumably
;; any serious application will use a different genome representation, different
;; genetic operators, and a different fitness ("evaluate") function. The functions
;; to alter are (at least) evaluate, new-individual, mutate, and crossover.
;;
;; Note that collisions are dispatched with pmap, so this is multithreaded.

(ns core)

(def succeeded (atom false))

(defrecord individual [genome error])

(defn report-success
  [e g]
  (reset! succeeded true)
  (println "Success! Error: " e ", Genome: " g))

(defn evaluate
  "Silly example evaluator for a binary genome, error = #1s."
  [g]
  (let [e (apply + g)]
    (when (zero? e) (report-success e g))
    e))

(defn new-individual
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
  "Returns an evaluated individual resulting from the recombination of i1 and i2."
  [i1 i2]
  (let [cross-loc (rand-int (count (:genome i1)))
        new-genome (concat (take cross-loc (:genome i1))
                     (drop cross-loc (:genome i2)))]
    (individual. new-genome (evaluate new-genome))))

(defn constructive-collision 
  "Returns a vector of evaluated individuals."
  [[i1 i2]]
  (vector i1 i2 (mutate i1) (mutate i2) (crossover i1 i2) (crossover i2 i1)))

(defn constructive-collisions
  [population]
  (let [pairs (partition 2 2 population population)]
    (apply concat (pmap constructive-collision pairs))))

(defn destructive-collision
  "Returns a single individual."
  [[i1 i2]]
  (if (< (:error i1) (:error i2))
    i1
    i2))

(defn destructive-collisions
  [population max-pop]
  (if (< (count population) max-pop)
    population
    (recur 
      (pmap destructive-collision 
        (partition 2 2 population population))
      max-pop)))

(defn digest
  [generation population]
  (println "Generation: " generation ", lowest error: " (apply min (map :error population)))
  population)

(defn evolve
  [max-pop max-gen]
  (reset! succeeded false)
  (loop [generation 0 
         population (repeatedly max-pop new-individual)]
    (when (not (or @succeeded (>= generation max-gen)))
      (let [next-gen (digest generation
                       (destructive-collisions 
                         (shuffle
                           (constructive-collisions population))
                         max-pop))]
        (recur (inc generation) next-gen)))))