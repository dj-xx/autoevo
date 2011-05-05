(ns pushcollider
  (:require [clojush :exclude (mutate crossover report)] [clojure.contrib.math] [clojure.contrib.string :as string])
  (:use [clojush :exclude (mutate crossover report)] [clojure.contrib.math]))

(def succeeded (atom false)) ;; global flag to indicate success

;(def maintain-ancestors true)
(def stagthreshold 12) ;; Number of times each individual can return unchanged by any operator
(def max-points 50) ;;max points any program can have
(def loopy 3)

(defrecord ind [genome error totalerror ancestor history]) ;; data structure for individuals


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


;List of mutation operators. Add any new mutation operators to this list
(def mutation-operators (list
                          'mutate
                          'crossover
                          'autopmutate
                          'autocrossover
                          'autocrossover1
                          'autocrossover2))

(defn new-individual
  "Returns a new, evaluated individual. "
  [& {:keys [genome error totalerror ancestor history]
      :or {genome (random-code max-points pushcollider-atom-generators)
           error 0
           totalerror 0
           ancestor ()
           history 0}}]
  (let [err (evaluate genome)
        totalerr (+ totalerror err)]
  (ind. genome err totalerr ancestor history)))


  
(defn autoconstruct
  "Autoconstruct by pushing item on stack and then running item on a copy
itself already on the stack"
  ([pgm] autoconstruct pgm make-push-state)
  ([pgm state]
    (top-item :code 
    (run-push pgm 
      (push-item 0 :auxiliary 
        (push-item pgm :code
          (state)))))))

(defn stagnant
  "Checks history of individual to make sure individual isnt just being recycled"
  [i]
  (if (<= (:history i) stagthreshold) 
            (new-individual 
              :genome (get i :genome()) 
              :error (get i :error()) 
              :totalerror (get i :totalerror()) 
              :ancestor (get i :ancestor())
              :history (inc (get i :history())))
            (new-individual
              :genome (random-code max-points pushcollider-atom-generators))))

(defn make-ancestor
  "Create ancestors for supplied individuals"
  ([x & more]
    (if maintain-ancestors
      (not-lazy (conj x more))
      ())))

(defn call [^String nm & args]
  "Allows you to call a function with a string."
    (when-let [fun (ns-resolve *ns* (symbol nm))]
        (apply fun args)))

(defn stringtokenizer
  "Breaks strings into small pieces"
  [string end]
  (let [abortion (string/split #" " (str string))
        yeah (map (fn [x] (symbol x)) (rest abortion))]
    (if (= end 1)
      (str (first abortion))
      (not-lazy (vector yeah)))))

(defn babymaker
"Making babies"
[state [i1 i2]]
(let [x (atom 0)]
(push-item (top-item :integer (run-push (:genome i1) (push-item 0 :auxiliary (push-item (:genome i1) :code state)))) :hay state)
(push-item (top-item :integer (run-push (:genome i2) (push-item 0 :auxiliary (push-item (:genome i2) :code state)))) :hay state)


(while (< @x (* (count mutation-operators) 2))
                        (push-item (top-item :integer 
                          (run-push (:genome 
                                      (call 
                                        (symbol (str (nth mutation-operators @x)))
                                        (if (= (count (first (:arglists (meta (resolve (symbol (str (nth mutation-operators @x)))))))) 1)
                                          (if (= (mod @x 2) 0)
                                            i1
                                            i2)
                                          (if (= (mod @x 2) 0)
                                            (i1 i2)
                                            (i2 i1))))
                            (push-item @x :auxiliary
                              (push-item (:genome  
                                           (call 
                                             (symbol (str (nth mutation-operators @x)))
                                             (if (= (count (first (:arglists (meta (resolve (symbol (str (nth mutation-operators @x)))))))) 1)
                                               (if (= (mod @x 2) 0)
                                                 i1
                                                 i2)
                                               (if (= (mod @x 2) 0)
                                                 (i1 i2)
                                                 (i2 i1))))) 
                                (:code (state))))))) :hay state)
(if (= (top-item :hay state) :no-stack-item)
  ((pop-item :hay state) (push-item @x :hay state)))
(swap! x inc))
(if (= (mod (count (:hay state)) 2) 1)
  (push-item @x :hay state))
  
(concat (:hay state))))

(defn mutate
  "Returns an evaluated individual resulting from the autoconstructive mutation of i."
  [i]
  (let [old-genome (:genome i)
        randomx (random-code max-points pushcollider-atom-generators)
        descendant (autoconstruct (:genome i))
        sibling (autoconstruct (:genome i))
        child-error (evaluate descendant)
        ancestor (make-ancestor (get i :ancestor()) old-genome)
        failed (or (= descendant :no-stack-item)
                 (= descendant sibling)
                 (= descendant old-genome)
                 (>= child-error (:error i))
                 (>= child-error 20000000)
                 (> (count-points descendant) max-points)
                 (= (discrepancy old-genome descendant) 0)
                 (some #{descendant} ancestor)
                 )]
    (if failed
      (new-individual :genome randomx)  
      (new-individual
        :genome descendant
        :error child-error
        :ancestor ancestor))))
       

(defn crossover
  "Returns an evaluated individual resulting from the recombination 
of i1 and i2."
  [i1 i2]
  (let [new-genome (insert-code-at-point (:genome i1) 
                     (select-node-index (:genome i1))
                     (code-at-point (:genome i2)
                       (select-node-index (:genome i2))))
        ancestor (make-ancestor (:ancestor i2) (:ancestor i1) (:genome i2) (:genome i1))]
    (if (and (> (count-points new-genome) max-points)
          (some #{new-genome} (not-lazy ancestor)))
      (stagnant i1)
      (new-individual
        :genome new-genome 
        :ancestor ancestor))))

(defn autocrossover
  "Returns an evaluated individual resulting from the autoconstruction of
one subtree and the subsequent translocation of that subtree to replace
another subtree in the same tree."
  [i]
  (let [old-genome (:genome i)
        place1 (select-node-index old-genome)
        place2 (select-node-index old-genome)
        stree (code-at-point old-genome place1)
        descendant (autoconstruct stree)
        ancestor (make-ancestor (get i :ancestor ()) old-genome)
        new-genome (insert-code-at-point old-genome place2 descendant)
        failed (or (= descendant :no-stack-item)
                 (= place1 place2)
                 (= descendant stree)
                 (= descendant (code-at-point old-genome place2))
                 (>= (evaluate descendant) (evaluate stree))
                 (>= (evaluate descendant) (evaluate (code-at-point old-genome place2)))
                 (= (discrepancy descendant stree) 0)
                 (some #{new-genome} ancestor)
                 (> (count-points new-genome) max-points)
                 )] 
    (if failed
      (stagnant i)
      (new-individual
        :genome new-genome
        :ancestor ancestor))))

(defn autocrossover1
  "Same as the mutation operator autocrossover except both subtrees are autoconstructed
and both subtrees are replaced"
  [i]
  (let [old-genome (:genome i)
        place1 (select-node-index old-genome)
        place2 (select-node-index old-genome)
        stree (code-at-point old-genome place1)
        stree2 (code-at-point old-genome place2)
        ancestor (make-ancestor (get i :ancestor ()) old-genome)
        descendant (autoconstruct stree)
        child2 (autoconstruct stree2)
        new-genome (insert-code-at-point (insert-code-at-point old-genome place2 descendant) place1 child2)
        failed (or (= descendant :no-stack-item)
                 (= place1 place2)
                 (= descendant stree)
                 (= child2 stree2)
                 (= descendant stree2)
                 (= child2 stree)
                 (>= (evaluate descendant) (evaluate stree))
                 (>= (evaluate child2) (evaluate stree2))
                 (>= (evaluate descendant) (evaluate (code-at-point old-genome place2)))
                 (>= (evaluate child2) (evaluate (code-at-point old-genome place1)))
                 (= (discrepancy descendant stree) 0)
                 (= (discrepancy child2 stree2) 0)
                 (some #{new-genome} ancestor)
                 (> (count-points new-genome) max-points)
                 )] 
    (if failed
      (stagnant i)
      (new-individual
        :genome new-genome
        :ancestor ancestor))))

(defn autocrossover2
  "Same as the mutation operator autocrossover except subtrees are on
two different trees"
  [i1 i2]
  (let [old-genome (:genome i1)
        old-genome2 (:genome i2)
        place1 (select-node-index old-genome)
        place2 (select-node-index old-genome2)
        stree (code-at-point old-genome place1)
        descendant (autoconstruct stree)
        new-genome (insert-code-at-point old-genome2 place2 descendant)
        ancestor (make-ancestor (:ancestor i2) (:ancestor i1) (:genome i2) (:genome i1))
        failed (or (= descendant :no-stack-item)
                 (= place1 place2)
                 (= descendant stree)
                 (= descendant (code-at-point old-genome2 place2))
                 (>= (evaluate descendant) (evaluate stree))
                 (>= (evaluate descendant) (evaluate (code-at-point old-genome2 place2)))
                 (= (discrepancy descendant stree) 0)
                 (some #{new-genome} ancestor)
                 (> (count-points new-genome) max-points)
                 )] 
    (if failed
      (stagnant i1)
      (new-individual
        :genome new-genome
        :ancestor ancestor))))



(defn autopmutate
  "Returns an evaluated individual resulting from the autoconstructive point mutation of i."
  [i]
  (let [old-genome (:genome i)
        place (select-node-index old-genome)
        stree (code-at-point old-genome place)
        ancestor (make-ancestor (get i :ancestor ()) old-genome)
        descendant (autoconstruct stree)
        new-genome (insert-code-at-point old-genome place descendant)
        failed (or (= descendant :no-stack-item)
                 (= descendant stree)
                 (>= (evaluate descendant) (evaluate stree))
                 (= (discrepancy descendant stree) 0)
                 (some #{new-genome} ancestor)
                 (> (count-points new-genome) max-points)
                 )]          
    (if failed
      (stagnant i)
      (new-individual
        :genome new-genome
        :ancestor ancestor))))


(defn constructive-collision
  "Takes a pair of individuals and returns a vector of individuals."
  [[i1 i2]]
  (let [state (make-push-state)
        probability (babymaker state [[i1 i2]])
        probs (map (fn [x] x) (:hay state))
        x (atom 0)
        y (atom 0)]
(assoc state :hay '())
(push-item (top-item :code (run-push (:genome i1) (push-item 0 :auxiliary (push-item (:genome i1) :code (state))))) :hay state)
(push-item (top-item :code (run-push (:genome i2) (push-item 0 :auxiliary (push-item (:genome i2) :code (state))))) :hay state)


(while (< @x (* (count mutation-operators) 2)) 
(while (<= @y (nth probs @x))
                        (push-item (top-item :code
                          (run-push (:genome 
                                      (call 
                                        (symbol (str (nth mutation-operators @x)))
                                        (if (= (count (first (:arglists (meta (resolve (symbol (str (nth mutation-operators @x)))))))) 1)
                                          (if (= (mod @y 2) 0)
                                            i1
                                            i2)
                                          (if (= (mod @y 2) 0)
                                            (i1 i2)
                                            (i2 i1))))
                            (push-item @y :auxiliary
                              (push-item (:genome  
                                           (call 
                                             (symbol (str (nth mutation-operators @x)))
                                             (if (= (count (first (:arglists (meta (resolve (symbol (str (nth mutation-operators @x)))))))) 1)
                                               (if (= (mod @y 2) 0)
                                                 i1
                                                 i2)
                                               (if (= (mod @y 2) 0)
                                                 (i1 i2)
                                                 (i2 i1))))) 
                                (:code (state))))))) :hay state)
(swap! y inc))
(swap! x inc))
 
(vec (:hay state))))
        
 
(comment
    (for [x (range (count mutation-operators))]
      (push-item (call 
                   (stringtokenizer (nth mutation-operators x) 1) 
                   (stringtokenizer (nth mutation-operators x) 2)) :auxiliary state))
    (if (not= (mod (reduce + (remove #{:no-stack-item} babymaker)) 2))
          (push-item i1 :auxiliary state))
    (concat (take (reduce + (remove #{:no-stack-item} babymaker)) (:auxiliary state))))
    

(defn constructive-collisions
  "Splits the population into pairs, collides the pairs constructively
(on multiple threads) and returns a vector of all of the resulting
individuals."
  [population]
  (let [pairs (partition 2 2 population population)]
    (apply concat (pmap constructive-collision pairs))))

(defn destructive-collision
  "Takes a pair of individuals and returns a single individual."
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
  (let [best (reduce (fn [i1 i2] (if (< (:error i1) (:error i2)) i1 i2))
                 population)]
  (println "Generation: " generation 
    ", lowest error: " (apply min (map :error population))
    ", Best Program: " (:genome best)))
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
      
  