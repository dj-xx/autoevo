(ns pushcollider
  (:require [clojush :exclude (mutate crossover report)] [clojure.contrib.math] [clojure.contrib.string :as string] [clojure.zip :as zip])
  (:use [clojush :exclude (mutate crossover report)] [clojure.contrib.math]))

(def succeeded (atom false)) ;; global flag to indicate success
(def op1 (atom 0)) ;;Counter for mutate operator
(def op2 (atom 0)) ;;Counter for crossover operator
(def op3 (atom 0)) ;;Counter for autocrossover operator
(def op4 (atom 0)) ;;Counter for autocrossover1 operator
(def op5 (atom 0)) ;;Counter for autocrossover2 operator
(def op6 (atom 0)) ;;Counter for autopmutate operator


;(def maintain-ancestors true)
(def stagthreshold 12) ;; Number of times each individual can return unchanged by any operator
(def max-points 50) ;;max points any program can have
(def loopy 3)

(defrecord ind [genome error totalerror ancestor history oper]) ;; data structure for individuals


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
  [& {:keys [genome error totalerror ancestor history oper]
      :or {genome (random-code max-points pushcollider-atom-generators)
           error 0
           totalerror 0
           ancestor nil
           history 0
           oper ""}}]
  (let [err (evaluate genome)
        totalerr (+ totalerror err)]
  (ind. genome err totalerr ancestor history oper)))


  
(defn after-eval
  "Returns top item on type stack in state after pushing individual unto
code stack and then exec'ing it"
  ([individual stack] (after-eval individual stack (make-push-state)))
  ([individual stack state]
    (top-item stack 
      (run-push individual
        (push-item 0 :auxiliary 
          (push-item individual :code state))))))
  


(defn autoconstruct
  "Autoconstruct by pushing item on stack and then running item on a copy 
itself already on the stack"
  [pgm]
  (after-eval pgm :code))

(defn stagnant
  "Checks history of individual to make sure individual isnt just being recycled"
  [i]
  (if (<= (:history i) stagthreshold) 
            (new-individual 
              :genome (get i :genome()) 
              :error (get i :error()) 
              :totalerror (get i :totalerror()) 
              :ancestor (get i :ancestor())
              :history (inc (get i :history()))
              :oper (get i :oper()))
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

;Calls function recursively till condition is fulfilled
;Used for generating random # of baies from each operator
(def caller
  (fn [y results op & args]
    (if (= y 0)
      results
      #(caller (dec y) (conj results (apply op args)) op args)))) 

(defn arrity
  "Find arrity of operators"
  [s]
  (count (first (:arglists (meta (resolve (symbol s)))))))

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
    ;(swap! op1 inc) ;;Increment each time operator is called
    (if failed
      (new-individual :genome randomx)  
      (new-individual
        :genome descendant
        :error child-error
        :ancestor ancestor
        :oper "mutate"))))
       

(defn crossover
  "Returns an evaluated individual resulting from the recombination 
of i1 and i2."
  [i1 i2]
  (let [new-genome (insert-code-at-point (:genome i1) 
                     (select-node-index (:genome i1))
                     (code-at-point (:genome i2)
                       (select-node-index (:genome i2))))
        ancestor (make-ancestor (:ancestor i2) (:ancestor i1) (:genome i2) (:genome i1))]
    (swap! op2 inc) ;;Increment each time operator is called
    (if (and (> (count-points new-genome) max-points)
          (some #{new-genome} (not-lazy ancestor)))
      (stagnant i1)
      (new-individual
        :genome new-genome 
        :ancestor ancestor
        :oper "crossover"))))

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
    (swap! op3 inc) ;;Increment each time operator is called
    (if failed
      (stagnant i)
      (new-individual
        :genome new-genome
        :ancestor ancestor
        :oper "autocrossover"
        ))))

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
    (swap! op4 inc) ;;Increment each time operator is called
    (if failed
      (stagnant i)
      (new-individual
        :genome new-genome
        :ancestor ancestor
        :oper "autocrossover1"))))

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
    (swap! op5 inc);;Increment each time operator is called
    (if failed
      (stagnant i1)
      (new-individual
        :genome new-genome
        :ancestor ancestor
        :oper "autocrossover2"))))


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
    (swap! op6 inc) ;;Increment each time operator is called
    (if failed
      (stagnant i)
      (new-individual
        :genome new-genome
        :ancestor ancestor
        :oper "autopmutate"))))

;;Tim had an identical mutator
(defn sizeFairMutation
  "Returns an evaluated individual resulting from the replacement of a subtree
with a randomly generated subtree with same number of points."
  [i]
  (let [old-genome (:genome i)
        place (select-node-index old-genome)
        stree (random-code-with-size (count-points place) pushcollider-atom-generators)
        new-genome (insert-code-at-point old-genome place stree)
        ancestor (make-ancestor (get i :ancestor ()) old-genome)]
    (if (> (count-points new-genome) max-points)
      (stagnant i)
      (new-individual
        :genome new-genome
        :ancestor ancestor
        :oper "sizeFairMutation"))))

;;Contributed by Tim
(defn mutateAll2
  "Mutates Acros the board wth p % of every point being replaced by a subtree"
  [i p]
  (let [old-genome (:genome i)
        zipper (zip/seq-zip old-genome)
        new-genome (loop [z zipper j 0]
                     (if (>= j (count-points old-genome))
                       (zip/root z)
                       (recur
                         (if-not (zip/end? z)
                           (if-not (seq? (zip/node z))
                             (if (< (rand-int 100) p)
                               (zip/next (zip/replace z (random-code 1
                                                          pushcollider-atom-generators)))
                               (zip/next z))
                             (zip/next z))
                           z)
                         (inc j))))
        ancestor (make-ancestor (get i :ancestor ()) old-genome)]
    (if (> (count-points new-genome) max-points)
      (stagnant i)
      (new-individual
        :genome new-genome
        :ancestor ancestor
        :oper "mutateAll2"))))


(comment
(defn constructive-collision 
  "Takes a pair of individuals and returns a vector of individuals."
  [[i1 i2]]
  (vec (flatten (vector i1 i2 (caller mutate [i1] (lrand-int 10) []) (caller mutate [i2] (lrand-int 10) [])
    (caller autopmutate [i1] (lrand-int 10) []) (caller autopmutate [i2] (lrand-int 10) [])
    (caller crossover [i1 i2] (lrand-int 10) []) (caller crossover [i2 i1] (lrand-int 10) [])
    (caller autocrossover [i1] (lrand-int 10) []) (caller autocrossover [i2] (lrand-int 10) [])
    (caller autocrossover1 [i1] (lrand-int 10) []) (caller autocrossover1 [i2] (lrand-int 10) [])
    (caller autocrossover2 [i1 i2] (lrand-int 10) []) (caller autocrossover2 [i2 i1] (lrand-int 10) []))))))

(defn constructive-collision
  "Takes a pair of individuals and returns a vector of individuals."
  [[i1 i2]]
  (vector i1 i2
    (mutate i1) (mutate i2)
    (crossover i1 i2) (crossover i2 i1)
    (autocrossover i1) (autocrossover i2)
    (autocrossover1 i1) (autocrossover1 i2)
    (autocrossover2 i1 i2) (autocrossover2 i2 i1)
    (autopmutate i1) (autopmutate i2)
    (mutateAll2 i1 (lrand-int 100)) (mutateAll2 i2 (lrand-int 100))
    (sizeFairMutation i1) (sizeFairMutation i2)))

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
      (pmap destructive-collision 
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
    ", Best Program: " (:genome best)
    ", Generated by: " (:oper best)))
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
  