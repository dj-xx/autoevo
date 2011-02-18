;; Lee Spector, 2011 (lspector@hampshire.edu)

;; This is autoconstructive evolution code, prepared for the Hampshire College course
;; "Research in Artificial Intelligence."

;; It is a quick and dirty hybrid of my current experimental code (which runs in Clojure 1.2
;; with the current (20110118) version of Clojush at https://github.com/lspector/Clojush) and
;; the code used for the runs published in:
;;
;;    Spector, L. 2010. Towards Practical Autoconstructive Evolution: Self-Evolution of
;;    Problem-Solving Genetic Programming Systems. In Genetic Programming Theory and
;;    Practice VIII, edited by R. L. Riolo, T. McConaghy, and E. Vladislavleva.
;;    New York: Springer. http://hampshire.edu/lspector/pubs/spector-gptp10-preprint.pdf
;;
;; This code should run without error in Clojure 1.2 with the current (20110118) version 
;; of Clojush but that does NOT mean that it will regularly find a solution!

(ns regressionForClass)


(load "clojush")
(in-ns 'clojush)

;; set some globals to particular values
(def top-level-push-code true) ;***** make false for nopush, recon
(def top-level-pop-code false)
(def maintain-ancestors true)
(def print-ancestors-of-solution true)
(def evalpush-limit 500)

;; define some new globals
(def successful-births (ref 0))
(def reproductive-discrepancy (ref 0))

(defn weighted-average
  "Returns the weighted average of nums with the given weights."
  [nums weights]
  (cond (= (count nums) 0) 0
    (= (count nums) 1) (first nums)
    :else  (/ (reduce +' (map * nums weights))
             (reduce +' weights))))

(defn report
  "Reports on the specified generation of a pushgp run. Returns the best                                                                    
  individual of the generation. HACKED to consider all natural born better
  than all others."
  [population generation error-function report-simplifications]
  (printf "\n\n;;;;;;;;;;;;;;;;;;;;;; AutoPush Report ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")(flush)
  (printf "\n;; -*- Report at generation %s" generation)(flush)
  (let [sorted (sort (fn [a b] 
                       (cond (and (empty? (:ancestors a)) (not (empty? (:ancestors b)))) false
                         (and (not (empty? (:ancestors a))) (empty? (:ancestors b))) true
                         :else (< (:total-error a) (:total-error b))))
                 population)
        best (first sorted)]
    (printf "\nBest program: %s" (not-lazy (:program best)))(flush)
    (when (> report-simplifications 0)
      (printf "\nPartial simplification (may beat best): %s"
        (not-lazy (:program (auto-simplify best error-function report-simplifications false 1000)))))
    (flush)
    (printf "\nErrors: %s" (not-lazy (:errors best)))(flush)
    (printf "\nTotal: %s" (:total-error best))(flush)
    (printf "\nHistory: %s" (not-lazy (:history best)))(flush)
    (printf "\nSize: %s" (count-points (:program best)))(flush)
    (print "\n--- Population Statistics ---\nAverage total errors in population: ")(flush)
    (print (*' 1.0 (/ (reduce + (map :total-error sorted)) (count population))))(flush)
    (printf "\nMedian total errors in population: %s"
      (:total-error (nth sorted (truncate (/ (count sorted) 2)))))(flush)
    (printf "\nAverage program size in population (points): %s"
      (*' 1.0 (/ (reduce + (map count-points (map :program sorted)))
               (count population))))(flush)
    (printf "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n")
    (flush)
    (problem-specific-report best population generation error-function report-simplifications)
    best))



(defn intervals
  "Returns a list of the intervals between successive items in lst."
  [lst]
  (doall (map #(- (first (rest %)) (first %)) (partition 2 1 lst))))

(defn improvements
  "Returns a list of 1 for each improvement (with low being good and the first
being most recent) and 0 for each non-improvement (including worsenings) in
the given lst."
  [lst]
  (doall (map #(if (pos? (-' (first (rest %)) (first %))) 1 0)
           (partition 2 1 lst))))

(def decay-rate 0.1) ;(/ 1 3))

(defn decay-weighted-average
  "Returns the weighted average of nums, with weights decaying according to decay-sequence."
  [nums decay-sequence]
  (cond (= (count nums) 0) 0
    (= (count nums) 1) (first nums)
    :else  (/ (reduce + (doall (map * nums decay-sequence)))
             (reduce + (take (count nums) decay-sequence)))))

(defn unitize
  "Replaces negative numbers with -1, positive numbers with 1, and 0 with 0 in sequence s."
  [s]
  (doall (map #(cond (neg? %) -1 (pos? %) 1 :else 0) s)))

(defn decayed-unitized-improvement
  "Returns the decay-weighted average of unitized improvements (decreases) in the 
provided total-error history (removing any non-numerical elements). The first
entry sould be the most recent and will be given the most weight."
  [hist]
  (decay-weighted-average (unitize (intervals (filter number? hist)))
    (iterate (fn [num] (* num (- 1.0 decay-rate))) 1.0)))


(defn problem-specific-report
  "Report function for autoconstructive runs, printing and resetting
the reproductive competence information."
  [best population generation error-function report-simplifications]
  (printf "\nSuccessful births: %s\n" @successful-births)
  (printf "Average reproductive discrepancy/natural birth: %s\n"
    (if (zero? @successful-births)
      "N/A"
      (/ @reproductive-discrepancy (* 1.0 @successful-births))))
  (printf "Ancestors of best: \n")
  (println (:ancestors best))
  (dosync (alter successful-births (fn [val] 0)))
  (dosync (alter reproductive-discrepancy (fn [val] 0)))
  (let [imps (sort > (map decayed-unitized-improvement (map :history population)))
        decile-size (truncate (/ (count imps) 10))]
    (printf "\nImprovement decile maxima: ")
    (dotimes [i 10] (printf "[%6.4f]  " (* 1.0 (nth imps (* i decile-size)))))
    (printf "\n")))

(defn stagnant
  [ind]
  (and (>= (count (:ancestors ind)) 12)
    (apply = (take (math/floor (/ (count (:ancestors ind)) 2)) (:ancestors ind)))))

(defn select
  "Conducts a tournament and returns the individual with the lower scaled error."
  [pop tournament-size radius location]
  (let [tournament-set 
        (doall
          (for [_ (range tournament-size)]
            (nth pop
              (if (zero? radius)
                (lrand-int (count pop))
                (mod (+ location (- (lrand-int (+ 1 (* radius 2))) radius))
                  (count pop))))))
        winner
        (reduce (fn [i1 i2] 
                  (let [thresh 2] 
                    (cond 
                      (< (min thresh (count (:ancestors i1))) (min thresh (count (:ancestors i2)))) i2
                      (< (min thresh (count (:ancestors i2))) (min thresh (count (:ancestors i1)))) i1
                      (and (stagnant i1) (not (stagnant i2))) i2
                      (and (stagnant i2) (not (stagnant i1))) i1
                      (< (:total-error i2) (:total-error i1)) i2
                      :else i1)))
          tournament-set)]
    winner))

(defn autoconstruct
  [pgm]
  (top-item :code (run-push pgm (push-item 0 :auxiliary (make-push-state)))))

(defn mutate
  "Returns a mutated version of the given individual -- AUTOCONSTRUCTIVE version."
  [ind mutation-max-points max-points atom-generators]
  (let [stagthresh 4
        hist (filter number? (:history ind))]
    (if (or (and (>= (count hist) stagthresh)
              (<= (decayed-unitized-improvement hist) 0.1))
          (> (:total-error ind) 1E8)
          (let [discreps (doall (map first (filter seq? (:history ind))))]
            (and (>= (count discreps) 2)
              (apply = discreps))))
      (make-individual :program (random-code max-points atom-generators) :history () :ancestors ())
      (let [new-program (autoconstruct (:program ind))
            sib-program (autoconstruct (:program ind))
            ancestors (get ind :ancestors ())
            failed (or (= new-program :no-stack-item)
                     (not (seq? new-program))
                     (> (count-points new-program) max-points)
                     (< (count-points new-program) 10)
                     (= new-program (:program ind))
                     (some #{new-program} ancestors)
                     (= new-program sib-program)
                     )
            discrep (if failed :new (discrepancy (:program ind) new-program))
            program (if failed
                      (random-code max-points atom-generators)
                      (do (dosync (commute successful-births inc))
                        (dosync (commute reproductive-discrepancy #(+ % discrep)))
                        new-program))]
        (make-individual 
          :program program
          :history (if failed () (cons (list discrep) (:history ind)))
          :ancestors (if failed
                       ()
                       (if maintain-ancestors
                         (cons (:program ind) ancestors)
                         ancestors)))))))


(defn random-atom
  "Returns a random atom using atom-generators."
  [atom-generators]
  (let [element (rand-nth atom-generators)]
    (if (fn? element)
      (element)
      element)))

(defn perturb
  [pgm denom atom-generators]
  ;(println "perturbing " pgm)
  (if (and (seq? pgm) (not (empty? pgm)))
    (let [zipper (zip/seq-zip pgm)
          points (count-points pgm)
          safe-denom (max 1 (math/abs denom))]
      (loop [z zipper i 1]
        (if (> i points)
          (zip/root z)
          (if (seq? (zip/node z))
            (recur (zip/next z) (inc i))
            (if (zero? (lrand-int safe-denom))
              (recur (zip/next (zip/replace z (random-atom atom-generators))) (inc i))
              (recur (zip/next z) (inc i)))))))
    (random-atom atom-generators)))

(define-registered code_perturb
  (fn [state]
    (if (and (not (empty? (:code state)))
          (not (empty? (:integer state))))
      (push-item (perturb (stack-ref :code 0 state)
                   (stack-ref :integer 0 state)
                   @global-atom-generators)
        :code
        (pop-item :code (pop-item :integer state)))
      state)))

(define-registered in 
  (fn [state] (push-item (stack-ref :auxiliary 0 state) :integer state)))

(defn factorial [n] (if (< n 2) 1 (* n (factorial (dec n)))))


(pushgp 
  :error-function (fn [program]
                    (let [inputs (range 10) ;1 6)
                          outputs 
                          (doall (for [input inputs]
                                   (let [state (run-push program 
                                                 (push-item input :auxiliary 
                                                   (push-item input :integer
                                                     (make-push-state))))
                                         top-int (top-item :integer state)]
                                     (if (or (not (number? top-int))
                                           (= (:termination state) :abnormal))
                                       :invalid
                                       top-int))))
                          penalty 1E10]
                      (if (apply = outputs)
                        (doall (map (fn [x] penalty) outputs))
                        (doall (map (fn [input output]
                                      (if (= output :invalid)
                                        penalty
                                        (math/abs (- output 
                                                    (+ (* input input input input input input)
                                                      (- (* 2 input input input input))
                                                      (* input input)
                                                      -2)))))
                                 inputs
                                 outputs)))))
	 :atom-generators (concat (list (fn [] (lrand-int 10))
					'in)
				  (registered-for-type :integer)
				  (registered-for-type :exec)
				  (registered-for-type :boolean)
				  (registered-for-type :code))
  :population-size 5000
  :tournament-size 100
  :mutation-probability 1
	:crossover-probability 0
  :simplification-probability 0
  :report-simplifications 0
  :max-generations 10000
	 ;:trivial-geography-radius 50
  :max-points 100
  ;:decimation-ratio 0.01
  ;:decimation-tournament-size 2
  )


