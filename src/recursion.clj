(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (first (reverse coll)))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
       (cons (first a-seq) (my-filter pred? (rest a-seq)))
       (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
   (cond
     (empty? a-seq) false
     (not= elem (first a-seq)) (sequence-contains? elem (rest a-seq))
     :else true))


(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else nil))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-take-while pred? (rest a-seq))
    :else a-seq))


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons
            (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons
      what-to-repeat
      (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    `()
    (cons
      (- up-to 1)
      (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
     `(())
     (cons
       (seq a-seq)
       (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
     `(())
     (cons
       (seq a-seq)
       (inits (pop a-seq)))))

(defn rotations [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (rotate-times a-seq (count a-seq))))

(defn rotate-times [a-seq times]
   (if (= times 1) (list a-seq)
      (cons
        (seq a-seq) (rotate-times (concat (rest a-seq) (take 1 a-seq)) (- times 1)))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
       (my-frequencies-helper (update-in freqs [(first a-seq)] inc) (rest a-seq))
       (my-frequencies-helper (conj {(first a-seq) 1} freqs) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-map key-seq]
  (if (empty? key-seq)
     key-seq
     (concat
       (repeat (get a-map (first key-seq)) (first key-seq)) ;repeat first map key by respective value
       (un-frequencies-helper a-map (rest key-seq))))) ;recur by rest keys

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map (map key a-map)))


(defn my-take [n coll]
  (cond
     (<= n 0) ()
     (> n (count coll)) (concat (list (first coll)) (my-take (count (rest coll)) (rest coll)))
     :else (concat (list (first coll)) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (>= n 1) (my-drop (- n 1) (rest coll))
    :else coll))

(defn halve [a-seq]
  (let [halfway (int (/ (count a-seq) 2))]
    (vector
       (my-take halfway a-seq)
       (my-drop halfway a-seq))))

(defn seq-merge [a-seq b-seq]
   (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     :else (if (< (first a-seq) (first b-seq))
             (concat (list (first a-seq)) (seq-merge (rest a-seq) b-seq))
             (concat (list (first b-seq)) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [[first-half last-half] (halve a-seq)
         first-sorted (cond (> 2 (count first-half)) first-half :else (merge-sort first-half))
         last-sorted  (cond (> 2 (count last-half)) last-half :else (merge-sort last-half))]
    (seq-merge first-sorted last-sorted)))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
