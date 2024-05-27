(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
    (let [fst (first a-seq)
          rst (my-filter pred? (rest a-seq))]
      (if (pred? fst)
        (cons fst rst)
        rst)
      )
    ))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)
          rst (my-take-while pred? (rest a-seq))]
      (if (pred? fst)
        (cons fst rst)
        []))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) '()
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (= k 0) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond 
    (> how-many-times 0) (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    :else '()))

(defn my-range [up-to]
  (cond
    (> up-to 0) (cons (dec up-to) (my-range (dec up-to)))
    :else '()))

(defn tails [a-seq]
  (cond 
    (empty? a-seq) [[]]
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
     [[]]
     (map concat (rest (tails a-seq)) (rest (inits a-seq)))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
            new-count (if (contains? freqs elem)
                        (inc (get freqs elem))
                        1)
            new-freqs (assoc freqs elem new-count)]
        (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) []
    :else (let [[key val] (first a-map)]
            (concat (repeat val key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (and ((complement empty?) coll) (> n 0)) (cons (first coll) (my-take (dec n) (rest coll)))
    :else '()))

(defn my-drop [n coll]
  (cond
    (and ((complement empty?) coll) (> n 0)) (my-drop (dec n) (rest coll))
    :else coll))

(defn halve [a-seq]
  (let [sec-len (count a-seq)
        mid-seq-pos (int (/ sec-len 2))]
    (conj [] (my-take mid-seq-pos a-seq) (my-drop mid-seq-pos a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) []
    (empty? a-seq) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (let [a (first a-seq) b (first b-seq)]
      (if (< a b)
        (cons a (seq-merge (rest a-seq) b-seq))
        (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (cond
    (< (count a-seq) 2) a-seq
    :else (let [[sq1 sq2] (halve a-seq)]
            (seq-merge (merge-sort sq1) (merge-sort sq2)))))

(defn split-into-monotonics [a-seq]
  (my-take-while (or (apply <= a-seq) (apply > a-seq)) a-seq))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

