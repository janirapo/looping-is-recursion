(ns looping-is-recursion)

(defn power [base exp]
  (let
    [helper (fn [acc n k]
              (cond
                (zero? k) acc
                :else (recur (* acc n) n (dec k))
              )
            )
    ]
    (helper 1 base exp)
  )
)

(defn last-element [a-seq]
  (let
    [helper (fn [acc coll]
              (if (empty? coll)
                  acc
                  (recur (first coll) (rest coll))
              )
            )
    ]
    (helper (first a-seq) a-seq)
  )
)

(defn seq= [seq1 seq2]
  (let
    [helper (fn [acc a b]
              (cond
                (not (= (first a) (first b))) acc
                (and (empty? a) (empty? b)) true
                (or (empty? a) (empty? b)) acc
                :else (recur acc (rest a) (rest b))
              )
            )
    ]
    (helper false seq1 seq2)
  )
)

(defn find-first-index [pred a-seq]
  (loop [i 0
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) i
      :else (recur (inc i) (rest a-seq))
    )
  )
)

(defn avg [a-seq]
  (loop [sum 0
         count 0
         a-seq a-seq]
    (cond
      (and (empty? a-seq) (zero? count)) nil
      (empty? a-seq) (/ sum count)
      :else (recur (+ sum (first a-seq)) (inc count) (rest a-seq))
    )
  )
)

(defn parity [a-seq]
  (loop [counts '{}
         a-seq a-seq]
    (if (empty? a-seq)
      (keys (filter #(odd? (val %)) counts))
      (let 
        [current (first a-seq)
         count (or (get counts current) 0)]
        (recur (assoc counts current (inc count)) (rest a-seq))
      )
    )
  )
)

(defn fast-fibo [n]
  (loop [fofn n
         fofn-1 1
         fofn-2 0]
    (cond
      (= fofn 0) fofn-2
      (< fofn 2) fofn-1
      :else (recur (dec fofn) (+ fofn-2 fofn-1) fofn-1))
  )
)

(defn cut-at-repetition [a-seq]
  (loop [prev []
         a-seq a-seq]
    (cond
      (empty? a-seq) prev
      (contains? (set prev) (first a-seq)) prev
      :else (recur (conj prev (first a-seq)) (rest a-seq))
    )
  )
)

