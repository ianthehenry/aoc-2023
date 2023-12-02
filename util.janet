(use judge)

(defn ^ [chars] ~(* (not (set ,chars)) 1))

(defn merge-with [f a b]
  (def answer @{})
  (eachp [k v] a
    (put answer k v))
  (eachp [k v] b
    (def already (in answer k))
    (put answer k
      (if (nil? already)
        v
        (f already v))))
  answer)

(test (merge-with max {:a 1} {:b 2}) @{:a 1 :b 2})
(test (merge-with max {:a 10} {:a 2}) @{:a 10})

(defn zip-all [f a b]
  (all |(f (in a $) (in b $)) (distinct [;(keys a) ;(keys b)])))

(defn zip-all [f d a b]
  (all |(f (in a $ d) (in b $ d)) (distinct [;(keys a) ;(keys b)])))

(test (zip-all < 0 {:a 1} {:a 4}) true)
(test (zip-all < 0 {:a 4} {:a 2}) false)
(test (zip-all < 0 {} {:a 1}) true)
(test (zip-all < 0 {:red 10} {:green 20}) false)

(defn sep [pat delim] ~(* (any (* ,pat ,delim)) ,pat))
(test (peg/match (sep ~'"a" " ") "a") @["a"])
(test (peg/match (sep ~'"a" " ") "a a aaa") @["a" "a" "a"])
(test (peg/match (sep ~'"a" " ") "a a a") @["a" "a" "a"])
