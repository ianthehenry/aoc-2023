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

(test (peg/match (sep ~(% (some '1)) " ") "one two three") @["one two three"])
(test (peg/match (sep ~(% (some ',(^ " "))) " ") "one two three") @["one" "two" "three"])

(defn incr [tab k &opt by]
  (default by 1)
  (def cur (in tab k))
  (if cur
    (put tab k (+ cur by))
    (put tab k by)))

(defn push [tab k v]
  (def cur (in tab k))
  (if cur
    (array/push cur v)
    (put tab k @[v])))

(defn . [k &opt d] (fn [t] (in t k d)))

(defn next-larger-integer [x]
  (def c (math/ceil x))
  (if (= c x)
    (+ x 1)
    c))

(test (next-larger-integer 5) 6)
(test (next-larger-integer 5.5) 6)

(defn next-smaller-integer [x]
  (def f (math/floor x))
  (if (= f x)
    (- x 1)
    f))

(test (next-smaller-integer 5) 4)
(test (next-smaller-integer 5.5) 5)

(defn solve-quadratic [a b c]
  (def root (math/sqrt (- (* b b) (* 4 a c))))
  (def two-a (* 2 a))
  [(/ (+ (- b) root) two-a)
   (/ (- (- b) root) two-a)])

(test (solve-quadratic 1 0 0) [0 0])
(test (solve-quadratic -2 1 1) [-0.5 1])

(defn concat [args]
  (array/concat @[] ;args))

(test (concat [[1] [2]]) @[1 2])

(defn find-map [ind f pred]
  (var result nil)
  (each x ind
    (def candidate (f x))
    (when (pred candidate)
      (set result candidate)
      (break)))
  result)

(defn transpose [arr]
  (map tuple ;arr))

(test (transpose [[1 2] [3 4] [5 6]]) @[[1 3 5] [2 4 6]])

# like map, but f returns a tuple of [output new-inputs-to-try].
# it will not terminate until no new inputs are generated.
(defn expanding-map [inputs f]
  # f returns a list of outputs, and a list of further inputs to try
  (def inputs (array/slice inputs))
  (def outputs @[])

  (var i 0)
  (while (< i (length inputs))
    (def [out new-inputs] (f (in inputs i)))
    (array/push outputs out)
    (array/concat inputs new-inputs)
    (++ i))
  outputs)

(test (expanding-map [1 2 3] (fn [x]
  [x
   (if (= x 1) [4] [])])) @[1 2 3 4])

(defn- comparing-helper [a b comparators]
  (if (empty? comparators)
    false
    (case ((first comparators) a b)
      -1 true
      0 (comparing-helper a b (drop 1 comparators))
      1 false)))

(defn comparing [& comparators]
  (fn [a b] (comparing-helper a b comparators)))

(defn by [f &opt comparator]
  (default comparator cmp)
  (fn [a b] (comparator (f a) (f b))))

(defn descending [comparator]
  (fn [a b] (* -1 (comparator a b))))

(defn cmp-each [a b &opt comparator]
  (default comparator cmp)
  (var result 0)
  (each [a b] (map tuple a b)
    (def c (comparator a b))
    (when (not= 0 c)
      (set result c)
      (break)))
  result)

(test (cmp-each [1 2 3] [1 2 4]) -1)
(test (cmp-each [1 2 4] [1 2 3]) 1)
(test (cmp-each [1 3 4] [1 2 3]) 1)
(test (cmp-each [1 2 3] [1 2 3]) 0)

(test (cmp-each [1 2 3] [1 2 4] cmp) -1)
(test (cmp-each [1 2 3] [1 2 4] (descending cmp)) 1)

(def- core/string/split string/split)
(defn string/split [str sep]
  (if (empty? sep)
    (map string/from-bytes str)
    (core/string/split sep str)))

(test (string/split "abc" "") @["a" "b" "c"])
(test (string/split "abc" "b") @["a" "c"])

(defn windows [list]
  (var result @[])
  (for i 0 (- (length list) 1)
    (array/push result [(list i) (list (+ i 1))]))
  result)

(test (windows [1 2 3]) @[[1 2] [2 3]])

(defn uncurry [f] (fn [args] (f ;args)))
(defn flip [f] (fn [a b] (f b a)))
