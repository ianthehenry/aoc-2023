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

(defn table/push [tab k v]
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
(test (windows [1]) @[])
(test (windows []) @[])

(defn uncurry [f] (fn [args] (f ;args)))
(defn flip [f] (fn [a b] (f b a)))

(defn contains? [list needle]
  (var result false)
  (each x list
    (when (= x needle)
      (set result true)
      (break)))
  result)

(test (contains? [1 2 3] 2) true)
(test (contains? [1 2 3] 4) false)

# works on sets
(defn empty? [x] (= (length x) 0))

(defn manhattan-distance [[l1 c1] [l2 c2]]
  (+ (math/abs (- l1 l2))
     (math/abs (- c1 c2))))

(defmacro sum-loop [rules & body]
  (with-syms [$sum] ~(do
    (var ,$sum 0)
    (loop ,rules (+= ,$sum (do ,;body)))
    ,$sum)))

(defn count-while [f xs]
  (sum-loop [x :in xs :while (f x)] 1))

(defmacro lazy-seq [rules & body]
  ~(coro (loop ,rules (yield (do ,;body)))))

(defn unordered-pairs [xs]
  (def len (length xs))
  (lazy-seq [i :range [0 len]
             j :range [(+ i 1) len]
             :let [a (xs i) b (xs j)]]
    [a b]))

(defn smuggle [x] ~(,(fn [] x)))

(defmacro module [name & body]
  (def env (make-env (curenv)))
  (def inputs (lazy-seq [form :in body] form))
  (run-context {
    :env env
    :read (fn [env source]
      (def form (resume inputs))
      (if form form (do (put env :exit true) nil)))})
  ~(,merge-module (,curenv) ,(smuggle env) ,(string name "/") true))

(defmacro defmemo [name args & body]
  (with-syms [$memo $f $result $args] ~(def ,name (do
    (var ,name nil)
    (defn ,$f ,args ,;body)
    (var ,$memo @{})
    (set ,name (fn [& ,$args]
      (when-let [,$result (in ,$memo ,$args)]
        (break ,$result))
      (def ,$result (apply ,$f ,$args))
      (put ,$memo ,$args ,$result)
      ,$result))))))

(test-macro (defmemo add [a b] (+ a b))
  (def add (do (var add nil) (defn <1> [a b] (+ a b)) (var <2> @{}) (set add (fn [& <3>] (when-let [<4> (in <2> <3>)] (break <4>)) (def <4> (apply <1> <3>)) (put <2> <3> <4>) <4>)))))

(do
  (defmemo add [a b] (+ a b))
  (test (add 1 2) 3)

  (test-macro (defmemo count [x] (if (= x 0) 0 (count (- x 1))))
    (def count (do (var count nil) (defn <1> [x] (if (= x 0) 0 (count (- x 1)))) (var <2> @{}) (set count (fn [& <3>] (when-let [<4> (in <2> <3>)] (break <4>)) (def <4> (apply <1> <3>)) (put <2> <3> <4>) <4>)))))

  (defmemo count [x] (if (= x 0) 0 (count (- x 1))))
  (test (count 5) 0))

(defn put* [tab k v]
  (put tab k v)
  v)

(defn multinvert [tab]
  (def result @{})
  (eachp [v k] tab
    (array/push (or (in result k) (put* result k @[])) v))
  result)

(test (multinvert {:a 3 :b 2 :c 3}) @{2 @[:b] 3 @[:c :a]})

(defn many-many-invert [tab]
  (def result @{})
  (eachp [v ks] tab
    (each k ks
      (array/push (or (in result k) (put* result k @[])) v)))
  result)

(test (many-many-invert {:a [3 2] :b [2] :c [3 1]}) @{1 @[:c] 2 @[:a :b] 3 @[:c :a]})

(module grid
  (def up [-1 0])
  (def down [1 0])
  (def left [0 -1])
  (def right [0 1])
  (def dirs [up down left right])
  (defn adjacent [[r c]] [[(+ r 1) c] [(- r 1) c] [r (+ c 1)] [r (- c 1)]])

  (defn parse [input]
    (def lines (string/split (string/trim input) "\n"))
    (def rows (length lines))
    (def cols (length (first lines)))
    {:contents (mapcat |(string/split $ "") lines) :size [rows cols]})

  (defn contents [grid]
    (grid :contents))

  (defn contains? [{:size [rows cols]} [row col]]
    (and
      (>= row 0)
      (>= col 0)
      (< row rows)
      (< col cols)))

  (defn get- [grid p]
    (if (contains? grid p)
      (let [[row col] p {:contents contents :size [rows cols]} grid]
        (contents (+ (* cols row) col)))))

  (defn set- [grid p x]
    (if (contains? grid p)
      (let [[row col] p {:contents contents :size [rows cols]} grid]
        (set (contents (+ (* cols row) col)) x))))

  (defn find [{:contents contents :size [rows cols]} f]
    (if-let [index (find-index f contents)]
      [(div index rows) (mod index rows)]))

  (def- core/map map)
  (defn map [{:contents contents :size size} f]
    {:contents (core/map f contents) :size size})

  (defn clone [{:contents contents :size size}]
    {:contents (array/slice contents) :size size})

  (defn top-left [_]
    [0 0])

  (defn bottom-right [{:size [rows cols]}]
    [(- rows 1) (- cols 1)])

  (defn print- [t &opt to-string]
    (default to-string string)

    (loop [:let [[rows cols] (t :size)]
           row :range [0 rows] :after (print)
           col :range [0 cols]
           :let [pos [row col]]]
      (prin (to-string (get- t pos)))))

  (def get get-)
  (def set set-)
  (def print print-)
  )

(defn non [f] (fn [x] (not (f x))))

(defn vec+ [& vs] (tuple/slice (map + ;vs)))
(defn vec- [& vs] (tuple/slice (map - ;vs)))
(defmacro vec+= [v & vs]
  ~(set ,v (,vec+ ,v ,;vs)))
(defmacro vec-= [v & vs]
  ~(set ,v (,vec- ,v ,;vs)))
(defn vec*n [v s] (tuple/slice (map |(* s $) v)))
(defmacro vec*n= [v1 s]
  ~(set ,v1 (,vec*n ,v1 ,s)))

(test (vec+ [1 2 3] [4 5 6] [0 0 1]) [5 7 10])

(defn vec/dot [v1 v2]
  (sum (map * v1 v2)))
(defn vec/mag [v]
  (math/sqrt (vec/dot v v)))
(defn vec/normalize [[x y]]
  (let [mag-squared (+ (* x x) (* y y))]
    (if (= mag-squared 0)
      [0 0]
      (let [mag (math/sqrt mag-squared)
            magi (/ mag)]
        [(* x magi) (* y magi)]))))

(defmacro max= [v & xs]
  ~(set ,v (,max ,v ,;xs)))
(defmacro min= [v & xs]
  ~(set ,v (,min ,v ,;xs)))

(def pred dec)
(def succ inc)

(defn area-of [vertices]
  (* 0.5
    (sum-loop [i :range [0 (pred (length vertices))]]
      (def [x1 y1] (vertices i))
      (def [x2 y2] (vertices (succ i)))
      (* (+ y1 y2) (- x1 x2)))))

(defn ref/new [value] @[value])
(defn ref/set [ref value] (set (ref 0) value))
(defn ref/get [ref] (ref 0))
(defn ref/update [ref f] (ref/set ref (f (ref/get ref))))

(defn filter-map [f xs]
  (filter |(not= nil $) (map f xs)))
