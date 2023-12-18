(use judge)

# we represent a heap as a tuple of two elements: an array and a comparator
(defn new [cmp]
  [@[] cmp])

# heaps are 1-indexed
(defn- get [heap i]
  ((heap 0) (- i 1)))

(defn- swap [heap i j]
  (def elements (heap 0))
  (def saved (elements (- j 1)))
  (set (elements (- j 1)) (elements (- i 1)))
  (set (elements (- i 1)) saved)
  nil)

(defn- has-index? [heap i]
  (<= i (length (heap 0))))

(defn- log2-floor [x]
  (var x x)
  (var shifts 0)
  (while (not= x 0)
    (set x (brushift x 1))
    (++ shifts))
  shifts)

(test (log2-floor 0) 0)
(test (log2-floor 1) 1)
(test (log2-floor 2) 2)
(test (log2-floor 3) 2)
(test (log2-floor 4) 3)
(test (log2-floor 5) 3)
(test (log2-floor 10) 4)

(defn- parent [i] (div i 2))
(defn- grandparent [i] (div i 4))

(defn min-level? [i] (= (% (log2-floor i) 2) 1))

(test (min-level? 1) true)
(test (min-level? 2) false)
(test (min-level? 3) false)
(test (min-level? 4) true)

(defn- heap/cmp [heap i1 i2]
  ((heap 1) (get heap i1) (get heap i2)))

(defn- heap/< [heap i1 i2] (neg? (heap/cmp heap i1 i2)))
(defn- heap/<= [heap i1 i2] (not (pos? (heap/cmp heap i1 i2))))
(defn- heap/> [heap i1 i2] (pos? (heap/cmp heap i1 i2)))
(defn- heap/>= [heap i1 i2] (not (neg? (heap/cmp heap i1 i2))))

(defn- heap/smaller-index [heap i1 i2]
  (if (> 0 ((heap 1) (get heap i1) (get heap i2))) i1 i2))

(defn- heap/larger-index [heap i1 i2]
  (if (< 0 ((heap 1) (get heap i1) (get heap i2))) i1 i2))

# returns the index of the best of i, i's children, and i's grandchildren
(defn- find-best [heap i choose-index]
  (var best-index i)
  (def first-child-index (* i 2))
  (def first-grandchild-index (* i 4))
  (loop [i :range [first-child-index (+ first-child-index 2)]
         :while (has-index? heap i)]
    (set best-index (choose-index heap best-index i)))
  (loop [i :range [first-grandchild-index (+ first-grandchild-index 4)]
         :while (has-index? heap i)]
    (set best-index (choose-index heap best-index i)))
  best-index)

(defn- smallest [heap i] (find-best heap i heap/smaller-index))
(defn- largest [heap i] (find-best heap i heap/larger-index))

(defn- grandparent? [i of]
  (= i (div of 4)))

(defn- push-down-general [heap i next-index heap/cmp]
  (var i i)
  (while true
    (def j (next-index heap i))
    (when (= i j) (break))
    (swap heap i j)
    (when (and (grandparent? i j)
               (heap/cmp heap (parent j) j))
      (swap heap j (parent j)))
    (set i j)))

(defn- push-down-min [heap i] (push-down-general heap i smallest heap/<))
(defn- push-down-max [heap i] (push-down-general heap i largest heap/>))

(defn- push-down [heap i]
  (if (min-level? i)
    (push-down-min heap i)
    (push-down-max heap i)))

(defn- push-up-min [heap i]
  (def j (grandparent i))
  (when (and (> j 0) (heap/< heap i j))
    (swap heap i j)
    (push-up-min heap j)))

(defn- push-up-max [heap i]
  (def j (grandparent i))
  (when (and (> j 0) (heap/> heap i j))
    (swap heap i j)
    (push-up-max heap j)))

(defn- push-up [heap i]
  (when (= i 1) (break))
  (if (min-level? i)
    (if (heap/> heap i (parent i))
      (do
        (swap heap i (parent i))
        (push-up-max heap (parent i)))
      (push-up-min heap i))
    (if (heap/< heap i (parent i))
      (do
        (swap heap i (parent i))
        (push-up-min heap (parent i)))
      (push-up-max heap i))))

(defn push [heap element]
  (def elements (heap 0))
  (array/push elements element)
  (push-up heap (length elements)))

(defn- max-index [heap]
  (var best 1)
  (loop [i :range-to [2 3] :when (heap/> heap i best)]
    (set best i))
  best)

(defn- min-index [heap] 1)

(defn- peek-helper [heap extra get-index]
  (when (> (length extra) 1)
    (error "too many arguments"))
  (def elements (heap 0))
  (when (empty? elements)
    (if (empty? extra)
      (error "cannot peek an empty heap with no default")
      (break (extra 0))))
  (get heap (get-index heap)))

(defn peek-min [heap & extra] (peek-helper heap extra min-index))
(defn peek-max [heap & extra] (peek-helper heap extra max-index))

(defn- pop-helper [heap extra get-index]
  (when (> (length extra) 1)
    (error "too many arguments"))
  (def elements (heap 0))
  (when (empty? elements)
    (if (empty? extra)
      (error "cannot pop an empty heap with no default")
      (break (extra 0))))
  (def index (get-index heap))
  (swap heap index (length elements))
  (def popped (array/pop elements))
  (push-down heap index)
  popped)

(defn pop-min [heap & extra] (pop-helper heap extra min-index))
(defn pop-max [heap & extra] (pop-helper heap extra max-index))

# TODO: this can be much more efficient
(defn contains? [heap needle]
  (has-value? (heap 0) needle))

(deftest "general heap operations"
  (def heap (new cmp))
  (each x [34 12 28 9 30 19 1 40]
    (push heap x))

  (test (peek-min heap) 1)
  (test (peek-max heap) 40)

  (test (pop-min heap) 1)
  (test (pop-min heap) 9)
  (test (pop-min heap) 12)
  (test (pop-min heap) 19)

  (test (pop-max heap) 40)
  (test (peek-min heap) 28)
  (test (peek-max heap) 34)
  (test (contains? heap 34) 34)
  (test (contains? heap 50) 34)
  (push heap 50)
  (test (contains? heap 50) 34)
  (test (pop-max heap) 50)
  (test (peek-max heap) 34)

  (test (pop-min heap) 28)
  (test (pop-min heap) 30)
  (test (pop-min heap) 34)
  (test-error (peek-min heap) "cannot peek an empty heap with no default")
  (test (pop-min heap :default) :default)
  (test-error (pop-min heap) "cannot pop an empty heap with no default"))

(def- core/length length)
(defn length [heap]
  (core/length (heap 0)))

(def- core/empty? length)
(defn empty? [heap]
  (core/empty? (heap 0)))
