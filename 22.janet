(use judge)
(use ./util)
(use cmp/import)
(import pat)
(use jimmy)

(def real-input (slurp "input/22.txt"))

(def test-input ```
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9

```)

(def peg ~{
  :brick (/ (* :p "~" :p) ,tuple)
  :p (/ (* (number :d+) "," (number :d+) "," (number :d+)) ,tuple)
  :main (some (* :brick "\n"))})

(defn parse [input]
  (peg/match peg input))

(test (parse test-input)
  @[[[1 0 1] [1 2 1]]
    [[0 0 2] [2 0 2]]
    [[0 2 3] [2 2 3]]
    [[0 0 4] [0 2 4]]
    [[2 0 5] [2 2 5]]
    [[0 1 6] [2 1 6]]
    [[1 1 8] [1 1 9]]])

(module brick
  (defn occupied [[[x1 y1 z1] [x2 y2 z2]]]
    (seq [x :range-to [x1 x2]
          y :range-to [y1 y2]
          z :range-to [z1 z2]]
      [x y z]))

  (defn supports [[[x1 y1 z1] [x2 y2 z2]]]
    (seq [x :range-to [x1 x2]
          y :range-to [y1 y2]]
      [x y (pred (min z1 z2))])))

(module jenga
  (defn- index-of [{:size [X Y Z]} [x y z]]
    (+ x (* X y) (* X Y z)))

  (defn- get- [t p] (in (t :data) (index-of t p)))
  (defn- set- [t p v] (set ((t :data) (index-of t p)) v))

  (defn new [bricks]
    (var max-x 0)
    (var max-y 0)
    (var max-z 0)
    (each [[x1 y1 z1] [x2 y2 z2]] bricks
      (max= max-x x1 x2)
      (max= max-y y1 y2)
      (max= max-z z1 z2))
    (++ max-x)
    (++ max-y)
    (++ max-z)
    (def bricks (map array/slice bricks))
    (def t
      {:size [max-x max-y max-z]
       :data (array/new-filled (* max-x max-y max-z) nil)
       :bricks bricks})

    (loop [brick :in bricks
           p :in (brick/occupied brick)]
      (set- t p brick))

    t)

  (def set set-)
  (def get get-)

  (defn translate [t brick by]
    #(def brick-index (get- t (brick 0)))
    #(assert brick-index)
    (def current (brick/occupied brick))
    (def new (map |(vec+ $ by) current))
    (each p current (set- t p nil))
    (each p new (set- t p brick))

    (vec+= (brick 0) by)
    (vec+= (brick 1) by))
  )

(defn bottom [[[_ _ z1] [_ _ z2]]]
  (min z1 z2))

(defn solve [input]
  (def jenga (jenga/new (parse input)))

  (def supporters @{})
  (def ordered-bricks (cmp/sort (jenga :bricks) (by bottom)))
  (loop [brick :in ordered-bricks]
    (var supported-by [])
    (while (and (> (bottom brick) 1) (empty? supported-by))
      (set supported-by
        (tuple/slice (distinct (filter-map |(jenga/get jenga $) (brick/supports brick)))))
      (when (empty? supported-by)
        (jenga/translate jenga brick [0 0 -1])))
    (put supporters brick supported-by))

  (def supporting (tabseq [brick :in ordered-bricks]
    brick @[]))
  (loop [[brick supported-by] :pairs supporters
         supporter :in supported-by]
    (table/push supporting supporter brick))

  (def essential-bricks (->
    (seq [[brick supporters] :pairs supporters
          :when (= (length supporters) 1)]
      (first supporters))
    (distinct)))

  (- (length ordered-bricks) (length essential-bricks)))

(test (solve test-input) 5)
(test (solve real-input) 509)

(defn solve2 [input]
  (def jenga (jenga/new (parse input)))

  (def supporters @{})
  (def ordered-bricks (cmp/sort (jenga :bricks) (by bottom)))
  (loop [brick :in ordered-bricks]
    (var supported-by [])
    (while (and (> (bottom brick) 1) (empty? supported-by))
      (set supported-by
        (tuple/slice (distinct (filter-map |(jenga/get jenga $) (brick/supports brick)))))
      (when (empty? supported-by)
        (jenga/translate jenga brick [0 0 -1])))
    (put supporters brick supported-by))

  (def supporting (tabseq [brick :in ordered-bricks] brick @[]))
  (loop [[brick supported-by] :pairs supporters
         supporter :in supported-by]
    (table/push supporting supporter brick))

  (def bricks-on-floor
    (seq [brick :in ordered-bricks :when (empty? (supporters brick))]
      brick))

  (def root-node @{:floor true})
  (each node bricks-on-floor
    (put supporters node [root-node]))
  (put supporting root-node bricks-on-floor)

  (def dominators @{})
  (def all-nodes (+ (set/new root-node) (set/of ordered-bricks)))
  (each node ordered-bricks
    (put dominators node all-nodes))
  (put dominators root-node (set/new root-node))

  (var keep-going true)
  (while keep-going
    (set keep-going false)

    (each node ordered-bricks
      (def current-dominators (dominators node))
      (def predecessors (supporters node))
      (def predecessor-dominators (map dominators predecessors))

      (def new-dominators
        (set/union
          (set/new node)
          (set/intersection ;predecessor-dominators)))

      (unless (= current-dominators new-dominators)
        (set keep-going true)
        (put dominators node new-dominators))))

  (def dominating (many-many-invert dominators))

  (sum-loop [[node dominating] :pairs dominating
             :when (not= node root-node)]
    (- (length dominating) 1)))


(test (solve2 test-input) 7)
(test (solve2 real-input) 102770)
