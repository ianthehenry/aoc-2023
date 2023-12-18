(use judge)
(use ./util)
(use cmp/import)
(import pat)
(import ./heap)

(def real-input (slurp "input/17.txt"))

(def test-input ```
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533

```)

(def north [-1 0])
(def south [1 0])
(def east [0 1])
(def west [0 -1])
(defn dir/inv [dir] (case dir north south east west south north west east))
(defn dir/to-string [dir] (case dir north "^" east ">" south "v" west "<"))

(def dirs [north south east west])

(defn neighbors [grid [pos dir age] min-age max-age]
  (seq [next-dir :in dirs
        :when (or (>= age min-age) (= dir next-dir))
        :when (not= next-dir (dir/inv dir))
        :let [next-age (if (= dir next-dir) (+ age 1) 0)]
        :when (< next-age max-age)
        :let [next-pos (vec+ pos next-dir)]
        :when (grid/contains? grid next-pos)]
    [next-pos next-dir next-age]))

(def test-grid (grid/parse test-input))

(test (neighbors test-grid [[0 0] east 0] -1 3) @[[[1 0] [1 0] 0] [[0 1] [0 1] 1]])
(test (neighbors test-grid [[0 0] east 0] 4 10) @[[[0 1] [0 1] 1]])

(defn better? [t k v]
  (if-let [cur (t k)]
    (< v cur)
    true))

(defn shortest [input min-age max-age]
  (def grid (grid/map (grid/parse input) scan-number))
  (def start (grid/top-left grid))
  (def goal (grid/bottom-right grid))
  (def cheapest-path @{})
  (def priors @{})

  (def shore (heap/new (by (fn [state]
    (+ (cheapest-path state) (manhattan-distance (state 0) goal))))))

  (def initial-state [start east 0])
  (put cheapest-path initial-state 0)
  (heap/push shore initial-state)

  (defn final? [[pos dir age]]
    (and (= pos goal) (>= age min-age)))

  (var final-state nil)
  (while true
    (def current (heap/pop-min shore))
    (when (final? current)
      (set final-state current)
      (break))
    (def current-cost (cheapest-path current))
    (each neighbor (neighbors grid current min-age max-age)
      (def edge-cost (grid/get grid (neighbor 0)))
      (def cost (+ current-cost edge-cost))
      (when (better? cheapest-path neighbor cost)
        (put cheapest-path neighbor cost)
        #(put priors neighbor current)
        (unless (heap/contains? shore neighbor)
          (heap/push shore neighbor)))))

  #(def path @[])
  #(var cursor final-state)
  #(while (not= cursor initial-state)
  #  (array/push path cursor)
  #  (set cursor (priors cursor)))
  #(def canvas (grid/map grid (fn [_] ".")))
  #(each [pos dir _] path
  #  (grid/set canvas pos (dir/to-string dir)))
  #(loop [:let [[rows cols] (canvas :size)]
  #       row :range [0 rows] :after (print)
  #       col :range [0 cols]]
  #  (prin (grid/get canvas [row col])))

  (cheapest-path final-state))

(defn solve [input]
  (shortest input 0 3))

(test (solve test-input) 102)

# takes 12s
#(test (solve real-input) 886)

(defn solve2 [input]
  (shortest input 3 10))

(test (solve2 test-input) 94)

(test (solve2 `
111111111111
999999999991
999999999991
999999999991
999999999991
`)
  71)

# takes 1m23s
#(test (solve2 real-input) 1055)
