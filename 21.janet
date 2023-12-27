(use judge)
(use ./util)
(use cmp/import)
(import pat)
(use jimmy)

(def real-input (slurp "input/21.txt"))

(def test-input ```
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
```)

(defn solve [input steps]
  (def grid (grid/parse input))
  (def openings (grid/map grid (fn [x]
    (pat/match x
      "S" true
      "." true
      "#" false))))

  (defn neighbors [cell]
    (seq [dir :in grid/dirs
          :let [neighbor (vec+ cell dir)]
          :when (grid/get openings neighbor)]
      neighbor))

  (def start (grid/find grid |(= $ "S")))
  (var working-set (set/new start))

  (for i 0 steps
    (set working-set (set/mapcat working-set neighbors)))

  (length working-set))

(test (solve test-input 6) 16)
(test (solve real-input 64) 3737)

(defn solve2 [input steps]
  (def grid (grid/parse input))
  (def openings (grid/map grid (fn [x]
    (pat/match x
      "S" true
      "." true
      "#" false))))

  (def [rows cols] (openings :size))

  (defn neighbors [cell]
    (seq [dir :in grid/dirs
          :let [neighbor (vec+ cell dir)]
          :let [[row col] neighbor]
          :let [local [(mod row rows) (mod col cols)]]
          :when (grid/get openings local)]
      neighbor))

  (defn sector [[row col]]
    [(div row rows) (div col cols)])

  (def visited-sectors @{})
  (def start (grid/find grid |(= $ "S")))
  (var working-set @{start true})

  (for i 0 steps
    (def next-working-set @{})
    (loop [cell :keys working-set
           neighbor :in (neighbors cell)
           ]
      (put next-working-set neighbor true))
    (set working-set next-working-set))

    #(loop [:let [[rows cols] (openings :size)]
    #       row :range [(* -4 rows) (* 5 rows)] :after (print)
    #       col :range [(* -4 cols) (* 5 cols)]
    #       :let [pos [row col]]
    #       :let [passable (grid/get openings [(mod row rows) (mod col cols)])]
    #       :let [occupied (working-set pos)]]
    #  (prin (if occupied "O" (if passable "." "#"))))
    #(print)

    (loop [R :range-to [-4 4] :after (print)
           C :range-to [-4 4]
           :let [sector [R C]]]
      (prinf "%6d"
        (sum-loop [r :range [0 rows]
                   c :range [0 cols]
                   :let [cell [(+ r (* R rows)) (+ c (* C cols))]]
                   :when (working-set cell)
                   ]
          1)))
  )

(test-stdout (solve2 test-input 33) `
       0     0     0     0     0     0     0     0     0
       0     0     0     0     2     0     0     0     0
       0     0     0    18    34    24     0     0     0
       0     0    18    39    42    39    24     0     0
       0     2    32    42    39    42    31     1     0
       0     0    21    39    42    37    12     0     0
       0     0     0    21    30    12     0     0     0
       0     0     0     0     1     0     0     0     0
       0     0     0     0     0     0     0     0     0
`)

# it takes a few minutes to compute all of these
#(test-stdout (solve2 real-input (+ 66 131 -1)) `
#       0     0     0     0     0     0     0     0     0
#       0     0     0     0     0     0     0     0     0
#       0     0     0     0     0     0     0     0     0
#       0     0     0   985  5749   961     0     0     0
#       0     0     0  5766  7632  5779     0     0     0
#       0     0     0   980  5796   969     0     0     0
#       0     0     0     0     0     0     0     0     0
#       0     0     0     0     0     0     0     0     0
#       0     0     0     0     0     0     0     0     0
#`)
#
#(test-stdout (solve2 real-input (+ 66 (* 2 131) -1)) `
#       0     0     0     0     0     0     0     0     0
#       0     0     0     0     0     0     0     0     0
#       0     0     0   985  5749   961     0     0     0
#       0     0   985  6694  7632  6704   961     0     0
#       0     0  5766  7632  7649  7632  5779     0     0
#       0     0   980  6721  7632  6724   969     0     0
#       0     0     0   980  5796   969     0     0     0
#       0     0     0     0     0     0     0     0     0
#       0     0     0     0     0     0     0     0     0
#`)
#
#(test-stdout (solve2 real-input (+ 66 (* 3 131) -1)) `
#       0     0     0     0     0     0     0     0     0
#       0     0     0   985  5749   961     0     0     0
#       0     0   985  6694  7632  6704   961     0     0
#       0   985  6694  7632  7649  7632  6704   961     0
#       0  5766  7632  7649  7632  7649  7632  5779     0
#       0   980  6721  7632  7649  7632  6724   969     0
#       0     0   980  6721  7632  6724   969     0     0
#       0     0     0   980  5796   969     0     0     0
#       0     0     0     0     0     0     0     0     0
#`)

(defn square [x] (* x x))

(defn answer [n]
  (+
    (* n (+ 985 980 969 961))
    (* (- n 1) (+ 6694 6721 6724 6704))
    (+ 5766 5749 5796 5779)
    (* 7632 (square n))
    (* 7649 (square (- n 1)))))

(test (answer 3) 187745)
(test (answer 202300) 625382480005896)
