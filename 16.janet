(use judge)
(use ./util)
(import pat)

(def real-input (slurp "input/16.txt"))

(def test-input ```
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....

```)

(defn beam/alive? [{:dead dead}] (not dead))

(def north [-1 0])
(def south [1 0])
(def east [0 1])
(def west [0 -1])
(defn dir/inv [[r c]] [(- r) (- c)])
(defn dir/horz? [[r c]] (not= 0 c))
(defn dir/vert? [[r c]] (not= 0 r))

(defn energized [board start-p start-v]
  (def visited (grid/map board (fn [_] @{})))
  (var beams @[@{:p start-p :v start-v :dead false}])
  (var continue true)

  (while continue
    (set continue false)
    (set beams (filter beam/alive? beams))
    (each beam beams
      #(pp beam)
      (def {:p p :v v} beam)
      (def key [p v])
      (def guest-book (grid/get visited p))
      (if (or (nil? guest-book) (has-key? guest-book key))
        (put beam :dead true)
        (do
          (set continue true)
          (case (grid/get board p)
            nil (put beam :dead true)
            "." ()
            "/"  (put beam :v (pat/match v ,east north ,west south ,north east ,south west))
            "\\" (put beam :v (pat/match v ,east south ,west north ,south east ,north west))
            "|" (when (dir/horz? v)
                  (put beam :v north)
                  (array/push beams @{:p p :v south :dead false}))
            "-" (when (dir/vert? v)
                  (put beam :v west)
                  (array/push beams @{:p p :v east :dead false}))
            (error "unknown"))
          (put guest-book key true)
          (put beam :p (vec+ p (beam :v))))
        )))

  #(loop [:let [[rows cols] (board :size)]
  #       row :range [0 rows] :after (print)
  #       col :range [0 cols]
  #       :let [guest-book (grid/get visited [row col])]]
  #  (prin (if (empty? guest-book) "." "#")))
  #(print)

  (count (non empty?) (grid/contents visited)))

(defn solve [input]
  (def board (grid/parse input))
  (energized board [0 0] east))

(test (solve test-input) 46)
(test (solve real-input) 7185)

(defn solve2 [input]
  (def board (grid/parse input))
  (def {:size [rows cols]} board)
  (var best 0)
  (loop [row :range [0 rows]]
    (max= best (energized board [row 0] east))
    (max= best (energized board [row (- cols 1)] west)))
  (loop [col :range [0 cols]]
    (max= best (energized board [0 col] south))
    (max= best (energized board [(- rows 1) col] north)))
  best)

(test (solve2 test-input) 51)

# takes 3.2s
#(test (solve2 real-input) 7616)
