(use judge)
(use ./util)
(use cmp/import)
(import pat)

(def real-input (slurp "input/18.txt"))

(def test-input ```
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)

```)

(def up [0 -1])
(def down [0 1])
(def left [-1 0])
(def right [1 0])

(def peg1
  ~{:line (group (* :dir " " (number :d+) " (#" '6 ")"))
    :dir (/ '1 ,{"R" right "D" down "L" left "U" up})
    :main (some (* :line (? "\n")))})

(def peg2
  ~{:line (/ (* 1 " " :d+ " (#" (number 5 16) :dir ")") ,|[$1 $0])
    :dir (/ '1 ,{"0" right "1" down "2" left "3" up})
    :main (some (* :line (? "\n")))})

(defn dig-size [instructions]
  (def vertices @[])
  (var cursor [0 0])
  (var perimeter 0)
  (each [dir count] instructions
    (+= perimeter count)
    (vec+= cursor (vec*n dir count))
    (array/push vertices cursor))

  (def interior-points (+ (area-of vertices) 1 (* -0.5 perimeter)))
  (+ perimeter interior-points))

(defn solve [input]
  (dig-size (peg/match peg1 input)))

(test (solve test-input) 62)
(test (solve real-input) 61865)

(defn solve2 [input]
  (dig-size (peg/match peg2 input)))

(test (solve2 test-input) 952408144115)
(test (solve2 real-input) 40343619199142)
