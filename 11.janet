(use judge)
(use ./util)

(def test-input ```
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....

```)
(def real-input (slurp "input/11.txt"))

(def peg
  ~{:here (/ (* (line) (column)) ,tuple)
    :main (some (+ (* :here "#") 1))})

(defn solve [input inflation]
  (def galaxies (peg/match peg input))
  (def by-line (group-by 0 galaxies))
  (def by-col (group-by 1 galaxies))

  (def lines (sort (keys by-line)))
  (def cols (sort (keys by-col)))

  (def empty-lines (seq [l :range [(first lines) (last lines)] :unless (has-key? by-line l)] l))
  (def empty-cols (seq [c :range [(first cols) (last cols)] :unless (has-key? by-col c)] c))

  (def inflated-galaxies
    (seq [[l c] :in galaxies]
      [(+ l (* (- inflation 1) (count-while |(< $ l) empty-lines)))
       (+ c (* (- inflation 1) (count-while |(< $ c) empty-cols)))]))

  (sum-loop [[a b] :in (unordered-pairs inflated-galaxies)]
    (manhattan-distance a b)))

(test (solve test-input 2) 374)
(test (solve real-input 2) 9723824)

(test (solve test-input 10) 1030)
(test (solve real-input 1000000) 731244261352)
