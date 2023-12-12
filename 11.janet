(use judge)
(use ./util)
(import pat)
(import jimmy/set)

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

(def peg ~{
  :here (/ (* (line) (column)) ,tuple)
  :main (some (+ (* :here "#") 1))
  })

(defn count-while [f xs]
  (var c 0)
  (loop [x :in xs :while (f x)]
    (++ c))
  c)

(defmacro each-unordered-pair [a b xs & body]
  (with-syms [$i $j $xs]
    ~(let [,$xs ,xs]
      (loop [,$i :range [0 (length ,$xs)] ,$j :range [(+ ,$i 1) (length ,$xs)]
             :let [,a (,$xs ,$i) ,b (,$xs ,$j)]]
        ,;body))))

(defn solve [input inflation]
  (def galaxies (peg/match peg input))
  (def by-line (group-by 0 galaxies))
  (def by-col (group-by 1 galaxies))

  (def lines (sort (keys by-line)))
  (def cols (sort (keys by-col)))

  (def empty-lines (seq [l :range [(first lines) (last lines)] :unless (has-key? by-line l)] l))
  (def empty-cols (seq [c :range [(first cols) (last cols)] :unless (has-key? by-col c)] c))

  (def galaxies
    (seq [[l c] :in galaxies]
      [(+ l (* (- inflation 1) (count-while |(< $ l) empty-lines)))
       (+ c (* (- inflation 1) (count-while |(< $ c) empty-cols)))]))

  (var sum 0)
  (each-unordered-pair a b galaxies
    (+= sum (manhattan-distance a b))
    )
  sum)

(test (solve test-input 2) 374)
(test (solve real-input 2) 9723824)

(test (solve test-input 10) 1030)
(test (solve real-input 1000000) 731244261352)
