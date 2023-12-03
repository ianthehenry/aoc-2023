(use judge)
(use ./util)
(import pat)

(def test-input ```
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
```)

(def peg ~{
  :nothing (+ "." "\n")
  :symbol (/ (* (line) (column) '1) ,(fn [line col c] [:symbol [line col] c]))
  :number (/ (* (line) (column) (number :d+) (column)) ,(fn [line col-start num col-end]
    [:number num [line col-start] [line col-end]]))
  :main (some (+ :nothing :number :symbol))
  })

(def real-input (slurp "input/03.txt"))

(defn solve [input]
  (def symbols @{})
  (def lines (peg/match peg input))

  (each line lines
    (match line
      [:symbol pos sym] (put symbols pos sym)))

  (sum (seq [line :in lines]
    (match line
      [:number num [start-line start-col] [end-line end-col]] (do
        (var adjacent false)
        (loop [line :range-to [(- start-line 1) (+ end-line 1)]
               col  :range-to [(- start-col 1) end-col]
               :when (in symbols [line col])]
          (set adjacent true))
        (if adjacent num 0))
      0))))

(test (solve test-input) 4361)

(test (solve ```
..467*..
........
..+467..
```) 934)

(test (solve ```
..467...
......*..
..123...
```) 0)

(test (solve real-input) 529618)

(defn solve2 [input]
  (def symbols @{})
  (def lines (peg/match peg input))

  (each line lines
    (match line
      [:symbol pos sym] (put symbols pos sym)))

  (def gear-incidents @{})

  (each line lines
    (match line
      [:number num [start-line start-col] [end-line end-col]] (do
        (loop [line :range-to [(- start-line 1) (+ end-line 1)]
               col  :range-to [(- start-col 1) end-col]
               :let [pos [line col] sym (in symbols pos)]
               :when (= sym "*")]
          (push gear-incidents pos num)))))

  (sum (seq [nums :in gear-incidents]
    (pat/match nums [a b] (* a b) 0))))

(test (solve2 test-input) 467835)

(test (solve2 real-input) 77509019)
