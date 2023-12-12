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
  :symbol (group (* (constant :symbol) (line) (column) '1))
  :number (group (* (constant :number) (line) (column) (number :d+) (column)))
  :main (some (+ :nothing :number :symbol))
  })

(def real-input (slurp "input/03.txt"))

(defn solve [input]
  (def symbols @{})
  (def lines (peg/match peg input))

  (each line lines
    (match line
      [:symbol line col sym] (put symbols [line col] sym)))

  (sum-loop [line :in lines]
    (match line
      [:number line start-col num end-col] (do
        (var adjacent false)
        (loop [line :range-to [(- line 1) (+ line 1)]
               col  :range-to [(- start-col 1) end-col]
               :when (in symbols [line col])]
          (set adjacent true))
        (if adjacent num 0))
      0)))

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
      [:symbol line col sym] (put symbols [line col] sym)))

  (def gear-incidents @{})

  (each line lines
    (match line
      [:number line start-col num end-col] (do
        (loop [line :range-to [(- line 1) (+ line 1)]
               col  :range-to [(- start-col 1) end-col]
               :let [pos [line col] sym (in symbols pos)]
               :when (= sym "*")]
          (push gear-incidents pos num)))))

  (sum (seq [nums :in gear-incidents]
    (pat/match nums [a b] (* a b) 0))))

(test (solve2 test-input) 467835)

(test (solve2 real-input) 77509019)
