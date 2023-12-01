(use judge)
(use ./util)

(def test-input ```
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
```)

(def real-input (slurp "input/01.txt"))

(def line ~(some (+ (number :d) ,(^ "\n"))))

(test (peg/match line "1abc2") @[1 2])

(def lines ~(some (* (group ,line) (+ "\n" -1))))

(test (peg/match lines test-input) @[@[1 2] @[3 8] @[1 2 3 4 5] @[7]])

(defn solve [input]
  (->> (peg/match lines input)
    (map |(+ (* 10 (first $)) (last $)))
    (sum)))

(test (solve test-input) 142)

(printf "part 1 = %d" (solve real-input))

### part two

(def digit ~(+
  (/ (if "one" 1) 1)
  (/ (if "two" 1) 2)
  (/ (if "three" 1) 3)
  (/ (if "four" 1) 4)
  (/ (if "five" 1) 5)
  (/ (if "six" 1) 6)
  (/ (if "seven" 1) 7)
  (/ (if "eight" 1) 8)
  (/ (if "nine" 1) 9)
  (number :d)))
(def line ~(some (+ ,digit ,(^ "\n"))))
(def lines ~(some (* (group ,line) (+ "\n" -1))))

(defn solve [input]
  (->> (peg/match lines input)
    (map |(+ (* 10 (first $)) (last $)))
    (sum)))

(def test-input ```
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
```)

(test (solve "twone") 21)
(test (solve test-input) 281)

(printf "part 2 = %d" (solve real-input))

(def single-peg ~{
  :digit (+
    (/ (if "one" 1) 1)
    (/ (if "two" 1) 2)
    (/ (if "three" 1) 3)
    (/ (if "four" 1) 4)
    (/ (if "five" 1) 5)
    (/ (if "six" 1) 6)
    (/ (if "seven" 1) 7)
    (/ (if "eight" 1) 8)
    (/ (if "nine" 1) 9)
    (number :d))
  :digits (* (some (+ :digit ,(^ "\n"))) "\n")
  :line (/ (group :digits) ,|(+ (* 10 (first $)) (last $)))
  :lines (some :line)
  :main (/ (group :lines) ,sum)
  })

(printf "part 2 redux = %d" (first (peg/match single-peg real-input)))
