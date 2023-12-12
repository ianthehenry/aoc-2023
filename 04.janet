(use judge)
(use ./util)
(import pat)
(import jimmy/set)

(def test-input ```
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

```)

(def peg
  ~{:card (group (* "Card" (any " ") (number :d+) ": " :numbers " | " :numbers))
    :numbers (group (some (* (any " ") (number :d+))))
    :main (some (* :card "\n"))})

(test (peg/match peg "Card 1: 1 2 3 | 4 5 6\n") @[@[1 @[1 2 3] @[4 5 6]]])

(def real-input (slurp "input/04.txt"))

(defn solve [input]
  (sum-loop [[_ winners choices] :in (peg/match peg input)]
    (def winners (length (* (set/of winners) (set/of choices))))
    (if (= winners 0)
      0
      (math/pow 2 (- winners 1)))))

(test (solve test-input) 13)
(test (solve real-input) 19135)

(defn solve2 [input]
  (def copies @{})
  (sum (seq [[card-num winners choices] :in (peg/match peg input)]
    (def card-count (+ (in copies card-num 0) 1))
    (def winners (length (* (set/of winners) (set/of choices))))
    (for card-won (+ card-num 1) (+ card-num winners 1)
      (incr copies card-won card-count))
    card-count)))

(test (solve2 test-input) 30)
(test (solve2 real-input) 5704953)
