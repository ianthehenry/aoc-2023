(use judge)
(use ./util)
(import pat)
(import jimmy/set)

(def test-input ```
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45

```)
(def real-input (slurp "input/09.txt"))

(def peg
  ~{:values (group (some (* (any " ") (number (* (? "-") :d+)))))
    :main (some (* :values "\n"))
    })

(test (peg/match peg test-input)
  @[@[0 3 6 9 12 15]
    @[1 3 6 10 15 21]
    @[10 13 16 21 30 45]])

(defn differences [list]
  (map (uncurry (flip -)) (windows list)))

(defn differences-rec [list]
  (if (all |(= $ 0) list)
    [list]
    (let [x (differences list)]
      [;(differences-rec x) list])))

(defn extend [all-diffs num f]
  (pat/match all-diffs
    [] num
    [diffs & rest] (extend rest (f (last diffs) num) f)))

(defn solve [input]
  (def x (map differences-rec (peg/match peg input)))

  (sum-loop [diffs :in x]
    (extend diffs 0 +)))

(test (solve test-input) 114)
(test (solve real-input) 1953784198)

(defn solve2 [input]
  (def x (map differences-rec (peg/match peg input)))

  (sum-loop [diffs :in x]
    (extend (map reverse diffs) 0 -)))

(test (solve2 test-input) 2)
(test (solve2 real-input) 957)
