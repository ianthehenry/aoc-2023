(use judge)
(use ./util)
(import pat)
(import jimmy/set)

(def test-input ```
Time:      7  15   30
Distance:  9  40  200

```)

(def peg
  ~{:values (* :w+ ":" (group (some (* :s* (number :d+)))))
    :main (/ (* :values "\n" :values) ,|(map |{:time $0 :dist $1} $0 $1))
    })

(test (peg/match peg test-input)
  @[@[{:dist 9 :time 7}
      {:dist 40 :time 15}
      {:dist 200 :time 30}]])

(def real-input (slurp "input/06.txt"))

(defn solve [input]
  (def races (first (peg/match peg input)))

  (product (seq [{:time time :dist dist} :in races]
    (count |$ (seq
      [hold-time :range [0 (+ time 1)]
      :let [remaining (- time hold-time)
            distance (* remaining hold-time)]
      ]
      (> distance dist))))))

(test (solve test-input) 288)
(test (solve real-input) 449820)

(defn solve2 [input]
  (def [[{:time time :dist dist}]] (peg/match peg (string/replace-all " " "" input)))
  (var total 0)
  (loop
    [hold-time :range [0 (+ time 1)]
    :let [remaining (- time hold-time)
          distance (* remaining hold-time)]
    :when (> distance dist)]
    (+= total 1))
  total)

(test (solve2 test-input) 71503)
# takes 750ms
#(test (solve2 real-input) 42250895)
