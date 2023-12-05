(use judge)
(use ./util)
(import pat)
(import jimmy/set)

(def test-input ```
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4

```)

(def peg
  ~{:maps (some (* :map (any "\n")))
    :mapping (/ (* (number :d+) :s+ (number :d+) :s+ (number :d+)) ,|{:dest-start $0 :src-start $1 :len $2})
    :map (* (thru "map:\n") (group (some (* :mapping "\n"))) :s*)
    :seeds (* "seeds: " (group (some (* (number :d+) :s*))))
    :main (/ (* :seeds :maps) ,|{:seeds $0 :maps $&})
    })

(test (peg/match peg test-input)
  @[{:maps [@[{:dest-start 50 :len 2 :src-start 98}
              {:dest-start 52 :len 48 :src-start 50}]
            @[{:dest-start 0 :len 37 :src-start 15}
              {:dest-start 37 :len 2 :src-start 52}
              {:dest-start 39 :len 15 :src-start 0}]
            @[{:dest-start 49 :len 8 :src-start 53}
              {:dest-start 0 :len 42 :src-start 11}
              {:dest-start 42 :len 7 :src-start 0}
              {:dest-start 57 :len 4 :src-start 7}]
            @[{:dest-start 88 :len 7 :src-start 18}
              {:dest-start 18 :len 70 :src-start 25}]
            @[{:dest-start 45 :len 23 :src-start 77}
              {:dest-start 81 :len 19 :src-start 45}
              {:dest-start 68 :len 13 :src-start 64}]
            @[{:dest-start 0 :len 1 :src-start 69}
              {:dest-start 1 :len 69 :src-start 0}]
            @[{:dest-start 60 :len 37 :src-start 56}
              {:dest-start 56 :len 4 :src-start 93}]]
     :seeds @[79 14 55 13]}])

(def real-input (slurp "input/05.txt"))

(defn inside? [num {:src-start start :len len}]
  (and (>= num start)
       (< num (+ start len))))

(defn transform [num rules]
  (if-let [{:src-start src-start :dest-start dest-start :len len} (find (partial inside? num) rules)]
    (let [offset (- num src-start)]
      (+ dest-start offset))
    num))

(defn solve [input]
  (def {:seeds seeds :maps maps} (first (peg/match peg input)))

  (min-of (seq [seed :in seeds]
    (reduce |(transform $0 $1) seed maps))))

(test (solve test-input) 35)
(test (solve real-input) 278755257)

(def peg
  ~{:maps (some (* :map (any "\n")))
    :mapping (/ (* (number :d+) :s+ (number :d+) :s+ (number :d+)) ,|{:dest-start $0 :src-start $1 :len $2})
    :map (* (thru "map:\n") (group (some (* :mapping "\n"))) :s*)
    :seeds (* "seeds: " (group (some (/ (* (number :d+) :s+ (number :d+) :s*) ,|{:start $0 :len $1}))))
    :main (/ (* :seeds :maps) ,|{:seeds $0 :maps $&})
    })

(test (get-in (peg/match peg test-input) [0 :seeds])
  @[{:len 14 :start 79}
    {:len 13 :start 55}])

(defn transform [rule range]
  (def {:src-start rule-start :dest-start rule-dest-start :len rule-len} rule)
  (def rule-end (+ rule-start rule-len))
  (def {:start start :len len} range)
  (def end (+ start len))

  (def new-start (max rule-start start))
  (def new-end (min rule-end end))
  (def new-len (- new-end new-start))

  (def offset (- new-start rule-start))

  (if (> new-len 0)
    {:start (+ rule-dest-start offset) :len new-len :from new-start}))

# range inside rule
(test (transform {:src-start 30 :len 10 :dest-start 130} {:start 32 :len 2}) {:from 32 :len 2 :start 132})
# rule inside range
(test (transform {:src-start 30 :len 10 :dest-start 130} {:start 20 :len 50}) {:from 30 :len 10 :start 130})
# range before rule
(test (transform {:src-start 30 :len 10 :dest-start 130} {:start 20 :len 5}) nil)
# range after rule
(test (transform {:src-start 30 :len 10 :dest-start 130} {:start 40 :len 5}) nil)
# overlap left
(test (transform {:src-start 30 :len 10 :dest-start 130} {:start 25 :len 10}) {:from 30 :len 5 :start 130})
# overlap right
(test (transform {:src-start 30 :len 10 :dest-start 130} {:start 35 :len 10}) {:from 35 :len 5 :start 135})
# exact
(test (transform {:src-start 30 :len 10 :dest-start 130} {:start 30 :len 10}) {:from 30 :len 10 :start 130})

(defn unmapped-ranges [original-range mapped-ranges]
  (sort-by (. :from) mapped-ranges)
  (def {:start start :len len} original-range)
  (def end (+ start len))
  (def result @[])
  (var current start)
  (each {:from from :len len} mapped-ranges
    (when (not= from current)
      (array/push result {:start current :len (- from current)}))
    (set current (+ from len)))

  (when (not= current end)
    (array/push result {:start current :len (- end current)}))
  result)

(defn transform-all [ranges rules]
  (catseq [range :in ranges]
    (def mapped-ranges
      (seq [rule :in rules
            :let [new-range (transform rule range)]
            :when new-range]
        new-range))
    (array/concat mapped-ranges (unmapped-ranges range mapped-ranges))))

(test (transform-all [{:start 30 :len 10}] [{:src-start 30 :len 10 :dest-start 130}]) @[{:from 30 :len 10 :start 130}])
(test (transform-all [{:start 30 :len 20}] [{:src-start 30 :len 5 :dest-start 130} {:src-start 40 :len 5 :dest-start 160}])
  @[{:from 30 :len 5 :start 130}
    {:from 40 :len 5 :start 160}
    {:len 5 :start 35}
    {:len 5 :start 45}])
(test (transform-all
  [{:start 30 :len 10}]
  [{:src-start 25 :len 10 :dest-start 125} {:src-start 35 :len 10 :dest-start 235}])
  @[{:from 30 :len 5 :start 130}
    {:from 35 :len 5 :start 235}])

(defn solve2 [input]
  (def {:seeds seeds :maps maps} (first (peg/match peg input)))
  (->> (reduce |(transform-all $0 $1) seeds maps)
    (map (. :start))
    min-of))

(test (solve2 test-input) 46)
(test (solve2 real-input) 26829166)
