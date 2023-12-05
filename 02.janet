(use judge)
(use ./util)

# multiline-string literals trim the trailing newline, so we explicitly add another one
(def test-input ```
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

```)

(defn round-to-table [& readings]
  (tabseq [[num color] :in readings]
    color num))

(def peg
  ~{:reading (group (* (number :d+) " " :color))
    :round (/ ,(sep :reading ", ") ,round-to-table)
    :color (+ (/ "blue" :blue) (/ "red" :red) (/ "green" :green))
    :id (* "Game " (number :d+))
    :game (/ (* :id ": " ,(sep :round "; ")) ,|{:id $0 :rounds $&})
    :main (some (* :game "\n"))})

(defn round-max [rounds]
  (reduce (partial merge-with max) @{} rounds))

(test (round-max [{:blue 3 :red 4} {:blue 6 :green 2 :red 1} {:green 2}])
  @{:blue 6 :green 2 :red 4})

(def real-input (slurp "input/02.txt"))

(test (peg/match peg test-input)
  @[{:id 1
     :rounds [@{:blue 3 :red 4}
              @{:blue 6 :green 2 :red 1}
              @{:green 2}]}
    {:id 2
     :rounds [@{:blue 1 :green 2}
              @{:blue 4 :green 3 :red 1}
              @{:blue 1 :green 1}]}
    {:id 3
     :rounds [@{:blue 6 :green 8 :red 20}
              @{:blue 5 :green 13 :red 4}
              @{:green 5 :red 1}]}
    {:id 4
     :rounds [@{:blue 6 :green 1 :red 3}
              @{:green 3 :red 6}
              @{:blue 15 :green 3 :red 14}]}
    {:id 5
     :rounds [@{:blue 1 :green 3 :red 6}
              @{:blue 2 :green 2 :red 1}]}])

(defn solve [input]
  (def possible-games (->> (peg/match peg input)
    (filter (fn [{:id id :rounds rounds}]
      (zip-all <= 0 (round-max rounds) {:red 12 :green 13 :blue 14})))))
  (sum (map (. :id) possible-games)))

(test (solve test-input) 8)
(test (solve real-input) 2528)

(defn solve2 [input]
  (sum (seq [{:id id :rounds rounds} :in (peg/match peg input)]
    (product (round-max rounds)))))

(test (solve2 test-input) 2286)
(test (solve2 real-input) 67363)
