(use judge)
(use ./util)
(import pat)
(import jimmy/set)

(def test-input ```
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)

```)
(def real-input (slurp "input/08.txt"))

(def peg
  ~{:node (/ (* ':w+ " = (" ':w+ ", " ':w+ ")") ,|{:name $0 :next [$1 $2]})
    :nodes (group (some (* :node "\n")))
    :instructions (group (some (+ (/ "L" 0) (/ "R" 1))))
    :main (/ (* :instructions "\n\n" :nodes) ,|{:instructions $0 :nodes $1})
    })

(test (peg/match peg test-input)
  @[{:instructions @[1 0]
     :nodes @[{:name "AAA" :next ["BBB" "CCC"]}
              {:name "BBB" :next ["DDD" "EEE"]}
              {:name "CCC" :next ["ZZZ" "GGG"]}
              {:name "DDD" :next ["DDD" "DDD"]}
              {:name "EEE" :next ["EEE" "EEE"]}
              {:name "GGG" :next ["GGG" "GGG"]}
              {:name "ZZZ" :next ["ZZZ" "ZZZ"]}]}])

(defn parse-graph [input]
  (def graph @{})

  (def {:instructions instructions :nodes nodes} (first (peg/match peg input)))

  (each {:name name} nodes
    (put graph name @[]))
  (each {:name name :next [l r]} nodes
    (array/push (in graph name) (in graph l) (in graph r) name))
  {:instructions instructions :graph graph})

(defn solve [input]
  (def {:instructions instructions :graph graph} (parse-graph input))

  (def start (in graph "AAA"))
  (def end (in graph "ZZZ"))
  (var current start)
  (var i 0)
  (while (not= current end)
    (def instruction (in instructions (% i (length instructions))))
    (set current (current instruction))
    (++ i))
  i)

(test (solve test-input) 2)

(def test-input ```
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)

```)
(test (solve test-input) 6)

(test (solve real-input) 12169)

(defn start? [[_ _ name]] (string/has-suffix? "A" name))
(defn end? [[_ _ name]] (string/has-suffix? "Z" name))

(test (start? [0 0 "111A"]) true)
(test (end? [0 0 "111A"]) false)
(test (end? [0 0 "111Z"]) true)

(defn graph-length [node instructions start-index]
  (var cursor node)
  (var steps 0)
  (var cycle false)
  (def visited @{})
  (while (or (= steps 0) (not (end? cursor)))
    (++ steps)
    (def i (% (+ steps start-index) (length instructions)))
    (when (in visited [cursor i])
      (pp ["found cycle" (node 2)])
      (set cycle true)
      (break))
    (put visited [cursor i] true)
    (def instruction (in instructions i))
    (set cursor (in cursor instruction)))
  (if cycle
    nil
    [steps cursor]))

#(defn lengths [cursors]
#  (set/of (seq [[len node] :in cursors] len)))

(defn lengths [cursors]
  (seq [[len node] :in cursors] len))

(defn different-lengths? [cursors]
  (> (length (distinct (lengths cursors))) 1))

(defn solve2 [input]
  (def {:instructions instructions :graph graph} (parse-graph input))

  (def x
  (tabseq [node :in graph :when (start? node)]
    node
    (do
    (def seen @{})
    (var steps 0)
    (var local-steps 0)
    (var cursor node)
    (def lens @[])

    (while true
      (def i (% steps (length instructions)))
      (def instruction (in instructions i))

      (when (seen [cursor i])
        (break))

      (when (end? cursor)
        (array/push lens local-steps)
        (set local-steps 0))
      (put seen [cursor i] true)

      (++ steps)
      (++ local-steps)
      (set cursor (cursor instruction)))

    lens)))

  (each lens x
    (assert (= (length (distinct lens)) 1)
      "this puzzle is stupid because the solution relies on
       the particular structure of the input which is not
       defined anywhere and the fact that terminal states
       cycle with the same length as the initial conditions
       that reach them which is absolutely not true in
       general and this whole thing is stupid"))

  # I really hate this puzzle
  (reduce math/lcm 1
    (seq [[[_ _ name] [len]] :pairs x] len)))

(def test-input ```
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)

```)

(test (solve2 test-input) 6)
(test (solve2 real-input) 12030780859469)
