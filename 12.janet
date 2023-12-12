(use judge)
(use ./util)
(import pat)

(def test-input ```
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
```)

(defn valid-combinations* [inputs group-lengths acc memos]
  (def key [inputs group-lengths acc])
  (when-let [x (in memos key)]
    (break x))

  (def result
    (pat/match inputs
      [] (pat/match [group-lengths acc]
        [[] 0] 1
        [[final []] (= final)] 1
        0)
      ["?" rest]
        (+ (valid-combinations* ["." rest] group-lengths acc memos)
           (valid-combinations* ["#" rest] group-lengths acc memos))
      ["." rest]
        (if (= 0 acc)
          (valid-combinations* rest group-lengths 0 memos)
          (pat/match group-lengths
            [(= acc) rest-group-lengths]
              (valid-combinations* rest rest-group-lengths 0 memos)
            0))
      ["#" rest]
        (valid-combinations* rest group-lengths (inc acc) memos)))
  (put memos key result)
  result)

(defn valid-combinations [inputs group-lengths acc]
  (valid-combinations* inputs group-lengths acc @{}))

(defn to-linked-list [iterable]
  (pat/match iterable
    [] []
    [x & rest] [x (to-linked-list rest)]))

(defn of-linked-list [iterable]
  (pat/match iterable
    [] []
    [x rest] [x ;(of-linked-list rest)]))

(defn options [input group-lengths]
  (valid-combinations
    (to-linked-list (string/split input ""))
    (to-linked-list group-lengths)
    0))

(test (options "###" [3]) 1)
(test (options "##" [3]) 0)
(test (options "####" [3]) 0)
(test (options "###?#" [3 1]) 1)
(test (options "????" [3 1]) 0)
(test (options "?????" [3 1]) 1)
(test (options "??????" [3 1]) 3)

(test (options "???.###" [1 1 3]) 1)
(test (options ".??..??...?##." [1 1 3]) 4)
(test (options "?#?#?#?#?#?#?#?" [1 3 1 6]) 1)
(test (options "????.#...#..." [4 1 1]) 1)
(test (options "????.######..#####." [1 6 5]) 4)
(test (options "?###????????" [3 2 1]) 10)

(defn solve [input]
  (sum-loop [line :in (string/split input "\n") :unless (empty? line)]
    (def [input group-lengths] (string/split line " "))
    (def group-lengths (map scan-number (string/split group-lengths ",")))
    (options input group-lengths)))

(defn solve2 [input]
  (sum-loop [line :in (string/split input "\n") :unless (empty? line)]
    (def [input group-lengths] (string/split line " "))
    (def group-lengths (map scan-number (string/split group-lengths ",")))

    (def input (string/join (seq [i :range [0 5]] input) "?"))
    (def group-lengths (catseq [i :range [0 5]] group-lengths))

    (options input group-lengths)
    ))

(def real-input (slurp "input/12.txt"))

(test (solve test-input) 21)
(test (solve real-input) 7163)

(defn options2 [input group-lengths inflation]
  (def input (string/join (seq [i :range [0 inflation]] input) "?"))
  (def group-lengths (catseq [i :range [0 inflation]] group-lengths))
  (options input group-lengths))

(test (options2 "??#????#.?.#???#?.??" [4 1 1 1 2 1] 1) 23)
(test (options2 "??#????#.?.#???#?.??" [4 1 1 1 2 1] 2) 984)

(defn solve2 [input]
  (sum-loop [line :in (string/split input "\n") :unless (empty? line)]
    (def [input group-lengths] (string/split line " "))
    (def group-lengths (map scan-number (string/split group-lengths ",")))
    (options2 input group-lengths 5)))

(test (solve2 test-input) 525152)

# takes around 2.5s
#(test (solve2 real-input) 17788038834112)

