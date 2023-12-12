(use judge)
(use ./util)
(import pat)

(def test-input ```
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483

```)

(def peg
  ~{:line (/ (* ':w+ " " (number :d+)) ,|{:hand (map string/from-bytes $0) :bid $1})
    :main (some (* :line "\n"))
    })

(test (peg/match peg test-input)
  @[{:bid 765 :hand @["3" "2" "T" "3" "K"]}
    {:bid 684 :hand @["T" "5" "5" "J" "5"]}
    {:bid 28 :hand @["K" "K" "6" "7" "7"]}
    {:bid 220 :hand @["K" "T" "J" "J" "T"]}
    {:bid 483 :hand @["Q" "Q" "Q" "J" "A"]}])

(defn card-strength [card]
  (pat/match card
    "T" 10
    "J" 11
    "Q" 12
    "K" 13
    "A" 14
    c (scan-number c)))

(test (card-strength "A") 14)
(test (card-strength "1") 1)
(test (card-strength "2") 2)

(defn strength-of-shape [shape]
  (pat/match shape
    [5] 6
    [1 4] 5
    [2 3] 4
    [1 1 3] 3
    [1 2 2] 2
    [1 1 1 2] 1
    [1 1 1 1 1] 0))

(defn hand-strength [hand]
  (strength-of-shape (sort (values (frequencies hand)))))

(test (hand-strength @["K" "K" "6" "7" "7"]) 2)
(test (hand-strength @["K" "K" "7" "7" "7"]) 4)

(def real-input (slurp "input/07.txt"))

(defn break-tie [hand1 hand2]
  (cmp-each
    (map card-strength hand1)
    (map card-strength hand2)))

(test (break-tie ["A" "2"] ["K" "2"]) 1)
(test (break-tie ["A" "2"] ["A" "2"]) 0)
(test (break-tie ["A" "2"] ["A" "3"]) -1)
(test (break-tie ["A" "3"] ["A" "2"]) 1)

(defn solve [input]
  (def plays
    (sorted (peg/match peg input) (comparing
      (by (. :hand) (by hand-strength))
      (by (. :hand) break-tie))))
  (sum-loop [[i {:bid bid}] :pairs plays]
    (* (+ i 1) bid)))

(test (solve test-input) 6440)
(test (solve real-input) 256448566)

#############################3

(defn card-strength [card]
  (pat/match card
    "T" 10
    "J" 1
    "Q" 12
    "K" 13
    "A" 14
    c (scan-number c)))

(defn break-tie [hand1 hand2]
  (cmp-each
    (map card-strength hand1)
    (map card-strength hand2)))

(defn hand-strength [hand]
  (def without-jokers (filter |(not= $ "J") hand))
  (def joker-count (- (length hand) (length without-jokers)))
  (def shape (sort (values (frequencies without-jokers))))

  (array/push shape (+ joker-count (or (array/pop shape) 0)))
  (strength-of-shape shape))

(test (hand-strength @["K" "K" "6" "7" "7"]) 2)
(test (hand-strength @["K" "K" "7" "7" "7"]) 4)

(defn solve2 [input]
  (def plays
    (sorted (peg/match peg input) (comparing
      (by (. :hand) (by hand-strength))
      (by (. :hand) break-tie))))

  (sum-loop [[i {:bid bid}] :pairs plays]
    (* (+ i 1) bid)))

(test (solve2 test-input) 5905)
(test (solve2 real-input) 254412181)
