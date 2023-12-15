(use judge)
(use ./util)
(import pat)

(def real-input (slurp "input/15.txt"))

(def test-input ```
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7

```)

(defn hash [input]
  (var state 0)
  (each c input
    (+= state c)
    (*= state 17)
    (%= state 256))
  state)

(defn solve [input]
  (sum (map hash (string/split (string/trim input) ","))))

(test (hash "rn=1") 30)
(test (solve test-input) 1320)
(test (solve real-input) 514281)

(defn parse [input]
  (pat/match (string/split input "=")
    [label x] [:set label (scan-number x)]
    [_] (pat/match (string/split input "-")
      [label ""] [:remove label])))

(defn ot/new []
  @{:index @{} :values @[]})

(defn ot/set [{:index index :values values} key value]
  (if-let [i (in index key)]
    (set (values i) value)
    (do
      (put index key (length values))
      (array/push values value))))

(defn ot/remove [{:index index :values values} key]
  (when-let [i (in index key)]
    (array/remove values i)
    (put index key nil)
    (loop [[k ki] :pairs index :when (> ki i)]
      (-- (index k)))))

(defn solve2 [input]
  (def inputs (map parse (string/split (string/trim input) ",")))

  (def boxes @[])
  (for i 0 256
    (array/push boxes (ot/new)))

  (each input inputs
    (pat/match input
      [:remove label] (ot/remove (in boxes (hash label)) label)
      [:set label value] (ot/set (in boxes (hash label)) label value)))

  (sum-loop [[box-num box] :pairs boxes
             [lens-num focal-length] :pairs (box :values)]
     (* (+ 1 box-num) (+ 1 lens-num) focal-length)))

(test (solve2 test-input) 145)
(test (solve2 real-input) 244199)
