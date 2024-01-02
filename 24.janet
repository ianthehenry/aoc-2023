(use judge)
(use ./util)
(use cmp/import)
(import pat)
(use jimmy)

(def real-input (slurp "input/24.txt"))

(def test-input ```
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3

```)

(def peg
  ~{:line (/ (* :p :s+ "@" :s+ :p) ,|{:p $0 :v $1})
    :p (/ (* :num :sep :num :sep :num) ,tuple)
    :num (number (* (? "-") :d+))
    :sep (* "," :s+)
    :main (some (* :line (? "\n")))
   })

(defn parse [input]
  (peg/match peg input))

(defn drop-z [[x y z]] [x y])
(defn drop-y [[x y z]] [x z])
(defn drop-x [[x y z]] [y z])

(defn xy [{:p p :v v}] {:p (drop-z p) :v (drop-z v)})
(defn xz [{:p p :v v}] {:p (drop-y p) :v (drop-y v)})
(defn yz [{:p p :v v}] {:p (drop-x p) :v (drop-x v)})

(test (parse test-input)
  @[{:p [19 13 30] :v [-2 1 -2]}
    {:p [18 19 22] :v [-1 -1 -2]}
    {:p [20 25 34] :v [-2 -2 -4]}
    {:p [12 31 28] :v [-1 -2 -1]}
    {:p [20 19 15] :v [1 -5 -3]}])

(test (map xy (parse test-input))
  @[{:p [19 13] :v [-2 1]}
    {:p [18 19] :v [-1 -1]}
    {:p [20 25] :v [-2 -2]}
    {:p [12 31] :v [-1 -2]}
    {:p [20 19] :v [1 -5]}])

(defn cross [[x1 y1] [x2 y2]]
  (- (* x1 y2) (* y1 x2)))

(def epsilon 0.00001)

(defn point-on-line [p origin v]
  (def p (vec- p origin))
  (def dist (/ (vec/dot p v) (vec/dot v v)))
  (if (>= dist 0)
    dist))

(defn intersection [{:p p1 :v v1} {:p p2 :v v2}]
  (cond
    (and (= v1 [0 0]) (= v2 [0 0])) (if (= p1 p2) p1 nil)
    (= v1 [0 0]) (vec*n v2 (point-on-line p1 p2 v2))
    (= v2 [0 0]) (vec*n v1 (point-on-line p2 p1 v1))
    (do
      (def start-to-start (vec- p2 p1))
      (def crossed (cross v1 v2))
      (if (< (math/abs crossed) epsilon)
        nil # colinear
        (let [dist1 (/ (cross start-to-start v2) crossed)
              dist2 (/ (cross start-to-start v1) crossed)]
          (if (and (>= dist1 0) (>= dist2 0))
            (vec+ p1 (vec*n v1 dist1))))))))

(test (intersection {:p [-10 0] :v [1 0]} {:p [-1 -10] :v [0 1]}) [-1 0])

(defn inside? [[x y] [[x-lo x-hi] [y-lo y-hi]]]
  (and
    (>= x x-lo)
    (<= x x-hi)
    (>= y y-lo)
    (<= y y-hi)))

(defn solve [input bounds]
  (sum-loop [[a b] :in (unordered-pairs (map xy (parse input)))
        :let [where (intersection a b)] :when where
        :when (inside? where bounds)]
    1))

(def test-bounds [[7 27] [7 27]])
(def real-bounds (|[$ $] [200000000000000 400000000000000]))

(test (solve test-input test-bounds) 2)
(test (solve real-input real-bounds) 18651)

(defn intersect-dist [{:p p1 :v v1} {:p p2 :v v2}]
  (cond
    (and (= v1 [0 0]) (= v2 [0 0])) (if (= p1 p2) 0 nil)
    (= v1 [0 0]) (point-on-line p1 p2 v2)
    (= v2 [0 0]) (point-on-line p2 p1 v1)
    (do
      (def start-to-start (vec- p2 p1))
      (def crossed (cross v1 v2))
      (if (< (math/abs crossed) epsilon)
        nil # colinear
        (let [dist1 (/ (cross start-to-start v2) crossed)
              dist2 (/ (cross start-to-start v1) crossed)]
          (if (and (>= dist1 0) (>= dist2 0))
            dist1))))))

(defn intersects-3d? [p1 p2]
  (def a (intersect-dist (xy p1) (xy p2)))
  (def b (intersect-dist (xz p1) (xz p2)))
  #(def c (intersect-dist (yz p1) (yz p2)))
  (and a b (= a b)))

(defn intersection-3d [p1 p2]
  (def a (intersect-dist (xy p1) (xy p2)))
  (def b (intersect-dist (xz p1) (xz p2)))
  (if (and a b (= a b))
    (vec+ (p1 :p) (vec*n (p1 :v) a))))

(test (intersects-3d?
  {:p [0 0 0] :v [0 1 0]}
  {:p [5 5 0] :v [-1 0 0]})
  true)

(test (intersection-3d
  {:p [0 0 0] :v [0 1 0]}
  {:p [5 5 0] :v [-1 0 0]})
  [0 5 0])

(test (intersects-3d?
  {:p [0 0 0] :v [0 1 0]}
  {:p [5 6 0] :v [-1 0 0]})
  false)

(test (intersects-3d?
  {:p [0 0 0] :v [1 1 3]}
  {:p [0 0 0] :v [-1 1 -2]})
  true)

(defn nearly= [a b]
  (< (math/abs (- a b)) 0.1))

(defn vec/nearly= [v1 v2]
  (all nearly= v1 v2))

(test (vec/nearly= [1 2 3] [1.00001 2 3]) true)

(def offset [0 0 0])
(defn solve2 [input [[x-lo x-hi] [y-lo y-hi] [z-lo z-hi]]]
  (def flakes (parse input))
  (def [a b c & _] flakes)
  (var result nil)

  (def xy-candidates
    (catseq [vx :range [x-lo x-hi]
             vy :range [y-lo y-hi]]
      (def rock-v [vx vy 0])
      (defn adjust [{:p p :v v}]
        {:p (vec- p offset) :v (vec- v rock-v)})

      (def ab (intersection (xy (adjust a)) (xy (adjust b))))
      (def ac (intersection (xy (adjust a)) (xy (adjust c))))
      (if (and ab ac (vec/nearly= ab ac))
        [[vx vy]]
        [])))

  (loop [[vx vy] :in xy-candidates
         vz :range [z-lo z-hi]]
    (def rock-v [vx vy vz])
    (defn adjust [{:p p :v v}]
      {:p (vec- p offset) :v (vec- v rock-v)})

    (def ab
      (if-let [[x y] (intersection (xy (adjust a)) (xy (adjust b)))
               [x2 z] (intersection (xz (adjust a)) (xz (adjust b)))]
        (if (nearly= x x2)
          [x y z])))
    (def ac
      (if-let [[x y] (intersection (xy (adjust a)) (xy (adjust c)))
               [x2 z] (intersection (xz (adjust a)) (xz (adjust c)))]
        (if (nearly= x x2)
          [x y z])))
    (when (and ab ac (vec/nearly= ab ac))
      (set result (vec+ ab offset))
      (break)))

  result)


(test (solve2 test-input [[-10 10] [-10 10] [-10 10]])
  [24 13 10])

# eyeballing this in toodle.studio implies it's around [130 250 100]
(test (solve2 real-input [[100 150] [200 300] [75 125]])
  [200027938836082
   127127087242193
   219339468239370])
