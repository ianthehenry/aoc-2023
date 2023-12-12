(use judge)
(use ./util)
(import pat)
(import jimmy/set)

(def test-input ```
..F7.
.FJ|.
SJ.L7
|F--J
LJ...

```)
(def real-input (slurp "input/10.txt"))

(defn parse [input]
  (map |(string/split $ "") (string/split (string/trim input) "\n")))

(def test-atlas (test (parse test-input)
                  @[@["." "." "F" "7" "."]
                    @["." "F" "J" "|" "."]
                    @["S" "J" "." "L" "7"]
                    @["|" "F" "-" "-" "J"]
                    @["L" "J" "." "." "."]]))

(defn lookup [atlas [l c]]
  (if-let [line (get atlas l)]
    (get line c)))

(defn north [[l c]] [(- l 1) c])
(defn south [[l c]] [(+ l 1) c])
(defn west [[l c]] [l (- c 1)])
(defn east [[l c]] [l (+ c 1)])

(defn neighbors [p] (seq [dir :in [north south east west]] (dir p)))
(defn neighbors+ [p] (seq [hdir :in [east west identity]
                           vdir :in [north south identity]] (vdir (hdir p))))

(test (neighbors+ [0 0])
  @[[-1 1]
    [1 1]
    [0 1]
    [-1 -1]
    [1 -1]
    [0 -1]
    [-1 0]
    [1 0]
    [0 0]])

(defn connected-neighbors [atlas p]
  (def c (lookup atlas p))

  (case c
    nil []
    "|" [(north p) (south p)]
    "-" [(east p) (west p)]
    "L" [(north p) (east p)]
    "J" [(north p) (west p)]
    "7" [(south p) (west p)]
    "F" [(south p) (east p)]
    "." []
    "S" (seq [other :in (neighbors p)
              :when (contains? (connected-neighbors atlas other) p)]
          other)
    (errorf "unknown char %q" c)))

(test (connected-neighbors test-atlas [0 0]) [])
(test (connected-neighbors test-atlas [2 0]) @[[3 0] [2 1]])
(test (connected-neighbors test-atlas [3 0]) [[2 0] [4 0]])
(test (connected-neighbors test-atlas [2 1]) [[1 1] [2 0]])

(defn find-start [atlas]
  (var result nil)
  (loop [[l line] :pairs atlas
         [c char] :pairs line
         :when (= char "S")]
    (set result [l c])
    (break))
  result)

(test (find-start test-atlas) [2 0])
(test (empty? set/empty) true)

(defn solve [input]
  (def atlas (parse input))
  (var visited set/empty)
  (var cursors (set/new (find-start atlas)))
  (var steps -1)

  (def connected-neighbors (partial connected-neighbors atlas))

  (while (not (empty? cursors))
    (+= visited cursors)
    (set cursors (- (set/of (mapcat connected-neighbors cursors)) visited))
    (++ steps))

  steps)

(test (solve test-input) 8)
# takes around 5 seconds to run
#(test (solve real-input) 6733)

(defn find-loop [atlas]
  (var visited set/empty)
  (var cursors (set/new (find-start atlas)))
  (def connected-neighbors (partial connected-neighbors atlas))
  (while (not (empty? cursors))
    (+= visited cursors)
    (set cursors (- (set/of (mapcat connected-neighbors cursors)) visited)))
  visited)

(def test-input `
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
`)

(def test-atlas (parse test-input))

(defn atlas/area [atlas]
  (* (length atlas) (length (first atlas))))

(test (atlas/area test-atlas) 99)

(def test-loop (find-loop test-atlas))

# we pretend that the map is bigger than it is during flood-filling
# so that the pipes can't partition the atlas
(defn atlas/contains-edge? [atlas edge]
  (all (fn [[l c]]
    (and
      (>= l -1)
      (>= c -1)
      (<= l (length atlas))
      (<= c (length (first atlas)))))
    edge))

(defn atlas/contains-point? [atlas [l c]]
  (and
    (>= l 0)
    (>= c 0)
    (< l (length atlas))
    (< c (length (first atlas)))))

(defn edges-of [p]
  (set/new
    (set/new p (north p))
    (set/new p (south p))
    (set/new p (east p))
    (set/new p (west p))))

(defn adjacent? [p1 p2]
  (= (manhattan-distance p1 p2) 1))

(defn set/mapcat [f s]
  (set/union ;(map |(set/of (f $)) s)))

(defn adjacent-edges [edge]
  (def ps (values (set/intersection ;(map (comp set/of neighbors+) edge))))
  (assert (= (length ps) 6))
  (set/of
    (seq [i :range [0 6]
          j :range [i 6]
          :let [p1 (in ps i) p2 (in ps j)]
          :when (adjacent? p1 p2)]
      (set/new p1 p2))))

(test (values (adjacent-edges (set/new [1 1] [1 2])))
  @["{(1 1) (1 2)}"
    "{(0 1) (0 2)}"
    "{(2 1) (2 2)}"
    "{(1 1) (2 1)}"
    "{(1 1) (0 1)}"
    "{(2 2) (1 2)}"
    "{(0 2) (1 2)}"])

(defn edge-neighbors [atlas edge]
  (set/filter (adjacent-edges edge) (partial atlas/contains-edge? atlas)))

(test (edge-neighbors test-atlas (set/new [1 1] [1 2])) "{<jimmy/set {(1 1) (1 2)}> <jimmy/set {(0 1) (0 2)}> <jimmy/set {(2 1) (2 2)}> <jimmy/set {(1 1) (2 1)}> <jimmy/set {(1 1) (0 1)}> <jimmy/set {(2 2) (1 2)}> <jimmy/set {(0 2) (1 2)}>}")
(test (edge-neighbors test-atlas (set/new [0 9] [0 10])) "{<jimmy/set {(0 9) (0 10)}> <jimmy/set {(0 9) (1 9)}> <jimmy/set {(0 9) (-1 9)}> <jimmy/set {(0 10) (1 10)}> <jimmy/set {(0 10) (-1 10)}> <jimmy/set {(-1 9) (-1 10)}> <jimmy/set {(1 9) (1 10)}>}")

(test (set/difference
  (set/new (set/new [0 0] [0 1]) (set/new [0 0] [1 0]))
  (set/new (set/new [0 0] [0 1])))
  "{<jimmy/set {(0 0) (1 0)}>}")

(defn edge-flood-fill [atlas obstructed edge]
  (var lake (set/new edge))
  (var last-shore lake)
  (while (not (empty? last-shore))
    (def shore (- (set/union ;(map |(edge-neighbors atlas $) last-shore)) lake obstructed))
    (set last-shore shore)
    (+= lake shore))
  lake)

(defn contains-point? [set-of-edges p]
  (set/subset? (edges-of p) set-of-edges))

(defn points-of-edges [set-of-edges]
  (def possible-points (set/of (seq [edge :in set-of-edges p :in edge] p)))
  (set/filter possible-points (partial contains-point? set-of-edges)))

(test (points-of-edges (edges-of [0 0])) "{(0 0)}")
(test (points-of-edges (+ (edges-of [0 0]) (edges-of [1 0]))) "{(0 0) (1 0)}")
(test (points-of-edges (+ (edges-of [0 0]) (edges-of [3 4]))) "{(3 4) (0 0)}")

(test (edges-of [0 0]) "{<jimmy/set {(0 -1) (0 0)}> <jimmy/set {(0 1) (0 0)}> <jimmy/set {(0 0) (-1 0)}> <jimmy/set {(0 0) (1 0)}>}")

(defn inflate [edge-set]
  (set/union ;(map adjacent-edges edge-set)))

(test (edges-of [0 0]) "{<jimmy/set {(0 -1) (0 0)}> <jimmy/set {(0 1) (0 0)}> <jimmy/set {(0 0) (-1 0)}> <jimmy/set {(0 0) (1 0)}>}")
(test (values (inflate (edges-of [0 0])))
  @["{(-1 1) (-1 0)}"
    "{(-1 -1) (-1 0)}"
    "{(1 1) (1 0)}"
    "{(1 -1) (1 0)}"
    "{(0 -1) (0 0)}"
    "{(0 1) (0 0)}"
    "{(1 -1) (0 -1)}"
    "{(1 1) (0 1)}"
    "{(-1 1) (0 1)}"
    "{(-1 -1) (0 -1)}"
    "{(0 0) (-1 0)}"
    "{(0 0) (1 0)}"])
(test (points-of-edges (inflate (edges-of [0 0]))) "{(0 0)}")

(defn obstructed-edges [atlas loop]
  (set/of (catseq [p :in loop]
    (def neighbors (connected-neighbors atlas p))
    (map |(set/new p $) neighbors))))

(test (length (values (obstructed-edges test-atlas test-loop))) 46)

(test (take 5 (values (obstructed-edges test-atlas test-loop)))
  ["{(3 9) (2 9)}"
   "{(1 3) (1 4)}"
   "{(1 1) (2 1)}"
   "{(7 7) (7 8)}"
   "{(4 1) (3 1)}"])

(defn solve [input]
  (def atlas (parse input))
  (def loop (find-loop atlas))
  (def flooded-edges (edge-flood-fill atlas (obstructed-edges atlas loop) (set/new [-1 0] [0 0])))
  (def actual-points (filter |(atlas/contains-point? atlas $) (points-of-edges flooded-edges)))
  (- (atlas/area atlas) (length loop) (length actual-points)))

(def flooded-edges (edge-flood-fill test-atlas (obstructed-edges test-atlas test-loop) (set/new [-1 0] [0 0])))
(test (length flooded-edges) 187)
(test (length (filter |(atlas/contains-point? test-atlas $) (points-of-edges flooded-edges))) 49)
(test (length test-loop) 46)

(test (solve test-input) 4)

(test (solve `
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
`) 8)

(test (solve `
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
`) 10)

# this takes like 7 seconds to run
#(test (solve real-input) 435)
