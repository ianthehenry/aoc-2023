(use judge)
(use ./util)
(use cmp/import)
(import pat)
(use jimmy)

(def real-input (slurp "input/23.txt"))

(def test-input ```
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#

```)

(defn open? [world pos]
  (case (grid/get world pos)
    "#" false
    nil false
    true))

(defn neighbors [world pos]
  (filter |(open? world $)
    (case (grid/get world pos)
      "." (grid/adjacent pos)
      ">" [(vec+ pos grid/right)]
      "<" [(vec+ pos grid/left)]
      "v" [(vec+ pos grid/down)]
      "^" [(vec+ pos grid/up)]
      (error "wut"))))

(defn traverse [world visited start end]
  (var cursor start)
  (var result nil)
  (while true
    (put visited cursor true)
    (when (= cursor end)
      (set result [visited])
      (break))
    (def nexts (filter |(not (in visited $)) (neighbors world cursor)))

    (if (= (length nexts) 1)
      (set cursor (in nexts 0))
      (do
        (set result (catseq [next-pos :in nexts]
          (def visited-shallow-clone @{})
          (table/setproto visited-shallow-clone visited)
          (traverse world visited-shallow-clone next-pos end)))
        (break))))
  result)

(defn deep-length [t]
  (var len 0)
  (var t t)
  (while (not= nil t)
    (+= len (length t))
    (set t (table/getproto t)))
  len)

(defn solve [input]
  (def world (grid/parse input))
  (def start (vec+ (grid/top-left world) [0 1]))
  (def end (vec+ (grid/bottom-right world) [0 -1]))

  (max
  ;(seq [path :in (traverse world @{} start end)]
    # subtract the initial position
    (- (deep-length path) 1))
  ))

#(test (solve test-input) 94)
#(test (solve real-input) 2166)

# ---------------------------

(defn neighbors [world pos]
  (filter |(open? world $) (grid/adjacent pos)))

# okay. we only need to traverse each cell one time.
# we'll identify a node by its position on the grid.
# when we reach a new junction that we've never reached before,
# we'll begin a recursive call.

# if there are multiple edges between two nodes,
# we'll take the longest one. edges are always bidirectional
(defn add-edge [node1 node2 len]
  (def existing-edge (in (node1 :edges) node2))
  (when (and existing-edge (>= existing-edge len))
    (break))
  (put (node1 :edges) node2 len)
  (put (node2 :edges) node1 len))

(defn traverse [world graph this-node last-pos start]
  (var edge-length 1)
  (var last-pos last-pos)
  (var cursor start)
  (while true
    (when-let [junction (graph cursor)]
      (add-edge this-node junction edge-length)
      (break))

    (def nexts (filter |(not= last-pos $) (neighbors world cursor)))
    (case (length nexts)
      0 (break)
      1 (do
        (++ edge-length)
        (set last-pos cursor)
        (set cursor (in nexts 0)))
      (do
        # if you found multiple neighbors, create a new node
        (def new-node @{:edges @{} :pos cursor})
        (put graph cursor new-node)
        (add-edge this-node new-node edge-length)
        (each neighbor nexts
          # and traverse in every direction
          (traverse world graph new-node cursor neighbor))
        (break)))))

(defn build-graph [world start end]
  (def graph @{})
  (def start-node @{:edges @{} :pos start})
  (def end-node @{:edges @{} :pos end})
  (put graph start start-node)
  (put graph end end-node)
  (traverse world graph start-node start (vec+ start [1 0]))
  graph)

(defn print-graph [input]
  (def world (grid/parse input))
  (def start (vec+ (grid/top-left world) [0 1]))
  (def end (vec+ (grid/bottom-right world) [0 -1]))
  (defn show [pos]
    (pat/match pos
      (= start) "start"
      (= end) "end"
      [r c] (string/format "%d,%d" r c)))

  (each {:edges edges :pos pos} (cmp/sort (values (build-graph world start end)) (by :pos))
    (print (show pos))
    (each [{:pos dest} len] (cmp/sort (pairs edges) (by 0 (by :pos)))
      (printf "  %q -> %s" len (show dest)))))

(test-stdout (print-graph `
#.###
#...#
###.#
`) `
  start
    4 -> end
  end
    4 -> start
`)

(test-stdout (print-graph `
#.###
#...#
#.#.#
#...#
###.#
`) `
  start
    1 -> 1,1
  1,1
    1 -> start
    4 -> 3,3
  3,3
    4 -> 1,1
    1 -> end
  end
    1 -> 3,3
`)

# if there are multiple edges between
# two vertices, only remember the longest edge
(test-stdout (print-graph `
#.#####
#.#...#
#...#.#
#.###.#
#.....#
#####.#
`) `
  start
    2 -> 2,1
  2,1
    2 -> start
    8 -> 4,5
  4,5
    8 -> 2,1
    1 -> end
  end
    1 -> 4,5
`)

(test-stdout (print-graph test-input) `
  start
    15 -> 5,3
  3,11
    22 -> 5,3
    30 -> 11,21
    24 -> 13,13
  5,3
    15 -> start
    22 -> 3,11
    22 -> 13,5
  11,21
    30 -> 3,11
    18 -> 13,13
    10 -> 19,19
  13,5
    22 -> 5,3
    12 -> 13,13
    38 -> 19,13
  13,13
    24 -> 3,11
    18 -> 11,21
    12 -> 13,5
    10 -> 19,13
  19,13
    38 -> 13,5
    10 -> 13,13
    10 -> 19,19
  19,19
    10 -> 11,21
    10 -> 19,13
    5 -> end
  end
    5 -> 19,19
`)

(defn traverse [graph len-so-far visited node end-node]
  (if (= node end-node)
    [len-so-far]
    (let [visited (set/add visited node)]
      (catseq [[neighbor len] :pairs (node :edges) :when (not (visited neighbor))]
        (traverse graph (+ len-so-far len) visited neighbor end-node)))))

(defn solve [input]
  (def world (grid/parse input))
  (def start (vec+ (grid/top-left world) [0 1]))
  (def end (vec+ (grid/bottom-right world) [0 -1]))
  (def graph (build-graph world start end))
  (max-of (traverse graph 0 set/empty (in graph start) (in graph end)))
  )

(test (solve test-input) 154)
# takes 20s
#(test (solve real-input) 6378)
