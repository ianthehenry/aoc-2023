(use judge)
(use ./util)
(import pat)
(use jimmy)
(use cmp/import)

(def real-input (slurp "input/14.txt"))

(def test-input ```
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
```)

(defn in-bounds? [[w h] [l c]]
  (and
    (>= l 0)
    (>= c 0)
    (< l h)
    (< c w)))

(defn board/obstructed? [{:walls walls :rocks rocks :bounds bounds} pos]
  (or
    (in walls pos)
    (in rocks pos)
    (not (in-bounds? bounds pos))))

(defn board/move-rock [{:rocks rocks} from to]
  (when (= from to) (break))
  (unless (in rocks from)
    (error "no rock there"))
  (when (in rocks to)
    (error "already a rock there"))
  (put rocks from nil)
  (put rocks to true)
  nil)

(defn board/tilt [board dir comparator]
  (def rocks-by-line (cmp/sort (keys (board :rocks)) comparator))

  (each pos rocks-by-line
    # my kingdom for a do-while
    (var new-pos pos)
    (var candidate (vec+ pos dir))
    (while (not (board/obstructed? board candidate))
      (set new-pos candidate)
      (vec+= candidate dir))
    (board/move-rock board pos new-pos))
  board)

(defn board/tilt-north [board] (board/tilt board [-1 0] (by 0)))
(defn board/tilt-south [board] (board/tilt board [1 0] (by 0 desc)))
(defn board/tilt-west [board] (board/tilt board [0 -1] (by 1)))
(defn board/tilt-east [board] (board/tilt board [0 1] (by 1 desc)))

(defn board/cycle [board]
  (-> board
    (board/tilt-north)
    (board/tilt-west)
    (board/tilt-south)
    (board/tilt-east)))

(defn board/parse [input]
  (def rocks @{})
  (def walls @{})

  (def lines (string/split (string/trim input) "\n"))

  (eachp [l line] lines
    (eachp [c char] line
      (case char
        (chr "O") (put rocks [l c] true)
        (chr "#") (put walls [l c] true))))

  @{:rocks rocks
    :walls walls
    :bounds [(length (first lines)) (length lines)]})

(defn board/print [{:walls walls :rocks rocks :bounds [w h]}]
  (for l 0 h
    (for c 0 w
      (def pos [l c])
      (prin
      (cond
        (walls pos) "#"
        (rocks pos) "O"
        ".")))
    (print)))

(defn board/calculate-load [{:rocks rocks :bounds [w h]}]
  (sum-loop [[l c] :keys rocks] (- h l)))

(test-stdout (board/print (board/parse test-input)) `
  O....#....
  O.OO#....#
  .....##...
  OO.#O....O
  .O.....O#.
  O.#..O.#.#
  ..O..#O..O
  .......O..
  #....###..
  #OO..#....
`)

(test-stdout (board/print (board/tilt-north (board/parse test-input))) `
  OOOO.#.O..
  OO..#....#
  OO..O##..O
  O..#.OO...
  ........#.
  ..#....#.#
  ..O..#.O.O
  ..O.......
  #....###..
  #....#....
`)

(test-stdout (board/print (board/cycle (board/parse test-input))) `
  .....#....
  ....#...O#
  ...OO##...
  .OO#......
  .....OOO#.
  .O#...O#.#
  ....O#....
  ......OOOO
  #...O###..
  #..OO#....
`)

(defn solve [input]
  (board/calculate-load (board/tilt-north (board/parse input))))

(test (solve test-input) 136)
(test (solve real-input) 103333)

# this is a bit faster than using the set of rocks
(defn board/signature [{:rocks rocks :bounds [w h]}]
  (def buf (buffer/new (* w h)))
  (for l 0 h
    (for c 0 w
      (buffer/push-byte buf (if (rocks [l c]) (chr "O") (chr ".")))))
  (string/slice buf))

(test (board/signature (board/parse test-input)) "O.........O.OO................OO..O....O.O.....O..O....O......O...O..O.......O.............OO.......")

# mutates its input!
# the returned board will be at the beginning of its cycle
(defn unique-board-cycles [board]
  (var steps 0)
  (def seen @{})

  (var cycle-start nil)

  (while true
    (def sig (board/signature board))
    (when-let [start (seen sig)]
      (set cycle-start start)
      (break))
    (put seen sig steps)
    (++ steps)
    (board/cycle board))

  [steps cycle-start])

(defn solve2 [board]
  (def [steps-taken cycle-start]
    (unique-board-cycles board))
  (def cycle-length (- steps-taken cycle-start))
  (def target-steps 1000000000)
  (for i 0 (% (- target-steps steps-taken) cycle-length)
    (board/cycle board))
  (board/calculate-load board))

(test (solve2 (board/parse test-input)) 64)

# takes around 5s
# (test (solve2 (board/parse real-input)) 97241)
