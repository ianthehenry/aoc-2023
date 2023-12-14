(use judge)
(use ./util)
(import pat)
(use jimmy)

(def test-input ```
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#

```)

(defn prefix-hamming-distance [a b]
  (sum-loop [i :range [0 (min (length a) (length b))]]
    (if (= (a i) (b i)) 0 1)))

(defn is-prefix [a b] (= 0 (prefix-hamming-distance a b)))

(test (is-prefix "a" "a") true)
(test (is-prefix "ab" "a") true)
(test (is-prefix "ba" "a") false)

(test (prefix-hamming-distance "a" "a") 0)
(test (prefix-hamming-distance "ab" "a") 0)
(test (prefix-hamming-distance "ba" "a") 1)
(test (prefix-hamming-distance "...#.." "...#...") 0)
(test (prefix-hamming-distance "...#.." "...#.#.") 1)
(test (prefix-hamming-distance "...#.." "...###.") 2)

(defn valid-mirrors [str]
  (set/of (seq [i :range [1 (length str)]
                :let [left (string/slice str 0 i) right (string/slice str i)]
                :when (is-prefix (string/reverse left) right)]
    i)))

(test (valid-mirrors ".##..##") "{2 4 6}")

(defn solve-one [lines]
  (def horizontal (string/split lines "\n"))
  (def vertical (map string/from-bytes ;horizontal))

  (def horizontal-candidates (set/to-tuple (set/intersection ;(map |(valid-mirrors $) horizontal))))
  (def vertical-candidates (set/to-tuple (set/intersection ;(map |(valid-mirrors $) vertical))))

  (+
    (pat/match horizontal-candidates [] 0 [only] only)
    (pat/match vertical-candidates [] 0 [only] (* 100 only))))

(defn solve [input]
  (sum-loop [board :in (string/split (string/trim input) "\n\n")]
    (solve-one board)))

(def real-input (slurp "input/13.txt"))

(test (solve test-input) 405)
(test (solve real-input) 34202)

(defn possible-mirrors [str]
  (seq [i :range [1 (length str)]
        :let [left (string/slice str 0 i) right (string/slice str i)]
        :let [distance (prefix-hamming-distance (string/reverse left) right)]
        :when (<= distance 1)]
    [distance i]))

(test (possible-mirrors ".##..##") @[[1 1] [0 2] [0 4] [0 6]])

(defn find-smudge [lines]
  (def freqs (frequencies (mapcat |(possible-mirrors $) lines)))
  (def wavelengths (multinvert freqs))
  (def all-but-one (- (length lines) 1))
  (seq [[distance index] :in (in wavelengths all-but-one [])
    :when (= distance 0)
    :when (= (in freqs [1 index]) 1)]
    index))

(defn solve-one [lines]
  (def horizontal (string/split lines "\n"))
  (def vertical (map string/from-bytes ;horizontal))

  (+
    (pat/match (find-smudge horizontal) [] 0 [only] only)
    (pat/match (find-smudge vertical) [] 0 [only] (* 100 only))))

(test (solve-one ```
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.
```)
  300)

(defn solve2 [input]
  (sum-loop [board :in (string/split (string/trim input) "\n\n")]
    (solve-one board)))

(test (solve2 test-input) 400)
(test (solve2 real-input) 34230)
