(use judge)
(use ./util)
(use cmp/import)
(import pat)

(def real-input (slurp "input/20.txt"))

(def test-input1 ```
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a

```)

(def test-input2 ```
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output

```)

(def peg
  ~{:rule (/ (* :sigil :name " -> " (some (* :name (? ", ")))) ,|{:name $1 :type $0 :targets $&})
    :name ':w+
    :sigil (+ (/ "&" :and) (/ "%" :ff) (constant :init))
    :main (some (* :rule (? "\n")))})

(defn ref/new [value] @[value])
(defn ref/set [ref value] (set (ref 0) value))
(defn ref/get [ref] (ref 0))
(defn ref/update [ref f] (ref/set ref (f (ref/get ref))))

(defn deq/new [capacity]
  @[0 0 (array/new-filled capacity nil)])

(defn deq/pop [deq]
  (def [start end arr] deq)
  (when (= start end)
    (error "deque is empty"))

  (def new-start (mod (succ start) (length arr)))

  (def x (arr start))
  (set (arr start) nil)
  (set (deq 0) new-start)
  x)

(defn deq/push [deq x]
  (def [start end arr] deq)
  (def new-end (mod (succ end) (length arr)))
  (when (= start new-end)
    (error "deque is full"))

  (set (arr end) x)
  (set (deq 1) new-end)
  nil)

(defn deq/length [[start end arr]]
  (if (<= start end)
    (- end start)
    (+ end (- (length arr) start))))

(deftest "deque"
  (def deq (deq/new 5))
  (deq/push deq 1)
  (test (deq/length deq) 1)
  (deq/push deq 2)
  (deq/push deq 3)
  (test (deq/pop deq) 1)
  (test (deq/pop deq) 2)
  (test (deq/pop deq) 3)
  (test (deq/length deq) 0)
  (deq/push deq 4)
  (deq/push deq 5)
  (deq/push deq 6)
  (test (deq/length deq) 3)
  (test (deq/pop deq) 4)
  (test (deq/length deq) 2)
  (test (deq/pop deq) 5)
  (test (deq/pop deq) 6)
  (test (deq/length deq) 0)
  (test-error (deq/pop deq) "deque is empty"))

(defn make-state [parsed-input]
  (def nodes
    (tabseq [{:name name :targets targets :type type} :in parsed-input]
      name
      {:type type
       :state (pat/match type :and @{} :ff (ref/new false) :init nil)
       :targets targets}))
  (loop [[name {:targets targets}] :pairs nodes
         target-name :in targets
         :let [target-node (in nodes target-name)]
         :when target-node
         :let [{:state target-state :type target-type} target-node]
         :when (= target-type :and)]
      (put target-state name false))
  nodes)

(defn send [event-queue src dest pulse]
  (deq/push event-queue [src dest pulse]))

(defn flush [nodes event-queue on-event]
  (while (> (deq/length event-queue) 0)
    (def event (deq/pop event-queue))
    (on-event event)
    (def [src dest pulse] event)

    (def dest-node (in nodes dest))
    (when dest-node
      (def {:type type :targets targets :state state} dest-node)

      (case type
        :init (each target targets (send event-queue dest target false))
        :ff (when (= pulse false)
          (ref/update state not)
          (each target targets
            (send event-queue dest target (ref/get state))))
        :and (do
          (put state src pulse)
          (def pulse (not (every? state)))
          (each target targets
            (send event-queue dest target pulse)))))))

(defn solve [input]
  (def nodes (make-state (peg/match peg input)))
  (def event-queue (deq/new 100))

  (var lo 0)
  (var hi 0)
  (for i 0 1000
    (send event-queue "button" "broadcaster" false)
    (flush nodes event-queue (fn [[src dest pulse]]
      (if pulse (++ hi) (++ lo)))))

  (* lo hi))

(defn length-until-sends-false [parsed-input focused-node]
  (def nodes (make-state parsed-input))
  (def event-queue (deq/new 100))

  (var cycle 0)
  (var result nil)

  (while (= result nil)
    (++ cycle)
    (send event-queue "button" "broadcaster" false)
    (flush nodes event-queue (fn [[_ dest pulse]]
      (when (and (= dest focused-node) (= pulse false))
        (set result cycle)))))

  result)

(test (peg/match peg test-input1)
  @[{:name "broadcaster"
     :targets ["a" "b" "c"]
     :type :init}
    {:name "a" :targets ["b"] :type :ff}
    {:name "b" :targets ["c"] :type :ff}
    {:name "c" :targets ["inv"] :type :ff}
    {:name "inv" :targets ["a"] :type :and}])
(test (peg/match peg test-input2)
  @[{:name "broadcaster"
     :targets ["a"]
     :type :init}
    {:name "a"
     :targets ["inv" "con"]
     :type :ff}
    {:name "inv" :targets ["b"] :type :and}
    {:name "b" :targets ["con"] :type :ff}
    {:name "con"
     :targets ["output"]
     :type :and}])

(test (solve test-input1) 32000000)
(test (solve test-input2) 11687500)
(test (solve real-input) 808146535)

(defn solve2 [input]
  # okay hardcoding this is gross but whatever
  # i'm over this puzzle
  (def parsed-input (peg/match peg input))
  (def precedents ["kd" "zf" "vg" "gs"])
  (reduce math/lcm 1 (map |(length-until-sends-false parsed-input $) precedents)))

(test (solve2 real-input) 224602953547789)

