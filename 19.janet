(use judge)
(use ./util)
(use cmp/import)
(import pat)

(def real-input (slurp "input/19.txt"))

(def test-input ```
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}

```)

(defn evaluate-condition [{:lhs lhs :op op :rhs rhs} part]
  (def lhs (in part lhs))
  (pat/match op
    ">" (> lhs rhs)
    "<" (< lhs rhs)))

(def peg
  ~{:rule (/ (* ':w+ "{" (some (* :instruction (? ","))) "}") ,|{:name $0 :instructions $&})
    :instruction (+
      (/ "A" :accept)
      (/ "R" :reject)
      (/ (* :field '(+ "<" ">") (number :d+) ":" :command) ,|[:if {:lhs $0 :op $1 :rhs $2} $3])
      :command)
    :field (/ '1 {"x" 0 "m" 1 "a" 2 "s" 3})
    :command (+ (/ "A" :accept) (/ "R" :reject) (/ ':w+ ,|[:goto $0]))
    :part (group (* "{x=" (number :d+) ",m=" (number :d+) ",a=" (number :d+) ",s=" (number :d+) "}"))
    :main (/ (*
      (group (some (* :rule "\n")))
      "\n"
      (group (some (* :part (? "\n")))))
    ,|{:rules $0 :parts $1}
    )})

(defn to-linked-list [iterable]
  (pat/match iterable
    [] []
    [x & rest] [x (to-linked-list rest)]))

(defn car [[car _]] car)
(defn cdr [[_ cdr]] cdr)

(defn parse-input [input]
  (def [{:rules rules :parts parts}] (peg/match peg input))
  (def rules
    (tabseq [{:name name :instructions instructions} :in rules]
      name (to-linked-list instructions)))
  {:rules rules :parts parts})

(defn solve [input]
  (def {:rules rules :parts parts} (parse-input input))

  (def accepted @[])
  (var state nil)

  (defn evaluate [command part]
    (pat/match command
      :accept (do (array/push accepted part) (set state nil))
      :reject (set state nil)
      [:goto next-rule] (set state (rules next-rule))))

  (each part parts
    (set state (rules "in"))
    (while (not= state nil)
      (pat/match state
        [[:if condition command] rest]
          (if (evaluate-condition condition part)
            (evaluate command part)
            (set state rest))
        [command _] (evaluate command part))))

  (sum-loop [part :in accepted field :in part] field))

(test (solve test-input) 19114)
(test (solve real-input) 409898)

# try to be fast
(defn set-index [t i v]
  [(if (= i 0) v (t 0))
   (if (= i 1) v (t 1))
   (if (= i 2) v (t 2))
   (if (= i 3) v (t 3))])

(defn update-lower-bound [value index new-lo]
  (def [lo hi] (value index))
  (def lo (max lo new-lo))
  (if (> lo hi)
    nil
    (set-index value index [lo hi])))

(defn update-upper-bound [value index new-hi]
  (def [lo hi] (value index))
  (def hi (min hi new-hi))
  (if (> lo hi)
    nil
    (set-index value index [lo hi])))

(def dummy-range [[1 100] [1 100] [1 100] [1 100]])

(test (update-lower-bound dummy-range 1 55) [[1 100] [55 100] [1 100] [1 100]])
(test (update-upper-bound dummy-range 3 55) [[1 100] [1 100] [1 100] [1 55]])
(test (update-lower-bound dummy-range 3 200) nil)

# restrict to only the ranges that match
(defn refine [value {:lhs lhs :op op :rhs rhs}]
  (pat/match op
    ">" (update-lower-bound value lhs (+ rhs 1))
    "<" (update-upper-bound value lhs (- rhs 1))))

# restrict to only the ranges that don't match
(defn unrefine [value {:lhs lhs :op op :rhs rhs}]
  (pat/match op
    "<" (update-lower-bound value lhs rhs)
    ">" (update-upper-bound value lhs rhs)))

(test (refine dummy-range {:lhs 0 :op ">" :rhs 10}) [[11 100] [1 100] [1 100] [1 100]])
(test (unrefine dummy-range {:lhs 0 :op ">" :rhs 10}) [[1 10] [1 100] [1 100] [1 100]])

(defn simulate [rules accept current cursor]
  (pat/match cursor
    [[:if refinement command] rest] (do
      (when-let [refined (refine current refinement)]
        (simulate rules accept refined [command rest]))
      (when-let [current (unrefine current refinement)]
        (simulate rules accept current rest)))
    [[:goto next-rule] _] (simulate rules accept current (rules next-rule))
    [:accept _] (accept current)
    [:reject _] nil))

(defn count-ways [ranges]
  (product (seq [[lo hi] :in ranges] (succ (- hi lo)))))

(test (count-ways [[1 5]]) 5)
(test (count-ways [[1 5] [2 4]]) 15)
(test (count-ways [[1 5] [4 4]]) 5)

(defn solve2 [input]
  (def {:rules rules} (parse-input input))

  (def accepted @[])
  (def all-ranges [[1 4000] [1 4000] [1 4000] [1 4000]])
  (simulate rules (partial array/push accepted) all-ranges (rules "in"))

  (sum-loop [way :in accepted]
    (count-ways way)))

(test (solve2 test-input) 167409079868000)
(test (solve2 real-input) 113057405770956)
