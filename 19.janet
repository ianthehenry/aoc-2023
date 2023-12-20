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

(defn always [then]
  {:if true :then then})

(def peg
  ~{:rule (/ (* ':w+ "{" (some (* :instruction (? ","))) "}") ,|{:name $0 :instructions $&})
    :instruction (+
      (/ "A" ,(fn [] (always :accept)))
      (/ "R" ,(fn [] (always :reject)))
      (/ (* :field '(+ "<" ">") (number :d+) ":" :command) ,|{:if {:lhs $0 :op $1 :rhs $2} :then $3})
      (/ :command ,always))
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

(defn solve [input]
  (def [{:rules rules :parts parts}] (peg/match peg input))
  (def rules
    (tabseq [{:name name :instructions instructions} :in rules]
      name (to-linked-list instructions)))

  (def accepted @[])
  (var state nil)

  (defn evaluate [command part]
    (pp [command part])
    (pat/match command
      :accept (do (array/push accepted part) (set state nil))
      :reject (set state nil)
      [:goto next-rule] (set state (rules next-rule))))

  (each part parts
    (set state (rules "in"))
    (while (not= state nil)
      (pat/match state
        [{:if (or true |(evaluate-condition $ part)) :then command} _]
          (evaluate command part)
        [_ rest]
          (set state rest))))

  (sum-loop [part :in accepted field :in part] field))

(test (solve test-input) 19114)
(test (solve real-input) 409898)
