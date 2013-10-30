(ns nock.core
  (:use [clojure.core.match :only (match)]
        [criterium.core]))

;; PREAMBLE, AND INTERNAL DATA STRUCTURES
;;
;; Internally, we're going to treat Nock nouns as regular lists for efficiency.
;; We treat a noun like [1 [2 3]] as '(1 2 3), for instance. This introduces a
;; complication: we have to treat singleton lists different from all other
;; lists. In Nock, the second element of a 2-element cell is just an atom. But
;; in Clojure, the second element of a list is the rest of the list - which for
;; a list of 2 elements, a.k.a. a Nock cell, is a singleton list. So we have to
;; introduce a function, buy (to continue this tradition of obfuscation), that
;; detects and unwraps singleton lists.

;; define this to be true for verbose reduction printing. This will cause tar
;; to pretty-print its stack traces as it goes through a reduction.
(def ^:dynamic *verbose* false)

;; sel creates a cell from a list of nouns. It's right associative, like in
;; Nock, but instead of making a long nested list of sels, it makes a regular
;; Clojure persistent list.
(defn sel [& args]
  #((last args)
    (apply (fn f [& args]
    (if
      (empty? (rest args))
      (first args)
      (let [x (apply f (rest args))]
        (if (seq? x)
          (conj x (first args))
          (list (first args) x))))) (butlast args))))

;; buy unwraps singleton seqs. We need to use this any time we're
;; going to operate on a noun that's passed in as a list, unless the
;; noun is the last argument of a call to (sel), which does this
;; unwrapping for us automatically if necessary.
;; e.g. (buy '(1)) -> 1, but (buy '(1 2)) -> '(1 2)
(defn buy [noun k]
  (if (seq (rest noun)) #(k noun) #(k (first noun))))


;; eat turns a Nock noun literal into our internal representation.
;; e.g. (eat [1 2 3]) -> '(1 2 3)
(defn eat [v]
  (if (coll? v) ((apply sel (concat (map eat v) `(~identity)))) v))

;; NOCK IMPLEMENTATION
;;
;; Now we get to the Nock implementation proper. This is a straightforward
;; interpreter that reduces expressions according to the Nock 5k specification,
;; given at http://www.urbit.org/2013/08/22/Chapter-2-nock.html

; ?
; The wut operator checks whether a noun is an atom or a cell
(defn wut [noun k] (if (number? noun) #(k 1) #(k 0)))

; +
; The lus operator increments an atom.
(defn lus [noun k] (if (number? noun) #(k (inc noun)) #(k noun)))

; =
; The tis operator checks for equality between two nouns.
(defn tis [noun k] (cond (number? noun) #(k noun)
                         (= (first noun) ((buy (rest noun) identity))) #(k 0)
                         :else #(k 1)))

; \
; The slot (or fas) operator indexes into a list like a binary tree.
(defn slot [noun k]
  (match [noun]
    [([1 & a] :seq)] #(buy a k)
    [([2 a & b] :seq)] #(k a)
    [([3 a & b] :seq)] #(buy b k)
    ;;[([(a :guard even?) & b] :seq)] (slot (sel 2 (slot (sel (quot a 2) b))))
    [([(a :guard even?) & b] :seq)] #(sel (quot a 2) b (fn [x] (slot x (fn [y] (sel 2 y (fn [z] (slot z k)))))))
    ;;[([(a :guard odd? ) & b] :seq)] (slot (sel 3 (slot (sel (quot a 2) b))))))
    [([(a :guard odd? ) & b] :seq)] #(sel (quot a 2) b (fn [x] (slot x (fn [y] (sel 3 y (fn [z] (slot z k)))))))))

;; define a few macros for verbose printing.
;; these macros, when *verbose* is set to true, emit print statements
;; that log the reduction steps of a noun. if *verbose* is set to false,
;; they emit as close to the original code as possible.
;; they assume the existence of 'depth and 'noun, the arguments of dotar.

;; indentstring makes a string of enough spaces to match depth
(def indentstring '(apply str (repeat depth "  ")))

;; printbefore prints out our noun before eval
(defmacro printbefore []
  (if *verbose* `(println ~indentstring ~'noun "...")))

;; printafter prints the result of form, along with the noun that made it
(defmacro printafter [form]
  (if *verbose*
    `(let [~'ret ~form]
       (println ~indentstring ~'noun "->" ~'ret)
       ~'ret)
    form))

;; temporarily define tar to be a macro that invokes dotar with either the
;; current depth plus one, if we're in verbose mode, or nil if we're not.
;; We use this so that inside of dotar we can write 'tar' when we mean 'tar',
;; and not worry about having to thread depth through everything.
(defmacro tar [form contform]
  (if *verbose*
    `(dotar ~form ~contform (inc ~'depth))
    `(dotar ~form ~contform nil)))

; *
; The tar operator is Nock itself. It evaluates a noun. Depth is how many
; levels of tar we've gone through so far.
; We temporarily define it as 'dotar' for the benefits of threading depth
; through the computation.
(defn dotar [noun k depth]
  (printbefore)
  (printafter (match [noun]
    ;;[([a ([b & c] :seq) & d] :seq)] (sel (tar (sel a b c)) (tar (sel a d)))
    [([a ([b & c] :seq) & d] :seq)] #(sel a b c (fn [selabc] (sel a d (fn [selad] (tar selabc (fn [tarselabc] (tar selad (fn [tarselad] (sel tarselabc tarselad k)))))))))
    ;;[([a 0 & b] :seq)] (slot (sel (buy b) a))
    [([a 0 & b] :seq)] #(buy b (fn [x] (sel x a (fn [y] (slot y k)))))
    ;;[([a 1 & b] :seq)] (buy b)
    [([a 1 & b] :seq)] #(buy b k)
    ;[([a 2 b & c] :seq)] (tar (sel (tar (sel a b)) (tar (sel a c))))
    [([a 2 b & c] :seq)] #(sel a b (fn [selab] (sel a c (fn [selac] (tar selab (fn [tarselab] (tar selac (fn [tarselac] (sel tarselab tarselac (fn [selboth] (tar selboth k)))))))))))
    ;;[([a 3 & b] :seq)] (wut (tar (sel a b)))
    [([a 3 & b] :seq)] #(sel a b (fn [x] (tar x (fn [y] (wut y k)))))
    ;;[([a 4 & b] :seq)] (lus (tar (sel a b)))
    [([a 4 & b] :seq)] #(sel a b (fn [x] (tar x (fn [y] (lus y k)))))
    ;;[([a 5 & b] :seq)] (tis (tar (sel a b)))
    [([a 5 & b] :seq)] #(sel a b (fn [x] (tar x (fn [y] (tis y k)))))
    ;; rules 6-10 are macros. we leave their naive form in comments above,
    ;; and implement them more efficiencly below.
    ;;
    ;; 6: if
    ;;[([a 6 b c & d] :seq)] (tar (sel a 2 '(0 1) 2 (sel 1 c d) '(1 0) 2 '(1 2 3) '(1 0) 4 4 b))
    ;;[([a 6 b c & d] :seq)] (let [r (tar (sel a b))]
    ;;                         (cond (= 0 r) (tar (sel a c))
    ;;                               (= 1 r) (tar (sel a d))
    ;;                               :else   :dead))
    [([a 6 b c & d] :seq)] #(sel a b (fn [selab] (tar selab (fn [tarselab]
                             (cond (= 0 tarselab) (sel a c (fn [selac] (tar selac k)))
                                   (= 1 tarselab) (sel a d (fn [selad] (tar selad k))))))))
    ;; 7: function composition
    ;;[([a 7 b & c] :seq)] (tar (sel a 2 b 1 c))
    ;;[([a 7 b & c] :seq)] (tar (sel (tar (sel a b)) c))
    [([a 7 b & c] :seq)] #(sel a b (fn [selab] (tar selab (fn [tarselab] (sel tarselab c (fn [seltarselab] (tar seltarselab k)))))))
    ;; 8: function composition, the second fn also gets the original noun
    ;;[([a 8 b & c] :seq)] (tar (sel a 7 (sel (sel 7 '(0 1) b) 0 1) c))
    ;;[([a 8 b & c] :seq)] (tar (sel (sel (tar (sel a b)) a) c))
    [([a 8 b & c] :seq)] #(sel a b (fn [selab] (tar selab (fn [tarselab] (sel tarselab a (fn [seltarselaba] (sel seltarselaba c (fn [selseltarselabac] (tar selseltarselabac k)))))))))
    ;; 9: core application. apply the bth arm of the core *[a c] to itself
    ;;[([a 9 b & c] :seq)] (tar (sel a 7 c 2 '(0 1) 0 b))
    ;;[([a 9 b & c] :seq)] (let [core (tar (sel a c))]
    ;;                       (tar (sel core (slot (sel b core)))))
    [([a 9 b & c] :seq)] #(sel a c (fn [selac] (tar selac (fn [core]
                          (sel b core (fn [selbcore] (slot selbcore (fn [slotselbcore]
                            (sel core slotselbcore (fn [selcoreslotselbcore] (tar selcoreslotselbcore k)))))))))))
    ;; hints. ignore.
    ;;[([a 10 ([b & c] :seq) & d] :seq)] (tar (sel a 8 c 7 '(0 3) d))
    ;;[([a 10 ([b & c] :seq) & d] :seq)] (tar (sel a d))
    [([a 10 ([b & c] :seq) & d] :seq)] #(sel a d (fn [selad] (tar selad k)))
    ;;[([a 10 b & c] :seq)] (tar (sel a c)))))
    [([a 10 b & c] :seq)] #(sel a c (fn [selac] (tar selac k))))))

(defn tar [noun]
  (trampoline dotar noun (fn [ret] ret) 0))

;; the main entry-point to Nock for users. Pass in a real-life Nock noun as a
;; Clojure literal: (nock [1 0 1]) -> 1
(defn nock [v]
  (tar (eat v)))

(comment
  (nock [42 7 [[4 0 1] 0 1] [4 0 3]]))

;; 2.5ms normally
;; 2ms with macro'd if
;; 1.5ms with macro'd comp
;; 1.3ms with macro'd core application
;;(time (tar (eat [110 [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]])))
;;(quick-bench (tar (eat [110 [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]])))
;;(time  (tar (eat [100 [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]]))) 
