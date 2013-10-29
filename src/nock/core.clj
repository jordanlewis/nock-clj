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
  (if
    (empty? (rest args))
    (first args)
    (let [x (apply sel (rest args))]
      (if (seq? x)
        (conj x (first args))
        (list (first args) x)))))

;; buy unwraps singleton seqs. We need to use this any time we're
;; going to operate on a noun that's passed in as a list, unless the
;; noun is the last argument of a call to (sel), which does this
;; unwrapping for us automatically if necessary.
;; e.g. (buy '(1)) -> 1, but (buy '(1 2)) -> '(1 2)
(defn buy [noun]
  (if (seq (rest noun)) noun (first noun)))

;; eat turns a Nock noun literal into our internal representation.
;; e.g. (eat [1 2 3]) -> '(1 2 3)
(defn eat [v]
  (if (coll? v) (apply sel (map eat v)) v))

;; NOCK IMPLEMENTATION
;;
;; Now we get to the Nock implementation proper. This is a straightforward
;; interpreter that reduces expressions according to the Nock 5k specification,
;; given at http://www.urbit.org/2013/08/22/Chapter-2-nock.html

; ?
; The wut operator checks whether a noun is an atom or a cell
(defn wut [noun] (if (number? noun) 1 0))

; +
; The lus operator increments an atom.
(defn lus [noun] (if (number? noun) (inc noun) noun))

; =
; The tis operator checks for equality between two nouns.
(defn tis [noun] (cond (number? noun) noun
                       (= (first noun) (buy (rest noun))) 0
                       :else 1))

; \
; The slot (or fas) operator indexes into a list like a binary tree.
(defn slot [noun]
  (match [noun]
    [([1 & a] :seq)] (buy a)
    [([2 a & b] :seq)] a
    [([3 a & b] :seq)] (buy b)
    [([(a :guard even?) & b] :seq)] (slot (sel 2 (slot (sel (quot a 2) b))))
    [([(a :guard odd? ) & b] :seq)] (slot (sel 3 (slot (sel (quot a 2) b))))))

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
(defmacro tar [form]
  (if *verbose*
    `(dotar ~form (inc ~'depth))
    `(dotar ~form nil)))

; *
; The tar operator is Nock itself. It evaluates a noun. Depth is how many
; levels of tar we've gone through so far.
; We temporarily define it as 'dotar' for the benefits of threading depth
; through the computation.
(defn dotar [noun depth]
  (printbefore)
  (printafter (match [noun]
    [([a ([b & c] :seq) & d] :seq)] (sel (tar (sel a b c)) (tar (sel a d)))
    [([a 0 & b] :seq)] (slot (sel (buy b) a))
    [([a 1 & b] :seq)] (buy b)
    [([a 2 b & c] :seq)] (tar (sel (tar (sel a b)) (tar (sel a c))))
    [([a 3 & b] :seq)] (wut (tar (sel a b)))
    [([a 4 & b] :seq)] (lus (tar (sel a b)))
    [([a 5 & b] :seq)] (tis (tar (sel a b)))
    ;; rules 6-10 are macros. we leave their naive form in comments above,
    ;; and implement them more efficiencly below.
    ;;
    ;; 6: if
    ;;[([a 6 b c & d] :seq)] (tar (sel a 2 '(0 1) 2 (sel 1 c d) '(1 0) 2 '(1 2 3) '(1 0) 4 4 b))
    [([a 6 b c & d] :seq)] (let [r (tar (sel a b))]
                             (cond (= 0 r) (tar (sel a c))
                                   (= 1 r) (tar (sel a d))
                                   :else   :dead))
    ;; 7: function composition
    ;;[([a 7 b & c] :seq)] (tar (sel a 2 b 1 c))
    [([a 7 b & c] :seq)] (tar (sel (tar (sel a b)) c))
    ;; 8: function composition, the second fn also gets the original noun
    ;;[([a 8 b & c] :seq)] (tar (sel a 7 (sel (sel 7 '(0 1) b) 0 1) c))
    [([a 8 b & c] :seq)] (tar (sel (sel (tar (sel a b)) a) c))
    ;; 9: core application. apply the bth arm of the core *[a c] to itself
    ;;[([a 9 b & c] :seq)] (tar (sel a 7 c 2 '(0 1) 0 b))
    [([a 9 b & c] :seq)] (let [core (tar (sel a c))]
                           (tar (sel core (slot (sel b core)))))
    ;; hints. ignore.
    ;;[([a 10 ([b & c] :seq) & d] :seq)] (tar (sel a 8 c 7 '(0 3) d))
    [([a 10 ([b & c] :seq) & d] :seq)] (tar (sel a d))
    [([a 10 b & c] :seq)] (tar (sel a c)))))

(defn tar [noun]
  (dotar noun 0))

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
;;(time  (tar (eat [250 [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]]))) 
