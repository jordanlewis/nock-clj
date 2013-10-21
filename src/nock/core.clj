(ns nock.core
  (:use [clojure.core.match :only (match)]))

;; PREAMBLE, AND INTERNAL DATA STRUCTURES
;;
;; Internally, we're going to treat Nock nouns as regular lists for efficiency.
;; We treat a noun like [1 [2 3]] as '(1 2 3), for instance. This introduces a
;; complication: we have to treat singleton lists different from all other
;; lists. In Nock, the second element of a sel is just an atom. But in
;; Clojure, the second element of list is the rest of the list - which for
;; a list of 2 elements, a.k.a. a Nock sel, is a singleton list. So we have
;; to introduce a couple of functions, ncdr and ncar, that detect and ignore
;; singleton lists in the right way.

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

;; eat turns a Nock noun literal into our internal representation.
;; e.g. (eat [1 2 3]) -> '(1 2 3)
(defn eat [v]
  (if (coll? v) (apply sel (map eat v)) v))

;; ncar unwraps singleton seqs. We need to use this any time we're
;; going to operate on a noun that's passed in as a list, unless the
;; noun is the last argument of a call to (sel), which does this
;; unwrapping for us automatically if necessary.
;; e.g. (ncar '(1)) -> 1, but (ncar '(1 2)) -> '(1 2)
(defn ncar [noun]
  (if (seq (rest noun)) noun (first noun)))

;; NOCK IMPLEMENTATION
;;
;; Now we get to the Nock implementation proper. This is a straightforward
;; interpreter that reduces expressions according to the Nock 5k specification,
;; given at http://www.urbit.org/2013/08/22/Chapter-2-nock.html

; ?
; The wut operator checks whether a noun is an atom or a sel.
(defn wut [noun] (if (number? noun) 1 0))

; +
; The lus operator increments an atom.
(defn lus [noun] (if (number? noun) (inc noun) noun))

; =
; The tis operator checks for equality between two nouns.
(defn tis [noun] (cond (number? noun) noun
                       (= (first noun) (ncar (rest noun))) 0
                       :else 1))

; \
; The slot (or fas) operator indexes into a list like a binary tree.
(defn slot [noun]
  (match [noun]
    [([1 & a] :seq)] (ncar a)
    [([2 a & b] :seq)] a
    [([3 a & b] :seq)] (ncar b)
    [([(a :guard even?) & b] :seq)] (slot (sel 2 (slot (sel (quot a 2) b))))
    [([(a :guard odd? ) & b] :seq)] (slot (sel 3 (slot (sel (quot a 2) b))))))

; *
; The tar operator is Nock itself. It evaluates a noun.
(defn tar [noun]
  (match [noun]
    [([a ([b & c] :seq) & d] :seq)] (sel (tar (sel a b c)) (tar (sel a d)))
    [([a 0 & b] :seq)] (slot (sel (ncar b) a))
    [([a 1 & b] :seq)] (ncar b)
    [([a 2 b & c] :seq)] (tar (sel (tar (sel a b)) (tar (sel a c))))
    [([a 3 & b] :seq)] (wut (tar (sel a b)))
    [([a 4 & b] :seq)] (lus (tar (sel a b)))
    [([a 5 & b] :seq)] (tis (tar (sel a b)))
    [([a 6 b c & d] :seq)] (tar (sel a 2 '(0 1) 2 (sel 1 c d) '(1 0) 2 '(1 2 3) '(1 0) 4 4 b))
    [([a 7 b & c] :seq)] (tar (sel a 2 b 1 c))
    [([a 8 b & c] :seq)] (tar (sel a 7 (sel (sel 7 '(0 1) b) 0 1) c))
    [([a 9 b & c] :seq)] (tar (sel a 7 c 2 (sel 0 1) 0 b))
    [([a 10 ([b & c] :seq) & d] :seq)] (tar (sel a 8 c 7 '(0 3) d))
    [([a 10 b & c] :seq)] (tar (sel a c))))

;; the main entry-point to Nock for users. Pass in a real-life Nock noun as a
;; Clojure literal: (nock [1 0 1]) -> 1
(defn nock [v]
  (tar (eat v)))
