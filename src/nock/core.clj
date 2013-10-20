(ns nock.core
  (:use [clojure.core.match :only (match)]
        [clojure.tools.logging :only (spy)]))

;; PREAMBLE, AND INTERNAL DATA STRUCTURES
;;
;; Internally, we're going to treat Nock nouns as regular lists for efficiency.
;; We treat a noun like [1 [2 3]] as '(1 2 3), for instance. This introduces a
;; complication: we have to treat singleton lists different from all other
;; lists. In Nock, the second element of a cell is just an atom. But in
;; Clojure, the second element of list is the rest of the list - which for
;; a list of 2 elements, a.k.a. a Nock cell, is a singleton list. So we have
;; to introduce a couple of functions, ncdr and ncar, that detect and ignore
;; singleton lists in the right way.

;; cell creates a cell from a list of nouns. It's right associative, like in
;; Nock, but instead of making a long nested list of cells, it makes a regular
;; Clojure persistent list.
(defn cell [& args]
  (if
    (empty? (rest args))
    (first args)
    (let [x (apply cell (rest args))]
      (if (seq? x)
        (conj x (first args))
        (list (first args) x)))))

;; eat turns a Nock noun literal into our internal representation.
;; e.g. (eat [1 2 3]) -> '(1 2 3)
(defn eat [x]
  (if (coll? x) (apply cell (map eat x)) x))

(defn ncdr [noun]
  (if (empty? (rest (rest noun))) (second noun) (rest noun)))

(defn ncar [noun]
  (if (empty? (rest noun)) (first noun) noun))

;; NOCK IMPLEMENTATION
;;
;; Now we get to the Nock implementation proper. This is a straightforward
;; interpreter that reduces expressions according to the Nock 5k specification,
;; given at http://www.urbit.org/2013/08/22/Chapter-2-nock.html

; ?
; The wut operator checks whether a noun is an atom or a cell.
(defn wut [noun] (if (number? noun) 1 0))

; +
; The lus operator increments an atom.
(defn lus [noun] (if (wut noun) (inc noun) noun))

; =
; The tis operator checks for equality between two nouns.
(defn tis [noun] (if (= (first noun) (ncdr noun)) 0 1))

; \
; The slot (or fas) operator indexes into a list like a binary tree.
(defn slot [noun]
  (let [p (first noun)
        q (ncdr noun)]
  (cond
    (= p 1) q
    (= p 2) (first q)
    (= p 3) (ncdr q)
    (even? p) (slot (cell 2 (slot (cell (quot p 2) q))))
    (odd? p) (slot (cell 3 (slot (cell (quot p 2) q)))))))

; *
; The tar operator is Nock itself. It evaluates a noun.
(defn tar [noun]
  (if (or (number? noun) (number? (ncdr noun)))
    noun
    (let [[a b & c] noun]
      (cond
        (list? b) (cell (tar (cell a (first b) (ncdr b)))
                        (tar (cell a c)))
        (= 0 b) (slot (cell (ncar c) a))
        (= 1 b) (ncar c)
        (= 2 b) (tar (cell (tar (cell a (first c))) (tar (cell a (ncdr c)))))
        (= 3 b) (wut (tar (cell a c)))
        (= 4 b) (lus (tar (cell a c)))
        (= 5 b) (tis (tar (cell a c)))
        (= 6 b) (tar (cell a 2 (cell 0 1) 2 (cell 1 (second c) (ncdr (rest c)))
                     (cell 1 0) 2 (cell 1 2 3) (cell 1 0) 4 4 (first c)))
        (= 7 b) (tar (cell a 2 (first c) 1 (ncdr c)))
        (= 8 b) (tar (cell a 7 (cell (cell 7 (cell 0 1) (first c)) 0 1) (ncdr c)))
        (= 9 b) (tar (cell a 7 (ncdr c) 2 (cell 0 1) 0 (first c)))
        (= 10 b) (tar (if (list? (first c))
                   (cell a 8 (ncdr (first c) 7 '(0 3) (ncdr c)))
                   (cell a (ncdr c))))
        :else noun))))

(defn nock [noun]
  (tar noun))
