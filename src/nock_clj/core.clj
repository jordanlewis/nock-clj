(ns nock-clj.core
  (:use [clojure.core.match :only (match)]
        [clojure.tools.logging :only (spy)]))

(declare tar)
(defn nock [noun]
  (tar noun))

(defn ncdr [noun]
  (if (empty? (rest (rest noun))) (second noun) (rest noun)))
(defn ncar [noun]
  (if (empty? (rest noun)) (first noun) noun))

(defn cell [& args]
  (if
    (empty? (rest args))
    (first args)
    (let [x (apply cell (rest args))]
      (if (seq? x)
        (conj x (first args))
        (list (first args) x)))))

;; turn a nock cell into a flattened list
(defn eat [x]
  (if (coll? x) (apply cell (map eat x)) x))

; is the noun an atom?
; ?
(defn wut [noun] (if (number? noun) 1 0))

; +
(defn lus [noun] (if (wut noun) (inc noun) noun))

; =
(defn tis [noun] (if (= (first noun) (ncdr noun)) 0 1))

; \
(defn slot [noun]
  (let [p (first noun)
        q (ncdr noun)]
  (cond
    (= p 1) q
    (= p 2) (first q)
    (= p 3) (ncdr q)
    (even? p) (slot (cell 2 (slot (cell (quot p 2) q))))
    (odd? p) (slot (cell 3 (slot (cell (quot p 2) q)))))))

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
