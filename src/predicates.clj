(ns predicates)

(defn apply-1 [f x]
  (f x))

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (fn [k] (contains? a-set k)))

(defn pred-and [pred1 pred2]
  (fn [k] (and (apply-1 pred1 k) (apply-1 pred2 k))))

(defn pred-or [pred1 pred2]
  (fn [k] (or (apply-1 pred1 k) (apply-1 pred2 k))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  (contains? (get book :awards) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? (fn [a] (has-award? book a)) awards))

(defn my-some [pred a-seq]
  (let [s (filter pred a-seq)]
    (if (empty? s)
      false
      (pred (first s)))
    ))

(defn my-every? [pred a-seq]
  (empty? (filter (complement pred) a-seq)))

(defn divides? [divisor n]
  (= (mod n divisor) 0))

(defn prime? [n]
  (let [possible-divisors (range 2 (Math/floor (+ (Math/sqrt n) 1)))
        divides-n? (fn [x] (divides? x n))]
(empty? (filter divides-n? possible-divisors))))
;^^