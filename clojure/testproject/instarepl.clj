;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(defn twothree []
  (if (< (rand) 0.5)
    2
    3))

(twothree)
(twothree)
(twothree)

(defn twothreelim [limit]
  (if (< (rand) limit)
    2
    3))

(twothreelim 0.9)
(twothreelim 0.9)
(twothreelim 0.9)
(twothreelim 0.9)


(defn arg-fn [x] x)

(arg-fn 23)
(arg-fn "s")

(defn abc [[a b c] x]
  (str a b c "-" x))

(abc [1 2 3] "4")


(defn greet [name]
  (str "Hello, " name))

(greet "foo")


;; Java-Zeugs nutzen

(Math/PI)

(java.util.Calendar/getInstance)
(java.util.Calendar/HOUR_OF_DAY)

(new java.util.Date)
(java.util.Date.) ;; ist äquiv. zu new

(def rng (java.util.Random.)) ;; variable definieren

(. rng nextInt)
(. rng nextInt)



(defn hours-now []
  (. (java.util.Calendar/getInstance)
     get
     (java.util.Calendar/HOUR_OF_DAY)))

(hours-now)

(defn hours-now2 []
  (.get (java.util.Calendar/getInstance)
     (java.util.Calendar/HOUR_OF_DAY)))

(hours-now2)


(defn greet [name]
  (if (< (hours-now) 12)
  (str "Good morning, " name)
  (str "Tach, " name)))

(greet "foo")

(defn greet [name]
  (if (< 6 (hours-now) 12) ;; zwischen 6 und 12
  (str "Good morning, " name)
  (str "Tach, " name)))

(def ^:const morning-start 6) ;; konstanten definieren
(def ^:const morning-end 6)

(defn greet [name]
  (if (< morning-start (hours-now) morning-end) ;; zwischen 6 und 12
  (str "Good morning, " name)
  (str "Tach, " name)))

(greet "foo")

;; alle in dieser session definierten funktionen/vars

(ns-publics *ns*)


(defn greet [name]
  (str
    (if (< morning-start (hours-now) morning-end) ;; zwischen 6 und 12
      "Good morning, "
      "Tach, " name)
   name))


(odd? 54)
(odd? 53)


;; hash set
(def myhashset #{"Foo" "Bar" 23})

(myhashset 23)
(myhashset 22)

;; multi arrity (funktion mit versch. anzahl von args)

;; higher order fns

(range 10)

(filter odd? (range 10))

;; seq / sequence: (1st 2nd ...)
;; geteilt in first und rest

(first (range 10))
(rest (range 10))

(first "foo")
(rest "foo")

(type(rest "foo"))


;; unendliche sequenz, wird lazy evaluiert
(def zahlen (iterate inc 0))

(take 100 zahlen)

;; anonyme funktion #(< % 5)

(filter #(< % 5) (range 10))

;; :foo <-- als key schneller als 'foo'

(get {:a 1 :b 2} :a);; maps mit key nachschauen
({:a 1 :b 2} :a)

(nth [3 2 1] 0)
([3 2 1] 0)

(contains? [1 2 3] 3) ;; es gibt kein Element mit Index 3 im Vektor! besserer name waere has-key

(contains? #{1 2 3} 3)


(defn make-range-filter [low high]
  (fn [x]
    (< low x high)))

(filter (make-range-filter 5 10) (range 20))
(filter (make-range-filter 10 15) (range 20))

(defn make-adder [x]
  (fn [y] (+ x y)))

((make-adder 5) 10)


(def two-sents "First sentence. Sencond sentence.")

(def some-text
  "Die ab dem 23. Oktober 2014 von Google umgesetzte deutliche Reduzierung
  der Textdarstellung und die Auslistung von Bilder-Darstellungen auf allen
  Google-Suchdiensten setzt die Presseverleger einem erheblichen wirtschaftlichen
  Druck aus. Sie sehen sich dadurch gezwungen, gegen ihren Willen die VG Media
  anzuweisen, Google eine „Gratiseinwilligung“ zu erklären.")


;; in-ns?

(reduce + (range 10))


(partition 3 (range 10))


;; ngramme!
(partition 3 two-sents)





