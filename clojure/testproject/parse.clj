(ns testproject.parse
  (:require [instaparse.core :as i] ))

(def two-sents "First sentence. Sencond sentence.")

(def some-text 
  "Die ab dem 23. Oktober 2014 von Google umgesetzte deutliche Reduzierung
  der Textdarstellung und die Auslistung von Bilder-Darstellungen auf allen
  Google-Suchdiensten setzt die Presseverleger einem erheblichen wirtschaftlichen
  Druck aus. Sie sehen sich dadurch gezwungen, gegen ihren Willen die VG Media
  anzuweisen, Google eine „Gratiseinwilligung“ zu erklären.")

