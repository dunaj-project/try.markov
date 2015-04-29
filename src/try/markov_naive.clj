;; Copyright (C) 2015, Jozef Wagner. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be
;; found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns try.markov-naive
  "Naive Markov chain."
  (:api dunaj)
  (:require
   [dunaj.host.int :refer [iCOMMA iDOT iQMARK iBANG iAPOS iint i==
                           ismall-letter? icapital-letter? idec]]
   [dunaj.coll :refer [edit conj!]]
   [dunaj.format.parser :refer
    [ITokenizerMachine IParserMachineFactory tokenizer-engine]]
   [incanter.core :as ic :refer [view save]]
   [incanter.charts :as ich :refer [bar-chart]]))


;;; Word tokenizer

(defn ^:private unread-char! :- nil
  "Unreads one char from `_char-batch_`. Returns `nil`."
  [char-batch :- (Batch Char)]
  (.position char-batch (idec (.position char-batch)))
  nil)

(defn ^:private next-char! :- (Maybe Char)
  "Returns next character from `_char-batch_`, or returns `nil` if
  there are no more chars to read."
  [char-batch :- (Batch Char)]
  (when (.hasRemaining char-batch)
    (.get char-batch)))

(defn ^:private word-char? :- Boolean
  "Returns `true` if `_c_` is a valid character for word, otherwise
  returns `false`."
  [c]
  (let [x (iint c)]
    (or (ismall-letter? x) (icapital-letter? x)
        (i== x (iCOMMA)) (i== (iAPOS) x))))

(defn ^:private word-token
  "Returns new instance of word tokenizer machine that contains
  `_item_`"
  [item]
  (let [word-ref (atom [item])]
    (reify
      ITokenizerMachine
      (-analyze-batch! [this bm batch]
        (loop [c (next-char! batch)]
          (cond
            (nil? c) this
            (word-char? c) (do (alter! word-ref conj c)
                               (recur (next-char! batch)))
            :else (do (unread-char! batch)
                      (str @word-ref)))))
      (-analyze-eof! [this] (str @word-ref)))))

(def word-tokenizer
  "A tokenizer machine factory for words."
  (reify
    IParserMachineFactory
    (-parser-config [this]
      {})
    (-parser-from-type [this]
      (keyword->class :char))
    (-dispatch-tokenizer [this config state item]
      (cond (word-char? item) (word-token item)
            (= item \.) :stop-dot
            (= item \!) :stop-bang
            (= item \?) :stop-qmark
            :else this))))

;;; Markov chain

(defn assoc-transition :- {}
  "Returns markov chain with added cur-nxt association."
  [chain :- {}, cur, nxt]
  (let [key (when-not (keyword? cur) cur)
        vals (get chain key (edit []))]
    (assoc chain key (conj! vals nxt))))

(defn markov-chain :- {}
  "Returns markov chain, in form of a map, from given collection of words."
  [words :- []]
  (let [transitions (zip (cons :start words) words)]
    (reduce-unpacked assoc-transition {} transitions)))

;;; Sentence generation

(defn random-sentence :- String
  "Returns a random sentence generated from a given markov-chain."
  [markov-chain :- {}]
  (let [stop-map {:stop-bang "!" :stop-qmark "?"}]
    (loop [sentence [], cur (rand-nth (markov-chain nil))]
      (if (keyword? cur)
        (str (append (get stop-map cur ".") (interpose " " sentence)))
        (recur (conj sentence cur) (rand-nth (markov-chain cur)))))))


;;;; Scratch

(scratch [[clojure.core :refer [binding]]]
  []

  ;;; load text from a file on a classpath

  (def text
    (with-scope
      (str (slurp (classpath "try/document/moby_dick.txt")))))
  ;;=> #'try.markov-naive/text

  (count text)
  ;;=> 1210052

  (pp! text)
  ;; "﻿Call me Ishmael. Some years ago--never mind how long precisely--having\r\nlittle or no money in my purse, and nothing particular to interest me on\r\nshore, I thought I would sail about a little and see ..."

  ;;; parse text into words

  (def words (tokenizer-engine word-tokenizer text))
  ;;=> #'try.markov-naive/words

  (count words)
  ;;=> 221846

  (seq (take 45 words))
  ;;=> ("Call" "me" "Ishmael" :stop-dot "Some" "years" "ago" "never" "mind" "how" "long" "precisely" "having" "little" "or" "no" "money" "in" "my" "purse," "and" "nothing" "particular" "to" "interest" "me" "on" "shore," "I" "thought" "I" "would" "sail" "about" "a" "little" "and" "see" "the" "watery" "part" "of" "the" "world" :stop-dot)

  ;;; create markov chain from words

  (def mc (markov-chain words))
  ;;=> #'try.markov-naive/words

  (count mc)
  ;;=> 23453

  (seq (take 20 (keys mc)))
  ;;=> (nil "shouted," "convince" "weary," "Lower" "mounting" "howled" "posse" "declares," "shelf," "heaped" "new," "rainbow" "Bartholomew" "in," "blandishments" "Christianity" "absorbed," "float" "sweet")

  ;; number of different starting words
  (count (distinct (mc nil)))
  ;;=> 1763

  ;; see possible successors for "Ishmael" state
  (seq (mc "Ishmael"))
  ;; (:stop-dot :stop-dot "can" :stop-dot "hope" :stop-qmark :stop-dot "but")

  ;; some more insight
  (let [s (vec (sort-by #(count (second %)) (vec mc)))
        pf #(->vec (first %)
                   (take 3 (vec (second %)))
                   (count (second %))
                   (count (distinct (second %))))]
    (vec (map pf (take 10 (reverse s)))))
  ;; [["the" ["watery" "world" "spleen"] 13514 4694]
  ;;  [nil ["Call" "Some" "It"] 9854 1763]
  ;;  ["of" ["the" "driving" "every"] 6400 1698]
  ;;  ["and" ["nothing" "see" "regulating"] 5859 2623]
  ;;  ["a" ["little" "way" "damp,"] 4476 2146]
  ;;  ["to" ["interest" "prevent" "get"] 4443 1155]
  ;;  ["in" ["my" "my" "this"] 3796 771]
  ;;  ["that" ["it" "noble" "man"] 2767 975]
  ;;  ["his" ["sword" "deepest" "legs,"] 2414 1281]
  ;;  ["I" ["thought" "would" "have"] 1882 494]]

  ;; tokens with most diverse transitions
  (let [cf (comp count distinct second)
        s (take 9 (reverse (sort-by cf (vec mc))))]
    (ic/view
     (ich/bar-chart
      (vec (map #(or (first %) :start) s))
      (vec (map cf s))
      :title "Tokens with most diverse transitions"
      :x-label ""
      :y-label "")))

  ;;; generating random sentences

  (random-sentence mc)
  ;;=> "I obtain ample vengeance, eternal democracy!"
  ;;=> "At last degree succeed in the cry."
  ;;=> "HUZZA PORPOISE."
  ;;=> "An old, old man, in the bows, at times you say, should be placed as laborers."
  ;;=> "So, so suddenly started on and strength, let the far fiercer curse the maid."

  ;; summary

  (let [cf (comp count distinct second)
        s (take 9 (reverse (sort-by cf (vec mc))))]
    (ic/save
     (ich/bar-chart
      (vec (map #(or (first %) :start) s))
      (vec (map cf s))
      :title "Tokens with most diverse transitions"
      :x-label ""
      :y-label "")
     "markov-bar.png"))

  (time
   (def mc
     (->> (slurp (classpath "try/document/moby_dick.txt"))
          (tokenizer-engine word-tokenizer)
          seq
          markov-chain
          with-scope)))
  
  (random-sentence mc)

)
