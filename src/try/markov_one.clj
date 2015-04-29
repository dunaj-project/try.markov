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

(ns try.markov-one
  "Markov chain."
  (:api dunaj)
  (:require
   [dunaj.host.int :refer
    [iSMALL_A iSMALL_Z iint i<= ismall-letter? icapital-letter? i==
     idigit? iLF iSPACE iCOLON iCOMMA iMINUS iDOT iQMARK iBANG izero?
     iSEMICOLON iAPOS idec]]
   [dunaj.coll :refer [settle! edit assoc! conj! settleable?]]
   [dunaj.coll.hamt-map :refer [empty-hamt-map]]
   [dunaj.coll.bvt-vector :refer [empty-bvt-vector]]
   [dunaj.string :as ds :refer [MutableString empty-string]]
   [dunaj.format.parser :refer [tokenizer-engine]]
   [try.markov :refer [word-tokenizer]]
   [rhizome.viz :as rv]))


;;; Markov chain of order 1

(defrecord MarkovWrap [cur chain])

(defn markov-conj :- {}
  "Returns `_wrap_` with added `_word_` to the given markov chain
   of order 1. The actual markov chain is stored in `:chain` key of
   the resulting map. No args version returns the initial wrap. "
  ([]
   (->MarkovWrap nil (edit empty-hamt-map)))
  ([wrap :- {}, word :- String]
   (let [{:keys [:cur :chain]} wrap]
     (if (or (= word cur) (and (nil? cur) (keyword? word)))
       wrap
       (let [vals (get chain cur ::no)
             vals (if (identical? ::no vals)
                    (edit empty-bvt-vector)
                    vals)
             new-vals (conj! vals word)
             new-chain (assoc! chain cur new-vals)
             new-cur (when-not (keyword? word) word)]
         (->MarkovWrap new-cur new-chain))))))

(defn markov-chain :- {}
  "Returns markov chain of order 1, in form of a transient map,
  from a given `_text_`. Returned markov chain will contain start
  words that begin with a capital letter only."
  [text :- []]
  (let [words (tokenizer-engine word-tokenizer text)
        chain (:chain (reduce markov-conj words))
        ff #(and (string? %) (icapital-letter? (iint (first %))))
        prune-start #(vec (filter ff %))]
    (assoc! chain nil (prune-start (get chain nil)))))

;;; Sentence generation

(def ^:private stop-map
  "A map for translating stop keyword tokens into their textual
  representation."
  {:stop-bang "!"
   :stop-qmark "?"})

(defn random-sentence :- String
  "Returns one sentence generated from a given `_markov-chain_` of
  order 2."
  [markov-chain :- {}]
  (loop [cur nil, sentence nil, idx (iint 10000)]
    (if (or (keyword? cur) (izero? idx))
      (str (conj! sentence (get stop-map cur ".")))
      (let [nxt (rand-nth (markov-chain cur))
            new-sentence (cond (nil? cur) sentence
                               (nil? sentence) (edit cur)
                               (string? cur) (conj! sentence " " cur)
                               :else (conj! sentence cur))]
        (recur nxt new-sentence (idec idx))))))

;;; Graph generation

(defn gen-graph! :- nil
  "Generates graph of `_markov-chain_` into png file named by
  `_file-name`. Returns `nil`.

  WARNING: Graphviz has to be installed see
  https://github.com/ztellman/rhizome for more information"
  ([markov-chain :- {}, file-name :- String]
   (gen-graph! markov-chain file-name
               [:stop-dot :stop-qmark :stop-bang]))
  ([markov-chain :- {}, file-name :- String, stop-tokens :- []]
   (let [msettle! #(if (settleable? %) (settle! %) %)
         mc (reduce-unpacked (fn [r k v] (assoc r k (msettle! v)))
                             {} (msettle! markov-chain))
         nodes
         (reduce-unpacked #(assoc % %2 (vec (distinct %3))) {} mc)]
     (rv/save-image
      (rv/graph->image
       (concat (keys mc) stop-tokens) nodes
       :node->descriptor (fn [n] {:label (or n "START")})
       :edge->descriptor
       (fn [s d]
         (let [v (mc s)
               cv (count v)
               cd (count (filter #(= % d) v))]
           {:label
            (str (print "%.2f" (clojure.core/float (/ cd cv))))})))
      file-name))))


;;;; Scratch

(scratch [[clojure.core :refer [binding]]]

  []

  (def text
  "I love sixpence, pretty little sixpence,
  I love sixpence better than my life.
  I spent a penny of it, I spent another,
  And took fourpence home to my wife.
  
  Oh, my little fourpence, pretty little fourpence,
  I love fourpence better than my life.
  I spent a penny of it, I spent another,
  And I took twopence home to my wife.
  
  Oh, my little twopence, my pretty little twopence,
  I love twopence better than my life.
  I spent a penny of it, I spent another,
  And I took nothing home to my wife.
  
  Oh, my little nothing, my pretty little nothing,
  What will nothing buy for my wife.
  I have nothing, I spend nothing,
  I love nothing better than my wife.")
  
  (count text)

  (count (markov-chain text))

  (count (tokenizer-engine word-tokenizer text))

  (let [mc (markov-chain text)
        s (vec (sort-by #(count (distinct (second %))) (vec mc)))
        pf #(->vec (first %)
                   (take 3 (vec (second %)))
                   (count (second %))
                   (count (distinct (second %))))]
    (vec (map pf (take 10 (reverse s)))))

  (random-sentence (markov-chain text))

  (gen-graph! (markov-chain text) "out.png" [:stop-dot])

)
