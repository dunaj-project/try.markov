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

(ns try.markov
  "Markov chain."
  (:api dunaj)
  (:require
   [dunaj.host.int :refer
    [iSMALL_A iSMALL_Z iint i<= ismall-letter? icapital-letter? i==
     idigit? iLF iSPACE iCOLON iCOMMA iMINUS iDOT iQMARK iBANG izero?
     iSEMICOLON iAPOS idec]]
   [dunaj.coll :refer [settle! edit assoc! conj!]]
   [dunaj.coll.hamt-map :refer [empty-hamt-map]]
   [dunaj.coll.bvt-vector :refer [empty-bvt-vector]]
   [dunaj.string :refer [MutableString empty-string]]
   [dunaj.format.parser :refer
    [ITokenizerMachine -analyze-eof! IParserMachineFactory
     tokenizer-engine]]
   [dunaj.format.helper :refer [string-cat-batch!]]
   [dunaj.format.json :refer [pretty-json]]
   [dunaj.dev :refer [not-implemented]]
   [criterium.core :as cc]))


;;; Word tokenizer

(deftype WordToken
  "A tokenizer machine for words. Support special handling of
   Mr., Mrs. and Ms., and apostrophes inside words."
  [config state ^:unsynchronized-mutable ts :- MutableString
   ^:unsynchronized-mutable apos :- Boolean]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (let [batch :- (Batch java.lang.Character) batch
          begin (.position batch)]
      (loop []
        (if-not (.hasRemaining batch)
          ;; no more stuff in batch
          (do (string-cat-batch!
               ts batch begin (.position batch) state)
              this)
          ;; check next character in batch
          (let [pos (.position batch)
                x (iint (.get batch))]
            (cond
              ;; word letter, append and continue
              (or (ismall-letter? x)
                  (icapital-letter? x)
                  (idigit? x)
                  (i== x (iCOMMA))
                  (i== x (iMINUS))
                  (i== x (iCOLON))
                  (i== x (iSEMICOLON)))
              (do (set! apos false) (recur))
              ;; special case for apostrophe, e.g. It's
              (i== (iAPOS) x)
              (do (set! apos true) (recur))
              ;; word delimiter, produce word string
              :else
              (do (string-cat-batch! ts batch begin pos state)
                  ;; special case for Mr. Mrs. Ms.
                  (if (and (i== (iDOT) x)
                           (let [s (str ts)]
                             (or (= s "Mr") (= s "Mrs") (= s "Ms"))))
                    (conj! ts \.)
                    (.position batch pos))
                  (-analyze-eof! this))))))))
  (-analyze-eof! [this]
    ;; EOF reached, produce word from what we got so far
    (let [s (settle! ts)]
      (if apos (str (drop-last 1 s)) s))))

(defn ^:private word-token
  "Returns new instance of word tokenizer machine that contains
   `_item_`, using provided configuration and state map."
  [config state item]
  (let [ts ^java.lang.StringBuilder (edit empty-string)]
    (->WordToken config state (.append ts (char item)) false)))

(deftype WordTokenizerMachine
  "A tokenizer machine for words."
  []
  IParserMachineFactory
  (-parser-config [this] {})
  (-parser-from-type [this] (keyword->class :char))
  (-parser-to-type [this] java.lang.Object)
  (-dispatch-tokenizer [this config state item]
    (let [x (iint item)]
      (cond (ismall-letter? x) (word-token config state x)
            (i== x (iSPACE)) this ;; fast path
            (icapital-letter? x) (word-token config state x)
            (i== x (iDOT)) :stop-dot
            (i== x (iBANG)) :stop-bang
            (i== x (iQMARK)) :stop-qmark
            (i== x (iMINUS)) "-"
            ;; no numbers
            ;; (idigit? x) (word-token config state x)
            :else this)))
  (-dispatch-parser [this config state token parents]
    (not-implemented)))

(def word-tokenizer
  "A tokenizer machine factory for words."
  (->WordTokenizerMachine))

;;; Markov chain

(defrecord MarkovWrap [pre cur chain])

(defn markov-conj :- {}
  "Returns `_wrap_` with added `_word_` to the given markov chain
   of order 2. The actual markov chain is stored in `:chain` key of
   the resulting map. No args version returns the initial wrap. "
  ([]
   (->MarkovWrap nil nil (edit empty-hamt-map)))
  ([wrap :- {}, word :- String]
   (let [{:keys [:pre :cur :chain]} wrap]
     (if (or (= word cur) (and (nil? cur) (keyword? word)))
       wrap
       (let [key (pair pre cur)
             vals (get chain key ::no)
             vals (if (identical? ::no vals)
                    (edit empty-bvt-vector)
                    vals)
             new-vals (conj! vals word)
             new-chain (assoc! chain key new-vals)
             new-cur (when-not (keyword? word) word)
             new-pre (when-not (keyword? word) cur)]
         (->MarkovWrap new-pre new-cur new-chain))))))

(defn markov-chain :- {}
  "Returns markov chain of order 2, in form of a transient map,
  from a given `_text_`. Returned markov chain will contain start
  words that begin with a capital letter only."
  [text :- []]
  (let [words (tokenizer-engine word-tokenizer text)
        chain (:chain (reduce markov-conj words))
        sk (pair nil nil)
        ff #(and (string? %) (icapital-letter? (iint (first %))))
        prune-start #(vec (filter ff %))]
    (assoc! chain sk (prune-start (get chain sk)))))

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
  (loop [pre nil, cur nil, sentence nil, idx (iint 10000)]
    (if (or (keyword? cur) (izero? idx))
      (str (conj! sentence (get stop-map cur ".")))
      (let [nxt (rand-nth (markov-chain (pair pre cur)))
            new-sentence (cond (nil? cur) sentence
                               (nil? sentence) (edit cur)
                               (string? cur) (conj! sentence " " cur)
                               :else (conj! sentence cur))]
        (recur cur nxt new-sentence (idec idx))))))

(def actual-count (atom 0))
(def actual-rval (atom 0))
(def running-max 200)

(defn running-count
  ([]
   (reset! actual-count 0)
   (reset! actual-rval 0))
  ([val]
   (let [cnt (count val)]
     (alter! actual-count + cnt)
     (if (< @actual-count running-max)
       @actual-rval
       (do (reset! actual-count cnt)
           (alter! actual-rval inc))))))

(defn gen-json :- []
  "Returns a json encoded vector of `_amount_` sentences generated
  from given `_markov-chain_`, that are no less than 10 and no more
  than 200 characters long."
  [markov-chain :- {}, amount :- Integer]
  (running-count)
  (->> #(random-sentence markov-chain)
       repeatedly
       (filter #(< (count %) 200))
       (partition-by running-count)
       (map #(str (interpose " " %)))
       (take amount)
       seq
       (print-one (assoc pretty-json :pretty-item-limit nil))))

(defn dump-fun!
  "Generates json file named `_ofname_` with `_amount_` random
  sentences generated from markov chain that is constructed from
  text from `_ifname_`."
  [ifname ofname amount]
  (with-scope
    (let [mc (markov-chain (slurp (classpath ifname)))
          js (gen-json mc amount)]
      (spit! ofname js :truncate))))


;;;; Scratch

(scratch []

  []

  (dump-fun! "try/document/alice_wonderland.txt" "alice.json" 1000)
  (dump-fun! "try/document/moby_dick.txt" "moby.json" 1000)
  (dump-fun! "try/document/pride_prejudice.txt" "pride.json" 1000)
  (dump-fun! "try/document/tom_sawyer.txt" "tom.json" 1000)
  (dump-fun! "try/document/buster_bear.txt" "buster.json" 1000)

  (time
   (def mc
     (with-scope
       (let [a (slurp (classpath "try/document/alice_wonderland.txt"))
             m (slurp (classpath "try/document/moby_dick.txt"))
             p (slurp (classpath "try/document/pride_prejudice.txt"))
             t (slurp (classpath "try/document/tom_sawyer.txt"))
             ;b (df (slurp (classpath "try/document/bible.txt")))
             ]
         (markov-chain (concat a m p t))))))

  (count mc)

  (seq (take 10 (keys mc)))

  (random-sentence mc)

  (let [s (vec (sort-by #(count (second %)) (vec mc)))
        pf #(->vec (first %)
                   #_(take 3 (vec (second %)))
                   (count (second %)))]
    (vec (map pf (take 50 (reverse s)))))

)
