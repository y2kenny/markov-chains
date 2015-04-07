(ns markov-chains.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

;;generating the chains
;Split a body of text into tokens

;Build a frequency table.

;The frequency table is a map where each word is a key
;the value is a structure that describes all words that follow the word ranked by frequence

;;generating the output from chains
;select a a random key from the frequency table as the initial state
;then select the next word using weighed random selection based on the frequency from the words in the chain
;associated with the word

;repeat until the end condition is met

(def frequency-table (atom {}))
(def starting-words (atom []))

(defn chain-word [chain word]
  (update-in chain [word] (fnil inc 0)))

(defn chain-words [frequency-table [last-word word next-word]]
  (if word
    (-> frequency-table
        (update-in [last-word] chain-word word)
        (update-in [word] chain-word next-word))
    frequency-table))

(defn weigh-by-frequency [chain]
  (let [total (->> chain (map second) (reduce + 0))]
    (when (pos? total)
      {:total total
       :chain
       (->> chain
            (sort-by second)
            reverse
            (reduce
             (fn [[words weight] [word occurances]]
               (let [weight (+ weight occurances)]
                 [(conj words [word weight]) weight]))
             [[] 0])
            first)})))

(defn partition-line [line]
  (->> (.split line " ") (map #(.trim %)) (remove empty?) (partition 3 1)))

(defn parse-line [frequency-table line]
  (reduce chain-words frequency-table (partition-line line)))

(defn clean-text [text]
  (-> text
     (s/replace #"\n" " ")
     (s/replace #"[^a-zA-Z0-9'. ]" "")))

(defn generate-frequencies [file]
  (->> (slurp file)
       (clean-text)
       (parse-line {})
       (map #(update-in % [1] weigh-by-frequency))
       (remove #(nil? (second %)))
       (into {})))

(defn select-by-frequency [{:keys [total chain]}]
  (let [position (rand-int total)]
    (->> chain (drop-while #(< (second %) position)) ffirst)))

(defn random-word [frequency-table]
  (rand-nth (seq frequency-table)))

(defn select-word [frequency-table chain]
  (if chain
    (let [word (select-by-frequency chain)]
      [word (frequency-table word)])
    (random-word frequency-table)))

(defn populate-starting-words! []
  (reset! starting-words
          (filter #(Character/isUpperCase (first %)) (keys @frequency-table))))

(defn random-starting-word []
  (let [word (rand-nth @starting-words)]
    [word (get @frequency-table word)]))

(defn generate-text []
  (loop [[word chain] (random-starting-word)
         result []]
    (if
     (and (not-empty result) (some #{(-> result last last)} [\. \? \! \;]))
     (clojure.string/join " " result)
     (recur (select-word @frequency-table chain)
            (conj result word)))))

(defn init-chains! [file]
  (reset! frequency-table (generate-frequencies file))
  (populate-starting-words!))

(defn tweet-sized []
  (->> (repeatedly generate-text) (drop-while #(or (< (count %) 50) (> (count %) 140))) first))

(defn -main [file]
  (init-chains! file)
  (println (generate-text)))

;(tweet-sized)
;(-main "corpus.txt")
