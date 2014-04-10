(ns digest.emailer.css-inliner
  (:require [net.cgrand.enlive-html :as h]
            [clojure.string :refer [join split]]
            [css.core :refer [parse-css]]))


(defn extract-styles
  [html]
  (let [stylesheets (map (comp :href :attrs)
                         (h/select html [[:link (h/attr= :rel "stylesheet")]]))]
    (concat (mapcat :content (h/select html [:style]))
            (map #(-> (clojure.lang.RT/baseLoader) (.getResourceAsStream %) slurp) stylesheets))))

(defn op->fn
  "Given a string representing a css attribute operator, given the
  corresponding enlive function"
  [op]
  (case op
    "=" h/attr=
    "~=" h/attr-has
    "*=" h/attr-contains
    "|=" h/attr|=
    "^=" h/attr-starts
    "$=" h/attr-ends))

(defn extract-attr-rule
  [sel]
  (if-let [[_ sel' attr op val] (re-matches #"(.*)\[([^=]+?)([~|^*$]?=)?([^]=]+)?\]" (name sel))]
    (if op
      [(keyword sel') ((op->fn op) (keyword attr) val)]
      [(keyword sel') (h/attr? (keyword attr))])
    sel))

(defn styles->selectors
  "Given a collection of style strings, parse them and return a vector of
  [[<enlive-selector> <style-string]..]"
  [styles]
  (let [parsed-rules (parse-css (join styles))
        val->str (fn [v] (case (:kind v)
                           :color (str "#" (:content v))
                           :number-with-unit (apply str (:content v))
                           (:content v)))]
    (mapcat (fn [{rule :content}]
              (let [selectors (->> (get-in rule [:selector :content])
                                   (map :content)
                                   (map #(split % #" "))
                                   (map (partial mapv (comp extract-attr-rule keyword))))
                    rule-styles (map
                                  (fn [{{v :value {prop :content} :property} :content}]
                                    (str prop ": "
                                         (if (vector? v)
                                           (join " " (map val->str v))
                                           (val->str v))
                                         ";"))
                                  (:declarations rule))]
                (map vector selectors (repeat (join " " rule-styles)))))
            parsed-rules)))

(defn remove-style-tags
  "Remove stylesheet links and style elements from the html"
  [html]
  (h/transform html #{[:style] [[:link (h/attr= :rel "stylesheet")]]} nil))

(defn append-style
  "Enlive-style transformer to append to the style attribute"
  [style]
  (fn [elt] (update-in elt [:attrs :style] str style)))

(defn inline-css
  "Given a string representation of an html document, inline css from both
  style tags and linked stylesheets (which are assumed to be in ./resources."
  [html-str]
  (let [html (h/html-resource (java.io.StringReader. html-str))
        sels (styles->selectors (extract-styles html))]
    (join (h/emit* (reduce (fn [doc [sel sty]]
                             (h/transform doc sel (append-style sty)))
                           (remove-style-tags html)
                           sels)))))
