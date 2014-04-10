; From https://github.com/simonhicks/css-parser/
(ns css.core
  (:require [clojure.string :as s])
  (:use name.choi.joshua.fnparse))

; TODO remove some of the duplication for colon being used as property/value separator

(defstruct node-s :kind :content)

(defstruct state-s :remainder :column :line)

(def remainder-a
  (accessor state-s :remainder))

(def make-node
  (partial struct node-s))

(defn- nb-char [subrule]
  (invisi-conc subrule (update-info :column inc)))

(def nb-char-lit
  (comp nb-char lit))

(defn- b-char [subrule]
  (invisi-conc subrule (update-info :line inc)))

(defn flat-str [& forms]
  (apply str (flatten forms)))

(defmacro defcl
  [sym char-lit]
  `(def ~sym (nb-char-lit ~char-lit)))

(defmacro defcls
  [& more]
  (let [pairs (partition 2 more)
        defs  (for [[s c] pairs] (list 'defcl s c))]
    `(do ~@defs)))

(defcls
  forward-slash         \/
  back-slash            \\
  asterisk              \*
  ampersand             \&
  dollar                \$
  caret                 \^
  open-paren            \(
  close-paren           \)
  open-brace            \{
  close-brace           \}
  comma                 \,
  dot                   \.
  minus-sign            \-
  plus-sign             \+
  equals-sign           \=
  underscore            \_
  new-line              \newline
  return                \return
  colon                 \:
  semicolon            \;
  double-quotes         \"
  single-quotes         \'
  exclamation           \!
  at-sign               \@
  percentage-sign       \%
  hash-tag              \#
  tab                   \tab
  space                 \space
  open-square-bracket   \[
  close-square-bracket  \]
  pipe-char             \|
  less-than             \<
  greater-than          \>
  question-mark         \?
  tilde                 \~
  backtick              \`
  zero                  \0)

(def quotes
  (alt double-quotes single-quotes))

(def non-zero-digit
  (lit-alt-seq "123456789" nb-char-lit))

(def number
  (alt zero non-zero-digit))

(def lower-case-letter
  (lit-alt-seq "abcdefghijklmnopqrstuvwxyz" nb-char-lit))

(def upper-case-letter
  (lit-alt-seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ" nb-char-lit))

(def letter
  (alt lower-case-letter upper-case-letter))

(def alphanumeric
  (alt letter number))

(def hexadecimal-digit
  (alt number (lit-alt-seq "ABCDEFabcdef" nb-char-lit)))

(def unescaped-char
  (except anything (alt back-slash double-quotes single-quotes)))

(def standard-escaped-characters
  {\\ \\, \/ \/, \b \backspace, \f \formfeed, \n \newline, \r \return,
   \t \tab})

(def standard-escapeable-char
  (semantics (lit-alt-seq (keys standard-escaped-characters) nb-char-lit)
    standard-escaped-characters))

(def standard-escape-sequence
  (complex [_         back-slash
            character standard-escapeable-char]
    character))

(def escaped-double-quote
  (constant-semantics (lit-conc-seq (list \\ \") nb-char-lit) \"))

(def escaped-single-quote
  (constant-semantics (lit-conc-seq (list \\ \') nb-char-lit) \'))

(def double-quote-string-char
  (alt escaped-double-quote standard-escape-sequence unescaped-char single-quotes back-slash))

(def single-quote-string-char
  (alt escaped-single-quote standard-escape-sequence unescaped-char double-quotes back-slash))

(def no-quote-string-char
  (alt alphanumeric minus-sign underscore forward-slash dot))

(def double-quote-string
  (complex [_       double-quotes
            content (rep* double-quote-string-char)
            _       double-quotes]
    (make-node :string (apply str content))))

(def single-quote-string
  (complex [_       single-quotes
            content (rep* single-quote-string-char)
            _       single-quotes]
    (make-node :string (apply str content))))

(def no-quote-string
  (semantics (rep+ no-quote-string-char)
             #(make-node :string (apply str %))))

(def string-literal
  (alt double-quote-string single-quote-string no-quote-string))

(def line-break
  (b-char
    (rep+ (alt new-line return))))

(def nb-whitespace
  (rep+ (alt space tab)))

(def whitespace
  (rep+ (alt nb-whitespace line-break)))

(def tag
  (semantics (alt (rep+ (alt lower-case-letter number))
                  asterisk
                  ampersand)
             flat-str))

(def lang-modifier
  (semantics (conc (lit-conc-seq ":lang(") letter letter (opt (conc minus-sign letter letter)) close-paren)
             flat-str))

(def other-pseudo-class-modifier
  (semantics (conc colon (rep+ (alt letter minus-sign)))
             flat-str))

(def pseudo-class-modifier
  (semantics (alt lang-modifier other-pseudo-class-modifier)
             flat-str))

(def css-start-class-name-char
  (semantics (alt letter minus-sign underscore)
             flat-str))

(def css-class-name-char
  (semantics (alt css-start-class-name-char number)
             flat-str))

(def class-modifier
  (semantics (rep+ (conc dot css-start-class-name-char (rep* css-class-name-char)))
             flat-str))

(def css-id-char
  (semantics (alt css-class-name-char dot colon)
             flat-str))

(def id-modifier
  (semantics (conc hash-tag letter (rep* css-id-char))
             flat-str))

(def css-attribute-modifier-value-description
  (semantics (conc (opt (alt pipe-char tilde asterisk dollar caret))
                   equals-sign (alt (semantics string-literal :content) (rep+ (except anything close-square-bracket))))
             flat-str))

(def attribute-modifier
  (semantics (conc open-square-bracket (rep+ letter) (opt css-attribute-modifier-value-description) close-square-bracket)
             flat-str))

(def modifier
  (semantics (alt pseudo-class-modifier class-modifier id-modifier attribute-modifier)
             flat-str))

(def element-selector
  (semantics (alt
               (conc tag (rep* modifier))
               id-modifier
               class-modifier)
             flat-str))

(def child-connector
  (semantics (conc (rep* nb-whitespace) greater-than (rep* nb-whitespace))
             flat-str))

(def sibling-connector
  (conc (rep* nb-whitespace) plus-sign (rep* nb-whitespace)))

(declare css-selector-finder)

(def descendant-selector
  (semantics (conc element-selector (rep+ nb-whitespace) css-selector-finder)
             flat-str))

(def child-selector
  (semantics (conc element-selector child-connector css-selector-finder)
             flat-str))

(def sibling-selector
  (semantics (conc element-selector sibling-connector css-selector-finder)
             flat-str))

(def css-selector-finder
  (semantics
    (alt sibling-selector
         child-selector
         descendant-selector
         element-selector)
    flat-str))

(def css-selector
  (semantics css-selector-finder #(make-node :selector %)))

(def additional-css-selector
  (complex [_ (opt whitespace)
            _ comma
            _ (opt whitespace)
            x css-selector] x))

(def css-selector-list
  (complex [first-selector css-selector
            rest-selectors (rep* additional-css-selector)]
    (make-node :selector-list (filter (comp not nil?) (cons first-selector rest-selectors)))))

(def start-css-rule
  (conc (rep* whitespace) open-brace (rep* whitespace)))

(def end-css-rule
  (conc (rep* whitespace) close-brace (rep* whitespace)))

(def css-property
  (semantics (rep+ no-quote-string-char)
             #(make-node :property (apply str (seq %)))))

(def unit-literal
  (semantics
    (alt (lit-conc-seq "em" nb-char-lit)
         (lit-conc-seq "ex" nb-char-lit)
         (lit-conc-seq "px" nb-char-lit)
         (lit-conc-seq "gd" nb-char-lit)
         (lit-conc-seq "rem" nb-char-lit)
         (lit-conc-seq "vw" nb-char-lit)
         (lit-conc-seq "vh" nb-char-lit)
         (lit-conc-seq "vm" nb-char-lit)
         (lit-conc-seq "ch" nb-char-lit)
         (lit-conc-seq "in" nb-char-lit)
         (lit-conc-seq "cm" nb-char-lit)
         (lit-conc-seq "mm" nb-char-lit)
         (lit-conc-seq "pt" nb-char-lit)
         (lit-conc-seq "pc" nb-char-lit)
         (lit-conc-seq "%" nb-char-lit)
         (lit-conc-seq "deg" nb-char-lit)
         (lit-conc-seq "rad" nb-char-lit)
         (lit-conc-seq "grad" nb-char-lit)
         (lit-conc-seq "turn" nb-char-lit)
         (lit-conc-seq "ms" nb-char-lit)
         (lit-conc-seq "s" nb-char-lit)
         (lit-conc-seq "Hz" nb-char-lit)
         (lit-conc-seq "kHz" nb-char-lit))
    #(apply str %)))

(def short-hex-color
  (complex [_ hash-tag
            d (factor= 3 hexadecimal-digit)]
    (make-node :color (apply str (interleave d d)))))

(def long-hex-color
  (complex [_ hash-tag
            d (factor= 6 hexadecimal-digit)]
    (make-node :color (apply str d))))

(def hex-color-literal
  (alt long-hex-color
       short-hex-color))

(def color-keywords
  {"white"    "ffffff"
   "gray"     "808080"
   "black"    "000000"
   "yellow"   "ffff00"
   "olive"    "808000"
   "fuchsia"  "ff00ff"
   "purple"   "800080"
   "aqua"     "00ffff"
   "teal"     "008080"
   "red"      "ff0000"
   "maroon"   "800000"
   "lime"     "00ff00"
   "green"    "008000"
   "blue"     "0000ff"
   "navy"     "000080"
   "silver"   "c0c0c0"
   "orange"   "ffa500"})

(defn color-keyword-rule [c]
  (constant-semantics (lit-conc-seq c nb-char-lit)
             (make-node :color (color-keywords c))))

(def color-keyword
  (eval `(alt ~@(for [c (keys color-keywords)] (list 'color-keyword-rule c)))))

(def color-literal
  (alt hex-color-literal
       color-keyword))

(def basic-number-literal
  (semantics (conc (opt (alt minus-sign plus-sign))
                   (rep+ number)
                   (opt (conc dot (rep+ number))))
             #(Double/parseDouble (apply str (flatten %)))))

(def number-literal-with-unit
  (complex [n basic-number-literal
            u unit-literal]
    (make-node :number-with-unit [n u])))

(def number-literal-without-unit
  (semantics basic-number-literal #(make-node :number %)))

(def number-literal
  (alt number-literal-with-unit
       number-literal-without-unit))

(def ratio-literal
  (complex [nmrtr number-literal
            _     (opt whitespace)
            _     forward-slash
            _     (opt whitespace)
            dnmtr number-literal]
    (make-node :ratio {:numerator nmrtr :denomenator dnmtr})))

(def false-literal
  (constant-semantics (lit-conc-seq "false" nb-char-lit)
                      {:kind :boolean :content false}))

(def true-literal
  (constant-semantics (lit-conc-seq "true" nb-char-lit)
                      {:kind :boolean :content true}))

(def boolean-literal
  (alt false-literal true-literal))

(def sass-variable
  (complex [_     dollar
            vname (conc (alt letter underscore)
                        (rep* (alt alphanumeric underscore)))]
    (make-node :sass-variable (flat-str vname))))

(def non-ratio-arg
  (alt boolean-literal
       sass-variable
       number-literal
       color-literal
       string-literal))

(def single-arg
  (alt ratio-literal
       non-ratio-arg))

(def extra-arg
  (complex [_   (opt whitespace)
            _   comma
            _   (opt whitespace)
            a   single-arg] a))

(def arg-list
  (complex [first-arg single-arg
            rest-args (rep* extra-arg)]
    (vec (cons first-arg rest-args))))

(def function-char
  (alt letter number underscore minus-sign))

(def function-name-literal
  (semantics (conc (except function-char number)
                   (rep* function-char))
             flat-str))

(def function-call
  (complex [func function-name-literal
            _    open-paren
            _    (opt whitespace)
            args (opt arg-list)
            _    (opt whitespace)
            _    close-paren]
    (make-node :function-call {:function func :args args})))

(def extra-string
  (complex [_ comma
            _ (opt whitespace)
            s string-literal] s))

(def string-list
  (complex [first-string string-literal
            rest-strings (rep+ extra-string)]
    (make-node :string-list (cons first-string rest-strings))))

(def css-declaration-value
  (alt ratio-literal
       number-literal-with-unit
       number-literal-without-unit
       function-call
       color-literal
       sass-variable
       string-list
       string-literal))

(def css-value-list
  (complex [first-value   css-declaration-value
            rest-values   (rep+ (complex [_ (rep+ nb-whitespace) value css-declaration-value] value))]
    (vec (cons first-value rest-values))))

(def css-value-declaration
  (alt css-value-list
       css-declaration-value))

(def assignment-separator
  (conc (rep* nb-whitespace) colon (rep* nb-whitespace)))

(def expr-terminator
  (conc (rep* nb-whitespace) semicolon (rep* whitespace)))

(def standard-css-rule-declaration
  (complex [property      css-property
            _             assignment-separator
            value         css-value-declaration
            _             expr-terminator]
    (make-node :css-rule-declaration {:property property :value value})))

(def css-nested-property-declaration
  (complex [namespace-p     css-property
            _               assignment-separator
            namespace-v     (opt css-value-declaration)
            _               start-css-rule
            nested-contents (rep+ standard-css-rule-declaration)
            _               end-css-rule]
    (make-node :css-nested-property-declaration {:namespace {:property namespace-p
                                                             :value    namespace-v}
                                                 :namespaced-declarations (vec nested-contents)})))

(def sass-variable-assignment
  (complex [variable sass-variable
            _        assignment-separator
            assign   css-value-declaration
            _        expr-terminator]
    (make-node :sass-variable-assignment {:variable variable :value assign})))

(def css-rule-declaration
  (alt sass-variable-assignment
       standard-css-rule-declaration
       css-nested-property-declaration))

(declare css-rule)

(def css-rule-contents
  (semantics (rep*
               (alt
                 css-rule-declaration
                 css-nested-property-declaration
                 css-rule))
             vec))

(def css-rule
  (complex [selectors css-selector-list
            _         start-css-rule
            contents  css-rule-contents
            _         end-css-rule]
    (make-node :css-rule {:selector selectors :declarations contents})))

(def begin-single-line-comment
  (conc forward-slash forward-slash))

(def single-line-comment
  (conc begin-single-line-comment (rep* (except anything line-break)) line-break))

(def begin-multi-line-comment
  (conc forward-slash asterisk))

(def end-multi-line-comment
  (conc asterisk forward-slash))

(def multi-line-comment
  (conc begin-multi-line-comment (rep* (except anything end-multi-line-comment)) end-multi-line-comment))

(def ignore
  (semantics (alt multi-line-comment single-line-comment whitespace)
             (fn [_] nil)))

(def css-expr
  (alt ignore
       sass-variable-assignment
       css-rule))

(def css-text
  (rep+ css-expr))

(defn parse-css
  [tokens]
  (filter (comp not nil?)
    (rule-match css-text
      (fn [_] (println "Total failure. There is no useful information I can give."))
      (fn [_ state] (println (str "Error at line " (:line state) ", column " (:column state) ".\n"
                                  (apply str (take 150 (:remainder state))))))
      (struct state-s tokens 0 0))))
