(ns digest.emailer.css-inliner-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [net.cgrand.enlive-html :as h]
            [digest.emailer.css-inliner :as inliner]))

(defn strip-whitespace
  [s]
  (string/replace s #"\s+" ""))

(deftest test-inlining-html
  (let [trimmed (fn [& args] (apply str (map string/trim args)))
        input-html (trimmed
                     "<html><head>"
                     " <link rel=\"stylesheet\" href=\"/email_templates/for_tests.css\">"
                     "  <style type=\"text/css\">"
                     "     h1 { font-size: 2.0em; background-color: #ff0000; }"
                     "     p { font-size: 1.0em; }"
                     " </style>"
                     "  <style>li p { border: 1px solid black; }</style>"
                     "  <style>li.foo, input[type=text] { font-color: #00ff00; }</style>"
                     "  <style>input[foo~=bar] { border: #0000ff; }</style>"
                     "<body>"
                     "  <h1>Hello world!</h1>"
                     "  <p>This is something</p>"
                     "  <ul>"
                     "     <li><p>bloop</p></li>"
                     "     <li class=\"foo\"><p>blap</p></li>"
                     "  </ul>"
                     "  <input type=\"text\">"
                     "  <input type=\"password\" foo=\"bar baz\">"
                     "</body></html>")
        output-html  (trimmed
                       "<html><head></iead>"
                       "  <body>"
                       "    <h1 style=\"font-size: 2.0em; background-color: #ff0000;\">Hello world!</h1>"
                       "    <p style=\"font-size: 1.0em;\">This is something</p>"
                       "    <ul style=\"margin: 0.0;\">"
                       "       <li><p style=\"font-size: 1.0em;border: 1.0px solid #000000;\">bloop</p></li>"
                       "       <li style=\"font-color: #00ff00;\" class=\"foo\">"
                       "           <p style=\"font-size: 1.0em;border: 1.0px solid #000000;\">blap</p>"
                       "       </li>"
                       "    </ul>"
                       "  <input style=\"font-color: #00ff00;\" type=\"text\">"
                       "  <input style=\"border: #0000ff;\" type=\"password\" foo=\"bar baz\">"
                       "</body></html>")]
    (let [styles (inliner/extract-styles (h/html-resource (java.io.StringReader. input-html)))
          sels (inliner/styles->selectors styles)]
      (is (= 5 (count styles)))
      (is (= 7 (count sels)))
      #_(is (= #{[:h1] [:p] [:li :p] [:li.foo] [:ul] [:input]}
             (set (map first sels))))
      (is (= "font-color: #00ff00;"
             (second (first (filter #(= (first %) [:li.foo]) sels)))
             #_(second (first (filter #(= (ffirst %) :input) sels)))))
      (is (= "font-size: 2.0em; background-color: #ff0000;"
             (second (first (filter #(= (first %) [:h1]) sels)))))
      (is (= "border: 1.0px solid #000000;"
             (second (first (filter #(= (first %) [:li :p]) sels))))))
    (is (= (h/html-resource (java.io.StringReader. (inliner/inline-css input-html)))
           (h/html-resource (java.io.StringReader. output-html))))))
