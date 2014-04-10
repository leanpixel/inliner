# Inliner

Inliner is a very simple Clojure library for inlining CSS.  Based on (a slightly
modified version of) [Simon Hicks' css-parser][css-parser] and, of course,
[cgrand's enlive][enlive].

## Artifact

Available on [clojars][clojars].  Latest version is `0.1.0`:

```
[inliner "0.1.0"]
```

## Usage

```clj
(require '[inliner.core :refer [inline-css]])

(def html-str (str "<html><head><style>p { color: #ff0000; }</style></head>"
                   "<body><p>hello</p><p>world</p>"))

(inline-css html-str)
; outputs
; "<html><head></head><body><p style=\"color: #ff0000;\">hello</p><p style=\"color: #ff0000;\">world</p></body></html>"
```

Any stylesheet `<link>`s will be resolved relative to `resources/` (e.g.
`<link rel="stylesheet" href="/email_templates/for_tests.css">"` will read
`resources/email_templates/for_tests.css`)

## Cavets

It is entirely possible that complicated CSS rules won't get matched properly.
Let me know if you have problems and I will do what I can!

## License

Copyright © 2014 James Cash

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

  [css-parser]: https://github.com/simonhicks/css-parser/
  [enlive]: https://github.com/cgrand/enlive
  [clojars]: https://clojars.org/inliner
