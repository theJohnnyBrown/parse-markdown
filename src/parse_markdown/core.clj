
(ns parse-markdown.core
  (:require [com.lithinos.amotoen.core :as am]
            [clojure.string :as string]))


;; markdown grammar
;; © 2008 John MacFarlane (jgm at berkeley dot edu).
;; GPL?

(def custom-collapse #(if % (apply str %) %))
(defmacro am-str [s]
  `(list ~'(symbol "f") custom-collapse (am/pegs ~s)))
(def headings (map #(am-str (apply str (repeat % "#"))) (range 6 0 -1)))
(def blocks '(:BlockQuote
              :Verbatim
              :Reference
              :HorizontalRule
              :Heading
              :OrderedList
              :BulletList
                                        ;:HtmlBlock
                                        ;:StyleBlock
              :Para
              :Plain))
(def inlines '(:Str :Endline :UlOrStarLine :Space :Strong :Emph :Image :Link
               :Code :Entity :EscapedChar
               :Symbol))
(def markdown-grammar
  {
   :Doc ['(| :BOM []) :Block '(* :Block)]
   :Block ['(* :Blankline) (apply list (list* '| blocks))]
   :Para      [:NonindentSpace :Inlines :Blankline '(* :Blankline)]
   :Plain [:Inlines]

   :AtxInline ['(! :Newline) '(! [:Sp? (* \#) :Sp :Newline] ) :Inline  ]
   :AtxStart  (apply list  (cons '| headings))
   :AtxHeading [:AtxStart :Sp?
                [:AtxInline '(* :AtxInline)]
                '(| [:Sp? (* \#) :Sp] []) :Newline]

   :SetextHeading '(| :SetextHeading1 :SetextHeading2)
   :SetextBottom1 [\= '(* \=) :Newline]
   :SetextBottom2 [\- '(* \-) :Newline]
   :SetextHeading1 ['(& [:RawLine :SetextBottom1])
                    ['(! :Endline) :Inline] '(* ['(! :Endline) :Inline])
                    :Sp? :Newline :SetextBottom1]
   :SetextHeading2 ['(& [:RawLine :SetextBottom2])
                    ['(! :Endline) :Inline] '(* [(! :Endline) :Inline])
                    :Sp? :Newline :SetextBottom2]

   :Heading '(| :SetextHeading :AtxHeading)

   :BlockQuote [[\> '(| \space [])]
                '(* [(! \> :Blankline ) :Line]) '(* :Blankline)]

   :NonblankIndentedLine ['(! :Blankline ) :IndentedLine]
   :VerbatimChunk ['(* :Blankline)
                   :NonblankIndentedLine '(* :NonblankIndentedLine)]
   :Verbatim [:VerbatimChunk '(* :VerbatimChunk)]

   :HorizontalRule [:NonindentSpace
                    '(| [\* :Sp \* :Sp \* (* [:Sp \*])])
                    '(| [\- :Sp \- :Sp \- (* [:Sp \-])])
                    '(| [\_ :Sp \_ :Sp \_ (* [:Sp \_])])]

   :Bullet ['(! :HorizontalRule)
            :NonindentSpace '(| \+ \* \-) :Spacechar '(* :Spacechar)]
   :BulletList ['(& :Bullet) '(| :ListTight :ListLoose)]
   :ListTight [:ListItemTight '(* :ListItemTight) '(* :Blankline)
               '(! :Bullet :Enumerator)]
   :ListLoose [[:ListItem '(* :Blankline)] '(* [:ListItem (* :Blankline)])]
   :ListItem ['(| :Bullet :Enumerator) :ListBlock '(* :ListContinuationBlock)]
   :ListItemTight ['(| :Bullet :Enumerator)
                   :ListBlock '(* [(! :Blankline) :ListContinuationBlock])
                   :ListContinuationBlock]
   :ListBlock ['(! :Blankline) :Line '(* :ListBlockLine)]
   :ListContinuationBlock ['(* :Blankline) [:Indent :ListBlock]
                           '(* [:Indent :ListBlock])]
   :Enumerator [:NonindentSpace :Digit '(* :Digit) \.
                :Spacechar '(* :Spacechar)]
   :Digit '(| \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 )
   :OrderedList ['(& :Enumerator) '(| :ListTight :ListLoose)]
   :ListBlockLine ['(! :Blankline)
                   '(! [(| :Indent []) (| :Bullet :Enumerator)])
                   '(! :HorizontalRule)
                   :OptionallyIndentedLine]

   ;; TODO HTML

   :Inlines ['(| [(! :Endline) :Inline] [:Endline (& :Inline)])
             '(* (| [(! :Endline) :Inline] [:Endline (& :Inline)]))
             '(| :Endline [])]
   :Inline (apply list (list* '| inlines)) ; TODO :Smart

   :Space [:Spacechar '(* :Spacechar)]
   :Str [:NormalChar '(* :NormalChar) '(* :StrChunk)]
   :StrChunk '(| [:NormalOrAN (* :NormalOrAN)] :AposChunk)
   :NormalOrAN '(| :NormalChar [\_ (* \_) :Alphanumeric])
   :AposChunk [\' '(& :Alphanumeric)]
   :EscapedChar [\\ '(! :Newline)
                 '(| \- \\ \` \| \* \_ \{ \} \[ \] \( \) \# \+ \. \! \> \<)]
   :Entity '(| :HexEntity :DecEntity :CharEntity)
   :Endline '(| :LineBreak :NormalEndline)
   :NormalEndline [:Sp :Newline '(! :Blankline \> :AtxStart
                                    [:Line (| [\= (* \=)]
                                              [\- (* \-)]) :Newline])]
   ;; :TerminalEndline [:Sp :Newline :$]
   :LineBreak [\space \space :NormalEndline]
   :Symbol [:SpecialChar]
   :UlOrStarLine '(| :UlLine :StarLine)
   :StarLine (list '|
                   [(am-str "***") '(* \*)]
                   [:Spacechar \* '(* \*) '(& :Spacechar)])
   :UlLine (list '|
                   [(am-str "___") '(* \_)]
                   [:Spacechar \_ '(* \_) '(& :Spacechar)])

   :Emph '(| :EmphStar :EmphUl)
   :Whitespace '(| :Spacechar :Newline)
   :EmphStar [\* '(! :Whitespace) ['(! \*) '(| :Inline :StrongStar)]
                              '(* [(! \*) (| :Inline :StrongStar)]) \*]
   :EmphUl [\_ '(! :Whitespace) ['(! \_) '(| :Inline :StrongUl)]
                            '(* [(! \_) (| :Inline :StrongUl)]) \_]

   :Strong '(| :StrongUl :StrongStar)
   :StrongStar [:DoubleStar '(! :Whitespace)
                :StrongStarInline '(* :StrongStarInline)
                :DoubleStar]
   :StrongStarInline ['(! :DoubleStar) :Inline ]
   :DoubleStar (am-str "**")
   :StrongUl [:DoubleUl
              '(! :Whitespace) :StrongUlInline '(* :StrongUlInline)
              :DoubleUl]
   :StrongUlInline ['(! :DoubleUl) :Inline ]
   :DoubleUl (am-str "__")

   :Image [\! '(| :ExplicitLink :ReferenceLink)]

   :Link '(| :ExplicitLink :ReferenceLink :AutoLink)
   :ReferenceLink '(| :ReferenceLinkDouble :ReferenceLinkSingle)
   :ReferenceLinkDouble [:Label :Spnl (list '! (am-str "[]")) :Label]
   :ReferenceLinkSingle [:Label (list '& [:Spnl (am-str "[]")])]
   :ExplicitLink [:Label \( :Sp :Source :Spnl :Title :Sp \) ]
   :Source '(| [\< :SourceContents \>] :SourceContents)
   :SourceContents '(| [:ScIn (* :ScIn)] [\( :SourceContents \) ])
   :ScIn ['(! (| \( \) \>)) :NonspaceChar]

   :Title '(| :TitleSingle :TitleDouble [])
   :TitleSingle [\' '(*  [(! \' :Sp (| \) :Newline))  :.] ) \']
   :TitleDouble [\" '(*  [(! \" :Sp (| \) :Newline))  :.] ) \"]
   :AutoLink '(| :AutoLinkUrl :AutoLinkEmail)
   :AutoLinkUrl [\< :Alphabetical '(* :Alphabetical) (am-str "://")
                 '(% (| :Newline \>)) '(* (% (| :Newline \>))) \>]
   :AutoLinkEmail [\< :EmailChar '(* :EmailChar) \@
                 '(% (| :Newline \>)) '(* (% (| :Newline \>))) \>]
   :Reference [:NonindentSpace (list '! (am-str "[]"))
               :Label \: :Spnl :RefSrc :RefTitle :Blankline '(* :Blankline)]
   :Label [\[ '(* [(! \]) :Inline])  \]]
   :RefSrc '(* :NonspaceChar)
   :RefTitle '(| :RefTitleSingle :RefTitleDouble :RefTitleParens [])
   :RefTitleSingle [:Spnl \' '(* (%  (| [\' :Sp :Newline] :Newline))) \']
   :RefTitleDouble [:Spnl \" '(* (%  (| [\" :Sp :Newline] :Newline))) \"]
   :RefTitleParens [:Spnl \( '(* (%  (| [\) :Sp :Newline] :Newline))) \)]
   :References ['(* (| :Reference :SkipBlock))]

   ;; Test this
   :Code '(| :Code1 :Code3)
   :Ticks1 [\` '(! \`)]
   :Ticks3 [(am-str "```") '(! \`)]
   :Code1 [:Ticks1 :Sp :_Code1 '(* :_Code1) :Sp :Ticks1]
   :_Code1 '(| [[(! \` ) :NonspaceChar] (* [(! \` ) :NonspaceChar])]
               [(! :Ticks1) \` (* \`)]
               [(! [:Sp :Ticks1]) (| :Spacechar [:Newline (! :Blankline)])])
   :Code3 [:Ticks3 :Sp :_Code3 '(* :_Code3) :Sp :Ticks3]
   :_Code3 '(| [[(! \` ) :NonspaceChar] (* [(! \` ) :NonspaceChar])]
               [(! :Ticks3) \` (* \`)]
               [(! [:Sp :Ticks3]) (| :Spacechar [:Newline (! :Blankline)])])
   ;; TODO RawHtml

   :Blankline [:Sp :Newline]
   :Quoted '(| [\" (* (% \")) \"] [\' (* (% \')) \'])
   :HtmlAttribute ['(| :AlphanumericAscii \-) '(* (| :AlphanumericAscii \-)) :Spnl '(| [\= :Spnl (| :Quoted [[(! \>) :NonspaceChar] (* [(! \>) :NonspaceChar])] )] []) :Spnl]
   ;; TODO HtmlComment, HtmlTag
   :Spacechar '(| \space \tab)
   :NonspaceChar '(% (| :Spacechar :Newline))
   :Newline '(| \newline [\return (| \newline [])])
   :Sp '(* :Spacechar)
   :Spnl [:Sp '(| [\newline :Sp] [])]
   :SpecialChar '(| \* \_ \` \& \[ \] \( \) \< \! \# \\ \' \")
   :NormalChar '(% (| :SpecialChar :Spacechar :Newline))
   :Alphanumeric (apply list (list* '| :Alphabetical :Digit
                        (map char (range 200 380))))
   ;; :Alphanumeric '(| :Alphabetical :Digit \È \É \Ê)
   :AlphanumericAscii '(| :Alphabetical :Digit)
   :HexEntity [\& \# '(| \x \X) :HexDigit '(* :HexDigit) \;]
   :HexDigit (apply list (list* '| :Digit
                             (map char (concat (range 65 71) (range 97 103)))))
   :DecEntity [\& \#  :Digit '(* :Digit) \;]
   :CharEntity [\& :AlphanumericAscii '(* :AlphanumericAscii) \;]
   :NonindentSpace (apply list '| (map #(am-str %) (list "   " "  " " " "")))
   :Indent '(| \tab (am-str "    "))
   :IndentedLine [:Indent :Line]
   :OptionallyIndentedLine ['(| :Indent []) :Line]
   :Line ['(& :RawLine) '(* [(! :Newline) :Inline]) :Newline]
   :RawLine (list '|
                   ['(* (% (| \return \newline))) :Newline]
                   [:Any '(* :Any) :$])
   :Any (list 'a (fn [gmr in] (if (am/c in) (am/m in) nil)))
   :SkipBlock '(| [[(% (| \# :SetextBottom1 :SetextBottom2 :Blankline)) :RawLine]
                   (* [(% (| \# :SetextBottom1 :SetextBottom2 :Blankline))
                       :RawLine])
                   :Blankline (* :Blankline)]
                  [:Blankline (* :Blanline)]
                  :RawLine) ; TODO HtmlBlock

   :EmailChar (list '| :Alphabetical (am/lpegs '| "0123456789+_./!%~$"))
   :Alphabetical (am/lpegs '| "etaoinsrhbcdfgjklmpquvwxyzETAOINSRHBCDFGJKLMPQUVWXYZ")
   :Sp? '(| :Sp [])
   :BOM [(list 'f custom-collapse (am/pegs "\357\273\277"))]})


;; Use example

(defn null? [a]
  (or (nil? a) (= a "") (= a []) (= a ()) (false? a)))

(defn str-str [a]
  (cond
   (string? a) a
   (char? a)   (str a)
   (map? a)    (apply str (map str-str (vals a)))
   (or (vector? a) (seq? a))    (apply str (map str-str a))))
(defn str-strip [a strip]
  (if-not (null? a)
    ()))

(defn wrap-tag [tag]
  (fn [a]
    (if-not (null? a)
      {:tag tag :content a}
      a)))

(defn atx-heading [a]
  (if-not (null? a)
    (let [start (a 0) content (a 2)]
      {:tag (symbol (str "h" (.length (str-str start))))
       :content (string/trim (string/replace (str-str content) "#" ""))})
      a))

(defn nothing [a]
  (if-not (null? a) "" a))

(defn list-loose [a]
  (if-not (null? a)
    (apply vector (concat [(:ListItem (first (a 0)))]
            (let [more-items (a 1)
                  get-item #(:ListItem (first %))
                  ii (get-item more-items)]
              (if ii [ii] (map get-item more-items))
              )))
    a))

(defn bullet-list [a]
  (if-not (null? a)
    {:tag :ul
     :content (or (:ListLoose (a 1)) (:ListTight (a 1)))}
    a))

(defn block [a]
  (if-not (null? a)
    (reduce (fn [acc item] (or acc (item (a 1)))) false blocks)
    a))

(defn heading [a]
  (if-not (null? a)
    (or (:AtxHeading a) (:SetextHeading a))
    a))

(defn label [a]
  (if-not (null? a)
    (if (vector? (a 1))
      (:Inline ((a 1) 1))
      (map #(:Inline (% 1)) (a 1)))
    a))

(defn inline [a]
  (if-not (null? a)
    (reduce (fn [acc item] (or acc (item a))) false inlines)
    a))

(defn process-inlines [a]
  (if-not (null? a)
    (let [first-element (a 0)
          more-elements (a 1)
          get-inline #(if (true? (first %)) (:Inline (% 1)) nil)]
      (apply vector
             (filter identity
                     (concat
                      [(get-inline first-element)]
                      (if (get-inline more-elements)
                        [(get-inline more-elements)]
                        (map get-inline more-elements))))))
    a))


(defn process-doc [a]
  (if-not (null? a)
    (apply vector
           (cons (:Block (a 1))
                 (if (seq? (a 2))
                   (map :Block (a 2))
                   (:Block (a 2)))))
    a))

(defn process-source [a]
  (if-not (null? a)
    (if (vector? a)
      (:SourceContents (a 1))
      (:SourceContents a))
    a))

(defn- get-href [link]
  (reduce (fn [acc item] (or acc item)) false (map :Source link)))
(defn- get-title [link]
  (reduce (fn [acc item] (or acc item)) false (map :Title link)))
(defn explicit-link [a]
  (if-not (null? a)
    {:tag :a :attrs {:href (get-href a) :title (get-title a)}
     :content (let [cont (a 0)]
                (if (string? (:Label cont))
                 (:Label cont)
                 (apply vector (:Label cont))))}
    a))

(defn- or-key [dict & keys]
  (reduce (fn [acc item] (or acc item)) false (map #(% dict) keys)))


(defn link [a]
  (if-not (null? a)
    (or-key a :ExplicitLink :ReferenceLink :AutoLink)
    a))

(defn emph [a]
  (if-not (null? a)
    (let [elems (or (:EmphUl a) (:EmphStar a))]
      {:tag :em
       :content (apply vector
                       (list*
                        (or-key ((elems 2) 1) :Inline :StrongStar :StrongUl)
                        (map #(or-key (% 1) :Inline :StrongStar :StrongUl)
                             (elems 3))))})
    a))

(defn line [a]
  (if-not (null? a)
    (or
     (:Inline (first (rest (a 1))))
     (apply vector (map #(:Inline (% 1)) (a 1))))
    a))

(defn optindentline [a]
  (if-not (null? a)
    (:Line (a 1))
    a))

(defn listblockline [a]
  (if-not (null? a)
    (:OptionallyIndentedLine (a 3))
    a))
(defn list-block [a]
  (if-not (null? a)
    (apply vector (concat (let [firstline (:Line (a 1))]
                            (if (string? firstline)
                              [firstline]
                              firstline))
                         (or (:ListBlockLine (a 2))
                             (apply concat (map :ListBlockLine (a 2))))))
    a))
(defn list-item [a]
  (if-not (null? a)
    {:tag :li :content (let [b (:ListBlock (a 1))]
                         (if (string? b)
                           [b]
                           (apply vector b)))}
    a))
(defn plain [a]
  (if-not (null? a)
    {:tag :p :content (:Inlines (first a))}
    a))
(defn para [a]
  (if-not (null? a)
    {:tag :p :content (:Inlines (a 1))}
    a))

(defn str-to-clj [markdown-string] (am/post-process  :Doc markdown-grammar
             (am/wrap-string markdown-string)
             {:Doc process-doc
              :Link link
              :ExplicitLink explicit-link
              :Source process-source
              :SourceContents str-str
              :Emph emph
              :Block block
              :OptionallyIndentedLine optindentline
              :ListBlockLine listblockline
              :ListBlock list-block
              :Line line
              :Str str-str
              :Inline inline
              :Inlines process-inlines
              :Label label
              :Space str-str
              :Heading heading
              :Title str-str
              :ListItem list-item
              :ListLoose list-loose
              :BulletList bullet-list
              :Bullet nothing
              :AtxHeading atx-heading
              :Plain plain
              :Para para
              }))

(defn to-clj [markdown-file]
  (str-to-clj (slurp markdown-file)))
