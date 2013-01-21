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
   :Inline '(| :Str :Endline :UlOrStarLine :Space :Strong :Emph :Image :Link
               :Code :Entity :EscapedChar
               :Symbol) ; TODO :Smart

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
   :EmphStar [\* '(! :Whitespace) ['(! \*) '(| :Inline :StrongStar)]
                              '(* ['(! \*) '(| :Inline :StrongStar)]) \*]
   :EmphUl [\_ '(! :Whitespace) ['(! \_) '(| :Inline :StrongUl)]
                            '(* ['(! \_) '(| :Inline :StrongUl)]) \_]

   :Strong '(| :StrongUl :StrongStar)
   :StrongStar [:DoubleStar '(! :Whitespace)
                :StrongStarInline '(* :StrongStarInline)]
   :StrongStarInline ['(! :DoubleStar) :Inline ]
   :DoubleStar (am-str "**")
   :StrongUl [:DoubleUl '(! :Whitespace) :StrongUlInline '(* :StrongUlInline)]
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
               :Label :Spnl :RefSrc :RefTitle :Blankline '(* :Blankline)]
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
   :Line [:RawLine]
   :RawLine '(| [(* (% (| \return \newline))) :Newline] [:Any (* :Any) :$])
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

(type (:HexDigit markdown-grammar))
;; Use example
(empty? 1)
(defn null? [a]
  (or (nil? a) (= a "") (= a []) (= a ())))

(defn str-str [a]
  (cond
   (string? a) a
   (char? a)   (str a)
   (map? a)    (apply str (map str-str (vals a)))
   (or (vector? a) (seq? a))    (apply str (map str-str a))))

(defn wrap-tag [tag]
  (fn [a]
    (if-not (null? a)
      {:tag tag :content (string/trim (str-str a))}
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
  (apply vector (cons (:ListItem ((a 0) 0))
                      (map #(:ListItem (% 0)) (a 1))))
  a))

(defn bullet-list [a]
  (if-not (null? a)
    {:tag :ul
      :content(or (:ListLoose (a 1)) (:ListTight) (a 1))}
    a))

(defn block [a]
  (if-not (null? a)
    (reduce (fn [acc item] (or acc (item (a 1)))) false blocks)
    a))

(defn heading [a]
  (if-not (null? a)
    (or (:AtxHeading a) (:SetextHeading a))
    a))

(pprint (am/post-process  :Doc markdown-grammar
             (am/wrap-string (slurp "test/parse_markdown/basics.md"))
             {:Doc #(apply vector (cons (:Block (% 1)) (map :Block (% 2))))
              :Block block
              :Str str-str
              :Heading heading
              :SourceContents str-str
              :ListItem (wrap-tag :li)
              :ListLoose list-loose
              :BulletList bullet-list
              :Bullet nothing
              :AtxHeading atx-heading
              }))

;; Experimental

(def a [()
    {:BulletList
     [{:tag :li, :content "asjlkdhfe sdkljhf"}
      {:tag :li, :content "lsakjdf fjksdl"}
      {:tag :li, :content "dkdkdkdkd"}]}])
(block a)
(map #(:ListItem (% 0)) (a 1))

(pprint "------")
(def pred {:a [\b '(& \a)]})
(am/pegasus :a pred (am/wrap-string "ba"))

(am/pegasus :Link
            markdown-grammar
            (am/wrap-string "[markdown specification](http://daringfireball.net/projects/markdown/syntax#list)\n"))
(am/pegasus :ScIn
            markdown-grammar
            (am/wrap-string ")"))

(apply vector '(1 2 3))
