; HTML Utils.


(def color (r g b)
  (zap clamp r 0 255)
  (zap clamp g 0 255)
  (zap clamp b 0 255)
  (obj :r :g :b))

(def dehex (str) (safe (coerce str 'int 16)))

(defmemo hex>color (str)
  (and (is (len str) 6)
       (withs (r (dehex (cut str 0 2))
               g (dehex (cut str 2 4))
               b (dehex (cut str 4 6)))
         (and r g b
              (color r g b)))))

(defmemo gray (n) (color n n n))

(= white     (gray 255)
   black     (gray 0)
   linkblue  (color 0 0 190)
   orange    (color 255 102 0)
   orangered (color 218 83 44)
   darkred   (color 180 0 0)
   darkblue  (color 0 0 120)
   teal      (color 91 186 213)
   )

(or= opmeths* (table))

(def opmeth (spec opt)
  (or (opmeths* (list spec opt))
      (case opt
        (id align valign) opsym
        (name type)       opstring
        (class style src) opstring
        (onclick onfocus) opstring
        (width height)    opstring
        (color bgcolor)   opcolor)))

(mac attribute (tag opt f)
  `(= (opmeths* (list ',tag ',opt)) ,f))

(= hexreps (table))

(for i 0 255 (= (hexreps i)
                (let s (coerce i 'string 16)
                  (if (is (len s) 1) (+ "0" s) s))))

(defmemo hexrep (col)
  (+ (hexreps (col 'r)) (hexreps (col 'g)) (hexreps (col 'b))))

(def opcolor (key val)
  (letu gv
    `(whenlet ,gv ,val
       (pr ,(string " " key "=#") (hexrep ,gv)))))

(def opstring (key val)
  `(aif ,val (pr ,(+ " " key "=\"") it #\")))

(def opnum (key val)
  `(aif ,val (pr ,(+ " " key "=") it)))

(def opsym (key val)
  `(aif ,val (pr ,(+ " " key "=") it)))

(def opsel (key val)
  `(if ,val (pr " selected")))

(def opcheck (key val)
  `(if ,val (pr " checked")))

(def opesc (key val)
  `(awhen ,val
     (pr ,(string " " key "=\""))
     (if (isa!string it) (pr-escaped it) (pr it))
     (pr  #\")))

; need to escape more?  =?

(def pr-escaped (x)
  (each c x
    (pr (case c #\<  "&#60;"
                #\>  "&#62;"
                #\"  "&#34;"
                #\&  "&#38;"
                c))))

(attribute a          href           opstring)
(attribute a          rel            opstring)
(attribute a          title          opstring)
(attribute body       alink          opcolor)
(attribute body       vlink          opcolor)
(attribute body       link           opcolor)
(attribute body       text           opcolor)
(attribute body       leftmargin     opnum)
(attribute body       marginheight   opnum)
(attribute body       marginwidth    opnum)
(attribute body       topmargin      opnum)
(attribute body       background     opstring)
(attribute br         clear          opsym)
(attribute font       face           opstring)
(attribute font       size           opnum)
(attribute form       action         opstring)
(attribute form       method         opsym)
(attribute form       onsubmit       opstring)
(attribute img        border         opnum)
(attribute img        vspace         opnum)
(attribute img        hspace         opnum)
(attribute img        src            opstring)
(attribute img        alt            opstring)
(attribute meta       property       opstring)
(attribute meta       content        opstring)
(attribute link       rel            opstring)
(attribute link       href           opstring)
(attribute link       sizes          opstring)
(attribute link       title          opstring)

(attribute input      size           opnum)
(attribute input      value          opesc)
(attribute input      checked        opcheck)
(attribute input      oninput        opstring)
(attribute input      autocorrect    opstring)
(attribute input      spellcheck     opstring)
(attribute input      autocapitalize opstring)
(attribute input      autofocus      opstring)
(attribute option     selected       opsel)
(attribute table      border         opnum)
(attribute table      cellpadding    opnum)
(attribute table      cellspacing    opnum)
(attribute textarea   cols           opnum)
(attribute textarea   rows           opnum)
(attribute textarea   wrap           opsym)
(attribute textarea   oninput        opstring)
(attribute td         colspan        opnum)
(attribute rss        version        opstring)
(attribute rss        xmlns:content  opstring)
(attribute rss        xmlns:dc       opstring)
(attribute script     src            opstring)

(mac gentag (:kws . args)
  (start-tag (+ args kws)))

(def pratom (x)
  (zap macex x)
  (if (in (type x) 'string 'sym) `(pr ,x) x))

(mac tag (spec :kws . body)
  (= spec (+ (listify spec) kws))
  `(do ,(start-tag spec)
       ,@(map pratom body)
       ,(end-tag spec)
       nil))

(mac tag-if (test spec :kws . body)
  `(if ,test
       (tag ,spec ,@kws ,@body)
       (do ,@body)))

(def start-tag (spec)
  (if (atom spec)
      `(pr ,(string "<" spec ">"))
      (let opts (tag-options (car spec) (hug (cdr spec)))
        (if (all isa!string opts)
            `(pr ,(string "<" (car spec) (apply string opts) ">"))
            `(do (pr ,(string "<" (car spec)))
                 ,@(map pratom opts)
                 (pr ">"))))))

(def end-tag (spec)
  `(pr ,(string "</" (carif spec) ">")))

(def literal (x)
  (case (type x)
    sym    ~t
    string (~cdr (#'codestring x))
    cons   (caris x 'quote)
           t))

; Returns a list whose elements are either strings, which can
; simply be printed out, or expressions, which when evaluated
; generate output.

(def tag-options (spec options)
  (if (no options)
      '()
      (let ((opt val) . rest) options
        (zap sym opt)
        (def meth (opmeth spec opt))
        (when (is opt 'value2)
          (= opt 'value meth opstring))
        (if meth
            (if val
                (cons (if (precomputable-tagopt val)
                          (tostring (eval (meth opt val)))
                          (meth opt val))
                      (tag-options spec rest))
                (tag-options spec rest))
            (do
              (pr "<!-- ignoring " opt " for " spec "-->")
              (tag-options spec rest))))))

(def precomputable-tagopt (val)
  (literal val))

(def br ((o n 1))
  (repeat n (pr "<br>"))
  (prn))

(def br2 () (prn "<br><br>"))

(mac center    (:kws . body)     `(tag center          ,@kws ,@body))
(mac underline (:kws . body)     `(tag u               ,@kws ,@body))
(mac tab       (:kws . body)     `(tag table border: 0 ,@kws ,@body))
(mac tr        (:kws . body)     `(tag tr              ,@kws ,@body))

(let pratoms (fn (body)
               (if (or (no body) 
                       (all [and (acons _) (isnt (car _) 'quote)]
                            body))
                   body
                   `((pr ,@body))))

  (mac td       (:kws . body)     `(tag td ,@kws ,@(pratoms body)))
  (mac trtd     (:kws . body)     `(tr (td ,@kws ,@(pratoms body))))
  (mac tdr      (:kws . body)     `(tag td align: 'right ,@kws ,@(pratoms body)))
  (mac tdcolor  (col :kws . body) `(tag td bgcolor: ,col ,@kws ,@(pratoms body)))
)

(mac row args
  `(tr ,@(map [list 'td _] args)))

(mac prrow args
  (letu g
    `(tr ,@(map (fn (a)
                  `(let ,g ,a
                     (if (number ,g)
                         (tdr (pr ,g))
                         (td (pr ,g)))))
                 args))))

(mac prbold body `(tag b (pr ,@body)))

(def para args
  (gentag p)
  (when args (apply pr args)))

(def menu (name items (o sel nil))
  (tag select :name
    (each i items
      (tag option selected: (is i sel)
        (pr i)))))

(mac whitepage body
  `(tag html
     (tag body bgcolor: white alink: linkblue ,@body)))

(def errpage args (whitepage (apply prn args)))

(def blank-url () "/s.gif")

; Could memoize these.

; If h = 0, doesn't affect table column widths in some Netscapes.

(def hspace (n)    (gentag img src (blank-url) height 1 width n))
(def vspace (n)    (gentag img src (blank-url) height n width 0))
(def vhspace (h w) (gentag img src (blank-url) height h width w))

(def new-hspace (n)
  (tag span style: "padding-left: @{n}px"))

;(def spacerow (h) (tr (td (vspace h))))

(def spacerow (h (o units "px")) (tr style: (+ "height: " h units)))
(def spacecol (w (o units "px")) (td style: (+ "width: " w units)))

; For use as nested table.

(mac zerotable (:kws . body)
  `(tag table border: 0 cellpadding: 1 cellspacing: 0 ,@kws ,@body))

; was `(tag (table border 0 cellpadding 0 cellspacing 7) ,@body)

(mac sptab (:kws . body)
  `(tag table style: "border-spacing: 7px 0px;" ,@kws ,@body))

(mac widtable (w :kws . body)
  `(tag table width: ,w ,@kws (tr (td ,@body))))

(def cellpr (x) (pr (or x "&nbsp;")))

(def but ((o text "submit") (o name nil))
  (gentag input type: 'submit :name value: text))

(def submit ((o value "submit"))
  (gentag input type: 'submit :value))

(def buts (name . texts)
  (if (no texts)
      (but)
      (do (but (car texts) name)
          (each text (cdr texts)
            (pr " ")
            (but text name)))))

(mac spanrow (n :kws . body)
  `(trtd colspan: ,n ,@kws ,@body))

(mac form (action :kws . body)
  `(tag form method: 'post action: ,action ,@kws ,@body))

(mac textarea (name rows cols :kws . body)
  `(tag textarea name: ,name rows: ,rows cols: ,cols ,@kws ,@body))

(def input (name (o value "") (o size 10) (o :type 'text) :id :oninput :onfocus)
  (gentag input :type :name :value :size :id :oninput :onfocus))

(mac inputs args
  `(tag table border: 0
     ,@(map (fn ((name label len text (o plain) (o autofocus)))
              (letu (gl gt)
                `(let ,gl ,len
                   (tr (td (pr ',label ":"))
                       (if (isa!cons ,gl)
                           (td (textarea ',name (car ,gl) (cadr ,gl)
                                 (let ,gt ,text (if ,gt (pr ,gt)))))
                           (td (gentag input
                                       type ',(if (is label 'password) 'password 'text)
                                       name ',name
                                       size ,len
                                       value ,text
                                       autofocus (if ,autofocus "true")
                                       autocorrect (if ,plain "off")
                                       spellcheck (if ,plain "false")
                                       autocapitalize (if ,plain "off"))))))))
            args)))

(def single-input (label name chars btext (o pwd) (o value))
  (pr label)
  (gentag input type (if pwd 'password 'text) name name size chars value value)
  (sp)
  (submit btext))

(mac cdata body
  `(do (pr "<![CDATA[")
       ,@body
       (pr "]]>")))

(def eschtml (str)
  (tostring
    (each c str
      (pr (case c #\<  "&#x3c;"
                  #\>  "&#x3e;"
                  #\"  "&#x22;"
                  #\'  "&#x27;"
                  #\&  "&#x26;"
                        c)))))

(def uneschtml (str)
  (multisubst
    '(("&#x3c;" "<")
      ("&#x3e;" ">")
      ("&#x22;" "\"")
      ("&#x27;" "'")
      ("&#x26;" "&"))
    str))

(def esc-tags (str)
  (tostring
    (each c str
      (pr (case c #\<  "&#x3c;"
                  #\>  "&#x3e;"
                  #\&  "&#x26;"
                        c)))))

(def unesc-tags (str)
  (multisubst
    '(("&#x3c;" "<")
      ("&#x3e;" ">")
      ("&#x26;" "&"))
    str))

(def nbsp () (pr "&nbsp;"))

(def link (text (o dest (+ (if (headmatch "/" (str text)) "" "/") text)) :color :onclick :title)
  (tag a href: dest :onclick :title
    (tag-if color font :color
      (pr text))))

(def underlink (text (o dest text) :onclick :title)
  (tag a href: dest :onclick :title
    (tag u (pr text))))

(def striptags (s)
  (let intag nil
    (tostring
      (each c s
        (if (is c #\<) (= intag t)
            (is c #\>) (wipe intag)
            (no intag) (pr c))))))

(def clean-url (u)
  (rem [in _ #\" #\' #\< #\>] u))

(def shortlink (url)
  (unless (or (no url) (< (len url) 7))
    (link (cut url 7) url)))

; this should be one regexp

(def parafy (str)
  (let ink nil
    (tostring
      (each c str
        (pr c)
        (unless (whitec c) (= ink t))
        (when (is c #\newline)
          (unless ink (pr "<p>"))
          (wipe ink))))))

(mac spanclass (name :kws . body)
  `(tag span class: ',name ,@kws ,@body))

(def pagemessage (text)
  (when text (prn text) (br2)))

; Could be stricter.  Memoized because looking for chars in Unicode
; strings is terribly inefficient in Mzscheme.

(defmemo valid-url (url)
  (and (len> url 10)
       (or (begins url "http://")
           (begins url "https://"))
       (~find [in _ #\< #\> #\" #\'] url)))

(mac fontcolor (c :kws . body)
  (letu g
    `(let ,g ,c
       (tag-if color font color: ,g ,@kws
         ,@body))))

(def script (js (o type "text/javascript"))
  (tag script :type (pr js)))
