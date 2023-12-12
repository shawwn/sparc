; Main Arc lib.  Ported to Scheme version Jul 06.

; don't like names of conswhen and consif

; need better way of generating strings; too many calls to string
;  maybe strings with escape char for evaluation
; make foo~bar equiv of foo:~bar (in expand-ssyntax)
; add sigs of ops defined in ac.scm
; get hold of error types within arc
; does macex have to be defined in scheme instead of using def below?
; write disp, read, write in arc
; could I get all of macros up into arc.arc?
; warn when shadow a global name
; some simple regexp/parsing plan

; compromises in this implementation: 
; no objs in code
;  (mac testlit args (listtab args)) breaks when called
; separate string type
;  (= (cdr (cdr str)) "foo") couldn't work because no way to get str tail
;  not sure this is a mistake; strings may be subtly different from 
;  lists of chars


(assign do (annotate 'mac
             (fn args `(%do ,@args))))

(assign warnset (fn (var)
                  (if (bound var)
                      (do (disp "*** redefining " (stderr))
                          (disp var (stderr))
                          (disp #\newline (stderr))))))

(assign safeset (annotate 'mac
                  (fn (var val)
                    (assign val `(assign ,var ,val))
                    (if (lexname) `(do (#'define ,(#'ac-env! var) nil) ,val)
                                  `(do (warnset ',var) ,val)))))

(assign def (annotate 'mac
               (fn (:tag name parms . body)
                 (assign body (if body `(fn ,parms ,@body) parms))
                 (if (lexname) nil
                     (assign body `(do (sref sig ',parms ',name) ,body)))
                 `(safeset ,name ,(if tag `(annotate ',tag ,body) body)))))

(def tag: mac mac (name parms . body)
  `(def tag: mac ,name ,parms ,@body))

(mac %brackets (:kws . args)
  `(fn (_) ,(+ args kws)))

(mac %braces (:kws . args)
"The function invoked on curly-bracket calls.
For example, {a 1 b 2} => (%braces a 1 b 2) => (obj a 1 b 2)"
  `(obj ,@(+ args kws)))

(def call (f :kws . args)
  (kwapply f kws args))

(def list (:kws . args)
  (+ args kws))

(def idfn (x) x)

(def con (x)
  (fn (:kws . args) x))

(def caar (xs) (car (car xs)))
(def cadr (xs) (car (cdr xs)))
(def cddr (xs) (cdr (cdr xs)))

(def no (x) (if x false true))
(def yes (x) (if x true false))

(def isa (x) [is (type _) x])

(def acons (x) (isa!cons x))

(def atom (x) (no (acons x)))

(def edge (x) (- (len x) 1))

(def alist (x)
  (if (acons x) t (null x)))

(def listify (x)
  (if (alist x) x (list x)))

; Maybe later make this internal.  Useful to let xs be a fn?

(def map1 (f xs)
  (if (no xs) 
      nil
      (cons (f (car xs)) (map1 f (cdr xs)))))

(def hug (xs (o f list))
  (if (no xs)       nil
      (no (cdr xs)) (cons (f (car xs)) nil)
                    (cons (f (car xs) (cadr xs))
                          (hug (cddr xs) f))))

(def reduce (f xs)
  (if (no (cdr xs))
      (car xs)
      (f (car xs) (reduce f (cdr xs)))))

(def cons args
  (if (cdr args)
      (reduce join args)
      (join (car args))))

(def snoc args
  (+ (car args) (cdr args)))

(def consif (x y) (if x (cons x y) y))

(def snocif (x y) (if y (snoc x y) x))

(mac cons! (var . args)
  `(atomic (= ,var (cons ,@args ,var))))

(mac snoc! (var . args)
  `(atomic (= ,var (snoc ,var ,@args))))

(mac let (var val . body)
  `((fn (,var) ,@body)
    ,val))

(mac with (var val . body)
  `(let ,var ,val
     ,@body
     ,var))

(mac withs (parms . body)
  (if (no parms) 
      `(do ,@body)
      `(let ,(car parms) ,(cadr parms) 
         (withs ,(cddr parms) ,@body))))

(mac w/uniq (names . body)
  (if names
      (let (var . names) (listify names)
        `(let ,var (uniq ',(lexname) ',var)
           (w/uniq ,names ,@body)))
      `(do ,@body)))

(mac and ((o x 'true) . args)
  (if args
      (w/uniq g
        `(let ,g ,x
           (if ,g (and ,@args) ,g)))
      x))

(mac or ((o x 'false) . args)
  (if args
      (w/uniq g
        `(let ,g ,x
           (if ,g ,g (or ,@args))))
      x))

(mac either ((o x 'nil) . args)
  (if args
      (w/uniq g
        `(let ,g ,x
           (if (null ,g) (either ,@args) ,g)))
      x))

(mac do1 args
  `(with ,(uniq) ,(car args)
     ,@(cdr args)))

(mac guard body
  `(on-err (fn (c) (list false c))
           (fn () (list true (do ,@body)))))

(mac eif (var expr (o fail var) . body)
  (w/uniq ok
    `(let (,ok ,var) (guard ,expr)
       (if ,ok (do ,@body) ,fail))))

(mac safe (expr)
  `(on-err nil (fn () ,expr)))

(def assoc (key al)
  (if (atom al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key (o else))
  (either (cadr (assoc key al))
          else))

; Need rfn for use in macro expansions.

(mac rfn (name parms . body)
  `(let ,name nil
     (assign ,name (fn ,parms ,@body))))

(mac afn (parms . body)
  `(rfn self ,parms ,@body))

(def flip (f)
  (fn (:kws . args)
    (kwapply f kws (rev args))))

(def part (f :kws . args)
  (fn (kws: kws1 . args1)
    (kwapply f (+ kws kws1) (+ args args1))))

(def trap (f :kws . args)
  (flip (kwapply part kws (flip f) (rev args))))

; Ac expands x:y:z into (compose x y z), ~x into (complement x)

; Only used when the call to compose doesn't occur in functional position.  
; Composes in functional position are transformed away by ac.

(mac compose fs
  (w/uniq (gk ga)
    `(fn (kws: ,gk . ,ga)
       ,((afn ((f . fs))
           (if fs
               (list f (self fs))
               `(kwapply ,f ,gk ,ga)))
         (or fs (list 'idfn))))))

; Ditto: complement in functional position optimized by ac.

(mac complement (f)
  (w/uniq gf
    `(let ,gf ,f
       (if (isa!fn ,gf)
           (compose no ,gf)
           (no ,gf)))))

(mac combine (op)
  `(fn fs
     (reduce (fn (f g)
               (fn (:kws . args)
                 (,op (kwapply f kws args)
                      (kwapply g kws args))))
             (or fs (list (con (,op)))))))

(def cand (combine and))
(def cor  (combine or))

(def rev (xs) 
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(def in (x . choices)
  (yes (mem x choices)))

(mac when (test . body)
  `(if ,test ((fn () ,@body))))

(mac unless (test . body)
  `(if (no ,test) ((fn () ,@body))))

(mac point (name :default . body)
  (w/uniq (g p)
    `(#'call/ec
       (fn (,g)
         (let ,name (fn ((o ,p ,default)) (,g ,p))
           ,@(snocif body default))))))

(mac catch body
  `(point throw ,@body))

(mac looping body
  `(let out (accfn)
     (point break default: (out)
       ,@body)))

(mac while (test . body)
  (w/uniq (gf gp)
    `(looping
       ((rfn ,gf (,gp)
          (when ,gp ((fn () ,@body)) (,gf ,test)))
        ,test))))

(mac until (test . body)
  `(while (no ,test) ,@body))

(def empty (seq) 
  (or (null seq)
      (let n (unless (acons seq)
               (safe:len seq))
        (is n 0))))

(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def recstring (test s (o start 0))
  ((afn (i)
     (and (< i (len s))
          (or (test i)
              (self (+ i 1)))))
   start))

(def testify (x)
  (if (isa!fn x) x [is _ x]))

; Like keep, seems like some shouldn't testify.  But find should,
; and all probably should.

(def some (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist f:car seq)
        (recstring f:seq seq))))

(def any (seq (o test idfn))
  (some test seq))

(def all (test seq) 
  (let f (testify test)
    (~some ~f seq)))
       
(def mem (test seq)
  (let f (testify test)
    (reclist [if (f:car _) _] seq)))

(def find (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist   [if (f:car _) (car _)] seq)
        (recstring [if (f:seq _) (seq _)] seq))))

; Possible to write map without map1, but makes News 3x slower.

;(def map (f . seqs)
;  (if (some1 no seqs)
;       nil
;      (no (cdr seqs))
;       (let s1 (car seqs)
;         (cons (f (car s1))
;               (map f (cdr s1))))
;      (cons (apply f (map car seqs))
;            (apply map f (map cdr seqs)))))


(def map (f . seqs)
  (if (some isa!string seqs)
       (withs (n   (apply min (map len seqs))
               new (newstring n))
         ((afn (i)
            (if (is i n)
                new
                (do (sref new (apply f (map [_ i] seqs)) i)
                    (self (+ i 1)))))
          0))
      (no (cdr seqs)) 
       (map1 f (car seqs))
      ((afn (seqs)
        (if (some no seqs)  
            nil
            (cons (apply f (map1 car seqs))
                  (self (map1 cdr seqs)))))
       seqs)))

(def mappend (f . args)
  (apply + nil (apply map f args)))

(def firstn (n xs)
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                        nil))

(def nthcdr (n xs)
  (if (no n)  xs
      (> n 0) (nthcdr (- n 1) (cdr xs))
              xs))

; Generalization of hug: (tuples x) = (hug x)

(def tuples (xs (o n 2))
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

; If ok to do with =, why not with def?  But see if use it.

(mac defs args
  `(do ,@(map [cons 'def _] (hug args))))

(def caris (x val) 
  (and (acons x) (is (car x) val)))

(def warn (msg . args)
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac atomic body
  `(atomic-invoke (fn () ,@body)))

(mac atlet args
  `(atomic (let ,@args)))
  
(mac atwith args
  `(atomic (with ,@args)))

(mac atwiths args
  `(atomic (withs ,@args)))


; setforms returns (vars get set) for a place based on car of an expr
;  vars is a list of gensyms alternating with expressions whose vals they
;   should be bound to, suitable for use as first arg to withs
;  get is an expression returning the current value in the place
;  set is an expression representing a function of one argument
;   that stores a new value in the place

; A bit gross that it works based on the *name* in the car, but maybe
; wrong to worry.  Macros live in expression land.

; seems meaningful to e.g. (push 1 (pop x)) if (car x) is a cons.
; can't in cl though.  could I define a setter for push or pop?

(assign setter (table))

(mac defset (name parms . body)
  (w/uniq gexpr
    `(sref setter 
           (fn (,gexpr)
             (let ,parms (cdr ,gexpr)
               ,@body))
           ',name)))

(defset car (x)
  (w/uniq g
    (list (list g x)
          `(car ,g)
          `(fn (val) (scar ,g val)))))

(defset cdr (x)
  (w/uniq g
    (list (list g x)
          `(cdr ,g)
          `(fn (val) (scdr ,g val)))))

(defset caar (x)
  (w/uniq g
    (list (list g x)
          `(caar ,g)
          `(fn (val) (scar (car ,g) val)))))

(defset cadr (x)
  (w/uniq g
    (list (list g x)
          `(cadr ,g)
          `(fn (val) (scar (cdr ,g) val)))))

(defset cddr (x)
  (w/uniq g
    (list (list g x)
          `(cddr ,g)
          `(fn (val) (scdr (cdr ,g) val)))))

; Note: if expr0 macroexpands into any expression whose car doesn't
; have a setter, setforms assumes it's a data structure in functional 
; position.  Such bugs will be seen only when the code is executed, when 
; sref complains it can't set a reference to a function.

(def setforms (expr0)
  (let expr (macex expr0)
    (if (isa!sym expr)
         (if (ssyntax expr)
             (setforms (ssexpand expr))
             (w/uniq (g h)
               (list (list g expr)
                     g
                     `(fn (,h) (assign ,expr ,h)))))
        ; make it also work for uncompressed calls to compose
        (and (acons expr) (metafn (car expr)))
         (setforms (expand-metafn-call (ssexpand (car expr)) (cdr expr)))
        (and (acons expr) (acons (car expr)) (is (caar expr) '%get))
         (setforms (list (cadr expr) (cadr (car expr))))
         (let f (setter (car expr))
           (if f
               (f expr)
               ; assumed to be data structure in fn position
               (do (when (caris (car expr) 'fn)
                     (warn "Inverting what looks like a function call"
                           expr0 expr))
                   (w/uniq (g h)
                     (let argsyms (map [uniq] (cdr expr))
                        (list (+ (list g (car expr))
                                 (mappend list argsyms (cdr expr)))
                              `(,g ,@argsyms)
                              `(fn (,h) (sref ,g ,h ,(car argsyms))))))))))))

(def metafn (x)
  (or (ssyntax x)
      (and (acons x) (in (car x) 'compose 'complement))))

(def expand-metafn-call (f args)
  (if (is (car f) 'compose)
       ((afn (fs)
          (if (caris (car fs) 'compose)            ; nested compose
               (self (+ (cdr (car fs)) (cdr fs)))
              (cdr fs)
               (list (car fs) (self (cdr fs)))
              (cons (car fs) args)))
        (cdr f))
      (is (car f) 'no)
       (err "Can't invert " (cons f args))
       (cons f args)))

(def uniq-place (place)
  (assign place (rem 'quote (flat:map ssexpand (flat:listify place))))
  (let g (keep [or (alphadig _)
                   (in _ #\* #\- #\=)]
               (reduce (fn (x y) (+ '|| x '-- y))
                       (if (<= (len place) 1)
                           (snoc place 'fn)
                           place)))
    (if (lex g) (uniq g) g)))

(def expand= (place val)
  (if (and (isa!sym place) (~ssyntax place))
      `(assign ,place ,val)
      (let (vars prev setter) (setforms place)
        (let g (uniq-place place)
          `(atwiths ,(+ vars (list g val))
             (,setter ,g))))))

(def expand=list (terms)
  `(do ,@(map (fn ((p v)) (expand= p v))  ; [apply expand= _]
                  (hug terms))))

(mac = args
  (expand=list args))

(mac or= args
  (def getter (var)
    (if (or (alist var) (lex var)) var `(bound ',var)))
  `(do ,@(hug (map1 ssexpand args)
              (fn (var val)
                `(atomic (or ,(getter var) (= ,var ,val)))))))

(def isnt args (#'pairwise ~is args))

(def >= args (#'pairwise ~< args))

(def <= args (#'pairwise ~> args))

(def whitec (c)
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c) (no (whitec c)))

(def letter (c) (or (<= #\a c #\z) (<= #\A c #\Z)))

(def digit (c) (<= #\0 c #\9))

(def alphadig (c) (or (letter c) (digit c)))

(def punc (c)
  (in c #\. #\, #\; #\: #\! #\?))

(mac loop (start test update . body)
  (w/uniq (gfn gparm)
    `(looping
       ,start
       ((rfn ,gfn (,gparm) 
          (if ,gparm
              (do ((fn () ,@body)) ,update (,gfn ,test))))
        ,test))))

(mac for (v init max . body)
  (w/uniq (gi gm)
    `(withs (,v nil ,gi ,init ,gm (+ ,max 1))
       (loop (assign ,v ,gi) (< ,v ,gm) (assign ,v (+ ,v 1))
         ,@body))))

(mac down (v init min . body)
  (w/uniq (gi gm)
    `(withs (,v nil ,gi ,init ,gm (- ,min 1))
       (loop (assign ,v ,gi) (> ,v ,gm) (assign ,v (- ,v 1))
         ,@body))))

(mac repeat (n . body)
  `(for ,(uniq) 1 ,n ,@body))

(def accfn ((o l))
  (fn xs
    (if (cdr xs) (cons! l xs)
        xs       (cons! l (car xs))
                 (atwith r (rev l)
                   (= l nil)))))

(mac accum (accfn . body)
  `(let ,accfn (accfn)
     ,@body
     (,accfn)))

(def across (l f)
  (if (alist l)
       (map1 [do (f _) unset] l)
      (isa!table l)
       (maptable (fn args (f args)) l)
       (for i 0 (edge l)
         (f (l i)))))

(mac each (var xs . body)
  (w/uniq f
    `(looping
       (across ,xs (rfn ,f (,var) ,@body)))))

(def clamp (x a b)
  (if (< x a) a
      (> x b) b
    x))

; (nthcdr x y) = (cut y x).

(def cut (seq (o start) (o end))
  (withs (n (len seq)
          end (clamp (if (no end)   n
                         (< end 0)  (+ n end)
                         end)
                     0 n)
          start (clamp (if (no start)  0
                           (< start 0) (+ n start)
                           start)
                       0 n))
      (if (~alist seq)
          (coerce (#'substring (str seq) start end) (type seq))
          (firstn (- end start) (nthcdr start seq)))))
      
(mac whilet (var test . body)
  `(while (iflet ,var ,test (do ,@body t))))

(def last (xs)
  (if (cdr xs)
      (last (cdr xs))
      (car xs)))

(def almost (xs)
  (if (cdr xs)
      (cons (car xs) (almost (cdr xs)))))

(def rem (test seq)
  (let f (testify test)
    (if (alist seq)
        (map1 [if (f _) unset _] seq)
        (coerce (rem test (coerce seq 'cons)) (type seq)))))

; Seems like keep doesn't need to testify-- would be better to
; be able to use tables as fns.  But rem does need to, because
; often want to rem a table from a list.  So maybe the right answer
; is to make keep the more primitive, not rem.

(def keep (test seq) 
  (let f (testify test)
    (rem ~f seq)))

(def trues (f seq) 
  (rem nil (map f seq)))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.

(mac caselet (var expr . args)
  (def ex (args)
    (if (no (cdr args)) 
        (car args)
        `(if (mem ,var ',(listify (car args)))
             ,(cadr args)
             ,(ex (cddr args)))))
  `(let ,var ,expr ,(ex args)))

(mac case (expr . args)
  `(caselet ,(uniq) ,expr ,@args))

(mac push (x place)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(let ,gx ,x
         (atwiths ,binds
           (,setter (cons ,gx ,val)))))))

(mac swap (place1 place2)
  (w/uniq (g1 g2)
    (withs ((binds1 val1 setter1) (setforms place1)
            (binds2 val2 setter2) (setforms place2))
      `(atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
  (withs (vars (map [uniq] places)
          forms (map setforms places))
    `(atwiths ,(mappend (fn (g (binds val setter))
                          (+ binds (list g val)))
                        vars
                        forms)
       ,@(map (fn (g (binds val setter))
                (list setter g))
              (+ (cdr vars) (list (car vars)))
              forms))))

(mac pop (place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list g val))
         (do1 (car ,g) 
              (,setter (cdr ,g)))))))

(def adjoin (x xs (o test is))
  (if (some [test x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (adjoin ,gx ,val ,@args))))))

(mac pull (test place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list g test) binds)
         (,setter (rem ,g ,val))))))

(mac togglemem (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (if (mem ,gx ,val)
                      (rem ,gx ,val)
                      (adjoin ,gx ,val ,@args)))))))

(mac ++ (place (o i 1))
  `(zap + ,place ,i))

(mac -- (place (o i 1))
  `(zap - ,place ,i))

; E.g. (++ x) equiv to (zap + x 1)

(mac zap (op place . args)
  (withs (gop    (uniq)
          gargs  (map [uniq] args)
          mix    (afn seqs 
                   (if (some no seqs)
                       nil
                       (+ (map car seqs)
                          (apply self (map cdr seqs))))))
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list gop op) (mix gargs args))
         (,setter (,gop ,val ,@gargs))))))

(mac alset (place key val)
  (w/uniq v
    `(atwith ,v ,val
       (pull [caris _ ,key] ,place)
       (unless (null ,v)
         (push (list ,key ,v) ,place)))))

(defset alref (l key)
  (w/uniq k
    (list (list k key)
          `(alref ,l ,k)
          `(fn (val) (alset ,l ,k val)))))

; Can't simply mod pr to print strings represented as lists of chars,
; because empty string will get printed as nil.  Would need to rep strings
; as lists of chars annotated with 'string, and modify car and cdr to get
; the rep of these.  That would also require hacking the reader.  

(def pr (:file :flush :sep :end . args)
  (or= file (stdout))
  (let c nil
    (each x args
      (if c (disp c file))
      (= c sep)
      (disp x file)))
  (if end (disp end file))
  (if flush (flushout file))
  (car args))

(def prt (:file :flush :sep :end . args)
  (apply pr (keep ~null args) :file :flush :sep :end)
  (car args))

(def prn (:file :flush :sep (o :end #\newline) . args)
  (apply pr args :file :flush :sep :end))

(def prs (:file :flush (o :sep #\space) :end . args)
  (apply pr args :file :flush :sep :end))

(def sp ((o n 1)) (repeat n (pr " ")))

(def atdisp (x)
  (unless (null x)
    (pr (unquoted x))))

(mac %atstring (x)
  `(tostring (atdisp ,x)))

(mac wipe args
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

; Destructuring means ambiguity: are pat vars bound in else? (no)

(mac iflet (var expr then . rest)
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr . args)
  `(let it ,expr
     (if it
         ,@(if (cddr args)
               `(,(car args) (aif ,@(cdr args)))
               args))))

(mac awhen (expr . body)
  `(let it ,expr (if it (do ,@body))))

(mac aand args
  (if (no args)
      't 
      (no (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

(mac w/accum args
  (w/uniq ga
    `(accum ,ga
       ,@(each expr args
           (out `(aif ,expr (,ga it)))))))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drain (expr (o eof 'eof))
  (w/uniq g
    `(whiler ,g ,expr ,eof
       (out ,g))))

; For the common C idiom while x = snarfdata != stopval.
; Rename this if use it often.

(mac whiler (var expr endval . body)
  (w/uniq gf
    `(withs (,var nil ,gf (testify ,endval))
       (until (,gf (= ,var ,expr))
         ,@body))))
  
;(def macex (e)
;  (if (atom e)
;      e
;      (let op (and (atom (car e)) (eval (car e)))
;        (if (isa!mac op)
;            (apply (rep op) (cdr e))
;            e))))

(def string args
  (apply + "" (map str args)))

(def cat args
  (apply string args))

(def flat x
  ((afn (x acc)
     (if (no x)   acc
         (atom x) (cons x acc)
                  (self (car x) (self (cdr x) acc))))
   x nil))

(mac check (x test (o alt))
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(def pos (test seq (o start 0))
  (let f (testify test)
    (if (alist seq)
        ((afn (seq n)
           (if (no seq)   
                nil
               (f (car seq)) 
                n
               (self (cdr seq) (+ n 1))))
         (nthcdr start seq) 
         start)
        (recstring [if (f (seq _)) _] seq start))))

(def even (n) (is (mod n 2) 0))

(def odd (n) (no (even n)))

(mac after (x . ys)
  `(protect (fn () ,x) (fn () ,@ys)))

(let expander 
     (fn (f var name body (o kws))
       `(let ,var (,f ,name ,@kws)
          (after (do ,@body) (close ,var))))

  (mac w/infile (var name :kws . body)
    (expander 'infile var name body kws))

  (mac w/outfile (var name :kws . body)
    (expander 'outfile var name body kws))

  ; what happens to a file opened for append if arc is killed in
  ; the middle of a write?

  (mac w/appendfile (var name :kws . body)
    `(w/outfile ,var ,name :append ,@kws ,@body))

  (mac w/instring (var str . body)
    (expander 'instring var str body))

  (mac w/socket (var port . body)
    (expander 'open-socket var port body))
  )

(mac w/outstring (var . body)
  `(let ,var (outstring) ,@body))

(mac w/param (var val . body)
  `(call-w/param ,var ,val (fn () ,@body)))

(mac defvar (name value (o doc) :guard :const)
  `(or= ,name (make-param ,value (or ,guard false) ',name)))

(mac defconst (name value (o doc) (o :guard))
  `(def ,name (make-param ,value (or ,guard false) ',name)))

(or= original-stdin* (stdin)
     original-stdout* (stdout)
     original-stderr* (stderr))

; rename this simply "to"?  - prob not; rarely use

(mac w/stdout (str . body)
  `(w/param stdout ,str ,@body))

(mac w/stderr (str . body)
  `(w/param stderr ,str ,@body))

(mac w/stdin (str . body)
  `(w/param stdin ,str ,@body))

(mac tobytes body
  `(tostring bytes: true ,@body))

(mac tostring (:bytes . body)
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,gv ,@body)
      (inside ,gv bytes: ,bytes))))

(mac tostrings (:bytes . body)
  (w/uniq (ge go)
   `(w/outstring ,ge
      (w/outstring ,go
        (w/stderr ,ge
          (w/stdout ,go
            ,@body))
        (list (inside ,go bytes: ,bytes)
              (inside ,ge bytes: ,bytes))))))

(mac fromstring (str :kws . body)
  (w/uniq gv
   `(w/instring ,gv ,str ,@kws
      (w/stdin ,gv ,@body))))

(mac fromfile (name :kws . body)
  (w/uniq gv
    `(w/infile ,gv ,name ,@kws
       (w/stdin ,gv ,@body))))

(mac tofile (name :kws . body)
  (w/uniq gv
    `(w/outfile ,gv ,name ,@kws
       (w/stdout ,gv ,@body))))

(def readstring1 (s (o eof eof) :code) (w/instring i s (read i eof :code)))

(def read ((o x (stdin)) (o eof eof) :code)
  (if (isa!string x) (readstring1 x eof :code) ((if code sread sdata) x eof)))

; inconsistency between names of readfile[1] and writefile

(def codefile (name) (w/infile s name (drain (read-code s))))

(def readfile (name) (w/infile s name (drain (read s))))

(def readfile1 (name (o eof)) (w/infile s name (read s eof)))

(def readall ((o src (stdin)) (o n) :code)
  (if (isa!string src) (zap instring src))
  (whiler e (read src :code) eof
    (if (and n (< (-- n) 0))
        (break)
        (out e))))

(def allcode (src (o n))
  (readall src n :code))

(def peekbytes (n (o i (stdin)))
  (#'peek-bytes n 0 i))

(def peekchars (n (o i (stdin)))
  (#'peek-string n 0 i))

(def readbytes (n (o i (stdin)))
  (#'read-bytes n i))

(def readchars (n (o i (stdin)))
  (#'read-string n i))

(def writebytes (x (o p (stdout)))
  (#'write-bytes x p))

(def writechars (x (o p (stdout)))
  (#'write-string x p))

(def allbytes ((o str (stdin)))
  (#'port->bytes str))

(def allchars ((o str (stdin)))
  (coerce (allbytes str) 'string 'utf8))

(def filebytes (name)
  (w/infile s name (allbytes s) :bytes))

(def filechars (name)
  (w/infile s name (allchars s)))

(def savefile (val file (o writer disp) (o :bytes (isa!bytes val)))
  (let tmpfile (+ file ".tmp")
    (w/outfile o tmpfile (writer val o) :bytes)
    (mvfile tmpfile file))
  val)

(def writefile (val file (o writer write))
  (savefile val file writer))

(def sym (x) (coerce x 'sym))
(def str (x) (coerce x 'string))
(def seq (x) (coerce x 'cons))

(def int (x (o b 10)) (coerce x 'int b))

(def bin (x) (if (< x 0) (+ "-" (bin (- x))) (coerce x 'string 2)))
(def oct (x) (if (< x 0) (+ "-" (oct (- x))) (coerce x 'string 8)))
(def hex (x) (if (< x 0) (+ "-" (hex (- x))) (coerce x 'string 16)))

(mac rand-choice exprs
  `(case (rand ,(len exprs))
     ,@(let key -1 
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
  (w/uniq ga
    `(let ,ga nil     
       (repeat ,n (push ,expr ,ga))
       (rev ,ga))))

#'(require racket/random)

(def rand-bytes (n)
  (#'bytes->list (#'crypto-random-bytes n)))

(withs (n 1024
        s (rand-bytes n))
  (def randb ()
    (atomic
      (when (<= n 0)
        (= n 1024
           s (rand-bytes 1024)))
      (s (-- n)))))

; rejects bytes >= 248 lest digits be overrepresented

(def rand-string (n)
  (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    (withs (nc 62 s (newstring n) i 0)
      (while (< i n)
        (let x (randb)
           (unless (> x 247)
             (= (s i) (c (mod x nc)))
             (++ i))))
      s)))

(def rand-char ()
  ((rand-string 1) 0))

(mac forlen (var s . body)
  `(for ,var 0 (edge ,s) ,@body))

(mac on (var s . body)
  (if (is var 'index)
      (err "Can't use index as first arg to on.")
      (w/uniq gs
        `(let ,gs ,s
           (forlen index ,gs
             (let ,var (,gs index)
               ,@body))))))

(def best (f seq)
  (if (no seq)
      nil
      (with wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (= wins elt))))))
              
(def max args (best > args))
(def min args (best < args))

; (mac max2 (x y)
;   (w/uniq (a b)
;     `(withs (,a ,x ,b ,y) (if (> ,a ,b) ,a ,b))))

(def most (f seq) 
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

; Insert so that list remains sorted.  Don't really want to expose
; these but seem to have to because can't include a fn obj in a 
; macroexpansion.
  
(def insert-sorted (test elt seq)
  (if (no seq)
       (list elt) 
      (test elt (car seq)) 
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  (if (no seq) 
       (list elt) 
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq)) 
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

; Could make this look at the sig of f and return a fn that took the 
; right no of args and didn't have to call apply (or list if 1 arg).

(def memo (f)
  (withs (cache (table) nilcache (table))
    (fn args
      (or (cache args)
          (and (no (nilcache args))
               (aif (apply f args)
                    (= (cache args) it)
                    (do (= (nilcache args) t)
                        nil)))))))


(mac defmemo (name parms . body)
  `(safeset ,name (memo (fn ,parms ,@body))))

(def readline ((o str (stdin)))
  (when (peekc str)
    (tostring 
      (whiler c (readc str) [in _ nil #\newline]
        (unless (is c #\return)
          (writec c))))))

; Don't currently use this but suspect some code could.

(mac summing (sumfn . body)
  (w/uniq (gc gt)
    `(with ,gc 0
       (let ,sumfn (fn (,gt) (if ,gt (++ ,gc)))
         ,@body))))

(def sum (f xs)
  (with n 0
    (each x xs (++ n (f x)))))

(def treewise (f base tree)
  (if (atom tree)
      (base tree)
      (f (treewise f base (car tree)) 
         (treewise f base (cdr tree)))))

(def carif (x) (if (atom x) x (car x)))

(def tree-subst (old new tree)
  (if (is tree old)
       new
      (atom tree)
       tree
      (cons (tree-subst old new (car tree))
            (tree-subst old new (cdr tree)))))

(def ontree (f tree)
  (f tree)
  (unless (atom tree)
    (ontree f (car tree))
    (ontree f (cdr tree))))

(def dotted (x)
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))

(def fill-table (table data)
  (each (k v) (hug data) (= (table k) v))
  table)

(def keys (h) 
  (each (k v) h (out k)))

(def vals (h) 
  (each (k v) h (out v)))

; These two should really be done by coerce.  Wrap coerce?

(def tablist (h)
  (each kv h (out kv)))

(def listtab (al)
  (with h (table)
    (each (k v) al
      (= (h k) v))))

(def obj (:kws . args)
  (with h (listtab (hug args))
    (each (k v) (hug kws)
      (= (h (sym k)) v))))

(def load-table (file (o eof))
  (w/infile i file (read-table i eof)))

(def read-table ((o i (stdin)) (o eof eof))
  (let e (read i eof)
    (if (alist e) (listtab e) e)))

(def load-tables (file)
  (fromfile file (drain (read-table))))

(def save-table (h file)
  (writefile h file write-table))

(def write-table (h (o o (stdout)))
  (write (unquoted (tablist h)) o))

(def copy (x . args)
  (let x2 (case (type x)
            sym    x
            cons   (apply (fn args args) x)
            string (with new (newstring (len x))
                     (forlen i x
                       (= (new i) (x i))))
            table  (with new (table)
                     (each (k v) x 
                       (= (new k) v)))
                   (err "Can't copy " x))
    (map (fn ((k v)) (= (x2 k) v))
         (hug args))
    x2))

(def abs (n)
  (if (< n 0) (- n) n))

; The problem with returning a list instead of multiple values is that
; you can't act as if the fn didn't return multiple vals in cases where
; you only want the first.  Not a big problem.

(def round (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2) 
        ((if (> n 0) + -) base 1)
        base)))

(def nearest (n quantum)
  (* (roundup (/ n quantum)) quantum))

(def avg (ns) (/ (apply + ns) (len ns)))

(def med (ns (o test >))
  ((sort test ns) (round (/ (len ns) 2))))

; Use mergesort on assumption that mostly sorting mostly sorted lists
; benchmark: (let td (n-of 10000 (rand 100)) (time (sort < td)) 1) 

(def sort (test seq)
  (if (alist seq)
      (mergesort test (copy seq))
      (coerce (mergesort test (coerce seq 'cons)) (type seq))))

; Destructive stable merge-sort, adapted from slib and improved 
; by Eli Barzilay for MzLib; re-written in Arc.

(def mergesort (less? lst)
  (let n (len lst)
    (if (<= n 1) lst
        ; ; check if the list is already sorted
        ; ; (which can be a common case, eg, directory lists).
        ; (let loop ([last (car lst)] [next (cdr lst)])
        ;   (or (null? next)
        ;       (and (not (less? (car next) last))
        ;            (loop (car next) (cdr next)))))
        ; lst
        ((afn (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (self j)
                        b (self (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (withs (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (withs (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil))
         n))))

; Also by Eli.

(def merge (less? x y)
  (def lup (r x y r-x?) ; r-x? for optimization -- is r connected to x?
    (if (less? (car y) (car x))
      (do (if r-x? (scdr r y))
          (if (cdr y) (lup y x (cdr y) nil) (scdr y x)))
      ; (car x) <= (car y)
      (do (if (no r-x?) (scdr r x))
          (if (cdr x) (lup x (cdr x) y t) (scdr x y)))))
  (if (no x) y
      (no y) x
      (less? (car y) (car x))
      (do (if (cdr y) (lup y x (cdr y) nil) (scdr y x))
          y)
      ; (car x) <= (car y)
      (do (if (cdr x) (lup x (cdr x) y t) (scdr x y))
          x)))

(def bestn (n f seq)
  (firstn n (sort f seq)))

(def split (seq pos)
  (list (cut seq 0 pos) (cut seq pos)))

(mac time (expr (o label))
  (w/uniq (t1 t2)
    `(let ,t1 (now)
       (do1 ,expr
            (let ,t2 (now)
              (ero ,label "time:" (* 1000 (- ,t2 ,t1)) "msec."))))))

(mac jtime (expr)
  `(do1 'ok (time ,expr)))

(mac time10 (expr)
  `(time (repeat 10 ,expr)))

; Optimized set operations. Runs in linear time if :sorted,
; otherwise sorts the args.

(mac onsort body
  (let self (lexname)
    `(withs (f (if key (compare f key) f)
             xs (listify xs) ys (listify ys) zs (map listify zs))
       (aif (if sorted zs (map [sort f _] zs))
             (apply ,self :sorted test: f (,self test: f xs ys) it)
            (is ys unset)
             xs
            (afn ((x . xs) (y . ys))
              (if ,@body))
             (it (if sorted xs (sort f xs))
                 (if sorted ys (sort f ys)))))))

(def union (:sorted :key test: (o f <) (o xs) (o ys) . zs)
  (onsort (no x)  (consif y ys)
          (no y)  (cons x xs)
          (f x y) (cons x (self xs (cons y ys)))
          (f y x) (cons y (self (cons x xs) ys))
                  (cons x (self xs ys))))

(def intersect (:sorted :key test: (o f <) (o xs) (o ys unset) . zs)
  (onsort (no x)  nil
          (no y)  nil
          (f x y)         (self xs (cons y ys))
          (f y x)         (self (cons x xs) ys)
                  (cons x (self xs ys))))

(def difference (:sorted :key test: (o f <) (o xs) (o ys) . zs)
  (onsort (no x)  nil
          (no y)  (cons x xs)
          (f x y) (cons x (self xs (cons y ys)))
          (f y x)         (self (cons x xs) ys)
                          (self xs ys)))

(or= templates* (table))

(mac deftem (tem . fields)
  (withs (name (carif tem) includes (if (acons tem) (cdr tem)))
    `(= (templates* ',name) 
        (+ (mappend templates* ',(rev includes))
           (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                        (hug fields)))))))

(mac addtem (name . fields)
  `(= (templates* ',name) 
      (union key: car
             (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                          (hug fields)))
             (templates* ',name))))

(def inst (tem :kws . args)
  (with x (table)
    (each (k v) (if (acons tem) tem (templates* tem))
      (unless (no v) (= (x k) (v))))
    (each (k v) (hug args)
      (= (x k) v))
    (each (k v) (hug kws)
      (= (x (sym k)) v))
    (or= (x 'type) tem)))

; To write something to be read by temread, (write (tablist x))

(def temread (tem (o str (stdin)))
  (templatize tem (read str nil)))

; Converts alist to inst; ugly; maybe should make this part of coerce.
; Note: discards fields not defined by the template.

(def templatize (tem raw)
  (withs (x (inst tem) fields (if (acons tem) tem (templates* tem)))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (map (fn (pairs) (templatize tem pairs))       
       (w/infile in file (readall in))))


(def number (n) (in (type n) 'int 'num))

(def since (t1) (- (seconds) t1))

(def minutes-since (t1) (/ (since t1) 60))
(def hours-since (t1)   (/ (since t1) 3600))
(def days-since (t1)    (/ (since t1) 86400))

; could use a version for fns of 1 arg at least

(def cache (timef valf)
  (withs (cached nil gentime nil)
    (fn ((o reset))
      (when (or reset
                (no gentime)
                (>= (since gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac defcache (name lasts . body)
  `(safeset ,name (cache (fn () ,lasts)
                         (fn () ,@body))))

(def saferead (arg) (safe:read arg nil))

(def safe-load-table (filename) 
  (or (safe:load-table filename)
      (table)))

(def date ((o s (seconds)))
  (rev (nthcdr 3 (timedate s))))

(def datestring ((o s (seconds)))
  (let (y m d) (date s)
    (string y "-" (if (< m 10) "0") m "-" (if (< d 10) "0") d)))

(def count (test x)
  (withs (n 0 testf (testify test))
    (each elt x
      (if (testf elt) (++ n)))
    n))

(def ellipsize (str (o limit 80))
  (if (<= (len str) limit)
      str
      (+ (cut str 0 limit) "...")))

(def rand-elt (seq) 
  (seq (rand (len seq))))

(def before (x y seq (o i 0))
  (withs (xp (pos x seq i) yp (pos y seq i))
    (and xp (or (no yp) (< xp yp)))))

(def orf fns (apply cor fns))

(def andf fns (apply cand fns))

(def atend (i s)
  (> i (- (len s) 2)))

(def multiple (x y)
  (is 0 (mod x y)))

(mac nor args `(no (or ,@args))) 

; Consider making the default sort fn take compare's two args (when do 
; you ever have to sort mere lists of numbers?) and rename current sort
; as prim-sort or something.

; Could simply modify e.g. > so that (> len) returned the same thing
; as (compare > len).

(def compare (comparer scorer)
  (fn (x y) (comparer (scorer x) (scorer y))))

; Cleaner thus, but may only ever need in 2 arg case.

;(def compare (comparer scorer)
;  (fn args (apply comparer map scorer args)))

(def only (:kws . args)
  (car args))

(mac conswhen (f x y)
  (w/uniq (gf gx)
   `(withs (,gf ,f ,gx ,x)
      (if (,gf ,gx) (cons ,gx ,y) ,y))))

; Could combine with firstn if put f arg last, default to (fn (x) t).

(def retrieve (n f xs)
  (if (no n)                 (keep f xs)
      (or (<= n 0) (no xs))  nil
      (f (car xs))           (cons (car xs) (retrieve (- n 1) f (cdr xs)))
                             (retrieve n f (cdr xs))))

(def dedup (xs)
  (withs (h (table) acc nil)
    (each x xs
      (unless (h x)
        (push x acc)
        (= (h x) t)))
    (rev acc)))

(def single (x) (and (acons x) (no (cdr x))))

(def intersperse (x ys)
  (and ys (cons (car ys)
                (mappend [list x _] (cdr ys)))))

(def counts (seq (o c (table)))
  (if (no seq)
      c
      (do (++ (c (car seq) 0))
          (counts (cdr seq) c))))

(def commonest (seq)
  (withs (winner nil n 0)
    (each (k v) (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

(let argsym (uniq)

  (def parse-format (str)
    (accum a
      (withs (chars nil  i -1)
        (w/instring s str
          (whilet c (readc s)
            (case c 
              #\# (do (a (str (rev chars)))
                      (wipe chars)
                      (a (read s nil)))
              #\~ (do (a (str (rev chars)))
                      (wipe chars)
                      (readc s)
                      (a (list argsym (++ i))))
                  (push c chars))))
         (when chars
           (a (str (rev chars)))))))
  
  (mac prf (str . args)
    `(let ,argsym (list ,@args)
       (pr ,@(parse-format str))))
)

(or= loaded-files*      nil
     loaded-file-times* (table))

(def loaded-files () (rev loaded-files*))

(def loaded (file) (car:mem (expandpath file) (loaded-files)))

(def loadtime (file) (loaded-file-times* (expandpath file)))

(def notetime (file (o secs (modtime file)))
  (zap expandpath file)
  (pushnew file loaded-files*)
  (= (loaded-file-times* file) secs))

(def arcfile? (file)
  (and (> (len file) 4)
       (is ".arc" (cut file -4))))

(def arcenv (file)
  (zap expandpath file)
  (list (list '__file__ file)))

(def evaluator (file)
  (if (arcfile? file)
      (let env (arcenv file)
        (with eval [eval _ env]))
      seval))

(def read-code ((o x (stdin)) (o eof eof))
  (read x eof :code))

(def load-code (file (o evalfn (evaluator file)))
  (with x nil
    (w/infile f file
      (whiler e (read-code f) eof
        (= x (evalfn e))))))

(def load (file :once)
  (zap expandpath file)
  (w/param cwd (expandpath ".." file)
    (with value (unless (and once (loaded file))
                  (do1 (load-code file)
                       (hook 'load file)))
      (notetime file))))

(mac require (x)
  (if (or (isa!sym x) (caris x 'quote))
      `(#'require ,x)
      `(load :once ,x)))

; This file is already loaded; note it.
(notetime "arc.arc")

(def file-changed? (file)
  (isnt (modtime file) (loadtime file)))

(def reload ((o file (loaded-files)) :force)
  (if (acons file)
       (map [reload _ :force] file)
      (or force (file-changed? file))
       (list file (load file))))

(def positive (x)
  (and (number x) (> x 0)))

(mac w/table (var . body)
  `(with ,var (table) ,@body))

(def ero ((o :file (stderr))
          (o :flush t)
          (o :sep " ")
          (o :end #\newline)
          . args)
  (apply pr args :file :flush :sep :end))

(def queue () (list nil nil 0))

; Despite call to atomic, once had some sign this wasn't thread-safe.
; Keep an eye on it.

(def enq (obj q)
  (atomic
    (++ (q 2))
    (if (no (car q))
        (= (cadr q) (= (car q) (list obj)))
        (= (cdr (cadr q)) (list obj)
           (cadr q)       (cdr (cadr q))))
    (car q)))

(def deq (q)
  (atomic (unless (is (q 2) 0) (-- (q 2)))
          (pop (car q))))

; Should redef len to do this, and make queues lists annotated queue.

(def qlen (q) (q 2))

(def qlist (q) (car q))

(def enq-limit (val q (o limit 1000))
 (atwiths (limit (if (< limit 1) 1 limit)
           n (- (qlen q) limit))
   (for i 0 n
     (deq q))
   (enq val q)))

(def median (ns)
  ((sort > ns) (trunc (/ (len ns) 2))))

(mac noisy-each (n var val . body)
  (w/uniq (gn gc)
    `(withs (,gn ,n ,gc 0)
       (after
         (each ,var ,val
           (when (multiple (++ ,gc) ,gn)
             (ero "." end: ""))
           ,@body)
         (ero)))))

(def downcase (x)
  (case (type x)
    string     (#'string-downcase x)
    (char sym) (coerce (downcase (str x)) (type x))
               (err "Can't downcase" x)))

(def upcase (x)
  (case (type x)
    string     (#'string-upcase x)
    (char sym) (coerce (upcase (str x)) (type x))
           (err "Can't upcase" x)))

(def inc (x (o n 1))
  (coerce (+ (coerce x 'int) n) (type x)))

(def range (start end)
  (if (> start end)
      nil
      (cons start (range (inc start) end))))

(def mismatch (s1 s2)
  (on c s1
    (when (isnt c (s2 index))
      (break index))))

(def memtable (ks)
  (with h (table)
    (each k ks (= (h k) t))))

(= bar* " | ")

(mac w/bars body
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (if ,needbars
                             (pr bar* ,out)
                             (do (= ,needbars t)
                                 (pr ,out))))))
                  body)))))

(def len< (x n) (< (len x) n))

(def len> (x n) (> (len x) n))

(mac thread body 
  `(new-thread (fn () ,@body)))

(mac trav (x . fs)
  (w/uniq g
    `((afn (,g)
        (when ,g
          ,@(map [list _ g] fs)))
      ,x)))

(or= hooks* (table))

(def hook (name . args)
  (with r nil
    (each (id it) (rev:hooks* name)
      (= r (apply it args)))))

(mac defhook (name name: (o id 'hook) . rest)
  (let f (+ name '-- id)
    `(let ,f (fn ,@rest)
       (alset (hooks* ',name) ',id ,f))))

; if renamed this would be more natural for (map [_ user] pagefns*)

(def %get (index) [_ index])

(or= savers* (table))

(mac fromdisk (var file init load save)
  (w/uniq (gf gv)
    `(do1 (= ,var (iflet ,gf (file-exists ,file)
                         (,load ,gf)
                         ,init))
          (= (savers* ',var) (fn (,gv) (,save ,gv ,file))))))

(mac diskfile (var file (o init nil))
  `(fromdisk ,var ,file ,init filechars savefile))

(mac diskvar (var file (o init nil))
  `(fromdisk ,var ,file ,init readfile1 writefile))

(mac disktable (var file (o init '(table)))
  `(fromdisk ,var ,file ,init load-table save-table))

(mac todisk (var (o expr var))
  `((savers* ',var) 
    ,(if (is var expr) var `(= ,var ,expr))))


(mac evtil (expr test)
  (w/uniq gv
    `(with ,gv ,expr
       (while (no (,test ,gv))
         (= ,gv ,expr)))))

(def rand-key (h)
  (unless (empty h)
    (let n (rand (len h))
      (each (k v) h
        (when (< (-- n) 0)
          (break k))))))

(def ratio (test xs)
  (if (empty xs)
      0
      (/ (count test xs) (len xs))))

(def readenv (name (o default))
  (aif (get-environment-variable name)
       (safe:read it nil)
       default))

(def macos? ()
  (is (system-type) 'macosx))

(def yesno ((o question) (o default))
  (if question (prn question))
  (pr "Continue? " (if default "[Y/n]" "[y/N]") " ")
  (let it (read)
    (if (is it eof)
        default
        (in it 'y 'yes 'Y 'YES))))

(def unzip (xs) (apply map list xs))

(def zip ls (unzip ls))

(def tmpfile ((o val "") (o writer disp) (o name "tmpXXXXXXXXXX.tmp") (o path (libpath "arc/tmp/")))
  (ensure-dir path)
  (with file (+ path (map [if (is _ #\X) (rand-char) _] name))
    (w/outfile o file (writer val o))))

(def call-w/tmpfile (val f (o writer disp) (o name "tmpXXXXXXXXXX.tmp") (o path (libpath "arc/tmp/")))
  (let file (tmpfile val writer name path)
    (after (f file)
      (safe:rmfile file))))

(mac w/tmpfile (var val . body)
  `(call-w/tmpfile ,val (fn (,var) ,@body)))

(def shellquote (str)
  (string "'" (multisubst (list (list "'" "'\"'\"'")) (string str)) "'"))

(def shellargs (cmd (o args))
  (string cmd " " (intersperse #\space (map shellquote:string (rem nil args)))))

(def shellrun (cmd (o args))
  (let s (shellargs cmd args)
    (let code (#'system/exit-code s)
      (unless (is code 0)
        (err (+ "Command exited with nonzero code " code ": ") (list cmd args))))))

(def shell (cmd :async :bytes . args)
  (if async
      (thread:shellrun cmd args)
      (tostring (shellrun cmd args) :bytes)))

(def shellsafe (cmd :async :bytes . args)
  (safe (apply shell cmd :async :bytes args)))

(def GET (url :bytes)
  (shell "curl" "-fsSL" (clean-url url) :bytes))

(def writes (x)
  (tostring (write x)))

(or= traced* (table))

(defvar trace-depth* 0)

(def tracer (f name)
  (annotate (type f)
    (fn (:kws . args)
      (w/param trace-depth* (+ (trace-depth*) 1)
        (def n (trace-depth*))
        (def pre (* " │ " (- n 1)))
        (ero pre "╭" n (cons name (+ args kws)))
        (with it (kwapply (rep f) kws args)
          (ero pre "╰" n name "==>" (writes it)))))))

(def traced (f name)
  (if (traced* f)
      f
      (with it (tracer f name)
        (= (traced* it) f))))

(def untraced (f)
  (with it (or (traced* f) f)
    (wipe (traced* f))))

(mac trace fs
  `(do ,@(each f fs
           (out `(def ,f (atomic (traced ,f ',f)))))))

(mac untrace fs
  `(do ,@(each f fs
           (out `(def ,f (atomic (untraced ,f)))))))

; any logical reason I can't say (push x (if foo y z)) ?
;   eval would have to always ret 2 things, the val and where it came from
; idea: implicit tables of tables; setf empty field, becomes table
;   or should setf on a table just take n args?

; idea: use constants in functional position for currying?
;       (1 foo) would mean (fn args (apply foo 1 args))
; another solution would be to declare certain symbols curryable, and 
;  if > was, >_10 would mean [> _ 10]
;  or just say what the hell and make _ ssyntax for currying
; idea: make >10 ssyntax for [> _ 10]
; solution to the "problem" of improper lists: allow any atom as a list
;  terminator, not just nil.  means list recursion should terminate on 
;  atom rather than nil, (def empty (x) (or (atom x) (is x "")))
; table should be able to take an optional initial-value.  handle in sref.
; warn about code of form (if (= )) -- probably mean is
; warn when a fn has a parm that's already defined as a macro.
;   (def foo (after) (after))
; idea: a fn (nothing) that returns a special gensym which is ignored
;  by map, so can use map in cases when don't want all the vals
; idea: anaph macro so instead of (aand x y) say (anaph and x y)
; idea: foo.bar!baz as an abbrev for (foo bar 'baz)
;  or something a bit more semantic?
; could uniq be (def uniq () (annotate 'symbol (list 'u))) again?
; idea: use x- for (car x) and -x for (cdr x)  (but what about math -?)
; idea: get rid of strings and just use symbols
; could a string be (#\a #\b . "") ?
; better err msg when , outside of a bq
; idea: parameter (p foo) means in body foo is (hug arg)
; idea: make ('string x) equiv to (coerce x 'string) ?  or isa?
;   quoted atoms in car valuable unused semantic space
; idea: if (defun foo (x y) ...), make (foo 1) return (fn (y) (foo 1 y))
;   probably would lead to lots of errors when call with missing args
;   but would be really dense with . notation, (foo.1 2)
; or use special ssyntax for currying: (foo@1 2)
; remember, can also double; could use foo::bar to mean something
; wild idea: inline defs for repetitive code
;  same args as fn you're in
; variant of compose where first fn only applied to first arg?
;  (> (len x) y)  means (>+len x y)
; use ssyntax underscore for a var?
;  foo_bar means [foo _ bar]
;  what does foo:_:bar mean?
; matchcase
; idea: atable that binds it to table, assumes input is a list
; crazy that finding the top 100 nos takes so long:
;  (let bb (n-of 1000 (rand 50)) (time10 (bestn 100 > bb)))
;  time: 2237 msec.  -> now down to 850 msec

