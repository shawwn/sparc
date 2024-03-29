; Application Server.  Layer inserted 2 Sep 06.

; ideas: 
; def a general notion of apps of which prompt is one, news another
; give each user a place to store data?  A home dir?

; A user is simply a string: "pg". Use /whoami to test user cookie.

(require "srv.arc")

(= hpwfile*   (libpath "arc/hpw")
   oidfile*   (libpath "arc/openids")
   adminfile* (libpath "arc/admins")
   cookfile*  (libpath "arc/cooks"))

(def asv ((o port 8080))
  (load-userinfo)
  (serve port))

(def load-userinfo ()
  (= hpasswords*   (safe-load-table hpwfile*)
     openids*      (safe-load-table oidfile*)
     admins*       (map string (safe (readfile adminfile*)))
     cookie->user* (safe-load-table cookfile*))
  (each (c u) cookie->user*
    (= (user->cookie* u) c))
  t)

(defhook reload-admins ()
  (= admins* (map string (safe (readfile adminfile*))))
  nil)

(defhook create-acct (user) name: first-acct-becomes-admin
  (atomic
    ; is this the only account?
    (when (is (len dc-usernames*) 1)
      (unless (file-exists adminfile*)
        ; make them an admin
        (savefile (tostring:prn user) adminfile*)
        (hook 'reload-admins))))
  nil)

; idea: a bidirectional table, so don't need two vars (and sets)

(or= cookie->user* (table) user->cookie* (table) user->email* (table) logins* (table))

(def get-ip ((o req (the-req*)))
  (or req!ip "::1"))

(mac cookies ((o :req '(the-req*)) . args)
  (iflet (key . rest) args
         `(alref (,req 'cooks) (str ,key) ,@rest)
         `(,req 'cooks)))

(def get-user ()
  (with u (aand cookies!user
                (cookie->user* it))
    (when u (= (logins* u) (get-ip)))))

(def is-user (u) (is (get-user) u))

(defmemo auth-hash (cookie)
  (shash:string cookie))

(def get-auth ((o user (get-user)))
  (aand user
        (user->cookie* user)
        (auth-hash it)))

(def is-auth (auth (o user (get-user)))
  (is auth (get-auth user)))

(mac when-umatch (user :redir . body)
  `(if (is ,user (get-user))
        (do ,@body)
       ,redir
        "mismatch"
        (mismatch-message)))

(def mismatch-message () 
  (prn "Dead link: users don't match."))

(mac when-umatch/r (user . body)
  `(when-umatch ,user :redir ,@body))

(defop mismatch req (mismatch-message))

(mac uform (user req :redir after . body)
  `(aform redir: ,redir
     (fn (,req)
       (when-umatch ,user redir: ,redir
         ,after))
     ,@body))

(mac urform (user req after . body)
  `(uform :redir ,user ,req ,after ,@body))

; Like onlink, but checks that user submitting the request is the
; same it was generated for.  For extra protection could log the 
; username and ip addr of every genlink, and check if they match.

(mac ulink (text :redir . body)
  (letu (u req)
    `(let ,u (get-user)
       (linkf ,text (,req) redir: ,redir
         (when-umatch ,u redir: ,redir
           ,@body)))))

(defop admin req (admin-gate "/admin" admin-page))

(def admin-gate (whence f (o :onlogin (fn () nil)) . args)
  (if (admin)
      (apply f args)
      (login-page 'both "Please log in as an administrator."
                  (list onlogin whence))))

(def admin ((o u (get-user))) (and u (mem u admins*)))

(def user-exists (u) (and u (hpasswords* u) u))

(def admin-page msg
  (whitepage 
    (prbold "Admin: ")
    (hspace 20)
    (pr (get-user) " | ")
    (ulink 'logout
      (whitepage (pr "Bye " (logout-user) ".")))
    (when msg (hspace 10) (map pr msg))
    (br2)
    (urform (get-user) req
      (withs (u arg!acct p arg!pw)
        (if (or (no u) (no p) (is u "") (is p ""))
            (flink [admin-page "Bad data."])
            (user-exists u)
            (flink [admin-page "User already exists: " u])
            (do (create-acct u p)
                "/admin")))
      (pwfields "create (server) account"))))

(def cook-user ((o user (get-user)) (o cookie cookies!user) (o alt))
  (when user
    (with id (if (is cookie t) (new-user-cookie user) cookie)
      (unless alt
        (= (user->cookie* user) id))
      (unless (is (cookie->user* id) user)
        (= (cookie->user* id) user)
        (save-table cookie->user* cookfile*))
      (= cookies!user id))))

(def cook-user! ((o user (get-user)) (o cookie (new-user-cookie user)))
  (whenlet c (cook-user user cookie)
    (prcookie c)
    (= (logins* user) (get-ip))
    c))

; Unique-ids are only unique per server invocation.

(def new-user-cookie (user)
  (let id (string user "&" (unique-id))
    (if (cookie->user* id) (new-user-cookie user) id)))

(def user-cookies ((o user (get-user)))
  (each (c u) cookie->user*
    (when (is u user)
      (out c))))

(def logout-user ((o user (get-user)))
  (when user
    (each c (user-cookies user)
      (wipe (cookie->user* c)))
    (save-table cookie->user* cookfile*)
    (wipe (user->cookie* user))
    (wipe (logins* user)))
  user)

(def create-acct (user pw (o email))
  (= (dc-usernames* (downcase user)) t)
  (set-pw pw user)
  (= (user->email* user) email)
  (hook 'create-acct user))

(def disable-acct (user)
  (set-pw (rand-string 20) user)
  (logout-user user))

(= bcrypt-work-factor* 10) ; must be >= 10

(def rand-salt ((o work-factor bcrypt-work-factor*))
  (+ "$2b$" (int work-factor) "$" (rand-string 22)))

(def clean-hash (h)
  (and h (last:tokens h)))

(def user-pw (user)
  (clean-hash:hpasswords* user))

(def bcrypt-pw (user pw)
  (aand (<= (len pw) 72) (user-pw user) (is it (bcrypt pw it nil))))

(def sha1-pw (user pw)
  (aand (<= (len pw) 72) (user-pw user) (is it (shash pw))))

(def check-pw (pw (o user (get-user)))
  (or (bcrypt-pw user pw)
      (and (sha1-pw user pw)
           (do (set-pw pw user)
               (bcrypt-pw user pw)))))
  
(def set-pw (pw (o user (get-user)))
  (= (hpasswords* user) (bcrypt pw (rand-salt)))
  (save-table hpasswords* hpwfile*))

(def hello-page ()
  (whitepage (prs "hello" (get-user) "at" (get-ip))))

(defop login req (login-page 'both nil "/"))

; switch is one of: register, login, both

; afterward is either a function on the newly created username and
; ip address, in which case it is called to generate the next page 
; after a successful login, or a pair of (function url), which means 
; call the function, then redirect to the url, or a url to redirect
; to.

; classic example of something that should just "return" a val
; via a continuation rather than going to a new page.

(def login-page (switch (o msg nil) (o afterward hello-page))
  (whitepage
    (pagemessage msg)
    (when (in switch 'login 'both)
      (login-form "Login" switch login-handler afterward)
      (hook 'login-form afterward)
      (br2))
    (when (in switch 'register 'both)
      (login-form "Create Account" switch create-handler afterward))))

(def login-form (label switch handler afterward)
  (prbold label)
  (br2)
  (fnform (fn (req) (handler req switch afterward))
          (fn () (pwfields (downcase label)))
          ((orf acons isa!string) afterward)))

(def login-handler (req switch afterward)
  (logout-user)
  (aif (good-login arg!acct arg!pw)
       (login it (user->cookie* it) afterward)
       (failed-login switch "Bad login." afterward)))

(def create-handler (req switch afterward)
  (logout-user)
  (withs (user arg!acct pw arg!pw email arg!email)
    (aif (bad-newacct user pw)
         (failed-login switch it afterward)
         (do (create-acct user pw email)
             (login user (cook-user! user) afterward)))))

(def login (user cookie afterward)
  (= (logins* user) (get-ip))
  (prcookie cookie)
  (hook 'login user)
  (if (acons afterward)
      (let (f url) afterward
        (f)
        url)
      (isa!string afterward)
      afterward
      (do (prn)
          (afterward))))

(def merge-args (args (o req (the-req*)))
  (let args1 req!args
    (each (k v) args
      (pull [caris _ k] args1)
      (push (list k v) args1))
    (= req!args args1)))

(def failed-login (switch msg afterward)
  (if (acons afterward)
      (let args (copy ((the-req*) 'args))
        (flink (fn ignore (merge-args args) (login-page switch msg afterward))))
      (do (prn)
          (login-page switch msg afterward))))

(def prcookie (cook (o key 'user) (o httponly (is key 'user)))
  (prn:string
    "Set-Cookie: " key "=" cook "; expires=Sun, 17-Jan-2038 19:14:07 GMT"
    (if httponly "; HttpOnly")
    (if (srvsecure) "; Secure")))

(def pwfields ((o label "login"))
  (if (headmatch "create" label)
      (inputs (acct  username 20 arg!acct  'plain)
              (pw    password 20 arg!pw)
              (email email?   20 arg!email 'plain))
      (inputs (acct  username 20 arg!acct  'plain 'autofocus)
              (pw    password 20 arg!pw)))
  (br)
  (submit label))

(or= good-logins* (queue) bad-logins* (queue))

(def good-login (user pw)
  (let record (list (seconds) (get-ip) user)
    (if (check-pw pw user)
        (do (cook-user! user)
            (enq-limit record good-logins*)
            user)
        (do (enq-limit record bad-logins*)
            nil))))

(or= dc-usernames* (table))

(def username-taken (user)
  (when (empty dc-usernames*)
    (each (k v) hpasswords*
      (= (dc-usernames* (downcase k)) t)))
  (dc-usernames* (downcase user)))

(def bad-newacct (user pw)
  (if (no (goodname user 2 15))
       "Usernames can only contain letters, digits, dashes and 
        underscores, and should be between 2 and 15 characters long.  
        Please choose another."
      (username-taken user)
       "That username is taken. Please choose another."
      (or (no pw) (< (len pw) 4))
       "Passwords should be a least 4 characters long.  Please 
        choose another."
       nil))

(def goodname (str (o min 1) (o max nil))
  (and (isa!string str)
       (>= (len str) min)
       (~find (fn (c) (no (or (alphadig c) (in c #\- #\_))))
              str)
       (isnt (str 0) #\-)
       (or (no max) (<= (len str) max))
       str))

(defopr logout req
  (logout-user)
  "/")

(defop whoami req
  (aif (get-user)
       (if (admin)
           (prs (get-user) 'at req!ip (tostring:write:the-req*))
           (prs (get-user) 'at req!ip))
       (do (pr "You are not logged in. ")
           (w/link (login-page 'both) (pr "Log in"))
           (pr "."))))


(= formwid* 60 bigformwid* 80 numwid* 16 formatdoc-url* nil)

; Eventually figure out a way to separate type name from format of 
; input field, instead of having e.g. toks and bigtoks

(def varfield (typ id val)
  (if (in typ 'string 'string1 'url)
       (gentag input type 'text name id value val size formwid*)
      (in typ 'num 'int 'posint 'sym)
       (gentag input type 'text name id value val size numwid*)
      (in typ 'users 'toks)
       (gentag input type 'text name id value (tostring (apply prs val))
                     size formwid*)    
      (is typ 'sexpr)
       (gentag input type 'text name id 
                     value (tostring (map [do (write _) (sp)] val))
                     size formwid*)
      (in typ 'syms 'text 'doc 'mdtext 'mdtext2 'lines 'bigtoks)
       (let text (if (in typ 'syms 'bigtoks)
                      (tostring (apply prs val))
                     (is typ 'lines)
                      (tostring (apply pr (intersperse #\newline val)))
                     (in typ 'mdtext 'mdtext2)
                      (unmarkdown val)
                     (no val)
                      ""
                     val)
         (tag (textarea cols (if (is typ 'doc) bigformwid* formwid*) 
                        rows (needrows text formwid* 4)
                        wrap 'virtual 
                        style (if (is typ 'doc) "font-size:8.5pt")
                        name id)
           (prn) ; needed or 1 initial newline gets chopped off
           (pr text))
         (when (and formatdoc-url* (in typ 'mdtext 'mdtext2))
           (pr " ")
           (tag (font size -2)
             (link "help" formatdoc-url* color: (gray 175)))))
      (caris typ 'choice)
       (menu id (cddr typ) val)
      (is typ 'yesno)
       (menu id '("yes" "no") (if val "yes" "no"))
      (is typ 'hexcol)
       (gentag input type 'text name id value val)
      (is typ 'time)
       (gentag input type 'text name id value (if val (english-time val) ""))
      (is typ 'date)
       (gentag input type 'text name id value (if val (english-date val) ""))
       (err "unknown varfield type" typ)))

(def text-rows (text wid (o pad 3))
  (+ (trunc (/ (len text) (* wid .8))) pad))

(def needrows (text cols (o pad 0))
  (+ pad (max (+ 1 (count #\newline text))
              (roundup (/ (len text) (- cols 5))))))

(def varline (typ id val (o liveurls))
  (if (in typ 'users 'syms 'toks 'bigtoks)  (apply prs val)
      (is typ 'lines)                       (map prn val)
      (is typ 'yesno)                       (pr (if val 'yes 'no))
      (caris typ 'choice)                   (varline (cadr typ) nil val)
      (is typ 'url)                         (if (and liveurls (valid-url val))
                                                (link val val)
                                                (pr val))
      (text-type typ)                       (pr (or val ""))
                                            (pr val)))

(def text-type (typ) (in typ 'string 'string1 'url 'text 'mdtext 'mdtext2))

; Newlines in forms come back as /r/n.  Only want the /ns. Currently
; remove the /rs in individual cases below.  Could do it in aform or
; even in the parsing of http requests, in the server.

; Need the calls to striptags so that news users can't get html
; into a title or comment by editing it.  If want a form that 
; can take html, just create another typ for it.

(def readvar (typ str (o fail nil))
  (case (carif typ)
    string  (striptags str)
    string1 (if (blank str) fail (striptags str))
    url     (if (blank str) "" (valid-url str) (clean-url str) fail)
    num     (let n (saferead str) (if (number n) n fail))
    int     (let n (saferead str)
              (if (number n) (round n) fail))
    posint  (let n (saferead str)
              (if (and (number n) (> n 0)) (round n) fail))
    text    (striptags str)
    doc     (striptags str)
    mdtext  (md-from-form str)
    mdtext2 (md-from-form str t)                      ; for md with no links
    sym     (or (sym:car:tokens str) fail)
    syms    (map sym (tokens str))
    sexpr   (safe (readall str))
    users   (rem [no (goodname _)] (tokens str))
    toks    (tokens str)
    bigtoks (tokens str)
    lines   (lines str)
    choice  (readvar (cadr typ) str)
    yesno   (is str "yes")
    hexcol  (if (hex>color str) str fail)
    time    (or (safe (parse-time str)) fail)
    date    (or (safe (parse-date str)) fail)
            (err "unknown readvar type" typ)))

; dates should be tagged date, and just redefine <

(def varcompare (typ)
  (if (in typ 'syms 'sexpr 'users 'toks 'bigtoks 'lines 'hexcol)
       (fn (x y) (> (len x) (len y)))
      (is typ 'date)
       (fn (x y) (or (no y) (yes:and x (< (date>seconds x) (date>seconds y)))))
      (is typ 'time)
       (fn (x y)
         (def ymd (date))
         (or (no y) (yes:and x (< (date>seconds (+ ymd x)) (date>seconds (+ ymd y))))))
       (fn (x y)
         (or (empty y) (yes:and (~empty x) (< x y))))))


; (= fail* (uniq))

(def fail* () nil) ; coudn't possibly come back from a form
  
; Takes a list of fields of the form (type label value view modify) and 
; a fn f and generates a form such that when submitted (f label newval) 
; will be called for each valid value.  Finally done is called.

(def vars-form (fields f done (o button "update") (o lasts) (o :user (get-user)))
  (tarform lasts
           (if (all [no (_ 4)] fields)
               (fn (req))
               (fn (req)
                 (when-umatch user
                   (each (k v) req!args
                     (let name (sym k)
                       (awhen (find [is (cadr _) name] fields)
                         ; added sho to fix bug
                         (let (typ id val sho mod) it
                           (when (and mod v)
                             (let newval (readvar typ v fail*)
                               (unless (is newval fail*)
                                 (f name newval))))))))
                   (done))))
     (tab
       (showvars fields))
     (unless (all [no (_ 4)] fields)  ; no modifiable fields
       (br)
       (submit button))))
                
(def showvars (fields (o liveurls))
  (each (typ id val view mod question (o n 1)) (rem empty fields)
    (when view
      (when question
        (tr (repeat n (td)) (td (prn question))))
      (tr (if question
              (repeat n (td))
              (tag (td valign 'top)  (pr id ":")))
          (td (if mod 
                  (varfield typ id val)
                  (varline  typ id val liveurls))))
      (prn))))

; http://daringfireball.net/projects/markdown/syntax

(def md-from-form (str (o nolinks) (o esc))
  (markdown (trim (rem #\return (if esc str (esc-tags str))) 'end) 100 nolinks))

(def markdown (s (o maxurl) (o nolinks))
  (withs (ital nil bold nil)
    (tostring
      (forlen i s
        (iflet (newi spaces) (indented-code s i (if (is i 0) 2 0))
               (do (pr  "<p><pre><code>")
                 (let cb (code-block s (- newi spaces 1))
                   (pr cb)
                   (= i (+ (- newi spaces 1) (len cb))))
                 (pr "</code></pre>"))
               (iflet newi (parabreak s i (if (is i 0) 1 0))
                      (do (unless (is i 0) (pr "<p>"))
                          (= i (- newi 1)))
                      (and (in (s i) #\\ #\*)
                           (~atend i s)
                           (is (s (+ i 1)) #\*))
                      (writec (s (++ i)))
                      (and (is (s i) #\*)
                           (or ital 
                               (atend i s) 
                               (and (~whitec (s (+ i 1)))
                                    (pos #\* s (+ i 1)))))
                       (do (pr (if ital "</i>" "<i>"))
                           (= ital (no ital)))
                      (and (is (s i) #\_)
                           (or bold 
                               (atend i s) 
                               (and (~whitec (s (+ i 1)))
                                    (pos #\_ s (+ i 1)))))
                       (do (pr (if bold "</b>" "<b>"))
                           (= bold (no bold)))
                      (and (no nolinks)
                           (and (or (litmatch "http://" s i) 
                                    (litmatch "https://" s i)
                                    (litmatch "/l/" s i))
                                (or (is i 0) (~in (s (- i 1)) #\" #\>))))
                       (withs (n   (urlend s i)
                               url (clean-url (cut s i n)))
                         (tag (a href url rel 'nofollow)
                           (pr (if (no maxurl) url (ellipsize url maxurl))))
                         (= i (- n 1)))
                       (writec (s i))))))))

(def indented-code (s i (o newlines 0) (o spaces 0))
  (let c (s i)
    (if (nonwhite c)
         (if (and (> newlines 1) (> spaces 1))
             (list i spaces)
             nil)
        (atend i s)
         nil
        (is c #\newline)
         (indented-code s (+ i 1) (+ newlines 1) 0)
         (indented-code s (+ i 1) newlines       (+ spaces 1)))))

; If i is start a paragraph break, returns index of start of next para.

(def parabreak (s i (o newlines 0))
  (let c (s i)
    (if (or (nonwhite c) (atend i s))
        (if (> newlines 1) i nil)
        (parabreak s (+ i 1) (+ newlines (if (is c #\newline) 1 0))))))

; Returns the indices of the next paragraph break in s, if any.

(def next-parabreak (s i)
  (unless (atend i s)
    (aif (parabreak s i) 
         (list i it)
         (next-parabreak s (+ i 1)))))

(def paras (s (o i 0))
  (if (atend i s)
      (aand (cut s i)
            (if (~blank it) (list it)))
      (iflet (endthis startnext) (next-parabreak s i)
             (cons (cut s i endthis)
                   (paras s startnext))
             (list (trim (cut s i) 'end)))))


; Returns the index of the first char not part of the url beginning
; at i, or len of string if url goes all the way to the end.

; Note that > immediately after a url (http://foo.com>) will cause
; an odd result, because the > gets escaped to something beginning
; with &, which is treated as part of the url.  Perhaps the answer
; is just to esc-tags after markdown instead of before.

; Treats a delimiter as part of a url if it is (a) an open delimiter
; not followed by whitespace or eos, or (b) a close delimiter 
; balancing a previous open delimiter.

(def urlend (s i (o indelim))
  (let c (s i)
    (if (atend i s)
         (if ((orf punc whitec opendelim) c) 
              i 
             (closedelim c)
              (if indelim (+ i 1) i)
             (+ i 1))
        (if (or (whitec c)
                (and (punc c) (whitec (s (+ i 1))))
                (and ((orf whitec punc) (s (+ i 1)))
                     (or (opendelim c)
                         (and (closedelim c) (no indelim)))))
            i
            (urlend s (+ i 1) (or (opendelim c)
                                  (and indelim (no (closedelim c)))))))))

(def opendelim (c)  (in c #\< #\( #\[ #\{))
 
(def closedelim (c) (in c #\> #\) #\] #\}))


(def code-block (s i)
  (tostring
    (until (let left (- (len s) i 1)
             (or (is left 0)
                 (and (> left 2)
                      (is (s (+ i 1)) #\newline)
                      (nonwhite (s (+ i 2))))))
     (writec (s (++ i))))))

(def unmarkdown (s (o skiplinks))
  (tostring
    (forlen i s
      (if (litmatch "<p>" s i)
           (do (++ i 2) 
               (unless (is i 2) (pr "\n\n")))
          (litmatch "<i>" s i)
           (do (++ i 2) (pr #\*))
          (litmatch "</i>" s i)
           (do (++ i 3) (pr #\*))
          (litmatch "<b>" s i)
           (do (++ i 2) (pr #\_))
          (litmatch "</b>" s i)
           (do (++ i 3) (pr #\_))
          (unless skiplinks
            (litmatch "<a href=" s i))
           (let endurl (posmatch [in _ #\> #\space] s (+ i 9))
             (if endurl
                 (do (pr (cut s (+ i 9) (- endurl 1)))
                     (= i (aif (posmatch "</a>" s endurl)
                               (+ it 3)
                               endurl)))
                 (writec (s i))))
          (litmatch "<pre><code>" s i)
           (awhen (findsubseq "</code></pre>" s (+ i 12))
             (pr (cut s (+ i 11) it))
             (= i (+ it 12)))
          (do (if (is (s i) #\*) (writec #\\))
              (writec (s i)))))))

(def english-time ((hr min sec))
  (def h (mod hr 12))
  (def s (trunc sec))
  (string (if (is h 0) "12" h)
          (+ ":" (pad (str min) 2))
          (when (> s 0)
            (+ ":" (pad (str s) 2)))
          (if (< hr 12) " am" " pm")))

(def parse-time (s)
  (let (nums (o label "")) (halve s letter)
    (withs ((h (o m 0) (o s 0)) (map int (tokens nums ~digit))
           cleanlabel  (downcase (rem ~alphadig label))
           h (if (is h 12)
                  (if (in cleanlabel "am" "midnight") 0 12)
                 (is cleanlabel "am")
                  h
                  (+ h 12)))
      (list h m s))))

(= days*   '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
             "Friday" "Saturday")
   months* '("January" "February" "March" "April" "May" "June" "July"
             "August" "September" "October" "November" "December"))

(def english-weekday (ymd :local :short)
  (aand (days* (weekday ymd :local))
        (if short (cut it 0 3) it)))

(def english-month (m :short)
  (aand (months* (- m 1))
        (if short (cut it 0 3) it)))

(def english-date (ymd :short)
  (let (y m d) ymd
    (string d " " (english-month m :short) " " y)))

(= month-names* (obj "january"    1  "jan"        1
                     "february"   2  "feb"        2
                     "march"      3  "mar"        3
                     "april"      4  "apr"        4
                     "may"        5
                     "june"       6  "jun"        6
                     "july"       7  "jul"        7
                     "august"     8  "aug"        8
                     "september"  9  "sept"       9  "sep"      9
                     "october"   10  "oct"       10
                     "november"  11  "nov"       11
                     "december"  12  "dec"       12))

(def monthnum (s) (month-names* (downcase s)))

; Doesn't work for BC dates.

(def parse-date (s)
  (let nums (date-nums s)
    (if (valid-date nums)
        nums
        (err (string "Invalid date: " s)))))

(def date-nums (s)
  (withs ((ynow mnow dnow) (date)
          toks             (tokens s ~alphadig))
    (if (all [all digit _] toks)
         (let nums (map int toks)
           (case (len nums)
             1 (list ynow mnow (car nums))
             2 (iflet d (find [> _ 12] nums)
                        (list ynow (find [isnt _ d] nums) d)
                        (cons ynow nums))
               (if (> (car nums) 31)
                   (firstn 3 nums)
                   (rev (firstn 3 nums)))))
        ([all digit _] (car toks))
         (withs ((ds ms ys) toks
                 d          (int ds))
           (aif (monthnum ms)
                (list (or (safe (int ys)) ynow) 
                      it
                      d)
                nil))
        (monthnum (car toks))
         (let (ms ds ys) toks
           (aif (safe (int ds))
                (list (or (safe (int ys)) ynow) 
                      (monthnum (car toks))
                      it)
                nil))
          nil)))

; To be correct needs to know days per month, and about leap years

(def valid-date ((y m d))
  (and y m d
       (< 0 m 13)
       (< 0 d 32)))

(mac defopl (name parm . body)
  `(defop ,name ,parm
     (if (get-user)
         (do ,@body) 
         (login-page 'both
                     "You need to be logged in to do that."
                     (list (fn () nil)
                           (string ',name (reassemble-args ,parm)))))))

(def fetch-title (url)
  (let s (GET url)
    (whenlet p1 (posmatch "<title" s)
      (whenlet p2 (posmatch "</title>" s p1)
        (whenlet p3 (posmatch ">" s p1)
          (trim (cut s (+ p3 1) p2)))))))

#'(require racket/date)

(def seconds>date ((o ts (now)) :local)
  (#'seconds->date ts (yes local)))

(def date>seconds (ymd :local)
  (let (y m d (o H 0) (o M 0) (o S 0)) ymd
    (#'find-seconds (trunc S) M H d m y (yes local))))

(def tzname (:local)
  (#'date*-time-zone-name (seconds>date :local)))

(def tzoffset ((o ts (now)) :local)
  (- (date>seconds :local (timedate ts :local))
     (date>seconds :local (timedate ts))))

(def days-in-month (ymd)
  (/ (- (date>seconds:next-month ymd)
        (date>seconds:this-month ymd))
     60 60 24))

(def next-month ((o ymd (date)))
  (let (y m) ymd
    (if (is m 12)
        (list (+ y 1) 1 1)
        (list y (+ m 1) 1))))

(def this-month ((o ymd (date)))
  (let (y m) ymd (list y m 1)))

(def first-month ((o ymd (date)))
  (let (y)   ymd (list y 1 1)))

(def weekday (ymd :local :iso)
  (aand (seconds>date (date>seconds ymd :local) :local)
        (#'date-week-day it)
        (if (and iso (is it 0)) 7 it)))

(def yearday (ymd :local :offset)
  (aand (seconds>date (date>seconds ymd :local) :local)
        (#'date-year-day it)
        (if offset
            (let o (yearday-offset ymd offset :local)
              (- it o -1))
            it)))

(def yearday-offset (ymd weekday :local)
  (assert (isnt weekday t) "TODO")
  (let (y m) ymd
    (for d 1 10
      (assert (< d 9) "Couldn't find weekday")
      (if (no weekday) (break 0))
      (if (is weekday (english-weekday (list y m d) :local))
          (break (- d 1))))))

(def strftime (fmt (o ts (now)) :local)
  ; TODO:
  ; By default, date pads numeric fields with zeroes.  The following
  ; optional flags may follow '%':
  ;
  ; -      (hyphen) do not pad the field
  ;
  ; _      (underscore) pad with spaces
  ;
  ; 0      (zero) pad with zeros
  ;
  ; ^      use upper case if possible
  ;
  ; #      use opposite case if possible
  ;
  ; After  any  flags  comes  an  optional field width, as a decimal
  ; number; then an optional modifier, which is either E to use the
  ; locale's alternate representations if available, or O to use the
  ; locale's alternate numeric symbols if available.
  (withs (secs (trunc ts)
          ymd (timedate :local ts)
          (Y m d H M S) ymd
          ns (trunc:* (mod S 1) 1e9)
          I (aand (mod H 12) (if (is it 0) 12 it))
          fmt (if (begins fmt "+") (cut fmt 1) fmt)
          n (len fmt))
    (apply string
      (forlen i fmt
        (if (~and (is (fmt i) #\%) (< i (- n 1)))
            (out (fmt i))
            (case (fmt (++ i))
; %%     a literal %
              #\% (out "%")
; %a     locale's abbreviated weekday name (e.g., Sun)
              #\a (out (english-weekday :local ymd :short))
; %A     locale's full weekday name (e.g., Sunday)
              #\A (out (english-weekday :local ymd))
; %b     locale's abbreviated month name (e.g., Jan)
              #\b (out (english-month m :short))
; %B     locale's full month name (e.g., January)
              #\B (out (english-month m))
; %c     locale's date and time (e.g., Thu Mar  3 23:05:25 2005)
              #\c (out (strftime :local "%a %b %e %H:%M:%S %Y" ts))
; %C     century; like %Y, except omit last two digits (e.g., 20)
              #\C (out (cut (str Y) 0 2))
; %d     day of month (e.g., 01)
              #\d (out (pad (str d) 2 "0"))
; %D     date; same as %m/%d/%y
              #\D (out (strftime :local "%m/%d/%y" ts))
; %e     day of month, space padded; same as %_d
              #\e (out (pad (str d) 2 " "))
; %F     full date; same as %Y-%m-%d
              #\F (out (strftime :local "%Y-%m-%d" ts))
; %g     last two digits of year of ISO week number (see %G)
; %G     year of ISO week number (see %V); normally useful only with %V
              ;#\g (err "todo")
              ;#\G (err "todo")
; %h     same as %b
              #\h (out (english-month m :short))
; %H     hour (00..23)
              #\H (out (pad (str H) 2 "0"))
; %I     hour (01..12)
              #\I (out (pad (str I) 2 "0"))
; %j     day of year (001..366)
              #\j (out (pad (str (+ 1 (yearday :local ymd))) 3 "0"))
; %k     hour, space padded ( 0..23); same as %_H
              #\k (out (pad (str H) 2 " "))
; %l     hour, space padded ( 1..12); same as %_I
              #\l (out (pad (str I) 2 " "))
; %m     month (01..12)
              #\m (out (pad (str m) 2 "0"))
; %M     minute (00..59)
              #\M (out (pad (str M) 2 "0"))
; %n     a newline
              #\n (out "\n")
; %N     nanoseconds (000000000..999999999)
              #\N (out (pad (str ns) 9 "0"))
; %p     locale's equivalent of either AM or PM; blank if not known
              #\p (out (if (>= H 12) "PM" "AM"))
; %P     like %p, but lower case
              #\P (out (if (>= H 12) "pm" "am"))
; %q     quarter of year (1..4)
; %r     locale's 12-hour clock time (e.g., 11:11:04 PM)
              #\r (out (strftime :local "%I:%M:%S %p" ts))
; %R     24-hour hour and minute; same as %H:%M
              #\R (out (strftime :local "%H:%M" ts))
; %s     seconds since 1970-01-01 00:00:00 UTC
              #\s (out (str:trunc ts))
; %S     second (00..60)
              #\S (out (pad (str:trunc S) 2 "0"))
; %t     a tab
              #\t (out "\t")
; %T     time; same as %H:%M:%S
              #\T (out (strftime :local "%H:%M:%S" ts))
; %w     day of week (0..6); 0 is Sunday
              #\w (out (weekday :local ymd))
; %u     day of week (1..7); 1 is Monday
              #\u (out (weekday :local ymd :iso))
; %U     week number of year, with Sunday as first day of week (00..53)
;         All days in a new year preceding the first Sunday are considered to be in week 0.
              #\U (out (aand (yearday :local ymd offset: "Sunday") (ceiling:/ it 7) (pad (str it) 2)))
; %W     week number of year, with Monday as first day of week (00..53)
;         All days in a new year preceding the first Monday are considered to be in week 0.
              #\W (out (aand (yearday :local ymd offset: "Monday") (ceiling:/ it 7) (pad (str it) 2)))
; %V     week number of year, with Monday as first day of week (01..53)
;         Week 01 is the week containing Jan 4.
             ;#\V (out (aand (yearday :local ymd :offset)          (ceiling:/ it 7) (pad (str it) 2)))
; %x     locale's date representation (e.g., 12/31/99)
              #\x (out (strftime :local "%m/%d/%y" ts))
; %X     locale's time representation (e.g., 23:13:48)
              #\X (out (strftime :local "%H:%M:%S" ts))
; %y     last two digits of year (00..99)
              #\y (out (cut (str Y) 2))
; %Y     year
              #\Y (out Y)
; %z     +hhmm numeric time zone (e.g., -0400)
              #\z (out:strfdtime "%H%M" (tzoffset ts :local))
; %:z    +hh:mm numeric time zone (e.g., -04:00)
              #\: (case (fmt (++ i))
                    #\z (out:strfdtime "%H:%M" (tzoffset ts :local))
; %::z   +hh:mm:ss numeric time zone (e.g., -04:00:00)
                    #\: (case (fmt (++ i))
                          #\z (out:strfdtime "%H:%M:%S" (tzoffset ts :local))
; %:::z  numeric time zone with : to necessary precision (e.g., -04, +05:30)
                          #\: (case (fmt (++ i))
                                ; todo: strip :00
                                #\z (out:strfdtime "%H:%M:%S" (tzoffset ts :local))
                                ; unknown
                                (out "%:::" (fmt i)))))
; %Z     alphabetic time zone abbreviation (e.g., EDT)
              #\Z (out (tzname :local))
              ; unknown
              (do (out #\% (fmt i)))))))))

(def strfdtime (fmt dt)
  (cat (if (>= dt 0) "+" "-")
       (strftime fmt (+ (date>seconds (date)) (abs dt)))))

(def idiv (i n) (trunc (/ i n)))
(def imod (i n) (trunc (mod i n)))

(def sec->msec (ts) (* ts 1000))
(def msec->sec (ms)  (idiv ms 1000))

(def moment ((o ms (mnow)))
  (withs (secs (msec->sec ms)
          msecs (imod ms 1000))
    (strftime (+ "+%Y-%m-%dT%H:%M:%S." (pad (str msecs) 3 "0") "Z") secs)))

(defmemo moment-secs (secs)
  (moment (sec->msec secs)))

(defmemo moment-ms (ms)
  (moment ms))

(defmemo rss-date (secs)
  (strftime "+%a, %d %b %Y %H:%M:%S GMT" secs))

(def send-email (from to subject message)
  (when (file-exists (libpath "../sendmail.py"))
    (tostring:shellsafe "python" (libpath "../sendmail.py") from to subject message)))

; (let ts (seconds)
;   (each c "%aAbBcCdDeFgGhHIjklmMnNpPqrRsStTuUVwWxXyYzZ"
;     (out:string "%" (str c) "\t" (tostring:write:strftime (string "%" (str c)) ts))))
; %%	"%"
; %a	"Sat"
; %A	"Saturday"
; %b	"Feb"
; %B	"February"
; %c	"Sat Feb 16 04:35:48 2019"
; %C	"20"
; %d	"16"
; %D	"02/16/19"
; %e	"16"
; %F	"2019-02-16"
; %g	"%g"
; %G	"%G"
; %h	"Feb"
; %H	"04"
; %I	"04"
; %j	"047"
; %k	" 4"
; %l	" 4"
; %m	"02"
; %M	"35"
; %n	"\n"
; %N	"000000000"
; %p	"AM"
; %P	"am"
; %q	"%q"
; %r	"04:35:48 AM"
; %R	"04:35"
; %s	"1550291748"
; %S	"48"
; %t	"\t"
; %T	"04:35:48"
; %u	"6"
; %U	"%U"
; %V	"%V"
; %w	"6"
; %W	"%W"
; %x	"02/16/19"
; %X	"04:35:48"
; %y	"19"
; %Y	"2019"
; %z	"-0000"
; %Z	"GMT"
