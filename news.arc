#!/usr/bin/env arc
; News.  2 Sep 06.

; to run news: ./news.arc or (nsv), then go to http://localhost:8080
; put usernames of admins, separated by whitespace, in arc/admins

; bug: somehow (+ votedir* nil) is getting evaluated.

(require "prompt.arc")
(require "firebase.arc")
(require "algolia.arc")

(= site-name*    "Lambda News"
   site-abbrev*  "LN"
   site-repo*    "https://github.com/shawwn/sparc"
   site-email*   "shawnpresser@@gmail.com"
   site-twitter* "theshawwn"
   site-discord* "shawwn#3694"
   discord-url*  nil
   site-url*     "http://localhost:8080"
   parent-url*   "http://localhost:8080"
   welcome-url*  "/welcome.html"
   site-desc*    "Lambda News"     ; for rss feed
   site-color*   (color 154 186 170)
   border-color* (color 154 186 170)
   prefer-url*   t
   site-img*     (obj url:    "https://user-images.githubusercontent.com/59632/213842621-78c527ed-c657-4126-b27d-2670f35fb053.png"
                      type:   "image/png"
                      width:  512
                      height: 512))

; Structures

; Could add (html) types like choice, yesno to profile fields.  But not
; as part of deftem, which is defstruct.  Need another mac on top of
; deftem.  Should not need the type specs in user-fields.

(deftem profile
  id         nil
  name       nil
  created    (seconds)
  auth       0
  member     nil
  submitted  nil
  favorites  nil
  votes      nil   ; for now just recent, elts each (time id by sitename dir)
  karma      1
  avg        nil
  weight     .5
  ignore     nil
  email      nil
  verified   nil ; if (is (uval u email) (uval u verified)), then email is valid
  about      nil
  showdead   nil
  notify     nil
  noprocrast nil
  firstview  nil
  lastview   nil
  maxvisit   20
  minaway    180
  topcolor   nil
  keys       nil
  delay      0)

(deftem item
  id         nil
  type       nil
  by         nil
  ip         nil
  time       (seconds)
  url        nil
  title      nil
  text       nil
  votes      nil   ; elts each (time ip user type score)
  score      0
  sockvotes  0
  flags      nil
  dead       nil
  deleted    nil
  parts      nil
  parent     nil
  kids       nil
  keys       nil)


; Load and Save

(= newsdir*  (libpath "arc/news/")
   storydir* (libpath "arc/news/story/")
   profdir*  (libpath "arc/news/profile/")
   votedir*  (libpath "arc/news/vote/"))

(or= votes* (table) profs* (table))

(= initload-users* t)

(def load-news ((o reload))
  (when reload
    (load (libpath "news.arc"))
    ;(= caching* 0)
    )
  (map ensure-dir (list arcdir* newsdir* storydir* votedir* profdir*))
  (unless stories* (load-items))
  (if (and initload-users* (empty profs*)) (load-users)))

(or= srv-port* (readenv "PORT" 8080))

(def nsv ((o port srv-port*))
  (load-userinfo)
  (load-news)
  (serve port))

(def run-news ((o port srv-port*))
  (ero 'srv-port*      (= srv-port* port))
  (ero 'srv-noisy*     (= srv-noisy* (readenv "NOISY" nil)))
  (ero 'caching*       (= caching* (readenv "CACHING" 1)))
  (ero 'explicit-flush (declare 'explicit-flush (readenv "FLUSH" t)))
  (nsv port))

(def load-users ()
  (ero "load users: " end: "")
  (noisy-each 100 id (dir profdir*)
    (load-user id))
  (each u (keys hpasswords*)
    (ensure-news-user u)))

; For some reason vote files occasionally get written out in a
; broken way.  The nature of the errors (random missing or extra
; chars) suggests the bug is lower-level than anything in Arc.
; Which unfortunately means all lists written to disk are probably
; vulnerable to it, since that's all save-table does.

(def load-user (u)
  (= (votes* u) (load-table (+ votedir* u))
     (profs* u) (temload 'profile (+ profdir* u)))
  u)

; Have to check goodname because some user ids come from http requests.
; So this is like safe-item.  Don't need a sep fn there though.

(def profile ((o u (get-user)))
  (or (profs* u)
      (aand (goodname u)
            (file-exists (+ profdir* u))
            (= (profs* u) (temload 'profile it)))))

(def votes ((o u (get-user)))
  (or (votes* u)
      (aand (file-exists (+ votedir* u))
            (= (votes* u) (load-table it)))))

(def make-user (name (o email (user->email* name)) (o ip (get-ip)))
  (inst 'profile
        'id       name
        ; For the moment, trust that users aren't giving bogus emails
        ; at signup. No need to force them to verify just yet.
        'email    email
        'verified email
        ; Punish users for giving out their email by spamming them
        ; with cat pictures and NP-hard problems.
        ; https://www.explainxkcd.com/wiki/index.php/287:_NP-Complete
        ; Actually, I've been reluctant to notify by default. But it's
        ; been a surprisingly popular feature, so this might be ok.
        'notify   t))

(def init-user (u (o email (user->email* u)))
  (= (votes* u) (table)
     (profs* u) (make-user u email))
  (save-votes u)
  (save-prof u)
  (newslog 'noob email)
  u)

; Need this because can create users on the server (for other apps)
; without setting up places to store their state as news users.
; See the admin op in app.arc.  So all calls to login-page from the
; news app need to call this in the after-login fn.

(def ensure-news-user ((o u (get-user)))
  (if (profile u) u (init-user u)))

(mac newshook (name args . body)
  `(defhook name: news ,name ,args
     ,@body))

(newshook login (user)
  (ensure-news-user user))

(def save-votes ((o u (get-user))) (save-table (votes* u) (+ votedir* u)))

(def save-prof  ((o u (get-user))) (save-table (profs* u) (+ profdir* u)) (hook 'save-prof (profs* u)))

(mac uval args (if (len> args 1)
                   (let (u k) args
                     `((profile ,u) ',k))
                   `((profile) ',@args)))

(mac karma   u `(uval ,@u karma))
(mac ignored u `(uval ,@u ignore))

; Note that users will now only consider currently loaded users.

(def users ((o f idfn))
  (keep f (keys profs*)))

(def check-key (k (o user (get-user)))
  (and user (mem k (uval user keys))))

(def author (i) (is-user i!by))


(or= stories* nil comments* nil
     items* (table) url->story* (table)
     maxid* 0 initload* 15000)

; The dir expression yields stories in order of file creation time
; (because arc infile truncates), so could just rev the list instead of
; sorting, but sort anyway.

; Note that stories* etc only include the initloaded (i.e. recent)
; ones, plus those created since this server process started.

; Could be smarter about preloading by keeping track of popular pages.

(def load-items ()
  (map rmrf (glob (+ storydir* "*.tmp")))
  (ero "load items: " end: "")
  (withs (items (table)
          ids   (sort > (map int (dir storydir*))))
    (if ids (= maxid* (car ids)))
    (noisy-each 100 id (firstn initload* ids)
      (let i (load-item id)
        (push i (items i!type))))
    (= stories*  (rev (merge (compare < !id) items!story items!poll))
       comments* (rev items!comment))
    (hook 'initload items))
  (ensure-topstories))

(def ensure-topstories ()
  (aif (safe (readfile1 (+ newsdir* "topstories")))
       (= ranked-stories* (map item it))
       (do (ero "ranking stories.")
           (gen-topstories))))

(def astory   (i) (is i!type 'story))
(def acomment (i) (is i!type 'comment))
(def apoll    (i) (is i!type 'poll))

(def load-item (id)
  (let i (temload 'item (+ storydir* id))
    (= (items* id) i)
    (awhen (and (astory&live i) (check i!url ~blank))
      (register-url i it))
    i))

; Note that duplicates are only prevented of items that have at some
; point been loaded.

(def register-url (i url)
  (= (url->story* (canonical-url url)) i!id))

; redefined later

(or= stemmable-sites* (table))

(def canonical-url (url)
  (if (stemmable-sites* (sitename url))
      (cut url 0 (pos #\? url))
      url))

(def new-item-id ()
  (do1 (evtil (++ maxid*) [~file-exists (+ storydir* _)])
       (hook 'maxid maxid*)))

(def item (id)
  (or (items* id) (safe:load-item id)))

(def kids (i) (map item i!kids))

; For use on external item references (from urls).  Checks id is int
; because people try e.g. item?id=363/blank.php

(def safe-item (id)
  (ok-id&item (if (isa!string id) (saferead id) id)))

(def ok-id (id)
  (and (exact id) (<= 1 id maxid*)))

(def live (i) (nor i!dead i!deleted (flagged i)))

(def save-item (i) (save-table i (+ storydir* i!id)) (hook 'save-item i))

(def kill (i how)
  (when (nor i!dead (mem how i!keys))
    (log-kill i how)
    (wipe (comment-cache* i!id))
    (= i!dead t))
  (when (in how 'flagged 'dupe)
    (pushnew how i!keys))
  (save-item i))

(or= kill-log* nil)

(def log-kill (i (o how (get-user)))
  (push (list i!id how) kill-log*))

(mac each-loaded-item (var . body)
  (letu g
    `(down ,g maxid* 1
       (whenlet ,var (items* ,g)
         ,@body))))

(def loaded-items (test)
  (each-loaded-item i (test&out i)))

(def newslog args (apply srvlog 'news (get-ip) (get-user) args))


; Ranking

; Votes divided by the age in hours to the gravityth power.
; Would be interesting to scale gravity in a slider.

(= gravity* 1.8 timebase* 120 front-threshold* -10
   nourl-factor* 1.0 lightweight-factor* .3 )

(def frontpage-rank (s (o scorefn realscore) (o gravity gravity*))
  (* (/ (let base (- (scorefn s) 0)
          (if (> base 0) (expt base .8) base))
        (expt (/ (+ (item-age s) timebase*) 60) gravity))
     (if (~metastory s)                 .5
         (blank s!url)                  nourl-factor*
         (lightweight s)                (min lightweight-factor*
                                             (contro-factor s))
                                        (contro-factor s))))

(def contro-factor (s)
  (aif (check (visible-family s) [> _ 20])
       (min 1 (expt (/ (realscore s) it) 2))
       1))

(def realscore (i)
  (if (mem 'bury i!keys) -1000 (- i!score i!sockvotes)))

(disktable lightweights* (+ newsdir* "lightweights"))

(def lightweight (s)
  (or s!dead
      (mem 'rally s!keys)  ; title is a rallying cry
      (mem 'image s!keys)  ; post is mainly image(s)
      (lightweights* (sitename s!url))
      (lightweight-url s!url)))

(defmemo lightweight-url (url)
  (in (downcase (last (tokens url #\.))) "png" "jpg" "jpeg"))

(def apath (x)
  (is ((str x) 0) #\/))

(def item-paths (i) (keep apath i!keys))

(def item-age (i) (minutes-since i!time))

(def user-age ((o u (get-user))) (minutes-since (uval u created)))

; Only looks at the 1000 most recent stories, which might one day be a
; problem if there is massive spam.

(def ranked-stories ((o n 500))
  (map !id (firstn n ranked-stories*)))

(def gen-topstories ()
  (= ranked-stories* (rank-stories 180 1000 (memo frontpage-rank)))
  (hook 'save-topstories))

(def save-topstories ()
  (let ids (ranked-stories)
    (writefile ids (+ newsdir* "topstories"))
    (hook 'save-topstories ids)))

(newshook save-topstories ((o ids (ranked-stories)))
  (firebase-set "v0/topstories" ids))

(def rank-stories (n consider scorefn)
  (bestn n (compare > scorefn) (latest-items metastory nil consider)))

; With virtual lists the above call to latest-items could be simply:
; (map item (retrieve consider metastory:item (gen maxid* [- _ 1])))

(def latest-items (test (o stop) (o n))
  (down id maxid* 1
    (let i (item id)
      (if (or (and stop (stop i)) (and n (<= n 0)))
          (break))
      (when (test i)
        (out i)
        (if n (-- n))))))

; redefined later

(def metastory (i) (and i (in i!type 'story 'poll)))

(def adjust-rank (s (o scorefn frontpage-rank))
  (insortnew (compare > (memo scorefn)) s ranked-stories*)
  (save-topstories))

; If something rose high then stopped getting votes, its score would
; decline but it would stay near the top.  Newly inserted stories would
; thus get stuck in front of it. I avoid this by regularly adjusting
; the rank of a random top story.

(defbg rerank-random 30 (rerank-random))

(def rerank-random ()
  (when ranked-stories*
    (adjust-rank (ranked-stories* (rand (min 50 (len ranked-stories*)))))))

(def rerank-stories ()
  (atomic (= ranked-stories*
             (sort (compare > (memo frontpage-rank))
                   (latest-items metastory)))))

(def subs (i)
  (let it (keep [headmatch "/l/" (string _)] i!keys)
    (if (mem '/l/private it)
        it
        (cons '/l/all it))))

(defmemo match-subs (x)
  (let x (or x "all")
    (apply orf
      (each x (tokens (str x) #\|)
        (let fns (each x (map downcase (tokens x #\&))
                   (if (begins x "!")
                       (let x (sym (string "/l/" (cut x 1)))
                         (out [~mem x (map downcase (subs _))]))
                       (let x (sym (string "/l/" x))
                         (out [mem x (map downcase (subs _))]))))
          (unless (empty fns)
            (out (apply andf fns))))))))

(def topstories (n (o sub) (o threshold front-threshold*))
  (let sub (match-subs sub)
    (retrieve n
              [and (>= (realscore _) threshold)
                   (cansee _)
                   (sub _)]
              ranked-stories*)))

(= max-delay* 10)

(def private (i)
  (let p (superparent i)
    (mem '/l/private p!keys)))

(def cansee (i)
  (if i!deleted   (admin)
      i!dead      (or (author i) (seesdead))
      (delayed i) (author i)
      (private i) (or (author i) (admin))
      t))

(def isfrom (url i)
  (let u (sitename i!url)
    (and (~empty u) (is url u))))

(let mature (table)
  (def delayed (i)
    (and (no (mature i!id))
         (acomment i)
         (or (< (item-age i) (min max-delay* (uval i!by delay)))
             (do (= (mature i!id) t)
                 nil)))))

(def seesdead ((o user (get-user)))
  (or (and user (uval user showdead) (~ignored user))
      (editor user)))

(def visible (items)
  (keep cansee items))

(def fromsite (url (o items stories*))
  (keep [isfrom url _] items))

(def cansee-descendant (c)
  (or (cansee c)
      (some cansee-descendant:item
            c!kids)))

(def editor ((o u (get-user)))
  (and u (or (admin u) (> (uval u auth) 0))))

(def member ((o u (get-user)))
  (and u (or (admin u) (uval u member))))


; Page Layout

(= logo-url* "ln.png" favicon-url* "favicon.ico")

(def rss-url ((o label))
  (if (is label "comments") "/rsscomments" "/rss"))

(mac w/newlines body
  `(do ,@(each expr body
           (out expr)
           (out '(prn)))))

(def gen-head (title label)
  (tag head
    (w/newlines
      (gentag meta name 'viewport content "width=device-width, initial-scale=1.0")
      (gentag meta name 'description             content site-desc*)
      (gentag meta name 'theme-color             content "#@(hexrep sand)")
      (gentag meta name 'msapplication-TileColor content "#@(hexrep orangered)")

      (gentag meta property 'og:type             content "website")
      (gentag meta property 'og:title            content title)
      (gentag meta property 'og:site_name        content site-name*)
      (gentag meta property 'og:description      content site-desc*)
      (gentag meta property 'og:image            content site-img*!url)
      (gentag meta property 'og:image:type       content site-img*!type)
      (gentag meta property 'og:image:width      content site-img*!width)
      (gentag meta property 'og:image:height     content site-img*!height)

      (gentag link rel  'manifest                  href (static-src "site.webmanifest"))
      (gentag link rel  'stylesheet type 'text/css href (static-src "news.css"))
      (gentag link rel  'mask-icon                 href (static-src "safari-pinned-tab.svg") color teal)
      (gentag link rel  "shortcut icon"            href (static-src favicon-url*))

      (gentag link rel  'apple-touch-icon     sizes '180x180 href (static-src "apple-touch-icon.png"))
      (gentag link rel  'icon type 'image/png sizes '512x512 href (static-src "android-chrome-512x512.png"))

      (gentag link rel   'alternate type 'application/rss+xml
                   title 'RSS       href (rss-url label))

      (tag title (pr:eschtml title))
      (tag (script) (pr votejs*))
      (when (in label "place" "/l/place")
        (tag (script src (static-src "place.js")))
        (tag (style) (pr "body { background-color: #@(hexrep sand); }"))))))

(mac npage (title label . body)
  `(tag html
     (gen-head ,title ,label)
     (tag body
       (center
         (tag (table id 'hnmain
                     border 0 cellpadding 0 cellspacing 0 width "85%"
                     bgcolor sand)
           ,@body)))))

(or= pagefns* nil)

(mac fulltop (lid label title whence . body)
  (letu (gi gl gt gw)
    `(withs (,gi ,lid ,gl ,label ,gt ,title ,gw ,whence)
       (npage (+ (if ,gt (+ ,gt bar*) "") site-name*) ,gl
         (if (check-procrast)
             (do (pagetop 'full ,gi ,gl ,gt ,gw)
                 (hook 'page ,gl)
                 ,@body)
             (row (procrast-msg ,gw)))))))

(mac longpage (t1 lid label title whence . body)
  (letu (gt gi)
    `(withs (,gt (con ,t1) ,gi ,lid)
       (fulltop ,gi ,label ,title ,whence
         (trtd ,@body)
         (trtd (longfoot (- (now) (,gt)) ,whence))))))

(def longfoot (elapsed whence)
  (when (in whence "/l/teapots" "/l/teapot" "/l/418")
    (vspace 10)
    (center (tag (img src "/teapot.jpg"))))
  (when (in whence "/l/chess")
    (vspace 10)
    (center
      (chess-board)))
  (when (in whence "/l/place")
    (vspace 10)
    (center
      (place-board)))
  (when (in whence "/l/qb" "/l/queensblood")
    (vspace 10)
    (center
      (qb-board)))
  (when (in whence "/l/templeos")
    (terry))
  (vspace 10)
  (color-stripe (main-color))
  (br)
  (center
    (hook 'longfoot)
    (w/bars
      (link "Welcome"     welcome-url*)
      (link "Guidelines"  "/newsguidelines.html")
      (link "FAQ"         "/newsfaq.html")
      (link "Bookmarklet" "/bookmarklet.html")
      ;(link "Feature Requests" "/item?id=230")
      (if site-repo*    (link "Source" site-repo*))
      (if site-email*   (link "Contact" "mailto:@site-email*"))
      (if site-twitter* (link "Twitter" "https://twitter.com/@site-twitter*"))
      (link "Lists" "/lists"))
    (br2)
    (w/bars
      (link "RSS (stories)" "/rss")
      (link "RSS (comments)" "/rsscomments"))
    (search-bar elapsed whence)
    (admin-bar elapsed whence)))

(def search-bar (elapsed whence)
  (br2)
  ;(tag (form method "get" action "//search.laarc.io/")
  ;  (inputs (q Search 17 nil 'plain)))
  )

(defcache memusage 5
  ;(repeat 3 (#'collect-garbage 'major))
  (memory))

(def admin-bar (elapsed whence)
  (br2)
  (when (admin)
    (w/bars
      (pr whence)
      (pr (len items*) "/" maxid* " loaded")
      (pr (num (round (/ (memusage) 1000))) " kb")
      (pr (len fns*) " fns")
      (pr (num (* elapsed 1000)) " msec")
      (link "settings" "/newsadmin")
      (link "pages" "/pages")
      (hook 'admin-bar whence))
    (br2))
  (when (in whence "/l/dev" "/l/programming" "/l/react" "/l/reactnative")
    (prn "<iframe src=\"https://open.spotify.com/embed/user/johanbrook/playlist/2mtlhuFVOFMn6Ho3JmrLc2\" width=\"300\" height=\"380\" frameborder=\"0\" allowtransparency=\"true\" allow=\"encrypted-media\"></iframe>")))

(def color-stripe (c)
  (tag (table width "100%" cellspacing 0 cellpadding 1)
    (tr (tdcolor c))))

(mac shortpage (lid label title whence . body)
  `(fulltop ,lid ,label ,title ,whence
     (trtd ,@body)))

(mac minipage (label . body)
  `(npage (+ ,label bar* site-name*) ,label
     (pagetop nil nil ,label)
     (trtd ,@body)))

(def msgpage (msg (o title) (o editurl) (o alert))
  (minipage (or title "Message")
    (awhen alert (pr it) (br2))
    (spanclass admin
      (center
        (awhen editurl
          (when (admin)
            (underlink "edit" it)
            (br2)))
        (if (len> msg 80)
            (widtable 320 msg)
            (pr msg))))
    (br2)))

;(= (max-age* 'news.css) 86400)   ; cache css in browser for 1 day

; turn off server caching via (= caching* 0) or won't see changes

; only need pre padding because of a bug in Mac Firefox

; Without setting the bottom margin of p tags to 0, 1- and n-para comments
; have different space at the bottom.  This solution suggested by Devin.
; Really am using p tags wrong (as separators rather than wrappers) and the
; correct thing to do would be to wrap each para in <p></p>.  Then whatever
; I set the bottom spacing to, it would be the same no matter how many paras
; in a comment. In this case by setting the bottom spacing of p to 0, I'm
; making it the same as no p, which is what the first para has.

; supplied by pb
;.vote { padding-left:2px; vertical-align:top; }
;.comment { margin-top:1ex; margin-bottom:1ex; color:black; }
;.vote IMG { border:0; margin: 3px 2px 3px 2px; }
;.reply { font-size:smaller; text-decoration:underline !important; }

(= votejs* "
function byId(id) {
  return document.getElementById(id);
}

function vote(node) {
  var v = node.id.split(/_/);   // {'up', '123'}
  var item = v[1];

  // adjust score
  var score = byId('score_' + item);
  var newscore = parseInt(score.innerHTML || '1') + (v[0] == 'up' ? 1 : -1);
  score.innerHTML = newscore + (newscore == 1 ? ' point' : ' points');

  // hide arrows
  byId('up_'   + item).style.visibility = 'hidden';
  byId('down_' + item).style.visibility = 'hidden';

  // ping server
  var ping = new Image();
  ping.src = node.href;

  return false; // cancel browser nav
}")

; Page top

(= sand (color 246 246 239) textgray (gray 130))

(def main-color ((o user (get-user)))
  (aif (and user (uval user topcolor))
       (hex>color it)
       site-color*))

(def pagetop (switch lid label (o title) (o whence))
; (tr (tdcolor black (vspace 5)))
  (tr (tdcolor (main-color)
        (tag (table border 0 cellpadding 0 cellspacing 0 width "100%"
                    style "padding:2px")
          (tr (gen-logo)
              (when (is switch 'full)
                (tag (td style "line-height:12pt; height:10px;")
                  (spanclass pagetop
                    (tag b (link site-name* "/l/all"))
                    (hspace 10)
                    (toprow label))))
             (if (is switch 'full)
                 (tag (td style "text-align:right;padding-right:4px;")
                   (spanclass pagetop (topright whence)))
                 (tag (td style "line-height:12pt; height:10px;")
                   (spanclass pagetop (tag b (link label "/l/all")))))))))
  (map [_] pagefns*)
  (spacerow 10))

(def gen-logo ()
  (tag (td style "width:18px;padding-right:4px")
    (tag (a href parent-url*)
      (tag (img src (static-src logo-url*) width 18 height 18
                style "border:1px #@(hexrep border-color*) solid;")))))

(= toplabels* '(nil "welcome" "new" "threads" "comments" "discord"
                    "show" "ask" "place" "*"))

(def toprow (label)
  (when (headmatch "/l/" label)
    (zap cut label 3))
  (w/bars
    (toplink "new" "/newest" label)
    (awhen (get-user)
      (toplink "threads" (threads-url it) label))
    (toplink "comments" "/newcomments" label)
    (awhen discord-url*
      (toplink "discord" it label))
    (link "tags" "/l")
    (toplink "ask" "/l/ask" label)
    (toplink "show" "/l/show" label)
    (toplink "place" "/l/place" label)
    (toplink "queen's blood" "/l/qb" label)
    (hook 'toprow label)
    (link "submit" "/submit")
    (unless (mem label toplabels*)
      (fontcolor white (pr:eschtml label)))))

(def toplink (name dest label)
  (tag-if (is name label) (span class 'topsel)
    (link name dest)))

(def topright (whence (o showkarma t))
  (awhen (get-user)
    (userlink it nil)
    (when showkarma (pr  "&nbsp;(@(karma it))"))
    (pr "&nbsp;|&nbsp;"))
  (aif (get-user)
       (ulink 'logout redir: whence
         (logout-user))
       (onlink "login"
         (login-page 'both nil
                     (list (fn ()
                             (newslog 'top-login))
                           whence)))))

(def noob ((o user (get-user)))
  (and user (< (days-since (uval user created)) 1)))


; News-Specific Defop Variants

(mac defopt (name parm test msg :kws . body)
  `(defop ,name ,parm ,@kws
     (if (,test (get-user))
         (do ,@body)
         (login-page 'both (+ "Please log in" ,msg ".")
                     (list (fn () nil)
                           (string ',name (reassemble-args ,parm)))))))

(mac defopg (name parm :kws . body)
  `(defopt ,name ,parm idfn "" ,@kws ,@body))

(mac defope (name parm :kws . body)
  `(defopt ,name ,parm editor " as an editor" ,@kws ,@body))

(mac defopa (name parm :kws . body)
  `(defopt ,name ,parm admin " as an administrator" ,@kws ,@body))

(mac opexpand (definer name parms :kws . body)
  `(,definer ,name ,(uniq 'req) ,@kws
     (withs (user (get-user) ip (get-ip))
       (withs ,(and parms (mappend [list _ `(arg ,(string _))]
                                   parms))
         (newslog ',name ,@parms)
         ,@body))))

(or= newsop-names* nil)

(mac newsop (name parms :kws . body)
  `(do (pushnew ',name newsop-names*)
       (opexpand defop ,name ,parms ,@kws ,@body)))

(mac newsopr (name parms :kws . body)
  `(do (pushnew ',name newsop-names*)
       (opexpand defopr ,name ,parms ,@kws ,@body)))

(mac adop (name parms :kws . body)
  (letu g
    `(opexpand defopa ,name ,parms ,@kws
       (let ,g (string ',name)
         (shortpage nil ,g ,g ,g
           ,@body)))))

(mac edop (name parms :kws . body)
  (letu g
    `(opexpand defope ,name ,parms ,@kws
       (let ,g (string ',name)
         (shortpage nil ,g ,g ,g
           ,@body)))))


; News Admin

(defopa newsadmin req
  (newslog 'newsadmin)
  (newsadmin-page))

; Note that caching* is reset to val in source when restart server.

(def nad-fields ()
  `((num      caching         ,caching*                       t t)
    (bigtoks  comment-kill    ,comment-kill*                  t t)
    (bigtoks  comment-ignore  ,comment-ignore*                t t)
    (bigtoks  lightweights    ,(sort < (keys lightweights*))  t t)))

; Need a util like vars-form for a collection of variables.
; Or could generalize vars-form to think of places (in the setf sense).

(def newsadmin-page ()
  (shortpage nil nil "newsadmin" "newsadmin"
    (vars-form (nad-fields)
               (fn (name val)
                 (case name
                   caching            (= caching* val)
                   comment-kill       (todisk comment-kill* val)
                   comment-ignore     (todisk comment-ignore* val)
                   lightweights       (todisk lightweights* (memtable val))
                   ))
               (fn () "/newsadmin"))
    (br2)
    (urform (get-user) req
            (let subject arg!id
              (if (profile subject)
                  (do (killallby subject)
                      (submitted-url subject))
                  "/newsadmin"))
      (single-input "" 'id 20 "kill all by"))
    (br2)
    (urform (get-user) req
            (do (set-ip-ban arg!ip t)
                "/newsadmin")
      (single-input "" 'ip 20 "ban ip"))))

(defmemo suggested-title (url)
  (or (fetch-title url) "unknown"))

(newsop suggest-title (url)
  (pr:suggested-title url))


; Users

(newsop user (id)
  (if (only&profile id)
      (user-page id)
      (pr "No such user.")))

(newsop user.json (id) header: "application/json"
  (aif (only&profile id)
       (write-json (user>json it))
       (pr "null")))

(newsop auth.json () header: "application/json"
  (aif (get-auth)
       (write-json (obj auth: it))
       (pr "null")))

(defop apple-app-site-association req header: "application/json"
  (write-json (obj webcredentials:
                   (obj apps: (list ;"B9452FEMTF.com.emilykolar.LaarcIOS"
                                    )))))

(def user>json (u)
  (obj id:        u!id
       created:   u!created
       karma:     u!karma
       about:     u!about
       submitted: u!submitted))

(newshook save-prof (u)
  (firebase-set "v0/user/@u!id" (user>json u)))

(def user-page (subject)
  (let here (user-url subject)
    (shortpage nil nil (+ "Profile: " subject) here
      (profile-form subject)
      (br2)
      (hook 'user subject))))

(def profile-form (subject)
  (let prof (profile subject)
    (vars-form (user-fields subject)
               (fn (name val)
                 (when (and (is name 'ignore) val (no prof!ignore))
                   (log-ignore subject 'profile))
                 (= (prof name) val))
               (fn () (save-prof subject)
                      (user-url subject)))))

(= topcolor-threshold* 250)

(def user-fields (subject)
  (withs (e (editor)
          a (admin)
          w (is-user subject)
          k (or a (and w (> (karma) topcolor-threshold*)))
          u (or a w)
          m (or a (and (member) w))
          p (profile subject)
          s subject)
  (w/accum
    `(string  user       ,subject                                  t   nil)
    `(string  name       ,(p 'name)                               ,m  ,m)
    `(string  created    ,(text-age:user-age subject)              t   nil)
    `(int     auth       ,(p 'auth)                               ,e  ,a)
    `(yesno   member     ,(p 'member)                             ,a  ,a)
    `(posint  karma      ,(p 'karma)                               t  ,a)
    `(num     avg        ,(p 'avg)                                ,a  nil)
    `(yesno   ignore     ,(p 'ignore)                             ,e  ,e)
    `(num     weight     ,(p 'weight)                             ,a  ,a)
    `(mdtext2 about      ,(p 'about)                               t  ,u)
    `(string  email      ,(p 'email)                              ,u  ,u)
    `(string  verified   ,(p 'verified)                           ,a  ,a)
    (unless (blank p!email)
      (if (isnt p!email p!verified)
          `(string  verify   ,(verify-link s)                     ,u  nil)
          `(yesno   notify   ,(p 'notify)                         ,u  ,u
            "Be notified of replies by email?")))
    `(yesno   showdead   ,(p 'showdead)                           ,u  ,u)
    `(yesno   noprocrast ,(p 'noprocrast)                         ,u  ,u)
    `(string  firstview  ,(p 'firstview)                          ,a   nil)
    `(string  lastview   ,(p 'lastview)                           ,a   nil)
    `(posint  maxvisit   ,(p 'maxvisit)                           ,u  ,u)
    `(posint  minaway    ,(p 'minaway)                            ,u  ,u)
    `(sexpr   keys       ,(p 'keys)                               ,a  ,a)
    `(hexcol  topcolor   ,(or (p 'topcolor) (hexrep site-color*)) ,k  ,k)
    `(int     delay      ,(p 'delay)                              ,u  ,u)
    `(string  password    ,(changepw-link)                        ,u  nil "")
    `(string  submissions ,(submissions-link s)                    t  nil "")
    `(string  comments    ,(comments-link s)                       t  nil "")
    `(string  upvoted     ,(+ (upvoted-link s)   " (private)")    ,u  nil "")
    `(string  favorites   ,(+ (favorites-link s) (if u " (shared)" "")) t  nil "")
    )))

(def verify-link (u (o label "verify email"))
  (tostring (underlink label "/verify")))

(def changepw-link ((o label "change password"))
  (tostring (underlink label "/changepw")))

(def submissions-link (u (o label "submissions"))
  (tostring (underlink label (submitted-url u))))

(def comments-link (u (o label "comments"))
  (tostring (underlink label (threads-url u))))

(def upvoted-link (u (o label "upvoted"))
  (tostring (underlink label (upvoted-url u))))

(def favorites-link (u (o label "favorites"))
  (tostring (underlink label (favorites-url u))))

(newsop welcome ()
  (pr "Welcome to " site-name* ", " user "!"))

; Verify email

(defopg verify req
  (verify-page (or arg!u (get-user))))

(def verify-page (subject (o msg) :hide)
  (if (nor (admin) (is-user subject))
      (pr "Sorry.")
    (minipage "Verify Email for @subject"
      (if msg (pr msg))
      (br2)
      (unless hide
        (uform (get-user) req
               (try-verify subject arg!e)
          (single-input "New email:  " 'e 30 "send verification email"
                        nil (uval subject email)))))))

(def try-verify (subject newemail)
  (if (len< newemail 4)
      (verify-page subject "Emails should be a least 4 characters long.
                   Please choose another.")
      (do (send-verification subject newemail)
          (verify-page subject "Verification email sent.
                       Please check your inbox." :hide))))

(def send-verification (subject newemail)
  (send-email site-email*
              newemail
              "Please verify your email address on @site-name*"
              (+ "Click here to verify your email: "
                 site-url*
                 (rflink [let u (profile subject)
                           (= u!email newemail
                              u!verified newemail)
                           (save-prof subject)
                           (user-url subject)]))))



; Main Operators

; remember to set caching to 0 when testing non-logged-in

(= caching* 1 perpage* 30 threads-perpage* 10 maxend* 1000)

; Limiting that newscache can't take any arguments except the user.
; To allow other arguments, would have to turn the cache from a single
; stored value to a hash table whose keys were lists of arguments.

(mac newsfn (user time . body)
  (letu gc
    `(let ,gc (cache (fn () (* caching* ,time))
                     (fn () (tostring (let ,user nil ,@body))))
       (fn (,user)
         (if (or ,user (arg))
             (do ,@body)
             (pr (,gc)))))))

(mac newscache (name user time . body)
  `(safeset ,name (newsfn ,user ,time ,@body)))

(newsop news () (newspage user))

(newsop ||   () (newspage user))

;(newsop index.html () (newspage user))

(= lncache* (table) lncache-time* 90)

(newsop l (path)
  (if (empty path)
      (tags-page user)
    ((or= (lncache* path)
          (newsfn user lncache-time* ()
            (let sub (+ "/l/" path)
              (listpage (topstories maxend* path)
                        sub sub sub))))
     user)))

(newscache newspage user 90
  (listpage (topstories maxend*) nil nil "/l/all"))

(def listpage (items label title (o url label) (o number t))
  (let t1 (now)
    (hook 'listpage)
    (longpage t1 nil label title url
      (display-items items label title url 0 perpage* number))))


(newsop newest () (newestpage user))

; Note: dead/deleted items will persist for the remaining life of the
; cached page.  If this were a prob, could make deletion clear caches.

(newscache newestpage user 40
  (listpage (newstories maxend*) "new" "New Links" "newest"))

(def newstories (n)
  (retrieve n cansee stories*))

(newshook create-story (s)
  (let ids (let (param the-req*) (table) ; TODO: turn this into a w/user macro
             (map !id (newstories 500)))
    (firebase-set "v0/newstories" ids)))

(newsop best () (bestpage user))

(newscache bestpage user 1000
  (listpage (beststories maxend*) "best" "Top Links"))

; As no of stories gets huge, could test visibility in fn sent to best.

(def beststories (n)
  (bestn n (compare > realscore) (visible stories*)))

(def sitestories (url (o n maxend*))
  (retrieve n cansee (fromsite url)))

(newsop from (site)
  (let site (clean-url site)
    (listpage (sitestories site maxend*) "from" "Submissions from @site")))

(newsop noobstories () (noobspage stories*))
(newsop noobcomments () (noobspage comments*))

(def noobspage (source)
  (listpage (noobs maxend* source) "noobs" "New Accounts"))

(def noobs (n source)
  (retrieve n cansee&bynoob source))

(def bynoob (i)
  (< (- (user-age i!by) (item-age i)) 2880))


(newsop bestcomments () (bestcpage user))

(newscache bestcpage user 1000
  (listpage (bestcomments maxend*)
            "best comments" "Best Comments" "bestcomments" nil))

(def bestcomments (n)
  (bestn n (compare > realscore) (visible comments*)))

(def stats-from-log (filename)
  (each x (lines:filechars filename)
    (let (s h) (halve x)
      (awhen (aand (saferead s) (if (isa!int it) it))
        (out it (car:halve (cut h 1)))))))

(def stats-day ((o ymd (date)))
  (let (y m d) ymd
    (let file (libpath (string "arc/logs/srv-" y "-" (pad m 2) "-" (pad d 2)))
      (stats-from-log file))))

(def stats-hour (H (o ymd (date)) (o day (stats-day ymd)))
  (withs ((Y m d) ymd
          from (date-seconds (list Y m d H))
          upto (+ from 3600))
    (keep [let (ts) _ (and (>= ts from) (< ts upto))]
          day)))

(def traffic-hour (H (o ymd (date)) (o day (stats-day ymd)))
  (let xs (stats-hour H ymd day)
    (list (len xs) (len:dedup (map cadr xs)))))

(or= traffic* (obj))

(def traffic-day ((o ymd (date)))
  (if (is 24 (len (traffic* ymd)))
      (traffic* ymd)
      (= (traffic* ymd)
         (let (y m d) ymd
           (let day (stats-day ymd)
             (aand (accum a (for i 0 23
                              (let x (traffic-hour i ymd day)
                                (let (requests uniques) x
                                  (when (or (> requests 0) (> uniques 0))
                                    (apply a (strftime "%Y-%m-%d %H:%M GMT" (date-seconds (+ ymd (list i)))) x))))))
                   (rev it)))))))

(defcache plot-traffic 60
  (lines:trim:shell 'sh "bin/plot-traffic.sh"))

(defcache traffic-page 30
  (withs (secs (seconds)
          ymd0 (date secs)
          ymd1 (date (- (date-seconds ymd0) (* 24 60 60)))
          ymd2 (date (- (date-seconds ymd1) (* 24 60 60)))
          ts (strftime "+%Y-%m-%d %H:%M:%S GMT" secs)
          (daily weekly) (hug (plot-traffic)))
    (tostring:minipage "traffic @ts"
      (center
        (pr "recent 48 hours")
        (br2)
        (sptab
          (row "hourly" "requests" "uniques")
          (let predicted nil
            (each (d r u) (firstn 48
                            (+ (traffic-day ymd0)
                               (traffic-day ymd1)
                               (traffic-day ymd2)))
              (if predicted (row d (pr:num r) (pr:num u))
                  (let m (aand (or (saferead (strftime "%M" secs)) 0)
                               (/ 60 (+ 1 it)))
                    (row (pr d " (current)") (pr:num r) (pr:num u))
                    (row (pr d " (predicted)")
                         (pr:num:trunc:* r m)
                         (pr:num:trunc:* u m))
                    (= predicted t))))))
        (vspace 35)
        (color-stripe textgray)
        (vspace 35)
        (pr "daily")
        (br2)
        (each x daily
          (let src (+ "/" (last:tokens x #\/))
            (tag (a href src) (gentag img src src width 900)))
          (br2))
        (sptab
          (row "daily" "requests" "uniques")
          (let predicted nil
            (each (d r u) (map tokens (rev:lines:trim:filechars "static/traffic.csv"))
              (withs (r (or (saferead r) 0)
                      u (or (saferead u) 0))
                (if predicted (row d (pr:num r) (pr:num u))
                  (let m (aand (+ (* 60 (or (saferead (strftime "%H" secs)) 0))
                                        (or (saferead (strftime "%M" secs)) 0))
                               (/ (* 60 24) (+ 1 it)))
                    (row (pr d " (current)") (pr:num r) (pr:num u))
                    (row (pr d " (predicted)")
                         (pr:num:trunc:* r m)
                         (pr:num:trunc:* u m))
                    (= predicted t)))))))
        (vspace 35)
        (color-stripe textgray)
        (vspace 35)
        (pr "weekly")
        (br2)
        (each x weekly
          (let src (+ "/" (last:tokens x #\/))
            (tag (a href src) (gentag img src src width 900)))
          (br2))
        (sptab
          (row "weekly" "requests" "uniques")
          (withs (predicted nil stats (map tokens (rev:lines:trim:filechars "static/traffic-weekly.csv")))
            (each (d r u) stats
              (withs (r (or (saferead r) 0)
                      u (or (saferead u) 0))
                (if predicted (row d (pr:num r) (pr:num u))
                  (let ts (date-seconds (map int (tokens d #\-)))
                    (let m (/ (* 7 24 60 60) (+ 1 (- secs ts)))
                      (row (pr d " (current)") (pr:num r) (pr:num u))
                      (row (pr d " (predicted)")
                           (pr:num:trunc:* r m)
                           (pr:num:trunc:* u m))
                      (= predicted t))))))))
      ))))

(defop traffic req
  (aif arg!on
      (minipage "traffic on @it"
        (center
          (sptab
            (row "hourly" "requests" "uniques")
            (each (d r u) (traffic-day (map int (tokens it #\-)))
              (row d (pr:num r) (pr:num u))))))
      (pr:traffic-page)))

#'(require racket/os)

(defop uptime req
  (aand (trim:shell "ps -o etime= -p" (#'getpid))
        (tokens it [in _ #\- #\:])
        (let (s m h d) (rev it)
          (or= s "00" m "00" h "00" d "00")
          (pr d "d " h "h " m "m " s "s"))))

(newsop lists ()
  (longpage (now) nil "lists" "Lists" "lists"
    (sptab
      (row (link "votes")        "Recent votes.")
      (row (link "best")         "Highest voted recent links.")
      (row (link "active")       "Most active current discussions.")
      (row (link "bestcomments") "Highest voted recent comments.")
      (row (link "noobstories")  "Submissions from new accounts.")
      (row (link "noobcomments") "Comments from new accounts.")
      (row (link "leaders")      "Users with most karma.")
      ;(row (link "traffic")      "Hourly, daily, and weekly traffic statistics.")
      (row (link "uptime")       "How long has racket been running?")
      (when (admin)
        (map row:link
             '(optimes topips flagged killed badguys badlogins goodlogins
                       badips badsites editors noobs pages)))
      (hook 'listspage))))


(def upvoted-url (user) (+ "/upvoted?id=" user))

(newsop upvoted (id)
  (if (only&profile id)
      (upvotedpage id)
      (pr "No such user.")))

(def upvotedpage (subject)
  (if (or (is-user subject) (admin))
      (listpage (sort (compare < item-age) (voted-items subject))
                "upvoted"
                "Upvoted items"
                (upvoted-url subject)
                nil)
      (pr "Can't display that.")))

(def voted-items (subject)
  (keep news-type&cansee
        (map item (keys:votes subject))))


; Story Display

(def display-items (items label title whence
                    (o start 0) (o end perpage*) (o number))
  (zerotable
    (let n start
      (each i (cut items start end)
        (display-item (and number (++ n)) i whence t)
        (spacerow (if (acomment i) 15 5))))
    (when end
      (let newend (+ end perpage*)
        (when (and (<= newend maxend*) (< end (len items)))
          (spacerow 10)
          (tr (tag (td colspan (if number 2 1)))
              (tag (td class 'title)
                (morelink display-items
                          items label title end newend number))))))))

; This code is inevitably complex because the More fn needs to know
; its own fnid in order to supply a correct whence arg to stuff on
; the page it generates, like logout and delete links.

(def morelink (f items label title . args)
  (tag (a href
          (url-for
            (afnid (fn (req)
                     (prn)
                     (let url  (url-for it)     ; it bound by afnid
                       (newslog 'more label)
                       (longpage (now) nil label title url
                         (apply f items label title url args))))))
          rel 'nofollow)
    (pr "More")))

(def display-story (i s whence)
  (when (or (cansee s) s!kids)
    (tr (display-item-number i)
        (td (votelinks s whence))
        (titleline s s!url whence))
    (tr (tag (td colspan (if i 2 1)))
        (tag (td class 'subtext)
          (hook 'itemline s)
          (itemline s)
          (editlink s)
          (when (apoll s) (addoptlink s))
          (unless i (flaglink s whence))
          (unless i (favlink s whence))
          (killlink s whence)
          ;(blastlink s whence)
          ;(blastlink s whence t)
          (deletelink s whence)
          (if (metastory s) (commentlink s))
          (scoreinfo s)))))

(def display-item-number (i)
  (when i (tag (td align 'right valign 'top class 'title)
            (pr i "."))))

(def scoreinfo (i)
  (when (admin)
    (pr bar*)
    (pr (aand (frontpage-rank i) (num it 6 t t)))))

(= follow-threshold* 1)

(def titleline (s url whence)
  (tag (td class 'title)
    (if (cansee s)
        (do (deadmark s)
            (titlelink s url)
            (awhen (sitename url)
              (spanclass comhead
                (pr " (" )
                (if (admin)
                    (w/rlink (do (set-site-ban it
                                               (case (car (banned-sites* it))
                                                 nil    'ignore
                                                 ignore 'kill
                                                 kill   nil))
                                 whence)
                      (let ban (car (banned-sites* it))
                        (tag-if ban (font color (case ban
                                                  ignore darkred
                                                  kill   darkblue))
                          (pr it))))
                    (link it "/from?site=@it"))
                (pr ") "))))
        (pr (pseudo-text s)))))

(def titlelink (s url)
  (let toself (blank url)
    (tag (a href (if toself
                      (item-url s!id)
                     (or (live s) (author s) (editor))
                      url
                      nil)
            rel  (unless (or toself (> (realscore s) follow-threshold*))
                   'nofollow))
      (pr s!title))))

(def pseudo-text (i)
  (if i!deleted "[deleted]" (flagged i) "[flagged]" "[dead]"))

(def deadmark (i)
  (when (mem 'dupe i!keys)
    (pr " [dupe] "))
  (when (private i)
    (pr " [private] "))
  (when (flagged i)
    (pr " [flagged] "))
  (when (and i!dead (seesdead))
    (pr " [dead] "))
  (when (and i!deleted (admin))
    (pr " [deleted] ")))

(= downvote-threshold* 200 downvote-time* 1440)

(= votewid* 14)

(def votelinks (i whence (o downtoo))
  (center
    (if (and (cansee i)
             (or (no (get-user))
                 (no ((votes) i!id))))
         (do (votelink i whence 'up)
             (if (and downtoo
                      (or (admin)
                          (< (item-age i) downvote-time*))
                      (canvote i 'down))
                 (votelink i whence 'down)
                 ; don't understand why needed, but is, or a new
                 ; page is generated on voting
                 (tag (span id (+ "down_" i!id)))))
        (author i)
         (do (fontcolor orange (pr "*"))
             (br)
             (hspace votewid*))
        (hspace votewid*))))

; could memoize votelink more, esp for non-logged in users,
; since only uparrow is shown; could straight memoize

; redefined later (identically) so the outs catch new vals of up-url, etc.

(def votelink (i whence dir)
  (tag (a id      (if (get-user) (string dir '_ i!id))
          onclick (if (get-user) "return vote(this)")
          href    (vote-url i dir whence))
    (tag (div class (+ "votearrow" (if (is dir 'down) " rotate180" ""))))))

(def vote-url (i dir whence)
  (+ "/vote?" "for=" i!id
              "&dir=" dir
              (aif (get-user) (+ "&by=" it "&auth=" (get-auth it)))
              "&whence=" (urlencode whence)))

(= lowest-score* -4)

; Not much stricter than whether to generate the arrow.  Further tests
; applied in vote-for.

(def canvote (i dir)
  (and (get-user)
       (news-type&live i)
       (or (is dir 'up) (> i!score lowest-score*))
       (no ((votes) i!id))
       (or (is dir 'up)
           (and (acomment i)
                (> (karma) downvote-threshold*)
                (no (aand i!parent (author:item it)))))))

; Need the by argument or someone could trick logged in users into
; voting something up by clicking on a link.  But a bad guy doesn't
; know how to generate an auth arg that matches each user's cookie.

(newsop vote (by for dir auth whence)
  (withs (i      (safe-item for)
          dir    (saferead dir)
          whence (if whence (urldecode whence) "news")
          user   (get-user))
    (if (no i)
         (pr "No such item.")
        (no (in dir 'up 'down))
         (pr "Can't make that vote.")
        (and by (or (isnt by user) (~is-auth auth user)))
         (pr "User mismatch.")
        (no user)
         (login-page 'both "You have to be logged in to vote."
                     (list (fn ()
                             (newslog 'vote-login)
                             (when (canvote i dir)
                               (vote-for i dir)
                               (logvote i)))
                           whence))
        (canvote i dir)
         (do (vote-for i dir)
             (logvote i))
         (pr "Can't make that vote."))))

(def itemline (i)
  (when (cansee i)
    (when (news-type i) (itemscore i))
    (byline i)))

(= show-score-threshold* 1)

(def itemscore (i)
  (tag (span id (+ "score_" i!id))
    (let score (if (is i!type 'pollopt) (realscore i) i!score)
      (when (or (is i!type 'pollopt)
                (> score show-score-threshold*))
        (pr (plural score "point")))))
  (hook 'itemscore i))

(def item-timestamp (i)
  "@(moment-secs i!time)")

; redefined later

(def byline (i)
  (pr " by @(userlink i!by)")
  (when (metastory i)
    (pr " to @(sublinks i)"))
  (pr " @(itemlink i (text-age:item-age i)) "))


(def itemlink (i (o label))
  (link (or label "link") (item-url i!id) title: (item-timestamp i)))

(def sublinks (i)
  (each p (item-paths i)
    (link (tostring (fontcolor black (pr:last:tokens p #\/))) p)
    (sp)))

(def user-url ((o user (get-user))) (+ "/user?id=" user))

(= show-avg* nil)

(def userlink (subject (o show-avg t))
  (link (user-name subject) (user-url subject))
  (awhen (and show-avg* (admin) show-avg (uval subject avg))
    (pr " (@(num it 1 t t))")))

(= admin-color* darkblue
   noob-color* (color 60 150 60)
   noob-time* (* 4 1440)) ; 4 days

(def user-name (subject)
  (if (and (admin) (admin subject))
       (tostring (fontcolor admin-color* (pr subject)))
      (and (editor) (ignored subject))
       (tostring (fontcolor darkred (pr subject)))
      (< (user-age subject) noob-time*)
       (tostring (fontcolor noob-color* (pr subject)))
      subject))

(= show-threadavg* nil)

(def commentlink (i)
  (when (cansee i)
    (pr bar*)
    (tag (a href (item-url i!id))
      (let n (- (visible-family i) 1)
        (if (> n 0)
            (do (pr (plural n "comment"))
                (awhen (and show-threadavg* (admin) (threadavg i))
                  (pr " (@(num it 1 t t))")))
            (pr "discuss"))))))

(def visible-family (i)
  (+ (if (cansee i) 1 0)
     (sum [visible-family:item _] i!kids)))

(def threadavg (i)
  (only&avg (map [or (uval _ avg) 1]
                 (rem admin (dedup (map !by (keep live (family i))))))))

(= user-changetime* 120 editor-changetime* 1440)

(or= everchange* (table) noedit* (table))

(def canedit (i)
  (or (admin)
      (and (~noedit* i!type)
           (editor)
           (< (item-age i) editor-changetime*))
      (own-changeable-item i)))

(def own-changeable-item (i)
  (and (author i)
       (~mem 'locked i!keys)
       (no i!deleted)
       (or (everchange* i!type)
           (private i)
           (< (item-age i) user-changetime*))))

(def editlink (i)
  (when (canedit i)
    (pr bar*)
    (link "edit" (edit-url i))))

(def addoptlink (p)
  (when (or (admin) (author p))
    (pr bar*)
    (onlink "add choice" (add-pollopt-page p))))

; reset later

(= flag-threshold* 1 flag-kill-threshold* 1 many-flags* 0)

; Un-flagging something doesn't unkill it, if it's now no longer
; over flag-kill-threshold.  Ok, since arbitrary threshold anyway.

(def flaglink (i whence)
  (when (and (get-user)
             (or (legit-user)
                 (> (karma) flag-threshold*)))
    (unless (is-user i!by)
      (pr bar*)
      (w/rlink (do (togglemem (get-user) i!flags)
                   (when (and (~mem 'nokill i!keys)
                              (len> i!flags flag-kill-threshold*))
                     (kill i 'flagged))
                   (when (admin)
                     (if (mem (get-user) i!flags)
                         (kill i 'flagged)
                         (do (pull 'flagged i!keys)
                             (save-item i))))
                   whence)
        (pr "@(if (mem (get-user) i!flags) 'un)flag")))
    (let label "notice"
      (when (and (admin) (or (flagged i) (len> i!flags many-flags*)))
        (pr bar* (plural (len i!flags) "flag") " ")
        (w/rlink (do (togglemem 'nokill i!keys)
                     (if (mem 'nokill i!keys) (wipe i!dead))
                     (save-item i)
                     whence)
          (pr (if (mem 'nokill i!keys) "un-notice" "noted")))))))

(def favlink (i whence)
  (when (and (get-user) (cansee i))
    (pr bar*)
    (w/rlink (do (togglemem i!id (uval favorites))
                 (save-prof)
                 (favorites-url))
      (pr "@(if (mem i!id (uval favorites)) 'un-)favorite"))))

(def killlink (i whence)
  (when (admin)
    (pr bar*)
    (w/rlink (do (zap no i!dead)
                 (if i!dead
                     (do (pull 'nokill i!keys)
                         (log-kill i))
                     (pushnew 'nokill i!keys))
                 (save-item i)
                 whence)
      (pr "@(if i!dead 'un)kill"))))

; Blast kills the submission and bans the user.  Nuke also bans the
; site, so that all future submitters will be ignored.  Does not ban
; the ip address, but that will eventually get banned by maybe-ban-ip.

(def blastlink (i whence (o nuke))
  (when (and (admin)
             (or (no nuke) (~empty i!url)))
    (pr bar*)
    (w/rlink (do (toggle-blast i nuke)
                 whence)
      (prt (if (ignored i!by) "un-") (if nuke "nuke" "blast")))))

(def toggle-blast (i (o nuke))
  (atomic
    (if (ignored i!by)
        (do (wipe i!dead (ignored i!by))
            (awhen (and nuke (sitename i!url))
              (set-site-ban it nil)))
        (do (= i!dead t)
            (ignore i!by (if nuke 'nuke 'blast))
            (awhen (and nuke (sitename i!url))
              (set-site-ban it 'ignore))))
    (if i!dead (log-kill i))
    (save-item i)
    (save-prof i!by)))

(def candelete (i)
  (or (admin) (own-changeable-item i)))

(def deletelink (i whence)
  (when (candelete i)
    (pr bar*)
    ; TODO: onlink?
    (linkf (if i!deleted "undelete" "delete") (req)
      (if (candelete i)
          (del-confirm-page i whence)
          (prn "You can't delete that.")))))

; Undeleting stories could cause a slight inconsistency. If a story
; linking to x gets deleted, another submission can take its place in
; url->story.  If the original is then undeleted, there will be two
; stories with equal claim to be in url->story.  (The more recent will
; win because it happens to get loaded later.)  Not a big problem.

(def del-confirm-page (i whence)
  (minipage "Confirm"
    (tab
      ; link never used so not testable but think correct
      (display-item nil i (flink [del-confirm-page i whence]))
      (spacerow 20)
      (tr (td)
          (td (urform (get-user) req
                      (do (when (candelete i)
                            (= i!deleted (is arg!b "Yes"))
                            (save-item i))
                          whence)
                (prn "Do you want this to @(if i!deleted 'stay 'be) deleted?")
                (br2)
                (but "Yes" "b") (sp) (but "No" "b")))))))

(def logvote (story)
  (newslog 'vote story!id (list story!title)))

(def text-age (a)
  (defs d (trunc:/ a 1440)
        h (trunc:/ a 60)
        m (trunc   a))
  (if (>= d 365)
      (strftime :local "on %b %e, %Y" (- (now) (* a 60)))
      (tostring
        (if (>= a 1440) (pr (plural d "day")    " ago")
            (>= a   60) (pr (plural h "hour")   " ago")
                        (pr (plural m "minute") " ago")))))


; Voting

; A user needs legit-threshold karma for a vote to count if there has
; already been a vote from the same IP address.  A new account below both
; new- thresholds won't affect rankings, though such votes still affect
; scores unless not a legit-user.

(= legit-threshold* 0 new-age-threshold* 0 new-karma-threshold* 2)

(def legit-user ((o user (get-user)))
  (or (editor user)
      (and user (mem 'legit (uval user keys)))))

(def possible-sockpuppet ((o user (get-user)))
  (or (ignored user)
      (< (uval user weight) .5)
      (and (< (user-age user) new-age-threshold*)
           (< (karma user) new-karma-threshold*))))

(= downvote-ratio-limit* .65 recent-votes* nil votewindow* 100)

; Note: if vote-for by one user changes (s 'score) while s is being
; edited by another, the save after the edit will overwrite the change.
; Actual votes can't be lost because that field is not editable.  Not a
; big enough problem to drag in locking.

(def vote-for (i (o dir 'up))
  (unless (or ((votes) i!id)
              (and (~live i) (~is-user i!by)))
    (withs (ip   (logins* (get-user))
            vote (list (seconds) ip (get-user) dir i!score))
      (unless (or (and (or (ignored) (check-key 'novote))
                       (~is-user i!by))
                  (and (is dir 'down)
                       (~editor)
                       (or (check-key 'nodowns)
                           (> (downvote-ratio) downvote-ratio-limit*)
                           ; prevention of karma-bombing
                           (just-downvoted i!by)))
                  (and (nor (legit-user)
                            (> (karma) legit-threshold*))
                       (~is-user i!by)
                       (find [is (cadr _) ip] i!votes))
                  (and (isnt i!type 'pollopt)
                       (biased-voter i vote)))
        (++ i!score (case dir up 1 down -1))
        ; canvote protects against sockpuppet downvote of comments
        (when (and (is dir 'up) (possible-sockpuppet))
          (++ i!sockvotes))
        (metastory&adjust-rank i)
        (unless (or (author i)
                    (and (is ip i!ip) (~editor))
                    (is i!type 'pollopt))
          (++ (karma i!by) (case dir up 1 down -1))
          (save-prof i!by))
        (wipe (comment-cache* i!id)))
      (if (admin) (pushnew 'nokill i!keys))
      (push vote i!votes)
      (save-item i)
      (push (list (seconds) i!id i!by (sitename i!url) dir)
            (uval votes))
      (= ((votes* (get-user)) i!id) vote)
      (save-votes)
      (zap [firstn votewindow* _] (uval votes))
      (save-prof)
      (push (cons i!id vote) recent-votes*))))

; redefined later

(def biased-voter (i vote) nil)

; ugly to access vote fields by position number

(def downvote-ratio ((o user (get-user)) (o sample 20))
  (ratio [is _!1!3 'down]
         (keep [let by ((item (car _)) 'by)
                 (nor (is by user) (ignored by))]
               (bestn sample (compare > car:cadr) (tablist (votes user))))))

(def just-downvoted (victim (o user (get-user)) (o n 3))
  (let prev (firstn n (recent-votes-by user))
    (and (is (len prev) n)
         (all (fn ((id sec ip voter dir score))
                (and (is victim ((item id) 'by))
                     (is dir 'down)))
              prev))))

; Ugly to pluck out fourth element.  Should read votes into a vote
; template.  They're stored slightly differently in two diff places:
; in one with the voter in the car and the other without.

(def recent-votes-by ((o user (get-user)))
  (keep [is _!3 user] recent-votes*))


; Story Submission

(newsop submit ()
  (if user
      (submit-page nil "" "" t)
      (submit-login-warning nil "" "" t)))

(def submit-login-warning ((o sub) (o url) (o title) (o showtext) (o text))
  (login-page 'both "You have to be logged in to submit."
              (fn ()
                (newslog 'submit-login)
                (submit-page sub url title showtext text))))

(def clean-sub (x)
  (+ '/l/ (downcase:last (tokens x #\/))))

(def url-input (url)
  (row "url" (input "u" url 50 "ln-url-input"))
  (row "" (underlink "suggest title" nil onclick: "suggestTitle();")))

(= submitjs* "
function tlen(el) { var n = el.value.length - 80; el.nextSibling.innerText = n > 0 ? n + ' too long' : ''; }

function suggestTitle() {
  var i = byId('ln-title-input');
  var msg = 'fetching...';
  i.value = msg;
  fetch('/suggest-title?url=' + encodeURIComponent(byId('ln-url-input').value)).then(x => {x.text().then(x => {
    if (i.value === msg) { i.value = x; tlen(i); i.focus(); }})})
  return false; // cancel browser nav
}
")

(def submit-page ((o sub) (o url) (o title) (o showtext) (o text "") (o msg))
  (minipage "Submit"
    (pagemessage msg)
    (urform (get-user) req
            (process-story (clean-sub arg!l)
                           (clean-url arg!u)
                           (striptags arg!t)
                           showtext
                           (and showtext (md-from-form arg!x)))
      (script submitjs*)
      (tab
        (row "to" (input "l" (or sub "news") 50))
        (row "title" (do (input "t" title 50 "ln-title-input" "tlen(this)" "tlen(this)")
                         (gentag span style "margin-left:10px")))
        (if prefer-url*
            (do (url-input url)
                (when showtext
                  (spacerow 20)
                  ;(row "" "<b>or</b>")
                  (row "text" (textarea "x" 4 50 (only&pr text)))))
            (do (row "text" (textarea "x" 4 50 (only&pr text)))
                (row "" "<b>or</b>")
                (url-input url)))
        (row "" (submit))
        (spacerow 20)
        (row "" submit-instructions*)))))

(= submit-instructions*
   "Leave url blank to submit a question for discussion. The text
   (if any) will appear at the top of the comments page.")

; For use by outside code like bookmarklet.
; http://news.domain.com/submitlink?l=news&u=http://foo.com&t=Foo
; Added a confirm step to avoid xss hacks.

(newsop submitlink (l u t x)
  (if user
      (submit-page l u t 't x)
      (submit-login-warning l u t 't x)))

(= title-limit* 80
   retry*       "Please try again."
   toolong*     "Please make title < @title-limit* characters."
   bothblank*   "The url and text fields can't both be blank.  Please
                 either supply a url, or if you're asking a question,
                 put it in the text field."
   toofast*     "You're submitting too fast.  Please slow down.  Thanks."
   spammage*    "Stop spamming us.  You're wasting your time.")

; Only for annoyingly high-volume spammers. For ordinary spammers it's
; enough to ban their sites and ip addresses.

(disktable big-spamsites* (+ newsdir* "big-spamsites"))

(def process-story (sub url title showtext text)
  (aif (and (~blank url) (live-story-w/url url))
       (do (vote-for it)
           (item-url it!id))
       (if (~get-user)
            (flink [submit-login-warning sub url title showtext text])
           (no (and (or (blank url) (valid-url url))
                    (~blank title)))
            (flink [submit-page sub url title showtext text retry*])
           (len> title title-limit*)
            (flink [submit-page sub url title showtext text toolong*])
           ;(and (blank url) (blank text))
            ;(flink [submit-page sub url title showtext text bothblank*])
           (let site (sitename url)
             (or (big-spamsites* site) (recent-spam site)))
            (flink [msgpage spammage*])
           (oversubmitting 'story url)
            (flink [msgpage toofast*])
           (let s (create-story sub url (process-title title) text)
             (story-ban-test s url)
             (if (ignored) (kill s 'ignored))
             (submit-item s)
             (maybe-ban-ip s)
             "newest"))))

(def submit-item (i)
  (push i!id (uval submitted))
  (save-prof)
  (vote-for i))

(def recent-spam (site)
  (and (caris (banned-sites* site) 'ignore)
       (recent-items [is (sitename _!url) site] 720)))

(def recent-items (test minutes)
  (let cutoff (- (seconds) (* 60 minutes))
    (latest-items test [< _!time cutoff])))

; Turn this on when spam becomes a problem.

(= enforce-oversubmit* nil)

; New user can't submit more than 2 stories in a 2 hour period.
; Give overeager users the key toofast to make limit permanent.

(def oversubmitting (kind (o url))
  (or (check-key 'rebuff)
      (and enforce-oversubmit*
           (or (check-key 'toofast)
               (ignored)
               (< (user-age) new-age-threshold*)
               (< (karma) new-karma-threshold*))
           (let ip (get-ip)
             (len> (recent-items [or (author _) (is _!ip ip)] 180)
                   (if (is kind 'story)
                       (if (bad-user) 0 1)
                       (if (bad-user) 1 10)))))))

; Note that by deliberate tricks, someone could submit a story with a
; blank title.

(diskvar scrubrules* (+ newsdir* "scrubrules"))

(def process-title (s)
  (let s2 (multisubst scrubrules* s)
    (zap upcase (s2 0))
    s2))

(def live-story-w/url (url)
  (aand (url->story* (canonical-url url)) (check (item it) live)))

(def parse-site (url)
  (rev (tokens (cadr (tokens url [in _ #\/ #\?])) #\.)))

(defmemo sitename (url)
  (and (valid-url url)
       (let toks (parse-site (rem #\space url))
         (if (isa!int (saferead (car toks)))
             (tostring (apply pr toks sep: "."))
             (let (t1 t2 t3 . rest) toks
               (if (and (~in t3 nil "www")
                        (or (mem t1 multi-tld-countries*)
                            (mem t2 long-domains*)))
                   (+ t3 "." t2 "." t1)
                   (and t2 (+ t2 "." t1))))))))

(= multi-tld-countries* '("uk" "jp" "au" "in" "ph" "tr" "za" "my" "nz" "br"
                          "mx" "th" "sg" "id" "pk" "eg" "il" "at" "pl"))

(= long-domains* '("blogspot" "wordpress" "livejournal" "blogs" "typepad"
                   "weebly" "posterous" "blog-city" "supersized" "dreamhosters"
                   ; "sampasite"  "multiply" "wetpaint" ; all spam, just ban
                   "eurekster" "blogsome" "edogo" "blog" "com"
                   "ycombinator"))

(def create-item (type :kws . args)
  (kwapply inst kws 'item 'type type 'id (new-item-id) args))

(def create-story (sub url title text (o user (get-user)) (o ip (get-ip)))
  (newslog 'create sub url (list title))
  (let s (create-item 'story :url :title :text by: user :ip)
    (update-subs s sub url title)
    (save-item s)
    (= (items* s!id) s)
    (unless (blank url) (register-url s url))
    (push s stories*)
    (hook 'create-story s)
    s))

(def update-subs (s (o sub) (o url) (o title))
  (when sub
    (each x (rev (tokens sub [or (whitec _) (in _ #\,)]))
      (pushnew (clean-sub x) s!keys)))
  (unless (mem (clean-sub "private") s!keys)
    (let title (downcase title)
      (if (headmatch (downcase "Show @site-abbrev*") title)
          (do (pull (clean-sub "news") s!keys)
              (pushnew (clean-sub "show") s!keys))
          (or (blank url)
              (headmatch (downcase "Ask @site-abbrev*") title))
          (do (pull (clean-sub "news") s!keys)
              (pushnew (clean-sub "ask") s!keys))))))


; Bans

(def ignore (subject cause)
  (= (ignored subject) t)
  (save-prof subject)
  (log-ignore subject cause))

(diskvar ignore-log* (+ newsdir* "ignore-log"))

(def log-ignore (subject cause)
  (todisk ignore-log* (cons (list subject (get-user) cause) ignore-log*)))

; Kill means stuff with this substring gets killed. Ignore is stronger,
; means that user will be auto-ignored.  Eventually this info should
; be stored on disk and not in the source code.

(disktable banned-ips*     (+ newsdir* "banned-ips"))   ; was ips
(disktable banned-sites*   (+ newsdir* "banned-sites")) ; was sites

(diskvar  comment-kill*    (+ newsdir* "comment-kill"))
(diskvar  comment-ignore*  (+ newsdir* "comment-ignore"))

(= ip-ban-threshold* 3)

(def set-ip-ban (ip yesno (o info))
  (= (banned-ips* ip) (and yesno (list (get-user) (seconds) info)))
  (todisk banned-ips*))

(def set-site-ban (site ban (o info))
  (= (banned-sites* site) (and ban (list ban (get-user) (seconds) info)))
  (todisk banned-sites*))

; Kill submissions from banned ips, but don't auto-ignore users from
; them, because eventually ips will become legit again.

; Note that ban tests are only applied when a link or comment is
; submitted, not each time it's edited.  This will do for now.

(def story-ban-test (i url)
  (site-ban-test i url)
  (ip-ban-test i)
  (hook 'story-ban-test i url))

(def site-ban-test (i url)
  (whenlet ban (banned-sites* (sitename url))
    (if (caris ban 'ignore) (ignore (get-user) 'site-ban))
    (kill i 'site-ban)))

(def ip-ban-test (i (o ip (get-ip)))
  (if (banned-ips* ip) (kill i 'banned-ip)))

(def comment-ban-test (i string kill-list ignore-list)
  (when (some [posmatch _ string] ignore-list)
    (ignore (get-user) 'comment-ban))
  (when (or (banned-ips* (get-ip)) (some [posmatch _ string] kill-list))
    (kill i 'comment-ban)))

; An IP is banned when multiple ignored users have submitted over
; ban-threshold* (currently loaded) dead stories from it.

; Can consider comments too if that later starts to be a problem,
; but the threshold may start to be higher because then you'd be
; dealing with trolls rather than spammers.

(def maybe-ban-ip (s)
  (when (and s!dead (ignored s!by))
    (let bads (loaded-items [and _!dead (astory _) (is _!ip s!ip)])
      (when (and (len> bads ip-ban-threshold*)
                 (some [and (ignored _!by) (isnt _!by s!by)] bads))
        (set-ip-ban s!ip t)))))

(def killallby (user)
  (map [kill _ 'all] (submissions user)))

; Only called from repl.

(def kill-whole-thread (c)
  (kill c 'thread)
  (map kill-whole-thread:item c!kids))


; Polls

; a way to add a karma threshold for voting in a poll
;  or better still an arbitrary test fn, or at least pair of name/threshold.
; option to sort the elements of a poll when displaying
; exclusive field? (means only allow one vote per poll)

(= poll-threshold* 0)

(newsop newpoll ()
  (if (and user (> (karma user) poll-threshold*))
      (newpoll-page)
      (pr "Sorry, you need @poll-threshold* karma to create a poll.")))

(def newpoll-page ((o title "Poll: ") (o text "") (o opts "") (o msg))
  (minipage "New Poll"
    (pagemessage msg)
    (urform (get-user) req
            (process-poll (striptags arg!t)
                          (md-from-form arg!x t)
                          (striptags arg!o))
      (tab
        (row "title"   (input "t" title 50))
        (row "text"    (textarea "x" 4 50 (only&pr text)))
        (row ""        "Use blank lines to separate choices:")
        (row "choices" (textarea "o" 7 50 (only&pr opts)))
        (row ""        (submit))))))

(= fewopts* "A poll must have at least two options.")

(def process-poll (title text opts (o user (get-user)) (o ip (get-ip)))
  (if (or (blank title) (blank opts))
       (flink [newpoll-page title text opts retry*])
      (len> title title-limit*)
       (flink [newpoll-page title text opts toolong*])
      (len< (paras opts) 2)
       (flink [newpoll-page title text opts fewopts*])
      (atlet p (create-poll (multisubst scrubrules* title) text opts)
        (ip-ban-test p ip)
        (when (ignored user) (kill p 'ignored))
        (submit-item p)
        (maybe-ban-ip p)
        "newest")))

(def create-poll (title text opts (o user (get-user)) (o ip (get-ip)))
  (newslog 'create-poll title)
  (with p (create-item 'poll :title :text by: user :ip)
    (update-subs p)
    (= (items* p!id) p)
    (= p!parts (map !id (map [create-pollopt p nil nil _ user ip]
                             (paras opts))))
    (save-item p)
    (push p stories*)))

(def create-pollopt (p url title text (o user (get-user)) (o ip (get-ip)))
  (with o (create-item 'pollopt :url :title :text parent: p!id by: user :ip)
    (save-item o)
    (= (items* o!id) o)))

(def add-pollopt-page (p)
  (minipage "Add Poll Choice"
    (urform (get-user) req
            (do (add-pollopt p (striptags arg!x))
                (item-url p!id))
      (tab
        (row "text" (textarea "x" 4 50))
        (row ""     (submit))))))

(def add-pollopt (p text)
  (unless (blank text)
    (atlet o (create-pollopt p nil nil text)
      (++ p!parts (list o!id))
      (save-item p))))

(def display-pollopts (p whence)
  (each o (visible (map item p!parts))
    (display-pollopt nil o whence)
    (spacerow 7)))

(def display-pollopt (n o whence)
  (tr (display-item-number n)
      (tag (td valign 'top)
        (votelinks o whence))
      (tag (td class 'comment)
        (tag (div style "margin-top:1px;margin-bottom:0px")
          (if (~cansee o)      (pr (pseudo-text o))
              (~live o)        (spanclass dead
                                 (pr (if (~blank o!title) o!title o!text)))
                               (if (and (~blank o!title) (~blank o!url))
                                   (link o!title o!url)
                                   (fontcolor black (pr o!text)))))))
  (tr (if n (td))
      (td)
      (tag (td class 'default)
        (spanclass comhead
          (itemscore o)
          (editlink o)
          (killlink o whence)
          (deletelink o whence)
          (deadmark o)))))


; Individual Item Page (= Comments Page of Stories)

(defmemo item-url (id) (+ "/item?id=" id))

(newsop item (id)
  (let s (safe-item id)
    (if (news-type s)
        (do (if s!deleted (note-baditem))
            (item-page s))
        (do (note-baditem)
            (pr "No such item.")))))

(newsop item.json (id) header: "application/json"
  (let s (safe-item id)
    (if (news-type s)
        (write-json (item>json s))
        (do (note-baditem)
            (pr "null")))))

(defop maxitem.json () header: "application/json"
  (write-json maxid*))

(newshook maxid (n)
  (firebase-set "v0/maxitem" maxid*))

(def descendants (i)
  (sum [visible-family _]
       (map item i!kids)))

(def story-comment-count (i)
  (when (metastory i) (descendants i)))

(def tnil (x) (if x #t nil))
(def tnull (x) (or x 'null))

(def item>json (i)
  (if (or i!deleted (private i))
      (obj id:      i!id
           deleted: (tnil i!deleted)
           dead:    (tnil i!dead)
           private: (tnil (private i))
           type:    (string i!type)
           time:    i!time
           parent:  i!parent)
      (obj id:      i!id
           dead:    (tnil i!dead)
           type:    (string i!type)
           kids:    i!kids
           by:      i!by
           time:    i!time
           title:   i!title
           url:     i!url
           text:    i!text
           parent:  i!parent
           score:   i!score
           descendants: (story-comment-count i))))

(def item>search (i)
  (if (or i!dead i!deleted (private i))
      (obj objectID:       (string i!id)
           parent_id:      (tnull i!parent)
           created_at_i:   i!time
           created_at:     (moment-secs i!time)
           deleted:        (tnil i!deleted)
           dead:           (tnil i!dead)
           private:        (tnil (private i))
           title:          'null
           url:            'null
           author:         'null
           points:         'null
           comment_text:   'null
           num_comments:   'null
           story_id:       'null
           story_text:     'null
           story_title:    'null
           story_url:      'null)
    (whenlet s (superparent i)
      (let r (and (no s!deleted) (no s!dead) (isnt s i))
        (obj objectID:     (string i!id)
             parent_id:    (tnull i!parent)
             created_at_i: i!time
             created_at:   (moment-secs i!time)
             deleted:      (tnil i!deleted)
             dead:         (tnil i!dead)
             private:      (tnil (private i))
             title:        (tnull i!title)
             url:          (tnull i!url)
             author:       (tnull i!by)
             points:       (tnull i!score)
             comment_text: (tnull:if (acomment i) (tnull i!text))
             num_comments: (tnull:if (metastory i) (story-comment-count i))
             story_id:     (tnull:if (isnt s i) s!id)
             story_text:   (tnull:if r s!text)
             story_title:  (tnull:if r s!title)
             story_url:    (tnull:if r s!url)
             _tags:        (list (string i!type) "author_@i!by" "story_@s!id"))))))

(newshook save-item (i)
  (firebase-set "v0/item/@i!id" (item>json i))
  (whenlet s (superparent i)
    (algolia-set "Item_production" (item>search i))
    (unless (is s!id i!id)
      (hook 'save-item s))))

(or= baditemreqs* (table) baditem-threshold* 1/100)

; Something looking at a lot of deleted items is probably the bad sort
; of crawler.  Throttle it for this server invocation.

(def note-baditem ((o ip (get-ip)))
  (unless (admin)
    (++ (baditemreqs* ip 0))
    (withs (r (requests/ip* ip) b (baditemreqs* ip))
       (when (and (> r 500) (> (/ b r) baditem-threshold*))
         (= (throttle-ips* ip) t)))))

; redefined later

(def news-type (i) (and i (in i!type 'story 'comment 'poll 'pollopt)))

(def item-page (i)
  (withs (title (and (cansee i)
                     (or i!title (aand i!text (ellipsize (striptags it)))))
          here (item-url i!id))
    (longpage (now) nil nil title here
      (tab (display-item nil i here t)
           (display-item-text i)
           (when (apoll i)
             (spacerow 10)
             (tr (td)
                 (td (tab (display-pollopts i here)))))
           (when (and (cansee i)      (or (admin) (comments-active i)))
             (spacerow 10)
             (row "" (comment-form i here))))
      (br2)
      (when (and i!kids (commentable i))
        (tab (display-subcomments i here))
        (br2)))))

(def commentable (i) (in i!type 'story 'comment 'poll))

; By default the ability to comment on an item is turned off after
; 45 days, but this can be overriden with commentable key.

(= commentable-threshold* (* 60 24 45))

(def comments-active (i)
  (and (live&commentable i)
       (live (superparent i))
       (or (< (item-age i) commentable-threshold*)
           (mem 'commentable i!keys))))


(or= displayfn* (table))

(= (displayfn* 'story)   (fn (n i here inlist)
                           (display-story n i here)))

(= (displayfn* 'comment) (fn (n i here inlist)
                           (display-comment n i here nil 0 nil inlist)))

(= (displayfn* 'poll)    (displayfn* 'story))

(= (displayfn* 'pollopt) (fn (n i here inlist)
                           (display-pollopt n i here)))

(def display-item (n i here (o inlist))
  ((displayfn* (i 'type)) n i here inlist))

(def superparent (i (o n))
  (aif (is n 0)
        i
       i!parent
        (superparent (item it) (if n (- n 1)))
       i))

(def display-item-text (s)
  (when (and (cansee s)
             (metastory s)
             ;(blank s!url)
             (~blank s!text))
    (spacerow 2)
    (row "" (spanclass comment (pr s!text)))))


; Edit Item

(def edit-url (i) (+ "/edit?id=" i!id))

(newsop edit (id)
  (let i (safe-item id)
    (if (and i
             (cansee i)
             (editable-type i)
             (or (news-type i) (admin) (author i)))
        (edit-page i)
        (pr "No such item."))))

(def editable-type (i) (fieldfn* i!type))

(or= fieldfn* (table))

(= (fieldfn* 'story)
   (fn (s)
     (withs (a (admin)       e (editor)        x (canedit s))
       `((string1 title     ,s!title        t ,x)
         (url     url       ,s!url          t ,e)
         (mdtext  text      ,s!text         t ,x)
         ,@(standard-item-fields s a e x)))))

(= (fieldfn* 'comment)
   (fn (c)
     (withs (a (admin)       e (editor)        x (canedit c))
       `((mdtext  text      ,c!text         t ,x)
         ,@(standard-item-fields c a e x)))))

(= (fieldfn* 'poll)
   (fn (p)
     (withs (a (admin)       e (editor)        x (canedit p))
       `((string1 title     ,p!title        t ,x)
         (mdtext2 text      ,p!text         t ,x)
         ,@(standard-item-fields p a e x)))))

(= (fieldfn* 'pollopt)
   (fn (p)
     (withs (a (admin)       e (editor)        x (canedit p))
       `((string  title     ,p!title        t ,x)
         (url     url       ,p!url          t ,x)
         (mdtext2 text      ,p!text         t ,x)
         ,@(standard-item-fields p a e x)))))

(def standard-item-fields (i a e x)
  (withs (d   (timedate i!time :local)
          ymd (aand (cut d 0 3) (if a it (english-date it)))
          hms (aand (cut d 3 6) (if a it (english-time it))))
       `((sexpr   votes     ,i!votes       ,a  nil)
         (date    date      ,ymd            t ,a)
         (time    time      ,hms            t ,a)
         (int     score     ,i!score        t ,a)
         (num     sockvotes ,i!sockvotes   ,a ,a)
         (yesno   dead      ,i!dead        ,e ,e)
         (yesno   deleted   ,i!deleted     ,a ,a)
         (sexpr   flags     ,i!flags       ,a  nil)
         (sexpr   keys      ,i!keys        ,a ,a)
         (string  ip        ,i!ip          ,e  nil))))

; Should check valid-url etc here too.  In fact make a fn that
; does everything that has to happen after submitting a story,
; and call it both there and here.

(def edit-page (i)
  (withs (here (edit-url i) ymd (timedate i!time :local))
    (shortpage nil nil "Edit" here
      (tab (display-item nil i here t)
           (display-item-text i))
      (br2)
      (vars-form ((fieldfn* i!type) i)
                 (fn (name val)
                   (unless (ignore-edit i name val)
                     (when (and (is name 'dead) val (no i!dead))
                       (log-kill i))
                     (case name
                       date (= ymd (+ val (nthcdr 3 ymd)))
                       time (= ymd (+ (firstn 3 ymd) val))
                            (= (i name) val))))
                 (fn () ;(if (admin) (pushnew 'locked i!keys))
                        (aif (ok-date ymd) (= i!time it))
                        (save-item i)
                        (metastory&adjust-rank i)
                        (wipe (comment-cache* i!id))
                        (edit-url i)))
      (hook 'edit i))))

(def ignore-edit (i name val)
  (and (~admin)
       (case name
         title (len> val title-limit*)
         dead (mem 'nokill i!keys))))

(def ok-time (ts) (and ts (<= ts (now)) ts))

(def ok-date (ymd) (ok-time:safe:date>seconds ymd :local))

; Comment Submission

(def comment-login-warning (parent whence (o text))
  (login-page 'both "You have to be logged in to comment."
              (fn ()
                (newslog 'comment-login)
                (addcomment-page parent whence text))))

(def addcomment-page (parent whence (o text) (o msg))
  (minipage "Add Comment"
    (pagemessage msg)
    (tab
      (let here (flink [addcomment-page parent whence text msg])
        (display-item nil parent here t))
      (spacerow 10)
      (row "" (comment-form parent whence text)))))

(= noob-comment-msg* nil)

; Comment forms last for 30 min (- cache time)

(def comment-form (parent whence (o text))
  (urform (get-user) req
          (process-comment parent arg!text whence)
    (textarea "text" 8 80
      (aif text (prn (unmarkdown it))))
    (when (and noob-comment-msg* (noob))
      (br2)
      (spanclass subtext (pr noob-comment-msg*)))
    (br2)
    (submit (if (acomment parent) "reply" "add comment"))))

(= comment-threshold* -20)

; Have to remove #\returns because a form gives you back "a\r\nb"
; instead of just "a\nb".   Maybe should just remove returns from
; the vals coming in from any form, e.g. in aform.

(def process-comment (parent text whence)
  (if (~get-user)
       (flink [comment-login-warning parent whence text])
      (empty text)
       (flink [addcomment-page parent whence text retry*])
      (oversubmitting 'comment)
       (flink [msgpage toofast*])
       (atlet c (create-comment parent (md-from-form text))
         (comment-ban-test c text comment-kill* comment-ignore*)
         (if (bad-user) (kill c 'ignored/karma))
         (if (dupe-reply c) (kill c 'dupe))
         (submit-item c)
         (process-reply parent c)
         (+ whence "#" (aif (dupe-reply c) it!id c!id)))))

(def dupe-reply (c)
  (find [and (live _)
             (is _!text c!text)
             (is _!by c!by)]
        (siblings c)))

(def process-reply (parent c)
  (whenlet subject parent!by
    (aand (isnt subject c!by)
          (live c)
          (uval subject notify)
          (uval subject email)
          (if (is it (uval subject verified)) it)
          (let p (superparent c)
            (send-email site-email* it
                        "New reply from @c!by"
                        "@{site-url*}@(item-url c!id) on: @p!title")))))

(def bad-user ((o u (get-user)))
  (or (ignored u) (< (karma u) comment-threshold*)))

(def create-comment (parent text (o user (get-user)) (o ip (get-ip)))
  (newslog 'comment parent!id)
  (let c (create-item 'comment :text parent: parent!id by: user :ip)
    (save-item c)
    (= (items* c!id) c)
    (push c!id parent!kids)
    (save-item parent)
    (push c comments*)
    c))


; Comment Display

(def display-comment-tree (c whence (o indent 0) (o initialpar))
  (when (cansee-descendant c)
    (display-1comment c whence indent initialpar)
    (display-subcomments c whence (+ indent 1))))

(def display-1comment (c whence indent showpar)
  (row (tab (display-comment nil c whence t indent showpar showpar))))

(def display-subcomments (c whence (o indent 0))
  (each k (sort (compare > frontpage-rank:item) c!kids)
    (display-comment-tree (item k) whence indent)))

(def display-comment (n c whence (o astree) (o indent 0)
                                 (o showpar) (o showon))
  (tr (display-item-number n)
      (when astree (td (hspace (* indent 40))))
      (tag (td valign 'top) (votelinks c whence t))
      (display-comment-body c whence astree indent showpar showon)))

; Comment caching doesn't make generation of comments significantly
; faster, but may speed up everything else by generating less garbage.

; It might solve the same problem more generally to make html code
; more efficient.

(= comment-cache* (table) comment-cache-timeout* (table) cc-window* 10000)

(= comments-printed* 0 cc-hits* 0)

(= comment-caching* t)

; Cache comments generated for nil user that are over an hour old.
; Only try to cache most recent 10k items.  But this window moves,
; so if server is running a long time could have more than that in
; cache.  Probably should actively gc expired cache entries.

(def display-comment-body (c whence astree indent showpar showon)
  (++ comments-printed*)
  (if (and comment-caching*
           astree (no showpar) (no showon)
           (live c)
           (nor (admin) (editor) (author c))
           (< (- maxid* c!id) cc-window*)
           (> (- (seconds) c!time) 60)) ; was 3600
      (pr (cached-comment-body c whence indent))
      (gen-comment-body c whence astree indent showpar showon)))

(def cached-comment-body (c whence indent)
  (or (and (> (or (comment-cache-timeout* c!id) 0) (seconds))
           (awhen (comment-cache* c!id)
             (++ cc-hits*)
             it))
      (= (comment-cache-timeout* c!id)
          (cc-timeout c!time)
         (comment-cache* c!id)
          (tostring (gen-comment-body c whence t indent nil nil)))))

; Cache for the remainder of the current minute, hour, or day.

(def cc-timeout (t0)
  (let age (- (seconds) t0)
    (+ t0 (if (< age 3600)
               (* (+ (trunc (/ age    60)) 1)    60)
              (< age 86400)
               (* (+ (trunc (/ age  3600)) 1)  3600)
               (* (+ (trunc (/ age 86400)) 1) 86400)))))

(def gen-comment-body (c whence astree indent showpar showon)
  (tag (td id c!id class 'default)
    (let parent (and (or (no astree) showpar) (c 'parent))
      (tag (div style "margin-top:2px; margin-bottom:-10px; ")
        (spanclass comhead
          (itemline c)
          (when parent
            (when (cansee c) (pr bar*))
            (link "parent" (item-url ((item parent) 'id))))
          (editlink c)
          (killlink c whence)
          ;(blastlink c whence)
          (deletelink c whence)
          ; a hack to check whence but otherwise need an arg just for this
          (unless (or astree (is whence "newcomments"))
            (flaglink c whence))
          (favlink c whence)
          (deadmark c)
          (when showon
            (pr " | on: ")
            (let s (superparent c)
              (link (ellipsize s!title 50) (item-url s!id))))))
      (when (or parent (cansee c))
        (br))
      (spanclass comment
        (if (~cansee c)                    (pr (pseudo-text c))
            (nor (live c) (author c))      (spanclass dead (pr c!text))
                                           (fontcolor (comment-color c)
                                             (pr c!text))))
      (when (and astree (cansee c) (live c))
        (para)
        (tag (font size 1)
          (if (and (~mem 'neutered c!keys)
                   (replyable c indent)
                   (comments-active c))
              (underline (replylink c whence))
              (fontcolor sand (pr "-----"))))))))

; For really deeply nested comments, caching could add another reply
; delay, but that's ok.

; People could beat this by going to the link url or manually entering
; the reply url, but deal with that if they do.

(= reply-decay* 1.8)   ; delays: (0 0 1 3 7 12 18 25 33 42 52 63)

(def replyable (c indent)
  (or (< indent 2)
      (> (item-age c) (expt (- indent 1) reply-decay*))))

(def replylink (i whence (o title 'reply))
  (link title (+ "/reply?id=" i!id "&whence=" (urlencode whence))))

(newsop reply (id whence)
  (withs (i      (safe-item id)
          whence (or (only&urldecode whence) "news"))
    (if (only&comments-active i)
        (if user
            (addcomment-page i whence)
            (login-page 'both "You have to be logged in to comment."
                        (fn ()
                          (newslog 'comment-login)
                          (addcomment-page i whence))))
        (pr "No such item."))))

(def comment-color (c)
  (if (> c!score 0) black (grayrange c!score)))

(defmemo grayrange (s)
  (gray (min 230 (round (expt (* (+ (abs s) 2) 900) .6)))))


; Threads

(def threads-url (user) (+ "/threads?id=" user))

(newsop threads (id)
  (if id
      (threads-page id)
      (pr "No user specified.")))

(def threads-page (subject)
  (if (profile subject)
      (withs (title (+ subject "'s threads")
              label (if (is-user subject) "threads" title)
              here  (threads-url subject))
        (longpage (now) nil label title here
          (awhen (keep cansee&~subcomment
                       (submissions subject maxend*))
            (display-threads it label title here))))
      (prn "No such user.")))

(def display-threads (comments label title whence
                      (o start 0) (o end threads-perpage*))
  (tab
    (each c (cut comments start end)
      (row
        (if (acomment c)
            (display-comment-tree c whence 0 t)
            (tab (display-item nil c whence t))))
      (spacerow (if (acomment c) 15 5)))
    (when end
      (let newend (+ end threads-perpage*)
        (when (and (<= newend maxend*) (< end (len comments)))
          (spacerow 10)
          (row (tab (tr (td (hspace 0))
                        (td (hspace votewid*))
                        (tag (td class 'title)
                          (morelink display-threads
                                    comments label title end newend))))))))))

(def submissions (user (o limit))
  (map item (firstn limit (uval user submitted))))

(def favorites (user (o limit))
  (map item (firstn limit (uval user favorites))))

(def comments (user (o limit))
  (map item (retrieve limit acomment:item (uval user submitted))))

(def subcomment (c)
  (some [and (acomment _) (is _!by c!by) (no _!deleted)]
        (ancestors c)))

(def ancestors (i)
  (accum a (trav i!parent a:item self:!parent:item)))

(def siblings (i)
  (map item (aand (item i!parent) (rem i!id it!kids))))

(def favorites-url ((o user (get-user))) (+ "/favorites?id=" user))

(newsop favorites (id)
  (if id
      (favorites-page id)
      (pr "No user specified.")))

(def favorites-page (subject)
  (if (profile subject)
      (withs (title (+ subject "'s favorites")
              label (if (is-user subject) "favorites" title)
              here  (favorites-url subject))
        (longpage (now) nil label title here
          (awhen (keep cansee
                       (favorites subject maxend*))
            (display-threads it label title here))))
      (prn "No such user.")))

; Submitted

(def submitted-url (user) (+ "/submitted?id=" user))

(newsop submitted (id)
  (if id
      (submitted-page id)
      (pr "No user specified.")))

(def submitted-page (subject)
  (if (profile subject)
      (withs (label (+ subject "'s submissions")
              here  (submitted-url subject))
        (longpage (now) nil label label here
          (if (or (~ignored subject)
                  (is-user subject)
                  (seesdead))
              (aif (keep metastory&cansee (submissions subject))
                   (display-items it label label here 0 perpage* t)))))
      (pr "No such user.")))


; RSS

(newsop rss () header: "application/rss+xml; charset=utf-8"
  (rsspage nil))

(newscache rsspage user 90
  (rss-stories (retrieve (len ranked-stories*) ~private&live ranked-stories*)))

(def rss-stories (stories)
  (tag (rss version "2.0")
    (tag channel
      (tag title (pr site-name*))
      (tag link (pr site-url*))
      (tag description (pr site-desc*))
      (each s stories
        (tag item
          (let comurl (+ site-url* (item-url s!id))
            (tag title    (pr (eschtml s!title)))
            (tag link     (pr (if (blank s!url) comurl (eschtml s!url))))
            (tag pubDate  (pr (rss-date s!time)))
            (tag comments (pr comurl))
            (tag description
              (cdata (link "Comments" comurl)))))))))

; RSS comments

(newsop rsscomments () header: "application/rss+xml; charset=utf-8"
  (rsscpage nil))

(newscache rsscpage user 90
  (rss-comments (retrieve (len comments*) ~private&live comments*)))

(def rss-comment-title (c)
  (tostring
    (let p (superparent c 1)
      (if (acomment p)
          (pr c!by " to " p!by bar*)
        (pr c!by bar*)))
    (let s (superparent c)
      (pr (ellipsize s!title 50)))))

(def rss-comments (comments)
  (tag (rss version "2.0")
    (tag channel
      (tag title (pr (+ site-name* ": new comments")))
      (tag link (pr (+ site-url* "/newcomments")))
      (tag description (pr (+ site-desc* ": the conversation")))
      (each c comments
        (tag item
          (let comurl (+ site-url* (item-url c!id))
            (tag title    (pr (rss-comment-title c)))
            (tag link     (pr (if (blank c!url) comurl (eschtml c!url))))
            (tag pubDate  (pr (rss-date c!time)))
            (tag comments (pr comurl))
            (tag description
              (cdata
                (pr c!text)
                (br2)
                (pr "on: ")
                (let s (superparent c)
                  (link (ellipsize s!title 50) (+ site-url* (item-url s!id))))))))))))

; memoize RSS pages

(defbg memoize-rss 30 (memoize-rss))

(def memoize-rss ()
  (tostring:rsscpage nil)
  (tostring:rsspage nil)
  ; memoize item dates too.
  (each i ranked-stories*
    (aand i!time (moment-secs i!time)))
  (each c comments*
    (aand c!time (moment-secs c!time)))
  nil)

; User Stats

(newsop leaders () (leaderspage user))

(= nleaders* 100)

(newscache leaderspage user 1000
  (longpage (now) nil "leaders" "Leaders" "leaders"
    (sptab
      (tr (td) (td) (tdr "total"))
      (spacerow 10)
      (let i 0
        (each u (firstn nleaders* (leading-users))
          (tr (tdr:pr (++ i) ".")
              (td (userlink u nil))
              (tdr:pr (karma u))
              (when (admin)
                (tdr:prt (only&num (uval u avg) 2 t t))))
          (if (is i 10) (spacerow 30)))))))

(= leader-threshold* 0)  ; redefined later

(def leading-users ()
  (sort (compare > [karma _])
        (users [and (> (karma _) leader-threshold*) (~admin _)])))

(adop editors ()
  (tab (each u (users [is (uval _ auth) 1])
         (row (userlink u)))))


(= update-avg-threshold* 0)  ; redefined later

(defbg update-avg 45
  (unless (or (empty profs*) (no stories*))
    (update-avg (rand-user [and (only&> (car (uval _ submitted))
                                        (- maxid* initload*))
                                (len> (uval _ submitted)
                                      update-avg-threshold*)]))))

(def update-avg (user)
  (= (uval user avg) (comment-score user))
  (save-prof user))

(def rand-user ((o test idfn))
  (evtil (rand-key profs*) test))

; Ignore the most recent 5 comments since they may still be gaining votes.
; Also ignore the highest-scoring comment, since possibly a fluff outlier.

(def comment-score (user)
  (aif (check (nthcdr 5 (comments user 50)) [len> _ 10])
       (avg (cdr (sort > (map !score (rem !deleted it)))))
       nil))


; Comment Analysis

; Instead of a separate active op, should probably display this info
; implicitly by e.g. changing color of commentlink or by showing the
; no of comments since that user last looked.

(newsop active () (active-page user))

(newscache active-page user 600
  (listpage (actives) "active" "Active Threads"))

(def actives ((o n maxend*) (o consider 2000))
  (visible (rank-stories n consider (memo active-rank))))

(= active-threshold* 1500)

(def active-rank (s)
  (sum [max 0 (- active-threshold* (item-age _))]
       (cdr (family s))))

(def family (i) (cons i (mappend family:item i!kids)))


(newsop newcomments () (newcomments-page user))

(newscache newcomments-page user 60
  (listpage (visible (firstn maxend* comments*))
            "comments" "New Comments" "newcomments" nil))


; Doc

(defop formatdoc req
  (msgpage formatdoc* "Formatting Options"))

(= formatdoc-url* "/formatdoc")

(= formatdoc*
"Blank lines separate paragraphs.
<p> Text surrounded by asterisks is italicized. To get a literal
asterisk, use \\* or **.
<p> Text after a blank line that is indented by two or more spaces is
reproduced verbatim.  (This is intended for code.)
<p> Urls become links, except in the text field of a submission.<br><br>")


; Noprocrast

(def check-procrast ()
  (or (no (get-user))
      (no (uval noprocrast))
      (let now (seconds)
        (unless (uval firstview)
          (reset-procrast))
        (or (when (< (/ (- now (uval firstview)) 60)
                     (uval maxvisit))
              (= (uval lastview) now)
              (save-prof)
              t)
            (when (> (/ (- now (uval lastview)) 60)
                     (uval minaway))
              (reset-procrast)
              t)))))

(def reset-procrast ()
  (= (uval lastview) (= (uval firstview) (seconds)))
  (save-prof))

(def procrast-msg (whence)
  (let m (+ 1 (trunc (- (uval minaway)
                        (minutes-since (uval lastview)))))
    (pr "<b>Get back to work!</b>")
    (para "Sorry, you can't see this page.  Based on the anti-procrastination
           parameters you set in your profile, you'll be able to use the site
           again in " (plural m "minute") ".")
    (para "(If you got this message after submitting something, don't worry,
           the submission was processed.)")
    (para "To change your anti-procrastination settings, go to your profile
           by clicking on your username.  If <tt>noprocrast</tt> is set to
           <tt>yes</tt>, you'll be limited to sessions of <tt>maxvisit</tt>
           minutes, with <tt>minaway</tt> minutes between them.")
    (para)
    (w/rlink whence (underline (pr "retry")))
    ; (hspace 20)
    ; (w/rlink (do (reset-procrast) whence) (underline (pr "override")))
    (br2)))


; Reset PW

(defopg changepw req
  (changepw-page (get-user)))

(def changepw-page ((o msg))
  (minipage "Reset Password for @(get-user)"
    (if msg
         (pr msg)
        (blank (uval email))
         (do (pr "Before you do this, please add your email address to your ")
             (underlink "profile" (user-url))
             (pr ". Otherwise you could lose your account if you mistype
                  your new password.")))
    (br2)
    (urform (get-user) req (try-changepw arg!oldpw arg!pw)
      (tab
        (row "Current Password:" (input 'oldpw "" 20 type: 'password))
        (row "New Password:"     (input 'pw    "" 20 type: 'password))
        (row ""                  (submit "Change"))))))

(def try-changepw (oldpw newpw)
  (if (or (len< newpw 4) (len> newpw 72))
       (flink [changepw-page "Passwords should be between 4 and 72 characters long.
                             Please choose another."])
      (~check-pw oldpw)
       (flink [changepw-page "Current password incorrect. Please try again."])
       (do (set-pw newpw)
           (logout-user)
           "/news")))

(def send-resetpw (subject email)
  (send-email site-email*
              email
              "@site-name* password recovery"
              (with s (+ "Someone (hopefully you) requested we reset your password for @subject at @{site-name*}."
                         " If you want to change it, please visit "
                         site-url* (flink [force-resetpw-page subject])
                         "\n\n"
                         "If not, just ignore this message.\n")
                (when (readenv "DEV" nil)
                  (ero s)))))

(def force-resetpw-page (subject (o msg))
  (minipage "Reset Password for @subject"
    (if msg (pr msg))
    (br2)
    (aform [try-force-resetpw subject arg!pw]
      (single-input "New password: " 'pw 20 "reset" t))))

(def try-force-resetpw (subject newpw)
  (if (len< newpw 4)
      (force-resetpw-page subject "Passwords should be a least 4 characters long.
                          Please choose another.")
      (do (set-pw subject newpw)
          (logout-user subject)
          (newspage nil))))

(defop forgot req (forgot-page))

(def forgot-page ((o subject arg!acct))
  (prbold "Reset your password")
  (br2)
  (aform (fn (req)
           (aand arg!acct
                 (profile it)
                 (send-resetpw it!id it!email))
           (msgpage "Password recovery message sent. If you don't see it, you might want to check your spam folder."))
    (inputs (acct username 20 subject 'plain 'autofocus))
    (br)
    (submit "Send reset email")))

(def forgot-url ((o subject arg!acct))
  (if subject (+ "/forgot?acct=" (eschtml subject)) "/forgot"))

(newshook login-form args
  (link "Forgot your password?" (forgot-url)))

; Scrubrules

(defopa scrubrules req
  (scrub-page scrubrules*))

; If have other global alists, generalize an alist edit page.
; Or better still generalize vars-form.

(def scrub-page (rules (o msg))
  (minipage "Scrubrules"
    (when msg (pr msg) (br2))
    (urform (get-user) req
            (withs (froms (lines arg!from)
                    tos   (lines arg!to))
              (if (is (len froms) (len tos))
                  (do (todisk scrubrules* (map list froms tos))
                      "/scrubrules")
                  (flink [scrub-page rules "To and from should be same length."])))
      (pr "From: ")
      (tag (textarea name 'from
                     cols (apply max 20 (map len (map car rules)))
                     rows (+ (len rules) 3))
        (apply pr #\newline (intersperse #\newline (map car rules))))
      (pr " To: ")
      (tag (textarea name 'to
                     cols (apply max 20 (map len (map cadr rules)))
                     rows (+ (len rules) 3))
        (apply pr #\newline (intersperse #\newline (map cadr rules))))
      (br2)
      (submit "update"))))


; Site pages

(def pages-url ((o anchor nil)) (+ "/pages" (aand anchor "#@it")))

(defopa pages req
  (edit-pages-page))

(def edit-pages-page ()
  (minipage "Edit Pages"
    (urform (get-user) req
            (do (todisk guidelines-page* (md-from-form arg!guidelines nil t))
                "/newsguidelines.html")
      (idtab "guidelines"
        (row (underlink "/newsguidelines.html"))
        (row (textarea "guidelines" 80 60
               (pr:esc-tags:unmarkdown guidelines-page* t)))
        (row (submit "update /newsguidelines.html"))))

    (urform (get-user) req
            (do (todisk welcome-page* (md-from-form arg!welcome nil t))
                "/welcome.html")
      (idtab "welcome"
        (row (underlink "/welcome.html"))
        (row (textarea "welcome" 80 60
               (pr:esc-tags:unmarkdown welcome-page* t)))
        (row (submit "update /welcome.html"))))

    (urform (get-user) req
            (do (todisk newsfaq-page* (md-from-form arg!newsfaq nil t))
                "/newsfaq.html")
      (idtab "newsfaq"
        (row (underlink "/newsfaq.html"))
        (row (textarea "newsfaq" 80 60
               (pr:esc-tags:unmarkdown newsfaq-page* t)))
        (row (submit "update /newsfaq.html"))))

    (urform (get-user) req
            (do (todisk bookmarklet-page* arg!bookmarklet)
                "/bookmarklet.html")
      (idtab "bookmarklet"
        (row (underlink "/bookmarklet.html"))
        (row (textarea "bookmarklet" 80 60
               (pr:esc-tags bookmarklet-page*)))
        (row (submit "update /bookmarklet.html"))))))


; Bookmarklet

(= bookmarklet-js*
   (+ "javascript:"
      "q=location.href;"
      "if(document.getSelection){d=document.getSelection();}else{d='';}"
      "p=document.title;"
      "void(open('@{site-url*}/submitlink?l=news'"
      "+'&u='+encodeURIComponent(q)"
      "+'&t='+encodeURIComponent(p)"
      "+'&x='+encodeURIComponent(d),"
      "'_blank','toolbar=no,width=700,height=600'));"))

(diskfile bookmarklet-page* (+ newsdir* "bookmarklet.html") "
<p id=\"first\">
    Thanks to Phil Kast for writing the
    <a href=\"https://news.ycombinator.com/bookmarklet.html\">
    <u>original bookmarklet</u></a>.
    <br><br> When you click on the bookmarklet, it will submit the
    page you're on to @{site-name*}. To install, drag this link to
    your browser toolbar:
    <br><br>
</p>

<center>

<!-- <div style=\"margin: auto; padding: 16px; width: 30%; background: #f7f7f7;\"> -->

<a style=\"color: #777; font-size: 2em;\" rel=\"nofollow\" href=\"@{bookmarklet-js*}\">
  <u>post to @{site-abbrev*}</u>
</a>

<br><br>
<br><br> On mobile devices, create a new bookmark, edit it, and
replace its url with the following text:
<br><br>

<textarea cols=\"60\" rows=\"7\" wrap=\"virtual\" name=\"about\">@{bookmarklet-js*}</textarea>

<br><br> It should look like this:
<br><br>
<img src=\"https://i.imgur.com/J1kFydT.png\" width=\"300px\" />
")

(newsop bookmarklet.html ()
  (msgpage bookmarklet-page* "Bookmarklet" (pages-url "bookmarklet")))


; Guidelines

(diskfile guidelines-page* (+ newsdir* "newsguidelines.html") (md-from-form "
_What to Submit_

On-Topic: STEM. Humanities. Humor. Anything intellectually engaging
and pro-social.

Off-Topic: That which is flame bait or vacuous.

_In Submissions_

If you submit a link to a video or pdf, please warn readers by
appending [video] or [pdf] to the title.

Please submit the original source.

_In Comments_

Be civil. On difficult subjects in particular, you should work hard at
being diplomatic. (It helps to picture yourself speaking to a friend.)

When disagreeing, reply to the argument instead of calling names.
\"That is idiotic; 1 + 1 is 2, not 3\" can be shortened to \"1 + 1 is
2, not 3.\"

Assume good faith.

Eschew flamebait.

Please limit your use of uppercase; it looks like shouting and is hard
to read.
" nil t))

(newsop newsguidelines.html ()
  (msgpage guidelines-page* "Guidelines" (pages-url "newsguidelines")))

; Welcome

(diskfile welcome-page* (+ newsdir* "welcome.html") (md-from-form "
_Welcome to @{site-name*}_

<a href=\"/\"><u>@{site-name*}</u></a> is a bit different from other
community sites, and we'd appreciate it if you'd take a minute to read
the following as well as the <a href=\"/newsguidelines.html\"><u>official
guidelines</u></a>.

@site-abbrev* is an experiment. As a rule, a community site that
becomes popular will decline in quality. Our hypothesis is that this
is not inevitable—that by making a conscious effort to resist decline,
we can keep it from happening.

Essentially there are two rules here: don't post or upvote crap links,
and don't be rude or dumb in comment threads.

A crap link is one that's only superficially interesting. Stories on
@site-abbrev* don't have to be about hacking, because good hackers
aren't only interested in hacking, but they do have to be deeply
interesting.

What does \"deeply interesting\" mean? It means stuff that teaches you
about the world. A story about a robbery, for example, would probably
not be deeply interesting. But if this robbery was a sign of some
bigger, underlying trend, perhaps it could be.

The worst thing to post or upvote is something that's intensely but
shallowly interesting: gossip about famous people, funny or cute
pictures or videos, partisan political articles, etc. If you let that
sort of thing onto a news site, it will push aside the deeply
interesting stuff, which tends to be quieter.

The most important principle on @{site-abbrev*}, though, is to make
thoughtful comments. Thoughtful in both senses: civil and substantial.

The test for substance is a lot like it is for links. Does your
comment teach us anything? There are two ways to do that: by pointing
out some consideration that hadn't previously been mentioned, and by
giving more information about the topic, perhaps from personal
experience.  Whereas comments like \"LOL!\" or worse still, \"That's
retarded!\" teach us nothing.

Empty comments can be ok if they're positive.  There's nothing wrong
with submitting a comment saying just \"Thanks.\" What we especially
discourage are comments that are empty and negative—comments that are
mere name-calling.

Which brings us to the most important principle on @{site-abbrev*}:
civility. Since long before the web, the anonymity of online
conversation has lured people into being much ruder than they'd be in
person. So the principle here is: don't say anything you wouldn't say
face to face.  This doesn't mean you can't disagree. But disagree
without calling names. If you're right, your argument will be more
convincing without them.
" nil t))

(newsop welcome.html ()
  (msgpage welcome-page* "Welcome" (pages-url "welcome")))

; FAQ

(diskfile newsfaq-page* (+ newsdir* "newsfaq.html") (md-from-form "
_@site-name* FAQ_

Are there rules about submissions and comments?
<a href=\"/newsguidelines.html\"><u>@{site-url*}/newsguidelines.html</u></a>

_How are stories ranked?_

The basic algorithm divides points by a power of the time since a
story was submitted. Comments in threads are ranked the same way.

Other factors affecting rank include user flags, anti-abuse software,
software which demotes overheated discussions, account or site
weighting, and moderator action.

_How is a user's karma calculated?_

Roughly, the number of upvotes on their posts minus the number of
downvotes. These don't match up exactly. Some votes are dropped by
anti-abuse software.

_Do posts by users with more karma rank higher?_

No.

_Why don't I see down arrows?_

There are no down arrows on stories. They appear on comments after
users reach a certain karma threshold, but never on direct replies.

_What kind of formatting can you use in comments?_

<a href=\"/formatdoc\"><u>@{site-url*}/formatdoc</u></a>

_How do I submit a question?_

Use the submit link in the top bar, and leave the url field blank.

_How do I submit a poll?_

<a href=\"/newpoll\"><u>@{site-url*}/newpoll</u></a>

_What are Ask @{site-abbrev*} and Show @{site-abbrev*}?_

<a href=\"/l/ask\"><u>Ask @{site-abbrev*}</u></a> lists questions and
other text submissions.
<a href=\"/l/show\"><u>Show @{site-abbrev*}</u></a> is for sharing
your personal work and has special <a href=\"/showhn.html\">rules</a>.

_What do green usernames mean?_

Green indicates a new account.

_Why are some comments faded?_

Faded text means that a comment has been downvoted. You can read the
comment in normal text by clicking on its timestamp to go to its page.

_What does [flagged] mean?_

Users flagged the post as breaking the
<a href=\"/newsguidelines.html\"><u>guidelines</u></a> or otherwise not
belonging on @{site-abbrev*}.

Moderators sometimes also add [flagged] (though not usually on submissions), and
sometimes turn flags off when they are unfair.

_How do I flag a comment?_

Click on its timestamp to go to its page, then click the 'flag' link
at the top. There's a small karma threshold before flag links appear.

<a name=dead>
_What does [dead] mean?_
</a>

The post was killed by software, user flags, or moderators.

Dead posts aren't displayed by default, but you can see them all by
turning on 'showdead' in your profile.

_What does [deleted] mean?_

The author deleted the post outright, or asked us to. Unlike
<a href=\"#dead\"><u>dead</u></a> posts, these remain deleted even when
showdead is turned on.

_Are reposts ok?_

If a story has not had significant attention in the last year or so, a
small number of reposts is ok. Otherwise we bury reposts as
duplicates.

Please don't delete and repost the same story. Deletion is for things
that shouldn't have been submitted in the first place.

_Can I ask people to upvote my submission?_

No. Users should vote for a story because they personally find it
intellectually interesting, not because someone has content to
promote. We penalize or ban submissions, accounts, and sites that
break this rule, so please don't.

_Can I ask people to comment on my submission?_

No, for the same reason. It's also not in your interest: @{site-abbrev*} readers
are sensitive to this and will detect it, flag it, and use unkind
words like 'spam'.

_Why can't I post a comment to a thread?_

Threads are closed to new comments after two weeks, or if the
submission has been killed by software, moderators, or user flags.

Why is A ranked below B even though A has more points and is newer?

You can't derive rank from votes and time alone. See \"How are stories
ranked?\" above.

_In my profile, what is delay?_

It gives you time to edit your comments before they appear to others.
Set it to the number of minutes you'd like. The maximum is 10.

_In my profile, what is noprocrast?_

It's a way to help you prevent yourself from spending too much time on
@{site-abbrev*}. If you turn it on you'll only be allowed to visit the
site for maxvisit minutes at a time, with gaps of minaway minutes in
between.  The defaults are 20 and 180, which would let you view the
site for 20 minutes at a time, and then not allow you back in for 3
hours.

_How do I reset my password?_

If you have an email address in your profile, you can do that
<a href=\"/forgot?id=\"><u>here</u></a>. If you haven't, email @{site-email*}
for help.

" nil t))

(newsop newsfaq.html ()
  (msgpage newsfaq-page* "@site-name* FAQ" (pages-url "newsfaq")))

(def tags-list ((o by arg!sort)
                (o rev? (or arg!rev (no arg!sort))))
  (aand (each-loaded-item i
          (when (cansee i)
            (map out (subs i))))
        (counts it)
        (sort (compare (if rev? > <)
                       (if (is (str by) "tag") car cadr))
              (tablist it))))

(def sortlink (whence name (o default))
  (withs (by   (or arg!sort (if default name))
          rev? (or arg!rev (no arg!sort)))
    (underlink name (+ whence "?sort=@name"
                       (when (is name by)
                         (unless rev? "&rev=t"))))))

(newscache tags-page user 90
  (minipage "Tags"
    (sptab
      (row (sortlink "/l" "tag") (sortlink "/l" "count" t))
      (each (site count) (tags-list)
        (tr (td (pr (link site))) (td count))))))

; Abuse Analysis

(adop badsites ()
  (sptab
    (row "Dead" "Days" "Site" "O" "K" "I" "Users")
    (each (site deads) (withs (banned (banned-site-items)
                               pairs  (killedsites))
                         (+ pairs (map [list _ (banned _)]
                                       (rem (fn (d)
                                              (some [caris _ d] pairs))
                                            (keys banned-sites*)))))
      (let ban (car (banned-sites* site))
        (tr (tdr (when deads
                   (onlink (len deads)
                           (listpage deads nil (+ "killed at " site) "badsites"))))
            (tdr (when deads (pr (round (days-since ((car deads) 'time))))))
            (td site)
            (td (w/rlink (do (set-site-ban site nil) "badsites")
                  (fontcolor (if ban gray!220 black) (pr "x"))))
            (td (w/rlink (do (set-site-ban site 'kill) "badsites")
                  (fontcolor (case ban kill darkred gray!220) (pr "x"))))
            (td (w/rlink (do (set-site-ban site 'ignore) "badsites")
                  (fontcolor (case ban ignore darkred gray!220) (pr "x"))))
            (td (each u (dedup (map !by deads))
                  (userlink u nil)
                  (pr " "))))))))

(def killedsites ()
  (with acc nil
    (defs bads (table) deadcount (table))
    (each-loaded-item i
      (awhen (and i!dead (sitename i!url))
        (push i (bads it))))
    (each (site items) bads
      (let n (len items)
        (when (> n 2)
          (= (deadcount site) n)
          (insort (compare > deadcount:car)
                  (list site (rev items))
                  acc))))))

(def banned-site-items ()
  (with bads (table)
    (each-loaded-item i
      (awhen (and i!dead (check (sitename i!url) banned-sites*))
        (push i (bads it))))))

; Would be nice to auto unban ips whose most recent submission is > n
; days old, but hard to do because of lazy loading.  Would have to keep
; a table of most recent submission per ip, and only enforce bannnedness
; if < n days ago.

(adop badips ()
  (withs ((bads goods) (badips)
          (subs ips)   (sorted-badips bads goods))
    (sptab
      (row "IP" "Days" "Dead" "Live" "Users")
      (each ip ips
        (tr (td (let banned (banned-ips* ip)
                  (w/rlink (do (set-ip-ban ip (no banned))
                               "badips")
                    (fontcolor (if banned darkred) (pr ip)))))
            (tdr (when (or (goods ip) (bads ip))
                   (pr (round (days-since
                                (max (aif (car (goods ip)) it!time 0)
                                     (aif (car (bads  ip)) it!time 0)))))))
            (tdr (onlink (len (bads ip))
                         (listpage (bads ip)
                                   nil (+ "dead from " ip) "badips")))
            (tdr (onlink (len (goods ip))
                         (listpage (goods ip)
                                   nil (+ "live from " ip) "badips")))
            (td (each u (subs ip)
                  (userlink u nil)
                  (pr " "))))))))

(defcache badips 300
  (withs (bads (table) goods (table))
    (each-loaded-item s
      (if (and s!dead (commentable s))
          (push s (bads  s!ip))
          (push s (goods s!ip))))
    (each (k v) bads  (zap rev (bads  k)))
    (each (k v) goods (zap rev (goods k)))
    (list bads goods)))

(def sorted-badips (bads goods)
  (withs (ips  (let ips (rem [len< (bads _) 2] (keys bads))
                (+ ips (rem [mem _ ips] (keys banned-ips*))))
          subs (with it (table)
                 (each ip ips
                   (= (it ip) (dedup (map !by (+ (bads ip) (goods ip))))))))
    (list subs
          (sort (compare > (memo [badness (subs _) (bads _) (goods _)]))
                ips))))

(def badness (subs bads goods)
  (* (/ (len bads)
        (max .9 (expt (len goods) 2))
        (expt (+ (days-since (aif (car bads) it!time 0))
                 1)
              2))
     (if (len> subs 1) 20 1)))


(edop flagged ()
  (display-selected-items [retrieve maxend* [len> _!flags 0]  _] "flagged"))

(def flagged (i)
  (and (~mem 'nokill i!keys)
       (or (mem 'flagged i!keys)
           (len> i!flags flag-kill-threshold*))))

(edop killed ()
  (display-selected-items [retrieve maxend* !dead _] "killed"))

(def display-selected-items (f whence)
  (display-items (f stories*) nil nil whence)
  (vspace 35)
  (color-stripe textgray)
  (vspace 35)
  (display-items (f comments*) nil nil whence))


; Rather useless thus; should add more data.

(adop badguys ()
  (tab (each u (sort (compare > [uval _ created])
                     (users [ignored _]))
         (row (userlink u nil)))))

(adop badlogins ()  (logins-page bad-logins*))

(adop goodlogins () (logins-page good-logins*))

(def logins-page (source)
  (sptab (each (time ip user) (firstn 100 (rev (qlist source)))
           (row time ip user))))


; Stats

(adop optimes ()
  (sptab
    (tr (tdr "total") (tdr "med") (tdr "avg") (tdr "req") (td "op"))
    (spacerow 10)
    (each name (sort < newsop-names*)
      (defs ts (qlist:optimes* name)
            ms (only&avg ts)
            n (opcounts* name))
      (tr (tdr:prt (and n (num (* n ms) 1)))
          (tdr:prt (only&med (map round ts)))
          (tdr:prt (only&num ms))
          (tdr:prt (and n (onlink n
                            (each s ts
                              (prn:num s 2 t t)
                              (br)))))
          (td (link (string "/" name)))))))

(newsop votes () (votes-page user))

(newscache votes-page user 90
  (longpage (now) nil "votes" "Votes" "votes"
    (sptab
      (tr (td "") (td "age") (tdr "id") (td "dir") (td "score") (td "") (tdr "voter") (td "") (tdr "author") (td "title"))
      (spacerow 10)
      (let n 0
        (each x (map [withs ((id (ts ip who dir score)) _ i (item id))
                       (unless (is who i!by)
                         (list (text-age (minutes-since ts))
                               id dir (+ 1 score) who i!by
                               (if (metastory i)
                                    "@i!title"
                                   (is i!type 'pollopt)
                                    "[@{i!text}]"
                                   "> @(ellipsize i!text)")))]
                     (sort (compare > car:cadr) (apply + (map tablist (vals votes*)))))
          (whenlet (age id dir score who by title) x
            (withs (i (item id) age (multisubst '(("hour" "hr") ("minute" "m") (" ago" "") (" " "")) age))
              (row "@(++ n). " age (pr:itemlink i id) dir score
                   (tdr (if (or (is-user who) (admin)) (userlink who) (pr "[hidden]")))
                   (tdr (userlink by) (pr ":"))
                   (tag (span) (itemlink i (multisubst '(("<p>" " ")) title))))
              (spacerow 10))))))))

(adop noobs ()
  (sptab
    (tr (td "") (td "age") (td "id") (td "email") (td "score") (td "votes"))
    (spacerow 10)
    (let i 0
      (each u (sort (compare > !created) (map profile (users)))
        (withs (rank "@(++ i). "
                age (text-age (user-age u!id))
                nvotes (len:votes u!id))
          (row rank age (userlink u!id)
               (unless (blank u!email) (pr:link u!email "mailto:@u!email"))
               u!karma nvotes))))))


(defop topcolors req
  (minipage "Custom Colors"
    (tab
      (each c (dedup (map downcase (trues [uval _ topcolor] (users))))
        (tr (td c) (tdcolor (hex>color c) (hspace 30)))))))


(or= chess-board* (trim (rem #\return "
rnbqkbnr
pppppppp




PPPPPPPP
RNBQKBNR
")))

(def chess-encode (x)
  (multisubst
    `(("K" "&#9812;")
      ("Q" "&#9813;")
      ("R" "&#9814;")
      ("B" "&#9815;")
      ("N" "&#9816;")
      ("P" "&#9817;")
      ("k" "&#9818;")
      ("q" "&#9819;")
      ("r" "&#9820;")
      ("b" "&#9821;")
      ("n" "&#9822;")
      ("p" "&#9823;")
      (" " "&nbsp;"))
    (string x)))

(def chess ((o board chess-board*))
  (accum a
    (each y (lines chess-board*)
      (a:accum a
        (map a:chess-encode (chars y))))))

(def chess-piece (text (o a) (o b) (o from) (o to))
  (let op (if (~blank from) (+ "from=" from "&to=") "from=")
    (tag (form method 'post action (+ "/chess?" op a "," b))
      (gentag input type 'submit value2 text style "width: 3em;"))))

(def chess-board ((o from) (o to) (o board chess-board*))
  (idtab "chess"
    (withs (i 0 j 0 from (or from "") to (or to ""))
      (each y (chess board)
        (= i 0)
        (++ j)
        (row (chess-piece y!0 (++ i) j from to)
             (chess-piece y!1 (++ i) j from to)
             (chess-piece y!2 (++ i) j from to)
             (chess-piece y!3 (++ i) j from to)
             (chess-piece y!4 (++ i) j from to)
             (chess-piece y!5 (++ i) j from to)
             (chess-piece y!6 (++ i) j from to)
             (chess-piece y!7 (++ i) j from to))))))

(def chess-page ((o from) (o to) (o board chess-board*))
  (longpage (now) nil "chess" "Chess" "chess"
    (center
      (chess-board from to board))))

(def chess-at (x y)
  (+ (* (- y 1) 9) (- x 1)))

(newsop chess (from to)
  (when (and (~blank from) (~blank to))
    (let ((a b) (x y)) (map [map int (tokens _ #\,)] (list from to))
      (swap (chess-board* (chess-at x y))
            (chess-board* (chess-at a b)))
      (wipe (lncache* "chess")))
    (wipe from)
    (wipe to))
  (if (blank from) (wipe to))
  (chess-page from to))

(defmemo gamma-gray (percent)
  (withs (x (expt (/ percent 1.0) (/ 1.0 2.2))
          n (min 255 (trunc (* x 256)))
          m (min 255 (trunc (* x 300))))
    (color n n m)))

(= place-default-colors*
   (obj #\0 (gamma-gray 0.05)
        #\1 (gamma-gray 0.10)
        #\2 (gamma-gray 0.20)
        #\3 (gamma-gray 0.30)
        #\4 (gamma-gray 0.40)
        #\5 (gamma-gray 0.50)
        #\6 (gamma-gray 0.60)
        #\7 (gamma-gray 0.70)
        #\8 (gamma-gray 0.80)
        #\9 (gamma-gray 0.90)
        #\# (color  27 136  20)
        #\$ (color  27 140 135)
        #\% (color 120 214 208)
        #\@ (color  27 136  20)
        #\B (color  40 173 146)
        #\E (color 255 253  55)
        #\K (color 189   8  28)
        #\M (color 191  18 181)
        #\N (color  96 154  51)
        #\P (color  31 143 240)
        #\Q (color 252 126  40)
        #\R (color 255 178  54)
        #\W (color 139  69  19)
        #\X (color 173 181 189)
        #\Y (color 233 236 239)
        #\Z site-color*
        #\b (color 238 100  92)
        #\c (color 108  80  57)
        #\d (color 253 230 213)
        #\e (color 241 229 214)
        #\f (color 173 152 149)
        #\g (color 189 183 172)
        #\h (color 156 139 110)
        #\i (color 137 100 100)
        #\j (color 205 150  88)
        #\k (color  24 173 241)
        #\l (color 220 198 170)
        #\m (color 209 194 201)
        #\n (color   0   0   0)
        #\o (color 232 224 200)
        #\p (color  54  54  54)
        #\q (color 112  52 142)
        #\r (color 245  54  92)
        #\s (color 230 173 153)
        #\t (color  38  72 121)
        #\u (color 213 240 169)
        #\~ (color 118 245  69)
        #\space white))

(disktable place-colors* (+ newsdir* "place-colors")
  place-default-colors*)

(def place-encode (x)
  (or (place-colors* x) black))

(def place-piece (text (o a) (o b) (o from) (o to) (o bgcol (color 0 255 0)))
  (withs (op (if (~blank from) (+ "from=" from "&to=") "from=")
          url (if (~blank from) "/placeop" "/place")
          whence (if (~blank from) (string "#" from) (string "#" a "," b)))
    (tag (form method 'post action (string url "?" op a "," b) onsubmit "return placeSubmit(this);")
      (gentag input type 'submit value2 text style "background-color: #@(hexrep bgcol);"))))

(= place-submit-url* "/submitlink?l=ask%20place&t=Ask%20TL:%20what%20should%20we%20draw%20next%3F"
   place-css* "
#place tr    { -webkit-appearance: none; border-radius: 0; display: flex !important; border-collapse: unset; border: 0px; outline: none; padding: 0px; margin: 0px; overflow-wrap: normal; }
#place td    { -webkit-appearance: none; border-radius: 0; display:    inline-block; border-collapse: unset; border: 0px; outline: none; padding: 0px; margin: 0px; }
#place form  { outline: none; margin-block-end: 0px; margin: 0px; padding: 0px; }
#place input { touch-action: manipulation; -webkit-appearance: none; border-radius: 0; outline: none; margin-block-end: 0px; margin: 0px; padding: 0px; height: 1.0em; width: 1.0em; border: 0px; text-shadow: #000 1px 0 10px; color: white; }
"
   place-info* "
If you want to coordinate, come into our @(underlink 'discord discord-url*), or @(underlink 'submit place-submit-url*) to @(underlink '/l/place).
&nbsp;
Click the tiles. The first click selects a color (the tile will be marked with an x).
To clear the selection, click the x again, or click here: @(underlink 'clear '/place)
")

(def place-board ((o from) (o to) (o board place-board*))
  (pr:tostring
    (tag (style) (pr place-css*))
    (tag (table id "place" style "table-layout: fixed; width: 100%; overflow: hidden;")
      (tag (tbody style "display: block; max-width: 100vw; overflow: scroll;")
        (each line (lines:trim place-info*)
          (row line))
        (spacerow 10)
        (withs (j -1 from (or from "") to (or to "")
                (((o a -1) (o b -1))) (map [map int (tokens _ #\,)] (list from)))
          (each y (lines board)
            (++ j)
            (tag tr
              (when (is j 0) (td "palette:"))
              (forlen i y
                (tag (td id (string i "," j))
                  (place-piece (if (and (is i a) (is j b)) "x" "") i j from to (place-encode (y i))))))
            (when (is j 0)
              (spacerow 4))
            )))))
  (flushout))

(def place-page ((o from) (o to) (o board place-board*))
  (longpage (now) nil "place" "place" "place"
    (center
      (place-board from to board))))

(def place-at (x y (o board place-board*))
  (let i -1
    (while (> y 0)
      (= i (pos #\newline board (+ i 1)))
      (-- y))
    (+ i x 1)))

(def place-blit (text x y (o board place-board*))
  (let j -1
    (each line (lines text)
      (++ j)
      (forlen i line
        (= (board (place-at (+ x i) (+ y j)))
           (line i))))))

(or= place-events* (queue)
     place-event-id* 0)

(= place-event-limit* 20
   place-event-lasts* 25
   place-event-sleep* 0.5)

(def place-event! (type evt)
  (let e (copy evt)
    (= e!time (str (now))
       e!type type)
    (atomic
      (= e!id (str (++ place-event-id*)))
      (enq-limit e place-events* place-event-limit*))
    e))

(def place-update (x y (o board place-board*))
  (place-event! "put"
    (obj path: (string x "," y)
         data: (obj style: (obj backgroundColor:
                                "#@(hexrep (place-encode (board (place-at x y board))))")))))

(def place-reset ()
  (= place-events* (queue)))

(def place-kill ((o reset))
  (atomic
    (when reset (place-reset))
    (place-event! "kill")))

(newsopr placeop (from to) (placeop from to))
(newsop placeset (from to) (placeop from to))

(def placeop (from to)
  (let ((a b) (x y)) (map [map int (tokens _ #\,)] (list from to))
    (when (safe (> y 0))
      (atomic
        (= (place-board* (place-at x y))
           (place-board* (place-at a b)))
        (place-update x y))
      (wipe (lncache* "place")))
    (if (is from to) "/place" (string "/place?from=" (if (is y 0) to from)))))

(defop place.txt req header: "text/plain"
  (pr place-board*)
  (flushout))

(defop place.json req header: "application/json" (pr:place-json))

(defcache place-json 5
  (tostring:write-json
    (obj board:
         (map [map str (chars _)]
              (lines place-board*))
         colors:
         (listtab
           (each (c col) place-colors*
             (out (sym:string c)
                  (obj r: col!r g: col!g b: col!b hex: "#@(hexrep col)")))))))

(defop place.events () header: (trim:tostring
                                 (prn "text/event-stream;")
                                 (prn "Access-Control-Allow-Origin: *")
                                 (prn "Cache-Control: no-cache;")
                                 (prn "X-Accel-Buffering: no"))
  (withs (seen (obj) ts (now))
    (while (< (since ts) place-event-lasts*)
      (each x (qlist place-events*)
        (unless (seen x!id)
          (= (seen x!id) t)
          (prn "event: " (or x!type "put"))
          (pr "data: ") (write-json x)
          (prn)
          (prn)
          (flushout)))
      (sleep place-event-sleep*))))

(newsop place (from to)
  (if (blank from) (wipe to))
  (place-page from to))

(def qb-board ((o from) (o to) (o board place-board*))
  (pr:tostring
    (tag (style) (pr place-css*))
    (tag (table id "qb" style "table-layout: fixed; width: 100%; overflow: hidden;")
      (tag (tbody style "display: block; max-width: 100vw; overflow: scroll;")
        (each line (lines:trim place-info*)
          (row line))
        (spacerow 10)
        (withs (j -1 from (or from "") to (or to "")
                (((o a -1) (o b -1))) (map [map int (tokens _ #\,)] (list from)))
          (each y (lines board)
            (++ j)
            (tag tr
              (when (is j 0) (td "palette:"))
              (forlen i y
                (tag (td id (string i "," j))
                  (place-piece (if (and (is i a) (is j b)) "x" "") i j from to (place-encode (y i))))))
            (when (is j 0)
              (spacerow 4))
            )))))
  (flushout))

(def qb-page ((o from) (o to) (o board place-board*))
  (longpage (now) nil "Queen's Blood" "Queen's Blood" "/l/qb"
    (center
      (qb-board from to board))))

(def lorem ()
  ; https://news.ycombinator.com/item?id=15609972
  (trim:shell "@(if (macos?) 'gshuf 'shuf) -n 32 /usr/share/dict/words | tr '\\n' ' '"))

(defcache terry 1 ()
  (awhen (lorem)
    (unless (blank it)
      (prn "\"God says... @it\""))))

(def prize-msg ()
  "You found an easter egg. Message @site-discord* on
  @(underlink 'discord discord-url*) or email @site-email* to
  claim a prize.")

(newsop test ()
  (msgpage (prize-msg)))

;; eventually generalize this to handle arbitrary batch jobs while
;; trapping and reporting errors properly. For now, this is ugly but
;; handy to have around for flushing all site data to disk and
;; to firebase.

(or= resave-items-finished* 0 resave-items-total*    0
     resave-items-thread* nil resave-items-errors* nil resave-items-failed* nil)
(= resave-items-throttle* 0.3)

(def resave-items-thread ((o throttle resave-items-throttle*))
  (= resave-items-done*   0   resave-items-total*  0
     resave-items-errors* nil resave-items-failed* nil)
  (each-loaded-item i
    (++ resave-items-total*))
  (each-loaded-item i
    (sleep throttle)
    (on-err (fn (c) (push (list c i) resave-items-errors*))
            (fn () (save-item i) (push i resave-items*)))
    (++ resave-items-done*)))

(def stop-resaving-items ()
  (when resave-items-thread*
    (kill-thread resave-items-thread*)
    (wipe resave-items-thread*)))

(def start-resaving-items ((o throttle resave-items-throttle*))
  (stop-resaving-items)
  (= resave-items-thread* (thread (resave-items-thread throttle))))

(= place-file* (+ newsdir* "place.txt"))

(diskfile place-board* place-file*
"lmostucdefghijEKQRBNPkqrbpXYZWn0123456789KQRBNMkqrbpXYZWn0123456789 KQRBNPkqrbpXYZWn0123456789KQRBNPkqrbpXYZWn0123456789
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
                 ZZZ        ZZZ 
                 WWW        WWW 
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 XXX        XXX  XXX        XXX
 YYY        YYY  YYY        YYY
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 XXX        XXX  XXX        XXX
 YYY        YYY  YYY        YYY
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 ZZZ        ZZZ  ZZZ        ZZZ
 WWW        WWW  WWW        WWW
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR
rnbqkbnrrnbqkbnrrnbqkbnrrnbqkbnr
pppppppppppppppppppppppppppppppp
99999999999999999999999999999999
 XXX        XXX  XXX        XXX
 YYY        YYY  YYY        YYY
99999999999999999999999999999999
PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
RNBQKBNRRNBQKBNRRNBQKBNRRNBQKBNR")


(defbg save-place 15 (save-place))

(def save-place ()
  (todisk place-board*)
  (todisk place-colors*))

(def load-place ()
  (let was (copy place-board*)
    (= place-board* (filechars place-file*))
    was))

run-news
