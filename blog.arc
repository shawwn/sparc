#!/usr/bin/env arc
; Blog tool example.  20 Jan 08, rev 21 May 09.

; To run:
; $ ./blog.arc
; or run from a repl:
; arc> (load "blog.arc")
; arc> (bsv)
; go to http://localhost:8080
; $ echo test > arc/admins
; click "new post", then create a user named test

(require "app.arc")

(= postdir*   (libpath "arc/posts/")
   blogtitle* "A Blog")

(or= postid* 0 posts* (table))

(deftem post  id nil  title nil  text nil)

(def load-posts ()
  (each id (map int (dir postdir*))
    (= postid*      (max postid* id)
       (posts* id) (temload 'post (string postdir* id)))))

(def save-post (p) (save-table p (string postdir* p!id)))

(def post (id) (posts* (errsafe:int id)))

(mac blogpage body
  `(whitepage 
     (center
       (widtable 600 
         (tag b (link blogtitle* "/blog"))
         (br 3)
         ,@body
         (br 3)
         (w/bars (link "archive" "/archive")
                 (link "new post" "/newpost"))))))

(defop viewpost req (blogop post-page arg!id))

(def blogop (f id)
  (aif (post id)
       (f it)
       (is id t)
       (f)
     (blogpage (pr "No such post."))))

(def blogopa (f id whence) (admin-gate whence blogop f id))

(def post-url (id) (string "/viewpost?id=" id))

(def edit-post-url (id) (string "/editpost?id=" id))

(def post-page (p) (blogpage (display-post p)))

(def display-post (p)
  (tag b (link p!title (post-url p!id)))
  (when (get-user)
    (sp)
    (link "[edit]" (edit-post-url p!id)))
  (br2)
  (pr p!text))

(defop newpost req (blogopa new-post-page t "/newpost"))

(def new-post-page ()
  (blogpage
    (urform (get-user) req
      (let p (addpost arg!t (md-from-form arg!b))
              (post-url p!id))
      (tab (row "title" (input "t" "" 60))
           (row "text"  (textarea "b" 10 80))
           (row ""      (submit))))))

(def addpost (title text)
  (let p (inst 'post id: (++ postid*) :title :text)
    (save-post p)
    (= (posts* p!id) p)))

(defop editpost req (blogopa edit-post-page arg!id (edit-post-url arg!id)))

(def edit-post-page (p)
  (blogpage
    (display-post p)
    (br2)
    (vars-form `((string title ,p!title t t)
                 (mdtext text  ,p!text  t t))
               (fn (name val) (= (p name) val))
               (fn () (save-post p)
                      (edit-post-url p!id)))))

(defop archive req
  (blogpage
    (tag ul
      (down i postid* 1
        (whenlet p (post i)
          (tag li (link p!title (post-url p!id))
            (when (get-user)
              (sp)
              (link "[edit]" (edit-post-url p!id)))))))))

(def blogmain ()
  (blogpage
    (for i 0 4
      (awhen (posts* (- postid* i)) 
        (display-post it)
        (br 3)))))

(defop blog req (blogmain))
(defop ||   req (blogmain))

(def bsv ((o port 8080))
  (ensure-dir postdir*)
  (load-posts)
  (asv port))

bsv
