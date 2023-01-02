; Prompt: Web-based programming application.  4 Aug 06.

(= appdir* (libpath "arc/apps/"))

(defop prompt req 
  (if (admin)
      (prompt-page)
      (pr "Sorry.")))

(def prompt-page ((o :user (get-user)) . msg)
  (ensure-dir appdir*)
  (ensure-dir (string appdir* user))
  (whitepage
    (prbold "Prompt")
    (hspace 20)
    (pr user " | ")
    (link "logout")
    (when msg (hspace 10) (apply pr msg))
    (br2)
    (tag (table border 0 cellspacing 10)
      (each app (dir (+ appdir* user))
        (tr (td app)
            (td (ulink 'edit   (edit-app app)))
            (td (ulink 'run    (run-app  app)))
            (td (hspace 40)
                (ulink 'delete (rem-app  app))))))
    (br2)
    (aform (fn (req)
             (when-umatch user req
               (aif (goodname arg!app)
                    (edit-app it)
                    (prompt-page "Bad name."))))
       (tab (row "name:" (input "app") (submit "create app"))))))

(def app-path (app)
  (let user (get-user)
    (and user app (+ appdir* user "/" app))))

(def read-app (app)
  (aand (app-path app)
        (file-exists it)
        (readfile it)))

(def write-app (app exprs)
  (awhen (app-path app)
    (w/outfile o it 
      (each e exprs (write e o)))))

(def rem-app (app)
  (let file (app-path app)
    (if (file-exists file)
        (do (rmfile (app-path app))
            (prompt-page "Program " app " deleted."))
        (prompt-page "No such app."))))

(def edit-app (app (o :user (get-user)))
  (whitepage
    (pr "user: " user " app: " app)
    (br2)
    (aform (fn (req)
             (if (is (get-user) user)
                 (do (when (is arg!cmd "save")
                       (write-app app (readall arg!exprs)))
                     (prompt-page))
                 (login-page 'both nil
                             (fn (u ip) (prompt-page)))))
      (textarea "exprs" 10 82
        (pprcode (read-app app)))
      (br2)
      (buts 'cmd "save" "cancel"))))

(def pprcode (exprs) 
  (each e exprs
    (ppr e) 
    (pr "\n\n")))

(def view-app (app)
  (whitepage
    (pr "user: " (get-user) " app: " app)
    (br2)
    (tag xmp (pprcode (read-app app)))))

(def run-app (app)
  (let exprs (read-app app)
    (if exprs 
        (on-err (fn (c) (pr "Error: " (details c)))
          (fn () (map eval exprs)))
        (prompt-page "Error: No application " app " for user " (get-user)))))

(or= repl-history* nil)

(= repl-history-max* 10000)

(defop repl req
  (if (admin)
      (replpage)
      (pr "Sorry.")))

(def replpage ()
  (whitepage
    (link "apps" "prompt")
    (br2)
    (repl (readall (or arg!expr "")) "repl")))

(def repl (exprs url)
    (each expr exprs 
      (on-err (fn (c) (push (list expr c t) repl-history*))
              (fn () 
                (= that (eval expr) thatexpr expr)
                (push (list expr that) repl-history*))))
    (form url
      (textarea "expr" 8 60
        (write:caar repl-history*))
      (sp) 
      (submit))
    (tag xmp
      (each (expr val err) (firstn repl-history-max* repl-history*)
        (pr "> ")
        (ppr expr)
        (prn)
        (prn (if err "Error: " "")
             (ellipsize (tostring (write val)) 800)))))

