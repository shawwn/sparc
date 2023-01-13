; Prompt: Web-based programming application.  4 Aug 06.

(= appdir* (libpath "arc/apps/"))

(defop prompt req (admin-gate "/prompt" prompt-page))

(def prompt-page msg
  (ensure-dir appdir*)
  (ensure-dir (string appdir* (get-user)))
  (whitepage
    (prbold (link "Prompt" "/prompt"))
    (hspace 20)
    (link "repl" "/repl")
    (hspace 20)
    (pr (get-user) " | ")
    (link "logout" "/logout")
    (when msg (hspace 10) (apply pr msg))
    (br2)
    (sptab
      (each app (dir (+ appdir* (get-user)))
        (row (app-link app)
          (ulink 'run    (run-app  app))
          (ulink 'edit   (edit-app app))
          (hspace 40)
          (ulink 'delete
            (whitepage
              (tab (row "Delete @{app}?")
                   (row (w/bars
                          (ulink 'yes (rem-app app))
                          (link  'no  "/prompt")))))))))
    (br2)
    (urform (get-user) req
            (aif (+ (goodname arg!app) ".arc")
                 (flink [edit-app it])
                 (flink [prompt-page "Bad name."]))
      (tab (row "name:" (input "app") (submit "create app"))))))

(def app-link (app)
  (if (has-vim)
      (ulink app redir: "/prompt" (vim-app app))
      (pr app)))

(def app-path (app)
  (and (get-user) app (+ appdir* (get-user) "/" app)))

(def app-exists (app)
  (aand (app-path app)
        (file-exists it)))

(def read-app (app)
  (aand (app-exists app)
        (readfile it)))

(defmemo has-vim ()
  (shellsafe 'which 'mvim))

(def vim-app (app)
  (aand (app-exists app)
        (shell 'mvim it :async)))

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

(def edit-app (app)
  (whitepage
    (pr "user: " (get-user) " app: " app)
    (br2)
    (urform (get-user) req
            (do (when (is arg!cmd "save")
                  (write-app app (readall arg!exprs)))
                "/prompt")
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
    (if (or exprs (app-exists app))
        (on-err (fn (c) (pr "Error: " (details c)))
                (fn () (map eval exprs)))
        (prompt-page "Error: No application " app " for user " (get-user)))))

(or= repl-history* nil)

(= repl-history-max* 10000)

(defop repl req (admin-gate "/repl" replpage))

(def replpage ()
  (whitepage
    (link "apps" "/prompt")
    (br2)
    (repl (readall (or arg!expr "")) "/repl")))

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

