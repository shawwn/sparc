#!/usr/bin/env arc
; Prompt: Web-based programming application.  4 Aug 06.

(require "app.arc")
(require "pprint.arc")

(= appdir* (libpath "arc/apps/"))

(defop prompt req (admin-gate "/prompt" prompt-page))
(defop ||     req (admin-gate "/prompt" prompt-page))

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
          (when (has-vim)
            (ulink 'vim  (vim-app app) redir: "/prompt"))
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
  (ulink app (view-app app)))

(def app-path (app (o u (get-user)))
  (and u app (+ appdir* u "/" app)))

(def app-exists (app (o u (get-user)))
  (aand (app-path app u)
        (file-exists it)))

(def read-app (app)
  (aand (app-exists app)
        (filechars it)))

(defmemo has-vim ()
  (shellsafe 'which 'mvim))

(def vim-app (app)
  (aand (app-exists app)
        (shell 'mvim it :async)))

(def write-app (app exprs)
  (awhen (app-path app)
    (w/outfile o it 
      (disp exprs o))))

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
                  (write-app app arg!exprs))
                "/prompt")
      (textarea "exprs" 10 82
        (only&pr (read-app app)))
      (br2)
      (buts 'cmd "save" "cancel"))))

(def view-app (app)
  (whitepage
    (pr "user: " (get-user) " app: " app)
    (br2)
    (tag pre (only&pr (read-app app)))))

(def run-app (app)
  (aif (app-exists app)
       (on-err (fn (c) (pr "Error: " (details c)))
               (fn () (map eval (codefile it))))
       (prompt-page "Error: No application " app " for user " (get-user))))

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
    (tag pre
      (each (expr val err) (firstn repl-history-max* repl-history*)
        (pr "> ")
        (ppr expr)
        (prn)
        (prn (if err "Error: " "")
             (ellipsize (tostring (write val)) 800)))))

asv
