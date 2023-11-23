#'(require file/glob)

(or= glob% #'glob)

(def glob (pat (o root))
  (or= root (cwd))
  (zap expandpath root)
  (each path (glob% pat #:capture-dotfiles? #t)
    (aand (#'path->string path)
          (if (dir-exists it) (+ it "/") it)
          (if (#'string-prefix? it root)
              (cut it (len root))
              it)
          (out it))))

