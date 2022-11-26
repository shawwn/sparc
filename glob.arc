(seval '(require file/glob))

(or= glob% glob)

(def glob (pat (o root (cwd)))
  (each path (glob% pat #:capture-dotfiles? #t)
    (aand (seval!path->string path)
          (if (dir-exists it) (+ it "/") it)
          (if (seval!string-prefix? it root)
              (cut it (len root))
              it)
          (out it))))

