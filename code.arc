; Code analysis. Spun off 21 Dec 07.

; Ought to do more of this in Arc.  One of the biggest advantages
; of Lisp is messing with code.

(def incode (file)
  (if (isa!string file) (infile file) file))

(def codelines (file)
  (zap incode file)
  (summing test
    (whilet line (readline file)
      (test (aand (find nonwhite line) (isnt it #\;))))))

(def codeflat (file)
  (len (flat (readall (incode file)))))

(def codetree (file)
  (treewise + (fn (x) 1) (readall (incode file))))

(def code-density (file)
  (/ (codetree file) (codelines file))) 

(def tokcount (files)
  (with counts (table)
    (each f files
      (each token (flat (readall (incode f)))
        (++ (counts token 0))))))

(def common-tokens (files)
  (let counts (tokcount files)
    (with ranking nil
      (each (k v) counts
        (unless (nonop k)
          (insort (compare > cadr) (list k v) ranking))))))

(def nonop (x)
  (in x 'quote 'unquote 'quasiquote 'unquote-splicing))

(def common-operators (files)
  (keep [and (isa (car _) 'sym) (bound (car _))] (common-tokens files)))

(def top40 (xs)
  (map prn (firstn 40 xs))
  t)

(def space-eaters (files)
  (let counts (tokcount files)
    (with ranking nil
      (each (k v) counts
        (when (and (isa k 'sym) (bound k))
          (insort (compare > [* (len (string (car _)))
                                (cadr _)])
                  (list k v (* (len (string k)) v))
                  ranking))))))

;(top40 (space-eaters allfiles*))

(mac flatlen args `(len (flat ',args)))
