; Deterministic RNG based on JAX
; https://github.com/google/jax/blob/c2c3669c15df27d151acadd98ca00f0d4cbc5465/docs/jep/263-prng.md

(def hex (x)
  (number->string x 16))

(def bin (x)
  (number->string x 2))

(def floordiv (x y)
  (floor (/ x y)))

(def u32mask ()
  (- (lshift 1 32) 1))

(def lshift (i by)
  (arithmetic-shift i by))

(def rshift (i by)
  (arithmetic-shift i (- by)))

(def lshift32 (i by)
  (bitwise-and (lshift i by)
               (u32mask)))

(def rshift32 (i by)
  (bitwise-and (rshift i by)
               (u32mask)))

(def ensure-u64 (n)
  (let v (make-u64vector 1)
    (u64vector-set! v 0 n)
    (u64vector-ref v 0)))

(def truncate-u32 (x)
  (bitwise-and x (u32mask)))

(def shapeof (x)
  (or (errsafe:list:len x) (list)))

(def broadcast (x y)
  (withs (x (listify x)
          y (listify y)
          xs (shapeof x)
          ys (shapeof y))
    (assert (or (iso xs '(1))
                (iso ys '(1))
                (iso xs ys))
            "One of the operand shapes must be 1, or the operand shapes must be equal"
            'x x 'xs xs
            'y y 'ys ys)
    (if (iso xs '(1)) (list (n-of (ys 0) (x 0)) y)
        (iso ys '(1)) (list x (n-of (xs 0) (y 0)))
      (list x y))))

;(def vop (op)
;  (fn (x y) (apply map op (broadcast x y))))

;(def vadd (x y)
;  (apply map + (broadcast x y)))

(def vadd args
  (truncate-u32 (apply + (map truncate-u32 args))))

;def _make_rotate_left(dtype):
;  if not jnp.issubdtype(dtype, np.integer):
;    raise TypeError("_rotate_left only accepts integer dtypes.")
;  nbits = np.array(jnp.iinfo(dtype).bits, dtype)

;  def _rotate_left(x, d):
;    if lax.dtype(d) != dtype:
;      d = lax.convert_element_type(d, dtype)
;    if lax.dtype(x) != dtype:
;      x = lax.convert_element_type(x, dtype)
;    return lax.shift_left(x, d) | lax.shift_right_logical(x, nbits - d)
;  return _rotate_left

;rotate_left = _make_rotate_left(np.uint32)

(def rotate-left (x by)
  (assert (<= 0 by 32))
  (bitwise-ior (lshift32 x by)
               (rshift32 x (- 32 by))))

(def fori-loop (lo hi body state)
  (for i lo (- hi 1)
    (= state (body i state)))
  state)

;def threefry_seed(seed: int) -> jnp.ndarray:
;  """Create a single raw threefry PRNG key given an integer seed.

;  Args:
;    seed: a 64- or 32-bit integer used as the value of the key.

;  Returns:
;    The PRNG key contents, modeled as an array of shape (2,) and dtype
;    uint32. The key is constructed from a 64-bit seed by effectively
;    bit-casting to a pair of uint32 values (or from a 32-bit seed by
;    first padding out with zeros).
;  """
;  # Avoid overflowerror in X32 mode by first converting ints to int64.
;  # This breaks JIT invariance for large ints, but supports the common
;  # use-case of instantiating with Python hashes in X32 mode.
;  if isinstance(seed, int):
;    seed_arr = jnp.asarray(np.int64(seed))
;  else:
;    seed_arr = jnp.asarray(seed)
;  if seed_arr.shape:
;    raise TypeError(f"PRNG key seed must be a scalar; got {seed!r}.")
;  if not np.issubdtype(seed_arr.dtype, np.integer):
;    raise TypeError(f"PRNG key seed must be an integer; got {seed!r}")

;  convert = lambda k: lax.reshape(lax.convert_element_type(k, np.uint32), [1])
;  k1 = convert(lax.shift_right_logical(seed_arr, lax._const(seed_arr, 32)))
;  k2 = convert(jnp.bitwise_and(seed_arr, np.uint32(0xFFFFFFFF)))
;  return lax.concatenate([k1, k2], 0)

(def randu8 () (randu 8))
(def randu16 () (randu 16))
(def randu32 () (randu 32))
(def randu64 () (randu 64))

(def randseed ()
  (randu64))

(def threefry-seed ((o seed))
  (withs (v (ensure-u64 (or seed (randseed)))
          k1 (rshift v 32)
          k2 (bitwise-and v (- (lshift 1 32) 1)))
    (list k1 k2)))

;def apply_round(v, rot):
;  v = v[:]
;  v[0] = v[0] + v[1]
;  v[1] = rotate_left(v[1], rot)
;  v[1] = v[0] ^ v[1]
;  return v

(def apply-round ((v0 v1) rot)
  (withs (v0 (vadd v0 v1)
          v1 (rotate-left v1 rot)
          v1 (bitwise-xor v0 v1))
    (list v0 v1)))

;def rotate_list(xs):
;  return xs[1:] + xs[:1]

(def rotate-list (xs)
  (+ (cut xs 1) (cut xs 0 1)))

;def rolled_loop_step(i, state):
;  x, ks, rotations = state
;  for r in rotations[0]:
;    x = apply_round(x, r)
;  new_x = [x[0] + ks[0], x[1] + ks[1] + jnp.asarray(i + 1, dtype=np.uint32)]
;  return new_x, rotate_list(ks), rotate_list(rotations)

(def rolled-loop-step (i (x ks rotations))
  ;(prs i x ks rotations "\n")
  (each r (rotations 0)
    (= x (apply-round x r)))
  (let new-x (list (vadd (x 0) (ks 0))
                   (vadd (x 1) (ks 1) (+ i 1)))
    (list new-x (rotate-list ks) (rotate-list rotations))))

;def _threefry2x32_lowering(key1, key2, x1, x2, use_rolled_loops=True):
;  """Apply the Threefry 2x32 hash.

;  Args:
;    keypair: a pair of 32bit unsigned integers used for the key.
;    count: an array of dtype uint32 used for the counts.

;  Returns:
;    An array of dtype uint32 with the same shape as `count`.
;  """
;  x = [x1, x2]

;  rotations = [np.array([13, 15, 26, 6], dtype=np.uint32),
;               np.array([17, 29, 16, 24], dtype=np.uint32)]
;  ks = [key1, key2, key1 ^ key2 ^ np.uint32(0x1BD11BDA)]

;  x[0] = x[0] + ks[0]
;  x[1] = x[1] + ks[1]

;  if use_rolled_loops:
;    x, _, _ = lax.fori_loop(0, 5, rolled_loop_step, (x, rotate_list(ks), rotations))

;  else:
;    for r in rotations[0]:
;      x = apply_round(x, r)
;    x[0] = x[0] + ks[1]
;    x[1] = x[1] + ks[2] + np.uint32(1)

;    for r in rotations[1]:
;      x = apply_round(x, r)
;    x[0] = x[0] + ks[2]
;    x[1] = x[1] + ks[0] + np.uint32(2)

;    for r in rotations[0]:
;      x = apply_round(x, r)
;    x[0] = x[0] + ks[0]
;    x[1] = x[1] + ks[1] + np.uint32(3)

;    for r in rotations[1]:
;      x = apply_round(x, r)
;    x[0] = x[0] + ks[1]
;    x[1] = x[1] + ks[2] + np.uint32(4)

;    for r in rotations[0]:
;      x = apply_round(x, r)
;    x[0] = x[0] + ks[2]
;    x[1] = x[1] + ks[0] + np.uint32(5)

;  return tuple(x)

(def threefry2x32-lowering (key1 key2 x0 x1)
  (withs (rotations (list '(13 15 26 6)
                          '(17 29 16 24))
          (ks0 ks1 ks2) (list key1 key2 (bitwise-xor key1 key2 #x1BD11BDA))
          x0 (vadd x0 ks0)
          x1 (vadd x1 ks1)
          x (list x0 x1)
          ks (list ks0 ks1 ks2)
          (x _ _) (fori-loop 0 5 rolled-loop-step (list x (rotate-list ks) rotations)))
    x))


;@partial(jit, inline=True)
;def threefry_2x32(keypair, count):
;  """Apply the Threefry 2x32 hash.

;  Args:
;    keypair: a pair of 32bit unsigned integers used for the key.
;    count: an array of dtype uint32 used for the counts.

;  Returns:
;    An array of dtype uint32 with the same shape as `count`.
;  """
;  key1, key2 = keypair
;  if not lax.dtype(key1) == lax.dtype(key2) == lax.dtype(count) == np.uint32:
;    msg = "threefry_2x32 requires uint32 arguments, got {}"
;    raise TypeError(msg.format([lax.dtype(x) for x in [key1, key2, count]]))

;  try:
;    odd_size = count.size % 2
;  except core.InconclusiveDimensionOperation as e:
;    msg = ("jax.random functions have limited support for shape polymorphism. "
;           "In particular, the product of the known dimensions must be even.")
;    raise core.InconclusiveDimensionOperation(msg) from e

;  if odd_size:
;    x = list(jnp.split(jnp.concatenate([count.ravel(), np.uint32([0])]), 2))
;  else:
;    x = list(jnp.split(count.ravel(), 2))

;  x = threefry2x32_p.bind(key1, key2, x[0], x[1])
;  out = jnp.concatenate(x)
;  assert out.dtype == np.uint32
;  return lax.reshape(out[:-1] if odd_size else out, count.shape)

(def threefry-2x32-1 (keypair count)
  (withs ((key1 key2) keypair
          x (listify count)
          odd? (odd:len x)
          x (if odd? `(,@x 0) x))
    (assert (in (len x) 1 2))
    (let it (threefry2x32-lowering key1 key2 (x 0) (x 1))
      (if odd? (list (it 0)) it))))

(def midpoint (l)
  (floordiv (len l) 2))

(def cleave (l (o i (midpoint l)))
  (list (cut l 0 i)
        (cut l i)))

(def threefry-2x32-v (keypair counts)
  (assert (even (len (listify counts))))
  (let it (map [threefry-2x32-1 keypair _]
               (apply zip (cleave counts)))
    (hug:append (map car it)
                (map cadr it))))

(def threefry-2x32 (keypair counts)
  (let x (listify counts)
    (assert (> (len x) 0))
    (if (in (len x) 1 2)
        (threefry-2x32-1 keypair x)
        (threefry-2x32-v keypair x))))

(def threefry-split (keypair num)
  (let counts (range 0 (* num 2))
    (threefry-2x32 keypair counts)))

(def threefry-fold-in (key data)
  (threefry-2x32 key (threefry-seed data)))

(def threefry-reduce (key data)
  (rreduce (fn (x k) (threefry-fold-in k x))
           (append data (list key))))

(def list->rng (l)
  (if (acons (car l))
      (list->rng (mappend idfn l))
      (vector->pseudo-random-generator (list->vector l))))

(def rng->list (r)
  (vector->list (pseudo-random-generator->vector r)))

(def rng->seed (r)
  (threefry-reduce (threefry-seed 0) (rng->list r)))

(def randu ((o bits 32))
  (rreduce (fn (x (o y 0)) (+ (lshift y 8) x))
           (n-of (floordiv bits 8)
                 (randb))))

(= current-rng current-pseudo-random-generator)

(mac w/rng (r . body)
  `(w/param current-rng ,r
     ,@body))

(def make-rng ((o seed))
  (if (no seed)
      (make-rng (threefry-seed))
      (is (type seed) 'int)
      (make-rng (threefry-seed seed))
      (is (type seed) 'pseudo-random-generator)
      seed
      (aand (threefry-split seed 3)
            (list->rng it))))

(def split-rng (r count)
  (aand (threefry-split (rng->seed r) count)
        (map make-rng it)))

(def rng-uniform ((o key) (o count) (o lo 0.0) (o hi 1.0) (o f idfn))
  (let rng (make-rng key)
    (aand (n-of (or count 1)
                (f (+ lo (* (random rng) (- hi lo)))))
          (if count it (car it)))))

(def rng-bernoulli (key (o p 0.5) (o count))
  (rng-uniform key count
               0.0 1.0
               [if (< _ p) 1 0]))

(def rng-normal (key (o count))
  (rng-uniform key count
               (nextafter -1.0 0.0) 1.0
               [* (sqrt 2) (erf-inv _)]))

(= nextafter  #'(get-ffi-obj 'nextafter  #f (_fun _double _double -> _double))
   nextafterf #'(get-ffi-obj 'nextafterf #f (_fun _float _float -> _float)))

;def polevl(x, coefs, N):
;    ans = 0
;    power = len(coefs) - 1
;    for coef in coefs:
;        ans += coef * x**power
;        power -= 1
;    return ans

(def polevl (x coefs N)
  (withs (ans 0
          power (- (len coefs) 1))
    (each coef coefs
      (++ ans (* coef (expt x power)))
      (-- power))
    ans))
  
;def p1evl(x, coefs, N):
;    return polevl(x, [1] + coefs, N)

(def p1evl(x coefs N)
  (polevl x (+ (list 1) coefs) N))


;# From scipy special/cephes/ndrti.c

(with
  ; approximation for 0 <= abs(z - 0.5) <= 3/8
  (P0 '(-5.99633501014107895267E1
        9.80010754185999661536E1
        -5.66762857469070293439E1
        1.39312609387279679503E1
        -1.23916583867381258016E0
        )
   Q0 '(1.95448858338141759834E0
        4.67627912898881538453E0
        8.63602421390890590575E1
        -2.25462687854119370527E2
        2.00260212380060660359E2
        -8.20372256168333339912E1
        1.59056225126211695515E1
        -1.18331621121330003142E0
        )

   ; Approximation for interval z = sqrt(-2 log y ) between 2 and 8
   ; i.e., y between exp(-2) = .135 and exp(-32) = 1.27e-14.
   P1 '(4.05544892305962419923E0
        3.15251094599893866154E1
        5.71628192246421288162E1
        4.40805073893200834700E1
        1.46849561928858024014E1
        2.18663306850790267539E0
        -1.40256079171354495875E-1
        -3.50424626827848203418E-2
        -8.57456785154685413611E-4
        )
   Q1 '(1.57799883256466749731E1
        4.53907635128879210584E1
        4.13172038254672030440E1
        1.50425385692907503408E1
        2.50464946208309415979E0
        -1.42182922854787788574E-1
        -3.80806407691578277194E-2
        -9.33259480895457427372E-4
        )

   ; Approximation for interval z = sqrt(-2 log y ) between 8 and 64
   ; i.e., y between exp(-32) = 1.27e-14 and exp(-2048) = 3.67e-890.
   P2 '(3.23774891776946035970E0
        6.91522889068984211695E0
        3.93881025292474443415E0
        1.33303460815807542389E0
        2.01485389549179081538E-1
        1.23716634817820021358E-2
        3.01581553508235416007E-4
        2.65806974686737550832E-6
        6.23974539184983293730E-9
        )
   Q2 '(6.02427039364742014255E0
        3.67983563856160859403E0
        1.37702099489081330271E0
        2.16236993594496635890E-1
        1.34204006088543189037E-2
        3.28014464682127739104E-4
        2.89247864745380683936E-6
        6.79019408009981274425E-9
        ))
  (def ndtri (y)
    ;s2pi = 2.50662827463100050242
    ;code = 1
    (withs (s2pi 2.50662827463100050242
            code 1)

      ;if y > (1.0 - 0.13533528323661269189):      # 0.135... = exp(-2)
      ;    y = 1.0 - y
      ;    code = 0
      (when (> y (- 1.0 0.13533528323661269189)) ; 0.135... = exp(-2)
        (= y (- 1.0 y)
           code 0))

      ;if y > 0.13533528323661269189:
      ;    y = y - 0.5
      ;    y2 = y * y
      ;    x = y + y * (y2 * polevl(y2, P0, 4) / p1evl(y2, Q0, 8))
      ;    x = x * s2pi
      ;    return x
      (if (> y 0.13533528323661269189)
          (withs (y (- y 0.5)
                  y2 (* y y)
                  x (+ y (* y (/ (* y2 (polevl y2 P0 4)) (p1evl y2 Q0 8))))
                  x (* x s2pi))
            x)

          ;x = math.sqrt(-2.0 * math.log(y))
          ;x0 = x - math.log(x) / x
          (withs (x (sqrt (* -2.0 (log y)))
                  x0 (- x (/ (log x) x))

                  ;z = 1.0 / x
                  ;if x < 8.0:                 # y > exp(-32) = 1.2664165549e-14
                  ;    x1 = z * polevl(z, P1, 8) / p1evl(z, Q1, 8)
                  ;else:
                  ;    x1 = z * polevl(z, P2, 8) / p1evl(z, Q2, 8)
                  z (/ 1.0 x)
                  x1 (if (< x 8.0) ; y > exp(-32) = 1.2664165549e-14
                         (/ (* z (polevl z P1 8)) (p1evl z Q1 8))
                         (/ (* z (polevl z P2 8)) (p1evl z Q2 8)))
                  ;x = x0 - x1
                  ;if code != 0:
                  ;    x = -x
                  x (- x0 x1)
                  x (if (isnt code 0) (- x) x))
            ;return x
            x)))))

;def inv_erf(z):
;    if z < -1 or z > 1:
;        raise ValueError("`z` must be between -1 and 1 inclusive")
;    if z == 0:
;        return 0
;    if z == 1:
;        return math.inf
;    if z == -1:
;        return -math.inf
;    return ndtri((z + 1) / 2.0) / math.sqrt(2)
(def erf-inv (z)
  (assert (<= -1 z 1) "`z` must be between -1 and 1 inclusive" z)
  (if (is z 0) 0
      (is z 1) +inf.0
      (is z 1) -inf.0
    (/ (ndtri (/ (+ z 1) 2.0)) (sqrt 2))))


;arc> (let seed 0 (map [n-of 10 (random 128 _)] (split-rng (make-rng seed) 8)))
;'((64 31 57 60 97 3 66 92 5 34)
;  (111 35 49 41 25 94 117 63 34 25)
;  (35 92 56 27 32 27 115 93 107 33)
;  (72 45 122 9 43 63 67 67 68 124)
;  (113 106 111 78 89 113 101 62 5 100)
;  (105 42 11 7 118 93 73 107 4 24)
;  (56 59 106 103 60 96 79 56 49 122)
;  (69 99 76 10 100 78 19 50 39 95))

;arc> (savefile (tostring:write-json (rng-normal (make-rng) 512)) "normal.json")
