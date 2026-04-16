# Handoff: Fix :kws rest-kwargs parameter with explicit keyword params

**Date:** 2026-04-16
**Branch:** master (uncommitted changes)

## What was accomplished

Fixed a bug where a function declaring both explicit keyword parameters (`:foo`, `:bar`) and a rest-kwargs collector (`:kws`) would incorrectly route all incoming keyword arguments into `:kws`, leaving the explicit params at their defaults.

**Files changed:**

- `ar.scm` — Rewrote `ar-kwproc` to correctly partition incoming keyword arguments between explicitly declared params and the `:kws` collector.

- `test.arc` — Added `define-test kws-with-keywords` with 7 test cases covering the fixed behaviour.

## The bug

Given:
```arc
(def f (:foo :bar :kws . args)
  (list foo bar kws))

(f foo: 1 bar: 2 baz: 3)
; expected: (1 2 (baz: 3))
; actual:   (nil nil (foo: 1 bar: 2 baz: 3))
```

The Arc compiler compiles `(:foo :bar :kws . args)` into a Racket lambda:
```scheme
(lambda (#:foo (foo nil) #:bar (bar nil) #:kws (kws nil) . args) ...)
```

Because `#:kws` is present, `ac-simple-fn` wraps it with `ar-kwproc`. The old `ar-kwproc` unconditionally routed **all** incoming keywords into `#:kws`, calling `(apply f args #:kws kwargs)` — so `foo` and `bar` always got `nil`.

## The fix

New `ar-kwproc` in `ar.scm` (lines 512–537):

1. At wrap time, call `(procedure-keywords f)` to discover which keyword params `f` declares directly (everything in `all-kws` except `#:kws` itself). These are stored as `f-direct-kws`.

2. At call time, loop over incoming `ks`/`vs` and partition them:
   - If `k` is in `f-direct-kws` → route directly to `f` as its own keyword arg.
   - Otherwise → collect into the extras plist for `#:kws`.

3. Build the final keyword alist (`direct kwargs` + `#:kws → extras`), sort it with `keyword<?` (required by `keyword-apply`), then call `(keyword-apply f sorted-ks sorted-vs positional-args)`.

`ar-sort-kvs` is still used for the extras plist so call-order is preserved via `ar-kworder`.

## Key decisions

**`procedure-keywords` at wrap time, not call time:** The inner lambda's keyword arity is fixed at compile time, so reading it once during `ar-kwproc` is correct and free.

**`f-direct-kws = #f` fallback:** If `procedure-keywords` returns `all-kws = #f` (accepts any keyword — shouldn't happen for Arc-generated fns), `f-direct-kws` is `#f` and the `(and f-direct-kws ...)` guard treats all incoming kwargs as extras. Safe no-op.

**`:kws`-only functions unaffected:** When `f` has only `#:kws` and no other keyword params, `f-direct-kws = ()` (empty list after `remove`). Every incoming kwarg falls through to extras, replicating the old behaviour exactly.

**Plain-procedure path unchanged:** `make-keyword-procedure`'s second argument is still `f` itself — when no keyword args are passed at the call site, Racket calls `f` directly without going through the wrapper.

## Test coverage added (`test.arc`)

```arc
(define-test kws-with-keywords
  (def f (:x :y :kws . args)
    (list x y kws args))
  ; known kwargs routed, extras collected, positional args passed through
  (test? '(1 2 (z: 3) (4 5))       (f x: 1 y: 2 z: 3 4 5))
  ; no extras → kws is nil
  (test? '(1 2 nil nil)             (f x: 1 y: 2))
  ; only unrecognized kwargs → declared params get defaults
  (test? '(nil nil (z: 3 w: 4) nil) (f z: 3 w: 4))
  ; partial match: one declared, one extra
  (test? '(1 nil (z: 3) nil)        (f x: 1 z: 3))
  ; no args at all → all defaults
  (test? '(nil nil nil nil)          (f))
  ; apply goes through the same fix path
  (test? '(1 2 (z: 3) nil)          (apply f '(x: 1 y: 2 z: 3)))
  ; extras preserve call order
  (test? '(nil nil (c: 3 a: 1 b: 2) nil) (f c: 3 a: 1 b: 2)))
```

## Context for future sessions

- `ar-kwproc` is defined in `ar.scm` around line 512. It is called at macro-expansion / compile time by `ac-simple-fn` in `ac.scm` whenever a function's compiled arglist contains `#:kws`.
- `ac-kwargs?` (ac.scm ~line 523) is the predicate that decides whether to emit `(ar-kwproc ...)` — it walks the arglist looking for the literal Racket keyword `#:kws`.
- The `:kws` feature only works for functions going through the **simple** fn path (`ac-simple-fn`). Complex-arg functions (`ac-complex-fn`) handle `:kws` differently via `ac-complex-args` and `ar-without`.
- Changes are on `master`, not yet committed.
- Run `arc test.arc` to execute the full test suite.
