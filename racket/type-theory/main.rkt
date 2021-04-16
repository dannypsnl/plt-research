#lang typed/racket

(require "data.rkt")

(data expr
      [var (name : String)]
      [abs (name : String) (body : expr)]
      [app (fn : expr) (arg : expr)]
      [pi (name : String) (e : expr) (body : expr)]
      [type (level : Integer)]
      [nat]
      [zero]
      [succ (n : expr)]
      [rec (e1 : expr) (e2 : expr) (e3 : expr) (e4 : expr)]
      [id (e1 : expr) (e2 : expr) (e3 : expr)]
      [refl (e : expr)]
      [J (e1 : expr) (e2 : expr) (e3 : expr) (e4 : expr) (e5 : expr) (e6 : expr)])

(data value
      [vabs (lam : (-> value value))]
      [vpi (v : value) (lam : (-> value value))])

(data neutral
      [nvar (name : String)]
      [napp (fn : neutral) (arg : value)]
      [nrec (n : neutral) (v1 : value) (v2 : value) (v3 : value)]
      [nj (v1 : value) (v2 : value) (v3 : value) (v4 : value) (v5 : value) (n : neutral)])
