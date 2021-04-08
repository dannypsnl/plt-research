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
