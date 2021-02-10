;; We have to handle different cases:
;;
;; 1) Scheme objects that are passed to python functions
;; 2) Python objects that are passed to scheme functions
;; 3) Bidirectional objects that support mutation on either side

(import (github.com/feeley pyffi))

\import math
(define pi \math.pi)
(define (py-sqrt n) \math.sqrt(\n))
(define res (py-sqrt 9))
(for-each display `("Square root of 9: " ,res "\n"))

(define (circle-area r)
  (* pi r r))
(for-each display `("Area of circle of radius pi: "
                    ,(circle-area pi) "\n"))
(define (hypot x y)
  \math.hypot(\x, \y))
(for-each display `("Hypothenuse of triangle of sides of length 3 and 4: "
                    ,(hypot 3 4) "\n"))

\import time
(define (_time) \time.time())
(define (sleep n) \time.sleep(\n))
(define then (_time))
(for-each display `("Current time is: " ,then "\n"))
(display "Sleeping for 3 seconds...") (newline)
(sleep 3)
(define now (_time))
(for-each display `("Current time is: " ,now "\n"))
(define delta (- now then))
(for-each display `("Time delta is : " ,delta "\n"))

;; \import datetime
;; (define then \datetime.datetime.now())
;; (sleep 3)
;; (define now \datetime.datetime.now())
;; (define delta (- now then))
;; (define seconds \\(delta).total_seconds())

;; (pip install requests)
;; \import requests
;; (define url "https://jsonplaceholder.typicode.com/todos/1")
;; (define r \requests.get(\url))
;; (define r.json \\(r).json())

;; \from flask import Flask
;; (define app \Flask(__name__))
;; (@ app route (list '/')
;;        (define (hello-world)
;;          "Hello, world!"))
