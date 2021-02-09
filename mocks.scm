;; We have to handle different cases:
;;
;; 1) Scheme objects that are passed to python functions
;; 2) Python objects that are passed to scheme functions
;; 3) Bidirectional objects that support mutation on either side

(import (github.com/feeley pyffi))

\import math
(define pi (\math.pi))
(define py-sqrt \math.sqrt)
(define three (py-sqrt 9))
(define (area r)
  (* pi r r))
(define (comb n k)
  \math.comb(\n, \k))
(define comb* \math.comb)

\import time
(define time \time.time)
(define sleep \time.sleep)
(define then (time))
(sleep 5)
(define now (time))
(define delta (- now then))

\from datetime import datetime
(define then \datetime.now())
(sleep 10)
(define now \datetime.now())
(define delta (- now then))
(define delta* \\now - \then)
(define seconds \\(delta).total_seconds())

\import requests
(define url "https://jsonplaceholder.typicode.com/todos/1")
(define r \requests.get(\url))
(define r.json \\(r).json())

\from flask import Flask
(define app \Flask(__name__))
(@ app route (list '/')
       (define (hello-world)
         "Hello, world!"))
