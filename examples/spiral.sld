(define-library (spiral)

  (import (except (gambit) six.infix)
          (github.com/feeley/pyffi))

  (begin

    \import turtle

    (define title    \turtle.title)
    (define clear    \turtle.clear)
    (define penup    \turtle.penup)
    (define pendown  \turtle.pendown)
    (define forward  \turtle.forward)
    (define backward \turtle.backward)
    (define left     \turtle.left)
    (define mainloop \turtle.mainloop)

    (define (spiral n)
      (if (> n 0)
          (begin
            (forward (* 20 n))
            (left 90)
            (spiral (- n 1)))))

    (define (main)
      (title "spiral")
      (clear)
      (penup)
      (backward 200)
      (pendown)
      (spiral 20)
      (mainloop))

    (main)
    ))
