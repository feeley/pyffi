(define-library (gui)

  (import (except (gambit) six.infix)
          (github.com/feeley/pyffi))

  (begin

    ;; Prerequisite:  pip3 install -U wxPython

    \import wx   ;; import wxPython

    (define (main)
      (let* ((app \foreign(wx.App()))
             (frm \foreign(wx.Frame(None))))

        \(\frm).Show()

        \(\app).MainLoop()
        ))

    (main)
    ))
