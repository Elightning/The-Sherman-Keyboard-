#lang racket 

(require racket/gui)
(require rsound)

(define (determine-note-on-click event)
  (cond 
    ;; White Keys
    ((or (and (< (send event get-x) 80) (< (send event get-y) 160)) 
         (and (< (send event get-x) 100) (>= (send event get-y) 160))) 
     (begin (play (make-tone 261.63 0.5 20000))
            (send msg set-label "C"))) ; play C
    ((or (and (> (send event get-x) 120) (< (send event get-x) 180) (< (send event get-y) 160)) 
         (and (> (send event get-x) 100) (< (send event get-x) 200) (>= (send event get-y) 160))) 
     (begin (play (make-tone 293.66 0.5 20000))
            (send msg set-label "D"))) ; play D
    ((or (and (> (send event get-x) 220) (< (send event get-x) 300) (< (send event get-y) 160)) 
         (and (> (send event get-x) 200) (< (send event get-x) 300) (>= (send event get-y) 160))) 
     (begin (play (make-tone 329.63 0.5 20000))
            (send msg set-label "E"))) ; play E
    ((or (and (> (send event get-x) 300) (< (send event get-x) 380) (< (send event get-y) 160)) 
         (and (> (send event get-x) 300) (< (send event get-x) 400) (>= (send event get-y) 160))) 
     (begin (play (make-tone 349.23 0.5 20000))
            (send msg set-label "F"))) ; play F
    ((or (and (> (send event get-x) 420) (< (send event get-x) 480) (< (send event get-y) 160)) 
         (and (> (send event get-x) 400) (< (send event get-x) 500) (>= (send event get-y) 160))) 
     (begin (play (make-tone 392.00 0.5 20000))
            (send msg set-label "G"))) ; play G
    ((or (and (> (send event get-x) 520) (< (send event get-x) 580) (< (send event get-y) 160)) 
         (and (> (send event get-x) 500) (< (send event get-x) 600) (>= (send event get-y) 160))) 
     (begin (play (make-tone 440.00 0.5 20000))
            (send msg set-label "A"))) ; play A
    ((or (and (> (send event get-x) 620) (< (send event get-x) 700) (< (send event get-y) 160)) 
         (and (> (send event get-x) 600) (< (send event get-x) 700) (>= (send event get-y) 160))) 
     (begin (play (make-tone 493.88 0.5 20000))
            (send msg set-label "B"))) ; play B
    ((or (and (> (send event get-x) 700) (< (send event get-x) 780) (< (send event get-y) 160)) 
             (and (> (send event get-x) 700) (>= (send event get-y) 160))) 
     (begin (play (make-tone 523.25 0.5 20000))
            (send msg set-label "C"))) ; play C
    
    ;; Black Keys
    ((and (> (send event get-x) 80) (< (send event get-x) 120) (< (send event get-y) 160)) 
     (begin (play (make-tone 277.18 0.5 20000))
            (send msg set-label "C#"))) ; play C#
    ((and (> (send event get-x) 180) (< (send event get-x) 220) (< (send event get-y) 160)) 
     (begin (play (make-tone 311.13 0.5 20000))
            (send msg set-label "D#"))) ; play D#
    ((and (> (send event get-x) 380) (< (send event get-x) 420) (< (send event get-y) 160)) 
     (begin (play (make-tone 369.99 0.5 20000))
            (send msg set-label "F#"))) ; play F#
    ((and (> (send event get-x) 480) (< (send event get-x) 520) (< (send event get-y) 160)) 
     (begin (play (make-tone 415.30 0.5 20000))
            (send msg set-label "G#"))) ; play G#
    ((and (> (send event get-x) 580) (< (send event get-x) 620) (< (send event get-y) 160)) 
     (begin (play (make-tone 466.16 0.5 20000))
            (send msg set-label "A#"))) ; play A#
    ((and (> (send event get-x) 780) (< (send event get-y) 160)) 
     (begin (play (make-tone 554.37 0.5 20000))
            (send msg set-label "C#"))) ; play C#
    ))

(define (draw-keyboard dc)
  (send dc draw-line 100 0 100 500)
  (send dc draw-line 200 0 200 500)
  (send dc draw-line 300 0 300 500)
  (send dc draw-line 400 0 400 500)
  (send dc draw-line 500 0 500 500)
  (send dc draw-line 600 0 600 500)
  (send dc draw-line 700 0 700 500)
  
  (send dc set-brush "black" 'solid)
  
  (send dc draw-rectangle 80 0 40 160)
  (send dc draw-rectangle 180 0 40 160)
  (send dc draw-rectangle 380 0 40 160)
  (send dc draw-rectangle 480 0 40 160)
  (send dc draw-rectangle 580 0 40 160)
  (send dc draw-rectangle 780 0 40 160))


; Make a frame by instantiating the frame% class
(define frame (new frame% (label "Keyboard")
                   (width 800) (height 300)
                   (min-width 800) (min-height 300)
                   (stretchable-height #f) (stretchable-width #f)))
 
; Make a static text message in the frame
(define msg (new message% (parent frame)
                          (label "Play a note:")))
  
; Derive a new canvas (a drawing window) class to handle events
(define keyboard%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (when (send event button-down? 'left)
        (determine-note-on-click event)))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))

    ; Call the superclass init, passing on all init args
    (super-new)))
 
; Make a canvas that handles events in the frame
(new keyboard% (parent frame)
     (paint-callback
      (lambda (canvas dc)
        (draw-keyboard dc)))
     (style (list 'border)))

; Show the frame by calling its show method
(send frame show #t)
