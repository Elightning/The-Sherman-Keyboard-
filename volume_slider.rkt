#lang racket

;; require libraries
(require racket/gui
         rsound
         ffi/vector
         images/icons/control
         images/icons/style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;SOUND CODE;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pitch  523.253) ;; C5
(define sample-rate 44100.0) ;; how many times we sample a sound. the higher the more realistic.
(define tpisr (* 2 pi (/ 1.0 sample-rate))) ;; 2pi * 44100.0, i took this from the racket documents.
;;(define volume .05) ;; affects amplitude of sin wave affecting it's loudness.

(define vec (make-s16vector (* 88200 2))) ;; creates an array of 16 bit ints of len 88200 * 2

(define (set-sample index volume)
  ;; code copied from documentation that was originally within a C style for loop.
  (define sample (real->s16 (* volume (sin (* tpisr index pitch)))))
  (s16vector-set! vec (* 2 index) sample)
  (s16vector-set! vec (add1 (* 2 index)) sample))

;; fills the array with sine wave samples.
(define (fill-vec vol)
  (define (helper curr-ind len ret-val)
    (if (> curr-ind len)
        (void) ;; return nothing.
        (helper (+ curr-ind 1) len (set-sample curr-ind vol))))
  (helper 0 (- (/ (s16vector-length vec) 2) 1) null))

(define (create-tone vol)
  (fill-vec vol) ;; create vec with specific volume setting
  (rsound vec
          0
          88200
          sample-rate))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;ICONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define my-play-icon (play-icon #:color run-icon-color #:height 32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;GUI CODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; instantiating a frame class instance.
(define frame (new frame% [label ""]
                   [min-width 300]
                   [min-height 150]))

(define (display-frame frame)
   (send frame show #t))

;; volume slider!
(define slider (new slider%
                    [label "volume"]
                    [parent frame]
                    [min-value 0]
                    [max-value 10]
                    [init-value 5]
                    [callback (lambda (control event)
                                (stop)
                                (play (create-tone (/ (send slider get-value) 80))))]))

; Make a button in the frame
(new button% [parent frame]
             [label "click to play sound."]
             ; Callback procedure for a button click:
             [callback (lambda (button event) ;; executed when a botton is clicked!
                         (stop) ;; prevent multiple presses from overlapping the tones playing.
                         (play (create-tone  (/ (send slider get-value) 80))))])

; Show the frame by calling its show method
;;(send frame show #f)
(display-frame frame)
  