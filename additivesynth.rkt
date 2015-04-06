#lang racket

(require portaudio
         ffi/vector)
 
(define fundametalFreq 200)

(define k1 1)
(define k2 3)
(define k3 5)
(define k4 7)

(define a1 .5)
(define a2 .25)
(define a3 .15)
(define a4 .05)

(define phase1 0)
(define phase2 .1)
(define phase3 .2)
(define phase4 0)
 
(define sample-rate 44100.0)
(define tpisr (* 2 pi (/ 1.0 sample-rate)))
(define (real->s16 x)
  (inexact->exact (round (* 32767 x))))

(define durationOfNote 2)
 
(define vec (make-s16vector (* (* 44100 durationOfNote) 4)))
(for ([t (in-range 88200)])
  (define sample1 (real->s16 (* a1 (sin (+ (* tpisr t (* fundametalFreq k1)) phase1)))))
  (define sample2 (real->s16 (* a2 (sin (+ (* tpisr t (* fundametalFreq k2)) phase2)))))
  (define sample3 (real->s16 (* a3 (sin (+ (* tpisr t (* fundametalFreq k3)) phase3)))))
  (define sample4 (real->s16 (* a4 (sin (+ (* tpisr t (* fundametalFreq k4)) phase4)))))
  (s16vector-set! vec (* 4 t) sample1)
  (s16vector-set! vec (+ 1 (* 4 t)) sample2)
  (s16vector-set! vec (+ 2 (* 4 t)) sample3)
  (s16vector-set! vec (+ 3 (* 4 t)) sample4))
  
 
(s16vec-play vec 0 88200 sample-rate)