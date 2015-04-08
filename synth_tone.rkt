#lang racket

(require portaudio
         ffi/vector)
 
(define fundametalFreq 200)


;; creates a multiple of the fundamental frequency for creating complex tones
(define k1 1)
(define k2 1.0000001)
(define k3 .99999999)
(define k4 1.0000001)


;; affects amplitudes -> volume
(define max-vol .5)

;; is set to a value later..
(define a null)

;; change volume of different tones played
(define a1 1)
(define a2 .5)
(define a3 .25)
(define a4 .125)


;; put tones out of phase to make it more life like
(define phase1 (* 0 pi))
(define phase2 (* .5 pi))
(define phase3 (* .25 pi))
(define phase4 (* .125 pi))

 
(define sample-rate 44100.0)
(define tpisr (* 2 pi (/ 1.0 sample-rate)))
(define (real->s16 x)
  (inexact->exact (round (* 32767 x))))

(define durationOfNote 2)
 
(define attack .002)

(define dampen null)

(define vec (make-s16vector (* (* 44100 durationOfNote) 4)))
(for ([t (in-range 88200)])
  
  (if (<= t (* attack sample-rate))
      (set! a (* max-vol (/ t (* sample-rate attack))))
      (begin
      (set! dampen (expt (* .5 (log (/ (* fundametalFreq max-vol) sample-rate))) 2))
      (set! a (* max-vol (expt (- 1 (/ (- t  (* sample-rate attack)) (* sample-rate (- 2 attack)))) dampen)))))
  

  ;; creating samples. i need to focus on adjusting k# values
  (define sample1 (real->s16 (* a1 a (sin (+ (* tpisr t (* fundametalFreq k1)) phase1 )))))
  (define sample2 (real->s16 (* a2 a (sin (+ (* tpisr t (* fundametalFreq k2)) phase2 )))))
  (define sample3 (real->s16 (* a3 a (sin (+ (* tpisr t (* fundametalFreq k3)) phase3 )))))
  (define sample4 (real->s16 (* a4 a (sin (+ (* tpisr t (* fundametalFreq k4)) phase4 )))))
  ;; filling in samples
  (s16vector-set! vec (* 4 t) sample1)
  (s16vector-set! vec (+ 1 (* 4 t)) sample2)
  (s16vector-set! vec (+ 2 (* 4 t)) sample3)
  (s16vector-set! vec (+ 3 (* 4 t)) sample4))
  
 
(s16vec-play vec 0 88200 sample-rate)