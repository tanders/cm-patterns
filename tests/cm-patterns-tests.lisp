
(require :cm-patterns)

(in-package :om)

;; cycle pattern
(setf p (cm:new cm:cycle :of '(a b c)))

(cm:next p)
; => a

(cm:next p)
; => b

(cm:next p 19)
; => (c a b c a b c a b c a b c a b c a b c)

(cm:next p T)
; => (a b c)


;; alberti bass
(setf bass (cm:new cm:cycle :of '(c3 e3 g3 e3)))

(cm:next bass 16)
; => (c3 e3 g3 e3 c3 e3 g3 e3 c3 e3 g3 e3 c3 e3 g3 e3)

;; heap pattern
(setf q (cm:new cm:heap :of '(c4 d4 eb4 b3) :for 20))

(cm:next q T)
; (d4 c4 eb4 b3 eb4 c4 b3 d4 b3 c4 eb4 d4 eb4 d4 c4 b3 eb4 d4 b3 c4)


;; pattern with sub-patterns
(setf pat1
      (cm:new cm:cycle 
        :of (list (cm:new cm:heap :of '(c4 e4 g4))
                  (cm:new cm:heap :of '(gs4 as4 cs5)))))
(cm:next pat1 T)
; => (c4 g4 e4 as4 gs4 cs5)

(setf pat1b 
      (cm:new cm:weighting
        :of `((,(cm:new cm:cycle :of '(a4 b4 c5 d5)) :max 2)
              (,(cm:new cm:heap :of '(gs4 as4 cs5 ds5)) :max 2))))
(cm:next pat1b 20)
; => (a4 b4 c5 d5 gs4 ds5 cs5 as4 as4 cs5 ds5 gs4 a4 b4 c5 d5 gs4 ds5 as4 cs5)


;; another nested pattern -- this time one pattern is distributed over multiple instances of the outer patter
(setf pat2 
      (cm:new cm:cycle 
        :of (list 'c4 'd4 (cm:new cm:cycle :of '(gs4 as4 bs4) :for 1))))
(cm:next pat2 20)
; => (c4 d4 gs4 c4 d4 as4 c4 d4 bs4 c4 d4 gs4 c4 d4 as4 c4 d4 bs4 c4 d4)

;; the length of patters controlled by a pattern
(setf pat3 
       (cm:new cm:cycle 
         :of (list (cm:new cm:cycle :of '(c4))
                   (cm:new cm:cycle 
                     :of '(cs5 ds5 fs5)
                     :for (cm:new cm:cycle :of '(1 2 3))))))
(cm:next pat3 20)
; => (c4 cs5 c4 ds5 fs5 c4 cs5 ds5 fs5 c4 cs5 c4 ds5 fs5 c4 cs5 ds5 fs5 c4 cs5)

;; combining two patterns by adding (of differing length) 
(let ((p1 (cm:new cm:cycle :of '(60 62 64 65)))
      (p2 (cm:new cm:cycle :of '(0 2 4 6 8))))
  (loop repeat 20
    for x = (cm:next p1)
    for y = (cm:next p2)
    collect (+ x y)))
; => (60 64 68 71 68 62 66 69 66 70 64 67 64 68 72 65 62 66 70 73)

;; add two nested patterns
(let ((p1 (cm:new cm:cycle :of '(0 2 4 6 8)))
      (p2 (cm:new cm:cycle :of '(60 62 64 65))))
  (loop repeat 4
    for x = (cm:next p1)
    append (loop repeat 4
              for y = (cm:next p2)
              collect (+ x y))))
; => (60 62 64 65 62 64 66 67 64 66 68 69 66 68 70 71)

;; join (list) values of parallel patters 
(setf pat4 
      (cm:new cm:join :of 
        (list (cm:new cm:weighting :of '(c4 g4) :for 1)
              (cm:new cm:cycle :of '(q q e e)))))
(cm:next pat4 20)
; => ((c4 q) (c4 q) (c4 e) (g4 e) (g4 q) (c4 q) (g4 e) (g4 e) (g4 q) (g4 q) (c4 e) (c4 e) (g4 q) (c4 q) (g4 e) (g4 e) (g4 q) (c4 q) (c4 e) (g4 e))

