(deftemplate feedback
   (slot black-pegs)
   (slot white-pegs))

(deftemplate guess
   (slot colors))

(deftemplate game-state
   (slot identified-colors)
   (slot potential-positions)
   (slot remaining-attempts))

(defglobal ?*colors* = (create$ 1 2 3 4 5 6))
(defglobal ?*identified-colors* = (create$))
(defglobal ?*potential-positions* = (create$ _ _ _ _))
(defglobal ?*remaining-attempts* = 10)

(deffacts initial
   (game-state (identified-colors nil) (potential-positions (create$ _ _ _ _)) (remaining-attempts 10)))

(defrule initial-simple-guess
   (game-state (identified-colors nil))
   =>
   (bind ?guess (create$ 1 1 1 1))
   (assert (guess (colors ?guess)))
   (printout t "Initial simple guess: " ?guess crlf))

(defrule single-color-feedback
   (guess (colors ?g))
   (feedback (black-pegs ?bp) (white-pegs ?wp))
   =>
   (retract (guess (colors ?g)))
   (modify ?fact 
      (remaining-attempts (- (fact-slot-value ?fact remaining-attempts) 1)))
   (if (> (+ ?bp ?wp) 0) then
      (bind ?color (nth$ 1 ?g))
      (if (not (member$ ?color ?*identified-colors*)) then
         (bind ?*identified-colors* (nconc ?*identified-colors* (create$ ?color)))))
   (printout t "Feedback: " ?bp " black pegs, " ?wp " white pegs" crlf)
   (printout t "Identified colors: " ?*identified-colors* crlf))

(defrule test-next-color
   (game-state (identified-colors ?ic&:(< (length$ ?ic) 4)) (remaining-attempts ?ra&:(> ?ra 5)))
   =>
   (foreach ?color ?*colors*
      (if (not (member$ ?color ?*identified-colors*)) then
         (bind ?guess (create$ ?color ?color ?color ?color))
         (assert (guess (colors ?guess)))
         (printout t "Testing next color: " ?guess crlf)
         (return)
      )
   )
)







(defrule build-pattern
   (game-state (identified-colors ?ic&:(= (length$ ?ic) 4)))
   =>
   (bind ?guess (create$ (nth$ 1 ?ic) (nth$ 2 ?ic) (nth$ 3 ?ic) (nth$ 4 ?ic)))
   (assert (guess (colors ?guess)))
   (printout t "Building pattern with identified colors: " ?guess crlf))



; NOTA IMPORTANTE: 
; Black Pegs ==> Colori in posizioni corrette
; White Pegs ==> Colori in posizioni errate
; potenatial-positions è un elenco di colori
; se l'i-esimo elemento è il colore j, vuol dire
; che il colore j si troverà nell'i-esima posizione
; del codice segreto
(defrule refine-pattern
   (guess (colors ?g))
   (feedback (black-pegs ?bp) (white-pegs ?wp))
   (game-state (identified-colors ?ic) (potential-positions ?pp) (remaining-attempts ?ra))
   =>
   (retract (guess (colors ?g)))
   (modify ?fact 
      (remaining-attempts (- (fact-slot-value ?fact remaining-attempts) 1)))
   ; Se i colori in posizione corretta (black pegs) sono più dei colori di posizione nota
   ; Cioè con il feedback corrente riesco a scoprire la posizione di qualche altro colore
   (if (> ?bp (length$ (intersection$ ?g ?pp))) then
      (foreach ?i 0 1 2 3
         ; Se non conosco ancora la posizione potenziale dell'i-esimo colore
         (if (eq (nth$ (+ ?i 1) ?g) (nth$ (+ ?i 1) ?pp)) then
            ; la vado a memorizzare
            (bind (nth$ (+ ?i 1) ?pp) (nth$ (+ ?i 1) ?g)))))
   (printout t "Refined pattern: " ?pp crlf))

(defrule final-pattern
   (game-state (remaining-attempts ?ra&:(= ?ra 0)))
   =>
   (printout t "Final code: " ?*potential-positions* crlf))



   (if (neq (nth$ (+ ?i 1) ?g) (nth$ (+ ?i 1) ?pp)) then
            (bind ?pp (replace$ ?pp (+ ?i 1) (+ ?i 1) (nth$ (+ ?i 1) ?g))))))
   (printout t "Refined pattern: " ?pp crlf)
   (assert (guess (colors ?pp)))))
