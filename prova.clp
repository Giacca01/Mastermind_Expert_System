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

(defrule initial-guess
   (game-state (current-guess nil))
   =>
   (assert (guess (colors (create$ 1 2 3 4))))
   (printout t "Initial guess: [1 2 3 4]" crlf))

(defrule process-feedback
   (guess (colors ?g))
   (feedback (black-pegs ?bp) (white-pegs ?wp))
   =>
   (retract (guess (colors ?g)))
   (if (> (+ ?bp ?wp) 0) then
      (printout t "Feedback for " ?g ": " ?bp " black pegs, " ?wp " white pegs" crlf)
      (if (< (+ ?bp ?wp) 4) then
         (bind ?new-colors (create$))
         (foreach ?color ?*colors*
            (if (not (member$ ?color ?g)) then
               (bind ?new-colors (add$ ?new-colors ?color))))
         (bind ?num-to-replace (- 4 (+ ?bp ?wp)))
         (bind ?new-guess (replace-colors ?g ?num-to-replace ?new-colors))
         (assert (guess (colors ?new-guess)))
         (printout t "Adjusting next guess with " ?num-to-replace " new colors: " ?new-guess crlf))
      else
         (assert (guess (colors ?g)))
         (printout t "Sum of black and white pegs reached four. Starting pattern refinement." crlf))))


(deffunction replace-colors (?guess ?num-to-replace ?new-colors)
   (bind ?new-guess (copy$ ?guess))
   (foreach ?i (number 1 ?num-to-replace)
      (bind ?index (random (length$ ?new-guess)))
      (bind ?color (nth$ ?index ?new-colors))
      (bind ?new-guess (replace$ ?new-guess ?index ?index ?color)))
   ?new-guess)







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




(defrule process-feedback
   (game-state (current-guess ?g1) (previous-guess ?g2))
   (feedback (black-pegs ?bp) (white-pegs ?wp))
   =>
   (retract (game-state (current-guess ?g1) (previous-guess ?g2)))
   (printout t "Feedback for " ?g1 ": " ?bp " black pegs, " ?wp " white pegs" crlf)
   (if (< ?bp 4) then
      (bind ?num-to-replace (- 4 ?bp))
      (bind ?new-guess (replace-positions ?g1 ?num-to-replace))
      (assert (game-state (current-guess ?new-guess) (previous-guess ?g1)))
      (printout t "Adjusting next guess by replacing " ?num-to-replace " positions: " ?new-guess crlf))
   else
      (assert (game-state (current-guess ?g1) (previous-guess ?g2)))
      (printout t "Final pattern found: " ?g1 crlf)))

(defrule final-pattern
   (game-state (remaining-attempts ?ra&:(= ?ra 0)))
   =>
   (printout t "Out of attempts. Final pattern not determined." crlf))

; Utility function to replace positions of colors
(deffunction replace-positions (?guess ?num-to-replace)
   (bind ?new-guess (copy$ ?guess))
   (foreach ?i (number 1 ?num-to-replace)
      (bind ?index1 (random (length$ ?new-guess)))
      (bind ?index2 (random (length$ ?new-guess)))
      (bind ?temp (nth$ ?index1 ?new-guess))
      (bind ?new-guess (replace$ ?new-guess ?index1 ?index1 (nth$ ?index2 ?new-guess)))
      (bind ?new-guess (replace$ ?new-guess ?index2 ?index2 ?temp)))
   ?new-guess)