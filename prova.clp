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
         (return))))