;; implementa le regole del gioco

(defmodule GAME (import MAIN ?ALL) (export deftemplate guess answer))

; Definizione del concetto di codice segreto: esattamente 4 colori
(deftemplate secret-code
	(multislot code (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
)

; modellazione della risposta dell'agente: anch'essa esattamente 4 colori
(deftemplate guess
	(slot step (type INTEGER))
	(multislot g (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
)

; Definizione struttura risposta
(deftemplate answer
	(slot step (type INTEGER))
	(slot right-placed (type INTEGER))
	(slot miss-placed (type INTEGER))
)


; Se la guess dell'utente al passo corrente corrisponde
; al codice segreto il gioco termina con la vittoria dell'utente
(defrule check-mate (declare (salience 100))
  (status (step ?s))
  ?f <- (guess (step ?s) (g ?k1 ?k2 ?k3 ?k4))
  (secret-code (code ?k1 ?k2 ?k3 ?k4) )
  =>
  (printout t "You have discovered the secrete code!" crlf)
  (retract ?f)
  (halt) 
)

;; inizializzazione risposta: i valori esatti verranno messi più avanti
(defrule prepare-answer
   (status (step ?s))
   (guess (step ?s))
=>
    (printout t "Preparazione risposta" crlf)
   (assert (answer (step ?s) (right-placed 0) (miss-placed 0)))
)      

;(defrule check-repeated-colors (declare (salience 100))
;  (status (step ?s))
  ; se in una posizione qualsiasi (modellata con $, cioè 0 o più simboli)
  ; c'è lo stesso simbolo di un'altra posizione precedente
  ; alolora c'è una ripetizione
;  ?g <- (guess (step ?s) (g $?prima ?k $?durante ?k $?dopo))
;=>
  ; rimuovo la guess per non dare alcun suggerimento all'utente
;  (retract ?g)
;  (pop-focus)
;)

(defrule check-miss-placed
  (status (step ?s))
  (secret-code (code $?prima ?k $?dopo) )
  (guess (step ?s) (g $?prima2 ?k $?dopo2))
  (test (neq (length$ $?prima2) (length$ $?prima)))
  (test (neq (length$ $?dopo2) (length$ $?dopo)))
=>
  (bind ?new (gensym*))
  ; nuovo fatto che modella il numero di celle fuori posto
  ; c'è sempre un solo fatto missplaced alla volta
  ; che fa riferimento al tentativo corrente
  (assert (missplaced ?new))
)

(defrule count-missplaced
  (status (step ?s))
  ?a <- (answer (step ?s) (miss-placed ?mp))
  ?m <- (missplaced ?)
=>
  ; in questo modo c'è sempre un unico fatto misplaced
  (retract ?m)
  ; conteggio il missplaced tra quelli della risposta
  (bind ?new-mp (+ ?mp 1))
  (modify ?a (miss-placed ?new-mp))  
)

; stesso funzionamento di sopra, ma conta il numero di celle ben posizionate
(defrule check-right-placed
  (status (step ?s))
  (secret-code (code $?prima ?k $?dopo) )
  (guess (step ?s) (g $?prima2  ?k $?dopo2))
  (test (eq (length$ $?prima2) (length$ $?prima)))
  (test (eq (length$ $?dopo2) (length$ $?dopo)))   
=>
  (bind ?new (gensym*))
  (assert (rightplaced ?new))
)

(defrule count-rightplaced
  (status (step ?s))
  ?a <- (answer (step ?s) (right-placed ?rp) (miss-placed ?mp))
  ?r <- (rightplaced ?)
=>
  (retract ?r)
  (bind ?new-rp (+ ?rp 1))
  (modify ?a (right-placed ?new-rp))
)


; stampa a video delle risposte
(defrule for-humans (declare (salience -10))
  ;(status (step ?s) (mode human))
  (status (step ?s) (mode computer))
  (answer (step ?s) (right-placed ?rp) (miss-placed ?mp)) 
=>
   (printout t "Right placed " ?rp " missplaced " ?mp crlf)
)  


(defrule for-humans-gameover (declare (salience -15))
  (status (step ?s) (mode human))
  (maxduration ?d&:(>= ?s ?d))
  (secret-code (code $?code))
=>
   (printout t "GAME OVER!! " crlf)
   (printout t "The secret code was: " $?code crlf)
)  


; da qui in poi ci sono regole per la creazione del codice segreto
(defrule  random-start (declare (salience 100))
	(random)
	(not (secret-code (code $?)))
=>
  (printout t "Generazione CODICE SEGRETO" crlf)
	(assert (secret-code (code (create$))))
)	
	

(defrule random-code (declare (salience 100))
	(random)
	(colors $?cls)
	(secret-code (code $?colors))
	(test (neq (length$ $?colors) 4))
=>
	(bind ?roll (random 1 8))
	(bind ?c-sym (nth$ ?roll $?cls))
	(assert (try ?c-sym))		
)

(defrule try-new-color-yes (declare (salience 100))
	(random)
	?s <- (secret-code (code $?colors))
	(test (neq (length$ $?colors) 4))
	?t <- (try ?c-sym)
	(test (not (member$ ?c-sym $?colors)))
=>
	(retract ?t)
	(modify ?s (code $?colors ?c-sym))	
)

(defrule try-new-color-no (declare (salience 100))
	(random)
	?s <- (secret-code (code $?colors))
	(test (neq (length$ $?colors) 4))
	?t <- (try ?c-sym)
	(test (member$ ?c-sym $?colors))
=>
	(retract ?t)
	(retract ?s)
	(assert (secret-code (code $?colors)))
)



(deffacts my-colors
 (colors blue green red yellow orange white black purple)
 )


