; Modulo che contiene la conoscenza di controllo

(defmodule MAIN (export ?ALL))

; Nota: questo è anche il costituente base dello stato dell'agente
;; human consente di giocare direttamente: serve soprattutto per le prove
; notare come il numero di step modelli lo stato corrente
(deftemplate status (slot step) (slot mode (allowed-values human computer)) )


; Se ci sono ancora tentativi rimasti inizio il turno dell'agente
(defrule go-on-agent  (declare (salience 30))
   (maxduration ?d)
   (status (step ?s&:(< ?s ?d)) )

 =>

    (printout t crlf crlf)
    (printout t "vado ad AGENT  step " ?s crlf)
    (focus AGENT)
)


; Se ci sono ancora tentativi rimasti inizio il tentativo
; di generazione della risposta
(defrule go-on-env  (declare (salience 30))
   (maxduration ?d)
  ?f1<-	(status (step ?s&:(< ?s  ?d)))

=>

  (printout t crlf crlf)
  (printout t "vado a GAME  step " ?s crlf)
  (focus GAME)

)

; Incremento il numero di passo corrente
(defrule next-step  (declare (salience 20))
   (maxduration ?d)
  ?f1<-	(status (step ?s&:(< ?s  ?d)))

=>
  
 (bind ?s2 (+ ?s 1))
 (modify ?f1 (step ?s2))

)

; Quando non ci sono più passi possibili, decreto la fine della partita
(defrule game-over
	(maxduration ?d)
	(status (step ?s&:(>= ?s ?d)))
=>
	(focus GAME)
)

; Dieci tentativi massimo, inizio in modalità umana
; l'agente inizia facendo una proposta di codice
(deffacts initial-facts
	(maxduration 10)
	;(status (step 0) (mode human))
	(status (step 0) (mode computer))
	(agent-first)
)

