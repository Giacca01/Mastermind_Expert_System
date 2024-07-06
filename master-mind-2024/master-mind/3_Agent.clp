; questo dobbiamo completarlo noi
; tip: dividerlo in moduli (ad es. uno per le varie strategie di reasoning).

;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

(deftemplate strategy-type
  (slot name (allowed-values Pattern Position))
)

; Questo serve per giocare a mano
(defrule human-player
  (status (step ?s) (mode human))
  =>
  (printout t "Your guess at step " ?s crlf)
  ; leggo il codice immesso dall'utente
  (bind $?input (readline))
  ; recupero i colori del codice
  (assert (guess (step ?s) (g  (explode$ $?input)) ))
  ; in questo modo deve tornare in esecuzione il modulo game
  (pop-focus)
 )
 
; Mi serve una regola che, qualora si stia giocando
; in modalità agente, processi l'input dell'utente ed attivi
; passandogli il focus (l'unico strumento che abbiamo per il passaggio tra moduli!)
; il modulo corrispondente

(defrule agent-player
  (status (step 0) (mode computer))
=>
  (printout t "Which strategy do you want to use (Pattern/Position)?" crlf)
  (bind ?choice (readline))
  (assert (strategy-type (name ?choice)))
  (if (eq ?choice "Pattern") then
    (focus PATTERN)
    else
      (if (eq ?choice "Position") then
        (focus POSITION)
      )
  )
)

; Serve per riportare il focus sulla strategia scelta ad ogni turno del giocatore
(defrule fire-agent
  (strategy-type (name ?choice))
  (status (step ?s) (mode computer))
=>
  (if (eq ?choice "Pattern") then
    (focus PATTERN)
    else
      (if (eq ?choice "Position") then
        (focus POSITION)
      )
  )
)