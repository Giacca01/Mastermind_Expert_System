;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

(deftemplate strategy-type
  (slot name (allowed-values Swa Imp))
)

(defrule human-player
  (status (step ?s) (mode human))
  =>
  (printout t "Your guess at step " ?s crlf)
  (bind $?input (readline))
  (assert (guess (step ?s) (g  (explode$ $?input)) ))
  (pop-focus)
 )
 
; Regola che, qualora si giochi in modalità computer,
; chiede all'utente di scegliere la strategia da utilizzare
; per poi spostare il focus sul modulo corrispondente
; in modo che possa entrare in azione
(defrule agent-player
  (status (step 0) (mode computer))
=>
  (printout t "Which strategy do you want to use (Swa/Imp)?" crlf)
  (bind ?choice (readline))
  
  (if (eq ?choice "Swa") then
    (assert (strategy-type (name Swa)))
    (focus SWA)
  else
    (if (eq ?choice "Imp") then
      (assert (strategy-type (name Imp)))
      (focus IMP)
    )
  )
)

; Ad ogni turno del giocatore, riporto il focus
; sul modulo corrispondente alla strategia scelta
(defrule fire-agent
  (strategy-type (name ?choice))
  (status (step ?s) (mode computer))
=>
  (if (eq ?choice Swa) then
    (focus SWA)
  else
    (if (eq ?choice Imp) then
      (focus IMP)
    )
  )
)