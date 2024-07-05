; questo dobbiamo completarlo noi
; tip: dividerlo in moduli (ad es. uno per le varie strategie di reasoning).

;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

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
 



