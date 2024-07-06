; Modulo che implementa la prima strategia di gioco, detta "pattern based"

; TODO: VEDERE SE SERVA UNA REGOLA DI INIZIALIZZAZIONE DELLA KB


(defmodule PATTERN (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

; Iniziamo ad implementare 4 tentativi per indovinare i colori
(deftemplate identified-colors
    ; TODO: serve la storia??
    (multislot colors (allowed-values nil 1 2 3 4 5 6 7 8))
)

(deftemplate numeric-guess
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

; inizialmente non ho identificato alcun colore
(deffacts initial 
    (identified-colors (colors nil))
)

(defrule starting-guess
    (status (step ?s & 0) (mode computer))
=>
    ; In teoria per rispondere io devo
    ; solo costruire guess, asserirla, e poi spostare il focus su game
    (bind ?firstColor 1)
    (bind ?secondColor 1)
    (bind ?thirdColor 1)
    (bind ?fourthColor 1)
    (assert (numeric-guess (step ?s) (numbers ?firstColor ?secondColor ?thirdColor ?fourthColor)))
    (printout t "Pattern: fatto tentativo iniziale" crlf)
)

; regola che processa l'output del sistema
; per stabilire quali colori siano stati individuati
(defrule color-feedback
    ; associo domanda e risposta tramite il
    ; valore di s
    (numeric-guess (step ?s) (numbers $?colors))
    (answer (step ?s) (right-placed ?rp) (miss-placed ?mp))
    ?ifColorsFact <- (identified-colors (colors $?idColors))
=>
    (if (> (+ ?rp ?mp) 0) then
        ; remainder: tutti i valori di colors sono uguali
        (bind ?correctColor (nth$ 1 ?colors))
        (if (not (member$ ?correctColor $?idColors)) then
            ; TODO: vedere se crei un nuovo fatto
            (modify ?ifColorsFact 
                (colors
                    (insert$ $?idColors (+ (length$ $?idColors) 1) ?correctColor)
                )
            )
        )
    )

    ; TODO: verificare se stampi la lista aggiornata
    (printout t "Identified colors: " $?idColors crlf)
)

(defrule next-color
    ; se non ho ancora identificato almeno 4 colori
    (identified-colors (colors $?idColors&:(< (length$ $?idColors) 4)))
    (status (step ?s&:(< ?s 4)) (mode computer))
=>
    ; TODO: rendere generale
    ; Create costruisce un multislot
    (bind $?colors (create$ 1 2 3 4 5 6 7 8))
    ; recupero il primo colore non ancora identificato
    (foreach ?color $?colors
        (if (not (member$ ?color $?idColors)) then
            (assert (numeric-guess (step ?s) (numbers ?color ?color ?color ?color)))
            (return)
        )
    )
)


(defrule numbers-to-colors
    (numeric-guess (step ?s) (numbers $?numbers))
    ; TODO: sarebbe carino toglierlo
    ; per migliorare l'efficienza
    (status (step ?s) (mode computer))
=>
    (bind $?colors (create$ blue green red yellow orange white black purple))

    (bind ?firstColor (nth$ (nth$ 1 $?numbers) $?colors))
    (bind ?secondColor (nth$ (nth$ 2 $?numbers) $?colors))
    (bind ?thirdColor (nth$ (nth$ 3 $?numbers) $?colors))
    (bind ?fourthColor (nth$ (nth$ 4 $?numbers) $?colors))

    (assert (guess 
                (step ?s) 
                (g ?firstColor ?secondColor ?thirdColor ?fourthColor)
            )
    )
    ; TODO: stampre la risposta tradotta
    (printout t "Guess: " $?numbers " at step: " ?s crlf)
    ; restituisco il controllo a game, in modo che processi la risposta
    ; TODO: vedere se serva davvero
    (pop-focus)
)