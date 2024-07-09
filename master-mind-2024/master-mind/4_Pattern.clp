; Modulo che implementa la prima strategia di gioco, detta "pattern based"
; TODO: fare una regola per la stampa del risultato finale


(defmodule PATTERN (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

; Iniziamo ad implementare 4 tentativi per indovinare i colori
(deftemplate phase
    (slot name (allowed-values COLOR POSITION))
)

(deftemplate numeric-guess
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate color-codes
    (multislot codes)
)

(deftemplate color-numbers
    (multislot numbers)
)

; inizialmente non ho identificato alcun colore
(deffacts initial 
    (phase (name COLOR))
    (color-codes (codes blue green red yellow orange white black purple))
    (color-numbers (numbers 1 2 3 4 5 6 7 8))
)

(defrule starting-guess
    (strategy-type (name Pattern))
    (status (step ?s & 0) (mode computer))
=>
    ; In teoria per rispondere io devo
    ; solo costruire guess, asserirla, e poi spostare il focus su game
    ; Guess iniziale contenente il maggior numero di colori possibili
    (bind ?firstColor 1)
    (bind ?secondColor 2)
    (bind ?thirdColor 3)
    (bind ?fourthColor 4)
    (assert (numeric-guess (step ?s) (numbers ?firstColor ?secondColor ?thirdColor ?fourthColor)))
    (printout t "Pattern: fatto tentativo iniziale" crlf)
)

; regola che processa l'output del sistema
; per stabilire quali colori siano stati individuati
(defrule color-feedback
    ; associo domanda e risposta tramite il
    ; valore di s
    (status (step ?s) (mode computer))
    (numeric-guess (step ?s1 &: (= (- ?s 1) ?s1)) (numbers $?colors))
    (answer (step ?s1 &: (= (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
    (color-numbers (numbers $?nc))
    ?phase <- (phase (name COLOR))
=>
    (printout t "Processamento feedback sistema" crlf)
    (if (>= (+ ?rp ?mp) 0) then
        (if (< (+ ?rp ?mp) 4) then
            ; Calcolo colori non ancora presenti nella risposta
            (bind $?newColors (difference$ $?nc $?colors))
            ; Introduco nuovi colori in numero pari a quelli errati
            (bind ?newColorsCounter (- 4 (+ ?rp ?mp)))
            ; Copia della risposta originale
            (bind $?newGuessColors (subseq$ $?colors 1 4))
            ; nel peggiore dei casi devo cambiare 4 colori
            (bind $?indexesList (create$ 1 2 3 4))
            ; Per un numero di volte pari a newColorsCounter...
            (foreach ?i $?indexesList
                ; Per ora introduco i colori in posizioni casuali
                ; il raffinamento della strategia starà proprio nel non metterli in posizioni qualsiasi
                ; preservando quanto prodotto nei tentativi precedenti
                ; Estraggo a caso un colore...
                (if (<= ?i ?newColorsCounter) then
                    (bind ?newPos (random 1 4))
                    (bind ?color (nth$ ?newPos $?newColors))
                    ;(printout t "Posizione sostituzione " ?newPos " Nuovo Colore: " ?color crlf)
                    ; e lo inserisco nella risposta
                    (bind $?newGuessColors (replace$ $?newGuessColors ?newPos ?newPos ?color))
                )
            )
            (assert (numeric-guess (step ?s) (numbers $?newGuessColors)))
        else
            ; Ho identificato tutti i colori: non mi rimane che provare le posizioni
            (retract ?phase)
            (assert (phase (name POSITION)))

            ; TODO: verificare se stampi la lista aggiornata
            (printout t "Colori identificati" crlf)
        )
    )
)



; Dopo aver identificato i 4 colori del codice
; cerco la combinazione con le posizioni esatte
; TODO: vedere cosa fare se finiscono i tentativi
(defrule numbers-to-colors
    (numeric-guess (step ?s) (numbers $?numbers))
    ; TODO: sarebbe carino toglierlo
    ; per migliorare l'efficienza
    (status (step ?s) (mode computer))
    (color-codes (codes $?colors))
=>
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
    (printout t "Guess: " ?firstColor " " ?secondColor " " ?thirdColor " " ?fourthColor " at step: " ?s crlf)
    ; restituisco il controllo a game, in modo che processi la risposta
    ; TODO: vedere se serva davvero
    (pop-focus)
)

; Per ora qui non arriva mai, perchè non supera la fase di identificazione del colore
(defrule refine-pattern
    (status (step ?s) (mode computer))
    (numeric-guess (step ?s1 &: (= (- ?s 1) ?s1)) (numbers $?colors))
    (answer (step ?s1 &: (= (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
    (phase (name POSITION))
=>
    (if (< ?rp 4) then
        (bind ?swapCounter (- 4 ?rp))
        (bind $?newGuess (subseq$ $?colors 1 4))
        (bind $?indexesList (create$ 1 2 3 4))
        (foreach ?i $?indexesList
            (if (<= ?i ?swapCounter) then
                (bind ?firstPos (random 1 4))
                (bind ?secondPos (random 1 4))
                (bind ?tmp (nth$ ?firstPos $?newGuess))
                (bind $?newGuess (replace$ $?newGuess ?firstPos ?firstPos (nth$ ?secondPos $?newGuess)))
                (bind $?newGuess (replace$ $?newGuess ?secondPos ?secondPos ?tmp))
            )
        )
        (assert (numeric-guess (step ?s) (numbers $?newGuess)))
    )
)