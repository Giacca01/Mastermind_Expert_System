; Modulo che implementa la prima strategia di gioco, detta "pattern based"

; TODO: VEDERE SE SERVA UNA REGOLA DI INIZIALIZZAZIONE DELLA KB (in teoria no)

; TODO: fare una regola per la stampa del risultato finale


(defmodule PATTERN (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

; Iniziamo ad implementare 4 tentativi per indovinare i colori
(deftemplate identified-colors
    ; TODO: serve la storia??
    ; In teoria no, però potrebbe servire doverli resettare
    (multislot colors (allowed-values nil 1 2 3 4 5 6 7 8))
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

; positions[i] == j => il colore j è nella posizione i del codice segreto
(deftemplate known-positions
    (multislot positions (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

; inizialmente non ho identificato alcun colore
(deffacts initial 
    (identified-colors (colors nil))
    (color-codes (codes blue green red yellow orange white black purple))
    (color-numbers (numbers 1 2 3 4 5 6 7 8))
)

(defrule starting-guess
    (strategy-type (name Pattern))
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
    (status (step ?s&:(< ?s 10)) (mode computer))
    (color-numbers (numbers $?colors))
=>
    ; recupero il primo colore non ancora identificato
    (foreach ?color $?colors
        (if (not (member$ ?color $?idColors)) then
            (assert (numeric-guess (step ?s) (numbers ?color ?color ?color ?color)))
            (bind $?debug (create$ ?color ?color ?color ?color))
            (printout t "Nuovo tentativo " $?debug crlf)
            (return)
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
    (printout t "Guess: " $?numbers " at step: " ?s crlf)
    ; restituisco il controllo a game, in modo che processi la risposta
    ; TODO: vedere se serva davvero
    (pop-focus)
)

; Per ora qui non arriva mai, perchè non supera la fase di identificazione del colore
(defrule initial-pattern
    (identified-colors (colors $?idColors &: (= (length$ $?idColors) 4)))
    (status (step ?s) (mode computer))
    (known-positions (positions $?knownP &: (= (length$ $?knownP) 0)))
=>
    (bind ?firstColor (nth$ 1 $?idColors))
    (bind ?secondColor (nth$ 2 $?idColors))
    (bind ?thirdColor (nth$ 3 $?idColors))
    (bind ?fourthColor (nth$ 4 $?idColors))
    (assert 
        (numeric-guess 
            (step ?s) 
            (numbers ?firstColor ?secondColor ?thirdColor ?fourthColor)
        )
    )
)

; TODO: Vedere se qui serva regolare gli step
(defrule refine-pattern
    (status (step ?s) (mode computer))
    ; Processamento della risposta data al turno precedente
    ; per costruire quella da dare al turno corrente
    (numeric-guess (step ?s1 &: (= (- ?s 1) ?s1)) (numbers $?guessColors))
    (answer (step ?s1 &: (= (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
    (known-positions (positions $?knownP))
=>
    (if (> ?rp (length$ (intersection$ $?guessColors $?knownP))) then
        (bind $?indexes (create$ 0 1 2 3))
        (foreach ?i $?indexes
            (if (neq (nth$ (+ ?i 1) $?guessColors) (nth$ (+ ?i 1) $?knownP)) then
                (bind $?knownP (replace$ $?knownP (+ ?i 1) (+ ?i 1) (nth$ (+ ?i 1) $?guessColors)))
            )
        )
        (assert 
            (numeric-guess 
                (step ?s) 
                (numbers $?knownP)
            )
        )
    ) 
)