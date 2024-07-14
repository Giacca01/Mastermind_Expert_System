(defmodule SWA (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

(deftemplate numeric-guess
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

; Struttura del potenziale codice segreto
(deftemplate candidate-secret-code
    (multislot code (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

; Numero di codici segreti plausibili rimasti
(deftemplate candidate-codes-number
    (slot val (type INTEGER))
)

; Numero di codici segreti ancora da processare durante la potatura
(deftemplate general-counter
    (slot val (type INTEGER))
)

; Struttura di una potenziale sottoposizione dell'agente al sistema
(deftemplate candidate-answer
    (multislot colors (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate color-codes
    (multislot codes (allowed-values 1 2 3 4 5 6 7 8))
)

(deftemplate phase
    (slot val (allowed-values CODES GUESS))
)

; Flag che indica la presenza di un codice candidato
; da usare come soluzione al passo i-esimo
(deftemplate candidate-sol-counter
    (slot val (allowed-values 0 1))
)

(deffacts initial
    (color-codes (codes (create$ blue green red yellow orange white black purple)))
    (candidate-codes-number (val 0))
    (phase (val CODES))
    (candidate-sol-counter (val 0))
)

; Generazione dell'elenco dei potenziali codici segreti
(defrule generate-secret-codes
    (status (step ?s & 0) (mode computer))
    ?ph <- (phase (val CODES))
    ?ccn <- (candidate-codes-number (val ?ccnVal))
=>
    (bind ?i 1)
    (bind ?counter 0)
    ; Il codice è composto da 4 cifre, ciascuna compresa tra 1 ed 8
    (while (<= ?i 8)
        (bind ?j 1)
        (while (<= ?j 8)
            ; mi assicuro che le cifre siano diverse tra di loro
            (if (neq ?i ?j) then
                (bind ?k 1)
                (while (<= ?k 8)
                    (if (and (neq ?i ?k) (neq ?k ?j)) then
                        (bind ?l 1)
                        (while (<= ?l 8)
                            (if (and (and (neq ?i ?l) (neq ?l ?j)) (neq ?k ?l)) then
                                (assert (candidate-secret-code (code ?i ?j ?k ?l)))
                                (bind ?counter (+ 1 ?counter))
                            )
                            (bind ?l (+ ?l 1))
                        )
                    )
                    (bind ?k (+ ?k 1))
                )
            )
            (bind ?j (+ ?j 1))
        )
        (bind ?i (+ ?i 1))
    )
    (modify ?ccn (val ?counter))
    (retract ?ph)
    (assert (phase (val GUESS)))
    (assert (general-counter (val ?counter)))
)

; Tentativo iniziale con 4 colori tutti diversi
(defrule starting-guess
    (status (step ?s & 0) (mode computer))
    (phase (val GUESS))
=>
    (bind ?firstColor 1)
    (bind ?secondColor 2)
    (bind ?thirdColor 3)
    (bind ?fourthColor 4)
    (assert (numeric-guess (step ?s) (numbers ?firstColor ?secondColor ?thirdColor ?fourthColor)))
)

; Potatura Insieme dei codici plausibili in base a risposta sistema
(defrule remove-inconsistent (declare (salience 10))
    (status (step ?s) (mode computer))
    ?cs <- (candidate-secret-code (code $?codeColors))
    (answer (step ?s1 &: (= (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
    (numeric-guess (step ?s1 &: (= (- ?s 1) ?s1)) (numbers $?answerColors))
    ; Devo esserci ancora dei codici segreti plausibili
    ?ccn <- (candidate-codes-number (val ?ccnVal &: (> ?ccnVal 0)))
    ; devono esserci dei codici non ancora controllati durante la corrente fase di potatura
    ?gc <- (general-counter (val ?gcVal &: (> ?gcVal 0)))
    (phase (val GUESS))
    ?candSolCounter <- (candidate-sol-counter (val ?candSolCounterVal))
=>
    ; ho controllato un codice in più
    (modify ?gc (val (- ?gcVal 1)))
    (bind $?indexesList (create$ 1 2 3 4))
    (bind ?rpCode 0)
    (bind ?mpCode 0)

    ; Calcolo la risposta che avrei ottenuto se ?cs fosse davvero il codice segreto
    (foreach ?i $?indexesList
        ; Se l'i-esimo colore del codice è uguale all'i-esimo della risposta
        ; ho un right placed
        (if (eq (nth$ ?i $?codeColors) (nth$ ?i $?answerColors)) then
            (bind ?rpCode (+ 1 ?rpCode))
        else
            (if (member$ (nth$ ?i $?codeColors) $?answerColors) then
                (bind ?mpCode (+ 1 ?mpCode))
            )
        )

    )

    (if (or (neq ?rpCode ?rp) (neq ?mpCode ?mp)) then 
        ; Codice inconsistente con la risposta effettivamente ottenuta: lo elimino
        (retract ?cs)
        (modify ?ccn (val (- ?ccnVal 1)))
    else
        (if (= ?candSolCounterVal 0) then 
            ; uso il primo codice che va bene come risposta
            (assert (candidate-answer (colors $?codeColors)))
            (modify ?candSolCounter (val 1))
        )
    )
)

; Sottopongo il codice scelto durante la potatura
(defrule reset-general-counter
    (status (step ?s) (mode computer))
    ?ccn <- (candidate-codes-number (val ?ccnVal))
    ; Ho controllato tutti i codici...
    ?gc <- (general-counter (val ?gcVal & 0))
    ; e c'è n'è almeno uno compatibile da usare come risposta
    ?ca <- (candidate-answer (colors $?candidateColors))
    ?candSolCounter <- (candidate-sol-counter (val ?candSolCounterVal))
=>
    (modify ?gc (val ?ccnVal))
    (assert (numeric-guess (step ?s) (numbers $?candidateColors)))
    (modify ?candSolCounter (val 0))
    (retract ?ca)
)

; Conversione della risposta al formato simbolico
(defrule numbers-to-colors (declare (salience 20))
    (numeric-guess (step ?s) (numbers $?numbers))
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
    
    (printout t "Guess: " ?firstColor " " ?secondColor " " ?thirdColor " " ?fourthColor " at step: " ?s crlf)
    ; restituisco il controllo a game, in modo che processi la risposta
    (pop-focus)
)