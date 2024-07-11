(defmodule SWA (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

; Proviamo ad implementare la strategia suggerita
; da Swaszek, cioè:
; 1) Creare una lista di tutti i possibili codici segreti (nel nostro caso non hanno ripetizioni)
; 2) Si parte da una guess con 4 colori (nell'originale si partiva con blue blue green green)
; 3) Fino a che non trovo il codice segreto
;   4) Elimino tutti i codici segreti che non avrebbero fornito la risposta data dal sistema
;    per la risposta del passo precedente
;   5) Il primo dei codici rimasti è la nuova risposta

(deftemplate numeric-guess
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate candidate-secret-code
    (multislot code (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

; Numero di codici segreti rimasti
(deftemplate candidate-codes-number
    (slot val (type INTEGER))
)

(deftemplate general-counter
    (slot val (type INTEGER))
)

(deftemplate candidate-answer
    (multislot colors (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate color-codes
    (multislot codes (allowed-values 1 2 3 4 5 6 7 8))
)

(deftemplate color-numbers
    (multislot numbers)
)

(deftemplate phase
    (slot val (allowed-values CODES GUESS))
)

(deftemplate candidate-sol-counter
    (slot val (type INTEGER))
)

(deffacts initial
    (color-codes (codes (create$ blue green red yellow orange white black purple)))
    (candidate-codes-number (val 0))
    (phase (val CODES))
    (candidate-sol-counter (val 0))
)

(defrule generate-secret-codes
    (status (step ?s & 0) (mode computer))
    ?ph <- (phase (val CODES))
    ?ccn <- (candidate-codes-number (val ?ccnVal))
=>
    (bind ?i 1)
    (bind ?counter 0)
    (while (<= ?i 8)
        (bind ?j 1)
        (while (<= ?j 8)
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

; Potatura Insieme dei codici plausibili
(defrule remove-inconsistent (declare (salience 10))
    (status (step ?s) (mode computer))
    ?cs <- (candidate-secret-code (code $?codeColors))
    (answer (step ?s1 &: (= (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
    (numeric-guess (step ?s1 &: (= (- ?s 1) ?s1)) (numbers $?answerColors))
    ; Devo esserci ancora dei codici plausibili
    ?ccn <- (candidate-codes-number (val ?ccnVal &: (> ?ccnVal 0)))
    ; devono esserci dei codici non ancora controllati
    ?gc <- (general-counter (val ?gcVal &: (> ?gcVal 0)))
    (phase (val GUESS))
    ?candSolCounter <- (candidate-sol-counter (val ?candSolCounterVal))
=>
    ; ho controllato un codice in più
    (modify ?gc (val (- ?gcVal 1)))
    (bind $?indexesList (create$ 1 2 3 4))
    (bind ?rpCode 0)
    (bind ?mpCode 0)
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
            ; Codice inconsistente: lo elimino
            (retract ?cs)
            (modify ?ccn (val (- ?ccnVal 1)))
            ;(printout t "Risultati codice " ?rpCode " " ?mpCode crlf)
        else
            (if (= ?candSolCounterVal 0) then 
                ;(printout t "Risposta possibile: " $?codeColors crlf)
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
    ; Ho controllato tutti i codici
    ?gc <- (general-counter (val ?gcVal & 0))
    ?ca <- (candidate-answer (colors $?candidateColors))
    ?candSolCounter <- (candidate-sol-counter (val ?candSolCounterVal))
=>
    (modify ?gc (val ?ccnVal))
    (assert (numeric-guess (step ?s) (numbers $?candidateColors)))
    (modify ?candSolCounter (val 0))
    (retract ?ca)
)


(defrule numbers-to-colors (declare (salience 20))
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