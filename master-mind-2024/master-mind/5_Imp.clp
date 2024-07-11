; TODO: miglioramenti possibili
;   1) evitare di rigenerare la stessa combinazione
;   2) evitare di sparare le posizioni a random, tenendo invece conto
;   anche qui del tentativo precedente

(defmodule IMP (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

(deftemplate phase
    (slot name (allowed-values COLOR POSITION))
)

(deftemplate comb-state
    (slot state (allowed-values RECOMPUTE OK))
)

(deftemplate numeric-guess
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate candidate-guess
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate color-codes
    (multislot codes)
)

(deftemplate color-numbers
    (multislot numbers)
)

(deftemplate best-comb
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
    (slot right-placed (type INTEGER))
	(slot miss-placed (type INTEGER))
)

(deftemplate second-best
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
    (slot right-placed (type INTEGER))
	(slot miss-placed (type INTEGER))
)

(deftemplate prev-seed
    (slot seed (type INTEGER))
)

; inizialmente non ho identificato alcun colore
(deffacts initial 
    (phase (name COLOR))
    (color-codes (codes blue green red yellow orange white black purple))
    (color-numbers (numbers 1 2 3 4 5 6 7 8))
    (prev-seed (seed 0))
)

(defrule starting-guess
    (strategy-type (name Imp))
    (status (step ?s & 0) (mode computer))
    (phase (name COLOR))
=>
    (bind ?firstColor 1)
    (bind ?secondColor 2)
    (bind ?thirdColor 3)
    (bind ?fourthColor 4)
    (assert (numeric-guess (step ?s) (numbers ?firstColor ?secondColor ?thirdColor ?fourthColor)))
    (assert (comb-state (state RECOMPUTE)))
)

(defrule process-first-feedback
    (status (step ?s & 1) (mode computer))
    (numeric-guess (step ?s1 & 0) (numbers $?colors))
    (answer (step ?s1 & 0) (right-placed ?rp) (miss-placed ?mp))
    (color-numbers (numbers $?nc))
    ?phase <- (phase (name COLOR))
    (not (numeric-guess (step ?s) (numbers $?nc)))
    (not (candidate-guess (step ?s) (numbers $?cc)))
    (not (guess (step ?s) (g $?cg)))
    ?pv <- (prev-seed (seed ?sd))
=>
    (modify ?pv (seed (+ ?sd 1)))
    (seed ?sd)
    (assert (best-comb (step 0) (numbers $?colors) (right-placed ?rp) (miss-placed ?mp)))
    (assert (second-best (step 0) (numbers $?colors) (right-placed ?rp) (miss-placed ?mp)))
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
            (assert (candidate-guess (step ?s) (numbers $?newGuessColors)))
        else
            ; Ho identificato tutti i colori: non ne introduco altri, ma faccio uno shuffling
            ; delle posizioni

            ; TODO: implementare shuffling
            (retract ?phase)
            (assert (phase (name POSITION)))
        )
    )
)

; Salience regolata in modo che scatti prima di color-feedback
(defrule process-second-feedback (declare (salience 10))
    (status (step ?s & 2) (mode computer))
    (numeric-guess (step ?s1 & 1) (numbers $?colors))
    (answer (step ?s1 & 1) (right-placed ?rp) (miss-placed ?mp))
    (phase (name COLOR))
    ?bc <- (best-comb (step ?sBest & 0) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
=>
    ; TODO: rivedere, non mi convince molto
    (if (> (+ ?mp ?rp) (+ ?rpBest ?mpBest)) then
        ; Il secondo tentativo è la best comb del giro precedente
        (retract ?bc)
        (assert (second-best (step 0) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest)))
        (assert (best-comb (step 1) (numbers $?colors) (right-placed ?rp) (miss-placed ?mp)))
    else
        ; Il secondo tentativo è la second best
        (assert (second-best (step 1) (numbers $?colors) (right-placed ?rp) (miss-placed ?mp)))
    ) 
)

; La salience è impostata in modo che scatti prima di color feedback
(defrule compute-best-comb (declare (salience 10))
    (status (step ?s) (mode computer))
    ;ultima combinazione provata
    (numeric-guess (step ?s1 &: (= (- ?s 1) ?s1)) (numbers $?colors))
    (answer (step ?s1 &: (= (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
    (phase (name COLOR))
    ?bc <- (best-comb (step ?sBest) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
    ?sbc <- (second-best (step ?sSecond) (numbers $?numbersSecond) (right-placed ?rpSecond) (miss-placed ?mpSecond))
=>
    (bind ?feedbackSum (+ ?rp ?mp))
    (printout t "Valore mossa: " ?feedbackSum crlf)
    (if (> ?feedbackSum (+ ?rpBest ?mpBest)) then
        (retract ?sbc)
        (assert (second-best (step ?sBest) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest)))
        (retract ?bc)
        (assert (best-comb (step ?s1) (numbers $?colors) (right-placed ?rp) (miss-placed ?mp)))
    )
)

(defrule color-feedback
    (status (step ?s) (mode computer))
    ;ultima combinazione provata
    (color-numbers (numbers $?nc))
    ?phase <- (phase (name COLOR))
    ; miglior combinazione prodotta fino a questo momento
    (best-comb (step ?sBest) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
    ; seconda miglior combinazione
    (second-best (step ?sSecond) (numbers $?numbersSecond) (right-placed ?rpSecond) (miss-placed ?mpSecond))

    ; DEBUG ONLY
    (color-codes (codes $?colorsDebug))

    (not (numeric-guess (step ?s) (numbers $?nc)))
    (not (candidate-guess (step ?s) (numbers $?cc)))
    (not (guess (step ?s) (g $?cg)))
    ?pv <- (prev-seed (seed ?sd))
=>

    (modify ?pv (seed (+ ?sd 1)))
    (seed ?sd)

    ; TODO: vedere quali valori usare (forse quelli della miglior combinazione??)
    (bind ?firstColor (nth$ (nth$ 1 $?numbersBest) $?colorsDebug))
    (bind ?secondColor (nth$ (nth$ 2 $?numbersBest) $?colorsDebug))
    (bind ?thirdColor (nth$ (nth$ 3 $?numbersBest) $?colorsDebug))
    (bind ?fourthColor (nth$ (nth$ 4 $?numbersBest) $?colorsDebug))
    (printout t "Miglior combinazione: " ?firstColor " " ?secondColor " " ?thirdColor " " ?fourthColor crlf)

    (bind ?firstColor (nth$ (nth$ 1 $?numbersSecond) $?colorsDebug))
    (bind ?secondColor (nth$ (nth$ 2 $?numbersSecond) $?colorsDebug))
    (bind ?thirdColor (nth$ (nth$ 3 $?numbersSecond) $?colorsDebug))
    (bind ?fourthColor (nth$ (nth$ 4 $?numbersSecond) $?colorsDebug))
    (printout t "Seconda Miglior combinazione: " ?firstColor " " ?secondColor " " ?thirdColor " " ?fourthColor crlf)



    (bind ?newColorsCounter (- 4 (+ ?rpBest ?mpBest)))
    
    (if (> ?newColorsCounter 0) then
        ; devo provare ad inserire nuovi colori

        ; tengo fermi i colori prensenti nelle ultime due combinazioni

        ; duplico la miglior combinazione
        (bind $?newGuess (subseq$ $?numbersBest 1 4))
        (bind ?currIndex 1)
        (foreach ?number $?numbersSecond
            ; Cambio massimo newColorsCounter colori
            (if (> ?newColorsCounter 0) then
                (if (not (member$ ?number $?numbersBest)) then
                    ; ho trovato un colore che non compare nelle ultime due migliori combinazioni
                    ; lo sostituisco con un altro estratto a caso
                    ; TODO: evitare di rigenerare la stessa combinazione più volte
                    (bind $?newColors (difference$ $?nc $?numbersBest))
                    (bind $?newColors (difference$ $?newColors $?newGuess))
                    (bind ?newPos (random 1 (length$ $?newColors)))
                    ; Nuovo colore da inserire
                    (bind ?color (nth$ ?newPos $?newColors))
                    (bind $?newGuess (replace$ $?newGuess ?currIndex ?currIndex ?color))
                    (bind ?newColorsCounter (- ?newColorsCounter 1))
                )
            )
            (bind ?currIndex (+ 1 ?currIndex))
        )
        (assert (candidate-guess (step ?s) (numbers $?newGuess)))
    else
        ; Ho indovinato i colori: passo a provare le posizioni
        ; Idea: provo a sfruttare il feedback, scambiando i colori
        ; per un numero di volte pari a 4 - missPlaced
        ; e provando a tenere fermi i colori presenti nelle ultime due migliori combinazioni
        (retract ?phase)
        (assert (phase (name POSITION)))

        ; TODO: verificare se stampi la lista aggiornata
        (printout t "Colori identificati" crlf)
    )
)

(defrule numbers-to-colors
    (numeric-guess (step ?s) (numbers $?numbers))
    ; TODO: sarebbe carino toglierlo
    ; per migliorare l'efficienza
    (status (step ?s) (mode computer))
    (color-codes (codes $?colors))
=>
    (print t "Combinazione generata: " $?numbers " " crlf)
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

(defrule update-best-pattern (declare (salience 10))
    ?bc <- (best-comb (step ?sBest) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
    (numeric-guess (step ?s) (numbers $?newGuess))
    (phase (name POSITION))
    (answer (step ?s1 &: (= (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
=>
    (if (> ?rp ?rpBest) then 
        (retract ?bc)
        (assert (best-comb (step ?s) (numbers $?newGuess) (right-placed ?rp) (miss-placed ?mp)))
    )
)

(defrule refine-pattern
    (status (step ?s) (mode computer))
    (best-comb (step ?sBest) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
    (phase (name POSITION))
    (not (numeric-guess (step ?s) (numbers $?nc)))
    (not (candidate-guess (step ?s) (numbers $?cc)))
    (not (guess (step ?s) (g $?cg)))
    ?pv <- (prev-seed (seed ?sd))
=>
    (modify ?pv (seed (+ ?sd 1)))
    (seed ?sd)
    ; Da qui in poi la somma di rp ed mp fa sempre 4
    ; quindi il tie breaker diventa rp
    (if (< ?rpBest 4) then
        (bind ?swapCounter (- 4 ?rpBest))
        ; Copio la miglior combinazione
        (bind $?newGuess (subseq$ $?numbersBest 1 4))
        (bind $?indexesList (create$ 1 2 3 4))

        (bind ?firstPos (random 1 4))
        (bind ?secondPos (random 1 4))
        (bind ?tmp (nth$ ?firstPos $?newGuess))
        (bind $?newGuess (replace$ $?newGuess ?firstPos ?firstPos (nth$ ?secondPos $?newGuess)))
        (bind $?newGuess (replace$ $?newGuess ?secondPos ?secondPos ?tmp))
        ;(foreach ?i $?indexesList
        ;    (if (<= ?i ?swapCounter) then
        ;        (bind ?firstPos (random 1 4))
        ;        (bind ?secondPos (random 1 4))
        ;        (bind ?tmp (nth$ ?firstPos $?newGuess))
        ;        (bind $?newGuess (replace$ $?newGuess ?firstPos ?firstPos (nth$ ?secondPos $?newGuess)))
        ;        (bind $?newGuess (replace$ $?newGuess ?secondPos ?secondPos ?tmp))
        ;    )
        ;)
        (assert (candidate-guess (step ?s) (numbers $?newGuess)))
    )
)

(defrule check-repetition
    (status (step ?s) (mode computer))
    ?cg <- (candidate-guess (step ?s) (numbers $?newGuess))
    ; non ci deve essere una risposta già data uguale
    (not (numeric-guess (step ?s1) (numbers $?newGuess)))
=>
    (retract ?cg)
    (assert (numeric-guess (step ?s) (numbers $?newGuess)))
)

(defrule signal-repetition
    (status (step ?s) (mode computer))
    ?cg <- (candidate-guess (step ?s) (numbers $?newGuess))
    ; non ci deve essere una risposta già data uguale
    (numeric-guess (step ?s1) (numbers $?newGuess))
=>
    (retract ?cg)
)