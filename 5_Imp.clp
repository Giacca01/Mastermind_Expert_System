(defmodule IMP (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

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
    (bind ?firstColor 1)
    (bind ?secondColor 2)
    (bind ?thirdColor 3)
    (bind ?fourthColor 4)
    (assert (numeric-guess (step ?s) (numbers ?firstColor ?secondColor ?thirdColor ?fourthColor)))
    (printout t "Pattern: fatto tentativo iniziale" crlf)
)

(defrule process-first-feedback
    (status (step ?s & 1) (mode computer))
    (numeric-guess (step ?s1 & 0) (numbers $?colors))
    (answer (step ?s1 &: 0) (right-placed ?rp) (miss-placed ?mp))
    ?phase <- (phase (name COLOR))
=>
    (assert (best-comb (step 0) (numbers $?colors) (right-placed ?rp) (miss-placed ?mp)))
    ; Uso la stessa logica di processamento dell'altro modulo
)

(defrule process-second-feedback
    (status (step ?s & 2) (mode computer))
    (numeric-guess (step ?s1 & 1) (numbers $?colors))
    (answer (step ?s1 & 1) (right-placed ?rp) (miss-placed ?mp))
    ?phase <- (phase (name COLOR))
    (best-comb (step ?sBest & 0) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
=>
    (if (> (+ ?mp ?rp) (+ ?rpBest ?mpBest)) then
        ; Il secondo tentativo è la best comb
    else
        ; Il secondo tentativo è la second best
    ) 
)

; La salience è impostata in modo che scatti prima di color feedback
(defrule compute-best-comb (declare (salience 10))
    (status (step ?s) (mode computer))
    ;ultima combinazione provata
    (numeric-guess (step ?s1 &: (= (- ?s 1) ?s1)) (numbers $?colors))
    (answer (step ?s1 &: (= (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
    ?phase <- (phase (name COLOR))
    ?bc <- (best-comb (step ?sBest) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
    ?sbc <- (second-best (step ?sSecond) (numbers $?numbersSecond) (right-placed ?rpSecond) (miss-placed ?mpSecond))
=>
    (bind ?feedbackSum (+ ?rp ?mp))
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
=>
    ; TODO: vedere quali valori usare (forse quelli della miglior combinazione??)
    (bind ?newColors (- 4 (+ ?rpBest ?mpBest)))
    (if (> (?newColors) 0) then
        ; devo provare ad inserire nuovi colori

        ; tengo fermi i colori prensenti nelle ultime due combinazioni

        ; duplico la miglior combinazione
        (bind $?newGuess (step ?s) (subseq$ $?numbersBest 1 4))
        (foreach ?number $?numbersSecond
            (if (not (member$ ?number $?numbersBest))
                ; ho trovato un colore che non compare nelle ultime due migliori combinazioni
                ; lo sostituisco con un altro estratto a caso
                ; TODO: evitare di rigenerare la stessa combinazione più volte
                (bind $?newColors (difference$ $?nc $?numbersBest))
                (bind ?newPos (random 1 4))
                (bind ?color (nth$ ?newPos $?newColors))
                (bind $?newGuess (replace$ $?newGuess ?newPos ?newPos ?color))
            )
        )
        (assert (numeric-guess (step ?s) (numbers $?newGuessColors)))
    )
)