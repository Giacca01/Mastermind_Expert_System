(defmodule IMP (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

; Struttura del fatto che modella la fase della strategia in corso
(deftemplate phase
    (slot name (allowed-values COLOR POSITION))
)

; Struttura del fatto che modella il generico tentativo di risposta dell'agente
; (necessariamente univoco)
(deftemplate numeric-guess
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

; Struttura del fatto che modella il generico tentativo candidato di risposta dell'agente
; (non per forza univoco)
(deftemplate candidate-guess
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

; Rappresentazione simbolica dei colori
(deftemplate color-codes
    (multislot codes (allowed-values blue green red yellow orange white black purple))
)

; Rappresentazione numerica dei colori
(deftemplate color-numbers
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8))
)

; Struttura del miglior tentativo di risposta
; (colori + feedback del sistema)
(deftemplate best-comb
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
    (slot right-placed (type INTEGER))
	(slot miss-placed (type INTEGER))
)

; Struttura del secondo miglior tentativo di risposta
; (colori + feedback del sistema)
(deftemplate second-best
    (slot step (type INTEGER))
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
    (slot right-placed (type INTEGER))
	(slot miss-placed (type INTEGER))
)

(deffacts initial 
    ; la prima fase della strategia è l'identificazione del colore
    (phase (name COLOR))
    (color-codes (codes blue green red yellow orange white black purple))
    (color-numbers (numbers 1 2 3 4 5 6 7 8))
)


; Primo tentativo del sistema: combinazione dei primi 4 colori
(defrule starting-guess
    (status (step ?s & 0) (mode computer))
    (phase (name COLOR))
=>
    (bind ?firstColor 1)
    (bind ?secondColor 2)
    (bind ?thirdColor 3)
    (bind ?fourthColor 4)
    (assert (numeric-guess (step ?s) (numbers ?firstColor ?secondColor ?thirdColor ?fourthColor)))
)

; Processamento della risposta data dal sistema al primo tentativo dell'agente
(defrule process-first-feedback
    (status (step ?s & 1) (mode computer))
    (numeric-guess (step ?s1 & 0) (numbers $?colors))
    (answer (step ?s1 & 0) (right-placed ?rp) (miss-placed ?mp))
    (color-numbers (numbers $?nc))
    ?phase <- (phase (name COLOR))
    ; La regola scatta solo se
    ; L'agente non ha ancora prodotto una risposta per il passo corrente
    (not (numeric-guess (step ?s)))
    (not (candidate-guess (step ?s)))
    (not (guess (step ?s)))
=>
    (assert (best-comb (step 0) (numbers $?colors) (right-placed ?rp) (miss-placed ?mp)))

    (if (< (+ ?rp ?mp) 4) then
        ; Caso in cui non ho ancora indovinato tutti i colori

        ; Evito di ripetere i colori già presenti nella risposta
        (bind $?newColors (difference$ $?nc $?colors))
        ; Introduco nuovi colori in numero pari a quelli errati
        (bind ?newColorsCounter (- 4 (+ ?rp ?mp)))
        ; Copia della risposta originale
        (bind $?newGuessColors (subseq$ $?colors 1 4))

        (while (> ?newColorsCounter 0)
            ; Estraggo a caso un colore...
            (bind ?colorPos (random 1 (length$ $?newColors)))
            (bind ?color (nth$ ?colorPos $?newColors))
            ; ...e lo inserisco nella risposta
            (bind ?newPos (random 1 4))
            (bind ?currColor (nth$ ?newPos $?newGuessColors))
            (bind $?newGuessColors (replace$ $?newGuessColors ?newPos ?newPos ?color))
            ; rimuovo il colore appena usato per evitare ripetizioni
            (bind $?newColors (delete-member$ $?newColors ?color))
            ; re-inserisco il colore che ho sostituito: non comparendo più nellla
            ; risposta, potrò usarlo in altre posizioni
            (bind $?newColors (insert$ $?newColors 1 ?currColor))
            (bind ?newColorsCounter (- ?newColorsCounter 1))
        )
        
        (assert (candidate-guess (step ?s) (numbers $?newGuessColors)))
    else
        ; Ho identificato tutti i colori: posso lavorare al corretto posizionamento
        (retract ?phase)
        (assert (phase (name POSITION)))
    )
)

; Regolazione dei due migliori tentativi
; a seguito della seconda risposta
; (La salience serve ad imporre che scatti prima di color-feedback)
(defrule process-second-feedback (declare (salience 10))
    (status (step 2) (mode computer))
    (numeric-guess (step 1) (numbers $?colors))
    (answer (step 1) (right-placed ?rp) (miss-placed ?mp))
    (phase (name COLOR))
    ?bc <- (best-comb (step 0) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
=>
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

; Aggiornamento del miglior tentativo fatto dal sistema
(defrule compute-best-comb (declare (salience 10))
    (status (step ?s) (mode computer))
    (numeric-guess (step ?s1 &: (= (- ?s 1) ?s1)) (numbers $?colors))
    (answer (step ?s1 &: (= (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
    (phase (name COLOR))
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

; Processamento dell'output del sistema per il tentativo i-esimo dell'agente
; e generazione del tentativo i+1-esimo
(defrule color-feedback
    (status (step ?s) (mode computer))
    (color-numbers (numbers $?nc))
    ?phase <- (phase (name COLOR))
    (best-comb (step ?sBest) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
    (second-best (step ?sSecond) (numbers $?numbersSecond) (right-placed ?rpSecond) (miss-placed ?mpSecond))

    ; La regola scatta soltanto se non sono ancora stati fatti
    ; tentativi al passo corrente
    (not (numeric-guess (step ?s)))
    (not (candidate-guess (step ?s)))
    (not (guess (step ?s)))
=>

    (bind ?newColorsCounter (- 4 (+ ?rpBest ?mpBest)))
    
    (if (> ?newColorsCounter 0) then
        ; devo provare ad inserire nuovi colori
        ; duplico la miglior combinazione
        (bind $?newGuess (subseq$ $?numbersBest 1 4))
        ; evito di ripetere i colori già presenti
        ; nella miglior combinazione
        (bind $?newColors (difference$ $?nc $?numbersBest))
        (bind ?currIndex 1)
        (foreach ?number $?numbersSecond
            ; Cambio massimo newColorsCounter colori
            (if (> ?newColorsCounter 0) then
                ; tengo fermi i colori prensenti nelle ultime due combinazioni
                (if (not (member$ ?number $?numbersBest)) then
                    ; ho trovato un colore che non compare nelle ultime due migliori combinazioni
                    ; lo sostituisco con un altro estratto a caso
                    
                    ; Copia del colore che sto per sostituire
                    (bind ?currColor (nth$ ?currIndex $?newColors))

                    ; Estrazione Nuovo colore da inserire
                    (bind ?newPos (random 1 (length$ $?newColors)))
                    (bind ?color (nth$ ?newPos $?newColors))

                    (bind $?newGuess (replace$ $?newGuess ?currIndex ?currIndex ?color))
                    (bind $?newColors (delete-member$ $?newColors ?color))
                    (bind ?newColorsCounter (- ?newColorsCounter 1))
                    ; Re-inserisco il colore tolto dal tentativo
                    (bind $?newColors (insert$ $?newColors 1 ?currColor))
                )
            )
            (bind ?currIndex (+ 1 ?currIndex))
        )
        (assert (candidate-guess (step ?s) (numbers $?newGuess)))
    else
        ; Ho indovinato i colori: passo a provare le posizioni
        (retract ?phase)
        (assert (phase (name POSITION)))
    )
)

; Calcolo della miglior combinazione durante
; la fase di adeguamento delle posizioni
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

; Affinamento del pattern tramite adeguamento delle posizioni
(defrule refine-pattern
    (status (step ?s) (mode computer))
    (best-comb (step ?sBest) (numbers $?numbersBest) (right-placed ?rpBest) (miss-placed ?mpBest))
    (phase (name POSITION))
    (not (numeric-guess (step ?s)))
    (not (candidate-guess (step ?s)))
    (not (guess (step ?s)))
=>
    ; Da qui in poi la somma di rp ed mp fa sempre 4
    ; quindi il tie breaker diventa rp
    (if (< ?rpBest 4) then
        ; Ci sono ancora delle posizioni da sistemare
        ; Copio la miglior combinazione
        (bind $?newGuess (subseq$ $?numbersBest 1 4))

        ; Estraggo le due posizioni da scambiare
        (bind ?firstPos (random 1 4))
        (bind ?secondPos (random 1 4))

        ; Eseguo lo scambio tra colori
        (bind ?tmp (nth$ ?firstPos $?newGuess))
        (bind $?newGuess (replace$ $?newGuess ?firstPos ?firstPos (nth$ ?secondPos $?newGuess)))
        (bind $?newGuess (replace$ $?newGuess ?secondPos ?secondPos ?tmp))
        (assert (candidate-guess (step ?s) (numbers $?newGuess)))
    )
)

; Verifico che la risposta candidata non sia uguale
; ad una già data in precedenza e la sottopongo al sistema
(defrule check-repetition
    (status (step ?s) (mode computer))
    ?cg <- (candidate-guess (step ?s) (numbers $?newGuess))
    ; non ci deve essere una risposta già data uguale
    ;(not (numeric-guess (step ?s1) (numbers $?newGuess)))
=>
    (retract ?cg)
    (assert (numeric-guess (step ?s) (numbers $?newGuess)))
)

; Qualora la risposta candidata sia uguale ad una già data
; ne blocco la sottoposizione, imponendo di rigenerarla
(defrule signal-repetition
    (status (step ?s) (mode computer))
    ?cg <- (candidate-guess (step ?s) (numbers $?newGuess))
    ; non ci deve essere una risposta già data uguale
    (numeric-guess (step ?s1) (numbers $?newGuess))
=>
    (retract ?cg)
)

; Conversione del tentativo dell'agente da forma numerica a forma simbolica
(defrule numbers-to-colors
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