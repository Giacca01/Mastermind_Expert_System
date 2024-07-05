; Modulo che implementa la prima strategia di gioco, detta "pattern based"

(defmodule PATTERN (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

; Iniziamo ad implementare 4 tentativi per indovinare i colori
(deftemplate identified-colors
    ; TODO: serve la storia??
    (multislot colors (allowed-values blue green red yellow orange white black purple))
)

(deftemplate numeric-guess
    (multislot numbers (allowed-values 1 2 3 4 5 6 7 8))
)

; inizialmente non ho identificato alcun colore
(deffacts initial 
    (identified-colors (colors nil))
)

(defrule starting-guess
    (status (step ?s & 0))
=>
    ; In teoria per rispondere io devo
    ; solo costruire guess, asserirla, e poi spostare il focus su game
    (assert numeric-guess (numbers (1 1 1 1)))
)

; regola che processa l'output del sistema
; per stabilire quali colori siano stati individuati
(defrule color-feedback
    ; associo domanda e risposta tramite il
    ; valore di s
    (guess (step ?s) (g ?colors))
    (answer (step ?s) (right-placed ?rp) (miss-placed ?mp))
    ?ifColorsFact <- (identified-colors (colors ?idColors))
=>
    (if (> (+ ?rp ?mp) 0) then
        ; remainder: tutti i valori di colors sono uguali
        (bind ?correctColor (nth$ 1 ?colors))
        (if (not (member$ ?correctColor ?idColors)) then
            (modify ?ifColorsFact (?idColors (insert$ ?idColors (+ (length$ ?idColors) 1) ?correctColor)))
        )
    )

    ; TODO: verificare se stampi la lista aggiornata
    (printout t "Identified colors: " ?idColors crlf)
)


(defrule next-color
    ; se non ho ancora identificato almeno 4 colori
    (identified-color (colors ?idColors&:(< length$ ?idColors 4)))
    (status (step ?s&:(< ?s 4)))
=>
    ; TODO: rendere generale
    (bind ?colors (create$ blue green red yellow orange white black purple))
    ; recupero il primo colore non ancora identificato
    (foreach ?color ?colors
        (if (not (member$ ?color ?idColors)) then
            (assert numeric-guess (?color ?color ?color ?color))
        )
    )
)


(defrule numbers-to-colors
    (numeric-guess (numbers ?numbers))
    ; TODO: sarebbe carino toglierlo
    ; per migliorare l'efficienza
    (status (step ?s))
=>
    (bind ?colors (create$ blue green red yellow orange white black purple))
    (assert (guess (step ?s) (g 
            (nth$(nth$ 1 ?number) ?colours)
            (nth$(nth$ 2 ?number) ?colours)
            (nth$(nth$ 3 ?number) ?colours)
            (nth$(nth$ 4 ?number) ?colours)
        )
    ))
    ; TODO: stampre la risposta tradotta
    (printout t "Guess: " ?numbers " at step: " ?s crlf)
    ; restituisco il controllo a game, in modo che processi la risposta
    ; TODO: vedere se serva davvero
    (pop focus)
)