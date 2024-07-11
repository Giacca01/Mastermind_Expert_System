;; messo qui in isolamente proprio per semplificare la generazione del codice segreto
;; in modo che si possa debuggare facilmente
;; scommetando la prima riga possiamo fissare il codice segreto

(deffacts secret-code 
  ;(secret-code (code white green black purple))
  (random)
 )


