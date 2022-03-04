;
; (car ''abracadabra)
; (car (quote 'abracadabra))
; (car (quote (quote abracadabra)))
; (car (quote abracadabra))
; quote
;
; NOTE: only outermost quote expression gets evaluated and applied

(car ''abracadabra)
