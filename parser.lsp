(setq example1 "france(country,europe).")
(setq example2 "country(france,europe).")
(setq example3 "spain is a country in europe")

(define (pre-parsed-human input) 
	(setq temp (parse input "(")) 
	(list (first temp) (first(parse (last temp) "\)."))))

(define (oneisa2in3-human input) 
	(apply (fn (x y) (string x " is a " (first (parse y ",")) " in " (nth 1 (parse y ",")))) 
	(pre-parsed-human input)))

(define (twoisa1in3-human input) 
	(apply (fn (x y) (string (first (parse y ",")) " is a " x " in " (nth 1 (parse y ",")))) 
	(pre-parsed-human input)))

(define (human-oneisa2in3 input) 
	(apply (fn (pred-name y) (string pred-name "(" (replace " in " y ",")")") ) 
	(isa? input)))

(define (human-twoisa1in3 input) 
	(apply (fn (x y) (string (first(setq parsed (parse y " in "))) "(" x "," (nth 1 parsed)")" )) 
	(isa? input)))

; human-spql, prolog-spql, human-grammar-check, sroiqd-prolog?, human-sroiqd?, ...

(define (isa? input) 
	(if (find "is a" input)
		(parse input " is a ")))

; builder-function which replaces e.g. 'is a' with 'has a' or 'in' with something else
; generalizer which replaces the noun(s) with others
; catching synonyms
