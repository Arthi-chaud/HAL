(cond
	((eq? 'foo (car '(foo bar))) 'here)
	((eq? 1 2) 'there)
	(#t 'nope)
)
