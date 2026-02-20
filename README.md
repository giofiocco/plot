# Plot

`(plot expr)`: to plot the expression
`(def (f a b ...) body)`: to define a function called `f` with args `a`, `b` etc and its body
`(def a expr)`: to define a symbol `a` to alias the expression

examples:

(def (f a b) (\* a b))
(plot (f (- x 2) y))

(plot (^ 2 x))
(plot (= (+ (^ 2 x) (^ 2 y)) 2))
(plot (& (< (+ (^ 2 x) (^ 2 y)) 1) (< (+ (^ 2 (- x 1)) (^ 2 y)) 1)))

(taylor x0 2 f)

# TODOs

- (def (f o) (o 1 2)) or something
