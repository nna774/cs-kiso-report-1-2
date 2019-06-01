import Prelude hiding (div, or, and)

type N = Integer
type B = Bool

or :: B -> B-> B
or True _ = True
or False x = x

and :: B-> B-> B
and True x = x
and False _ = False

eq :: N -> N -> B
eq = (==)

div :: N -> N -> B
div x y = y `mod` x == 0

prime :: N -> B
prime n = and (n >= 2) (prmall n n)

prm :: N -> N -> B
prm 0 _ = True
prm y x = or (not (div y x)) (eq y 1)

prmall :: N -> N -> B
prmall 0 x = True
prmall n x = and (prm (pred n) x) (prmall (pred n) x)

bp :: N -> N-> N
bp x y = if x > y then undefined else if prime y then y else bp x (pred y)

double :: N -> N
double 0 = 0
double n = 2 + double(pred n)

biggerprime :: N -> N
biggerprime 0 = 2
biggerprime 1 = 2
biggerprime n = (largestFactor.fact.smallerprime.pred.pred) n

smallerprime :: N -> N
smallerprime n = if prime (succ (succ n)) then (succ (succ n)) else smallerprime (pred n)

fact :: N -> N
fact 0 = 1
fact n = n * fact (pred n)

largestFactor :: N -> N
largestFactor 0 = 1
largestFactor n = largestFactorImpl (succ n) (succ n)

largestFactorImpl :: N -> N -> N
largestFactorImpl n d = if and (prime d) (div d n) then d else largestFactorImpl n (pred d)

main = print $ take 100 $ map biggerprime [0..]
