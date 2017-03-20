module Complex where 

data Complex a = a :+ a

instance Num a => Num (Complex a) where 
    (a:+b) + (c:+d) = (a+c):+(b+d)
    negate (a:+b) = negate a :+ negate b
    abs (a:+b) = abs a :+ abs b
    signum (a:+b) = signum a :+ signum b
    fromInteger n = fromInteger n :+ fromInteger  n

magnitude :: (Floating a) => Complex a -> a
magnitude (a:+b) = sqrt(a^2 + b^2)

square :: (Num a) => Complex a -> Complex a
square (a:+b) = r :+ i
    where r = a^2 - b^2
          i = 2*a*b

toPair :: Complex a -> (a,a)
toPair (a:+b) = (a,b)

fromPair :: (a,a) -> Complex a
fromPair (a, b) = a:+b 