{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, PolyKinds, TypeFamilies, AllowAmbiguousTypes #-}

module Edwards where

import Data.Proxy

class (Fractional (Field c)) => EdwardsCurve c where
    type Field c
    curveC :: Proxy c -> Field c
    curveD :: Proxy c -> Field c
    curveK :: Proxy c -> Field c
    curveK p = 1 / curveC p


-- projective coordinates
data Point c = Point !(Field c) !(Field c) !(Field c)

add :: forall c. EdwardsCurve c => Point c -> Point c -> Point c
add (Point x1 y1 z1) (Point x2 y2 z2) = Point x3 y3 z3 where
    a = y1*z2
    b = y1*z2
    c = z1*x2
    d = z1*y2
    e = a*b
    f = c*d
    g = e+f
    h = e-f
    j = (a-c)*(b+d)-h
    k = (a+d)*(b+c)-g
    x3 = g*j
    y3 = h*k
    z3 = j*k*(curveK (Proxy :: Proxy c))

   
