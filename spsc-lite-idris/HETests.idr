module HETests

import Test.Unit

import SLanguage
import SParsers
import HE

vaTrue : String -> IO Bool
vaTrue input =
  assertEquals (aVarIsUnderAttack <$> parseExp input) (Just True)

vaFalse : String -> IO Bool
vaFalse input =
  assertEquals (aVarIsUnderAttack <$> parseExp input) (Just False)

va_x : IO Bool
va_x = vaTrue "x"

va_A : IO Bool
va_A = vaFalse "A"

va_fx : IO Bool
va_fx = vaFalse "f(x)"

va_gxy : IO Bool
va_gxy = vaTrue "g(x,y)"

va_g1g2x : IO Bool
va_g1g2x = vaTrue "g1(g2(x))"

va_gA : IO Bool
va_gA = vaFalse "g(A())"

va_gfx : IO Bool
va_gfx = vaFalse "g(f(x))"

--

heTrue : String -> String -> IO Bool
heTrue a1 a2 =
  assertEquals (he <$> parseExp a1 <*> parseExp a2) (Just True)

heFalse : String -> String -> IO Bool
heFalse a1 a2 =
  assertEquals (he <$> parseExp a1 <*> parseExp a2) (Just False)

heVV : IO Bool
heVV = heTrue "v1" "v2"

heVF : IO Bool
heVF = heTrue "v1" "f(v2)"

heFV : IO Bool
heFV = heFalse "f(v2)" "v1"

heDiving : IO Bool
heDiving = heTrue "f(v1)" "g(v0, f(H(v2)))"

heCoupling1 : IO Bool
heCoupling1 = heTrue "f(v1, g(v2))" "f(H(w1), g(w2))"

heCoupling2 : IO Bool
heCoupling2 = heFalse "f(v1)" "g(w1)"

export
allTests : IO ()
allTests = runTests
  [ va_x
  , va_A
  , va_fx
  , va_gxy
  , va_g1g2x
  , va_gA
  , va_gfx
  , heVV
  , heVF
  , heFV
  , heDiving
  , heCoupling1
  , heCoupling2
  ]
