module ShowUtil where

showParams :: [String] -> String

showParams [] = ""
showParams (x : xs) = x ++ showParamsTail xs

showParamsTail :: [String] -> String

showParamsTail [] = ""
showParamsTail xs@(_ : _) = "," ++ showParams xs

showArgs :: (Show t) => [t] -> String

showArgs [] = "()"
showArgs (x : xs) = "(" ++ show x ++ showArgsTail xs ++ ")"

showArgsTail :: (Show t) => [t] -> String

showArgsTail [] = ""
showArgsTail (x : xs) = "," ++ show x ++ showArgsTail xs

showBindings :: (Show a) => [(String, a)] -> String

showBindings [] = ""
showBindings ((v, e) : xs) = v ++ "=" ++ show e ++ showBindingsTail xs

showBindingsTail :: (Show a) => [(String,a)] -> String

showBindingsTail [] = ""
showBindingsTail xs@(_ : _) = "," ++ showBindings xs

showPat :: String -> [String] -> String

showPat cname [] = cname
showPat cname cparams =
  cname ++ "(" ++ showParams cparams ++ ")"
    