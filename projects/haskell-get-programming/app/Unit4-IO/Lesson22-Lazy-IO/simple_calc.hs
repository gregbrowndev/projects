import Data.List.Split

{-
Questions
  Q22.1: Write a program, simple_calc.hs, that reads simple equations involving
  adding two numbers or multiplying two numbers. The program should solve the
  equation each user types into each line as each line is entered.
-}

data Operator = Add | Mul
data Equation = Equation Int Operator Int

solve :: Equation -> Int
solve (Equation l Add r) = l + r
solve (Equation l Mul r) = l * r

toEquations :: [Char] -> [Equation]
toEquations = map toEquation . lines

toEquation :: String -> Equation
toEquation line = Equation left operator right
   where tokens   = splitOn " " line
         left     = read (tokens !! 0)
         operator = if (tokens !! 1) == "+"
                    then Add
                    else Mul
         right    = read (tokens !! 2)

main :: IO ()
main = do
  inputs <- getContents
  let equations = toEquations inputs
  mapM_ (print . solve) equations

{-
  $ stack exec -- ghc -o .stack-work/simple_calc.hs ./app/Unit4-IO/Lesson22-Lazy-IO/simple_calc.hs
  $ .stack-work/simple_calc.hs
-}
