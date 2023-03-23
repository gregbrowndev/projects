-- This is a simple calculator program in Haskell.

-- This function takes a string representing an equation and returns the result of
-- the equation as a Double.
-- The equation should be of the form "x op y" where x and y are numbers and op is
-- either + or * (for addition or multiplication, respectively).
-- If the equation is not of this form, the function returns 0.
calculate :: String -> Double
calculate equation =
  let
    -- Split the equation into a list of words.
    words = words equation
  in
    case words of
      -- If the list has three elements, try to parse the first and third elements
      -- as numbers, and perform the operation specified by the second element.
      [x, op, y] ->
        case (reads x, op, reads y) of
          -- If the parsing succeeds, perform the operation and return the result.
          ((x', ""), "+", (y', "")) -> x' + y'
          ((x', ""), "*", (y', "")) -> x' * y'
          -- Otherwise, return 0.
          _                         -> 0

      -- If the list has any other number of elements, return 0.
      _ -> 0

main :: IO ()
main = do
  -- Read a line of input from the user.
  line <- getLine

  -- Calculate the result of the equation.
  let result = calculate line

  -- Print the result.
  putStrLn $ show result
