import qualified Data.Map as Map
import qualified Data.Set as Set

data Colour = Red | Blue | Yellow | Purple | Orange | Green | Brown | White
  deriving (Show, Eq)

instance Semigroup Colour
  where
    (<>) White rhs   = rhs
    (<>) lhs White   = lhs
    (<>) Red Blue    = Purple
    (<>) Blue Red    = Purple
    (<>) Red Yellow  = Orange
    (<>) Yellow Red  = Orange
    (<>) Blue Yellow = Green
    (<>) Yellow Blue = Green
    (<>) lhs rhs     = if lhs == rhs then lhs else Brown

instance Monoid Colour
  where
    mempty = White

-- Write a generic function to merge to maps together where the values
-- are instances of Semigroup, e.g. Colour

map1 = Map.fromList [("a", Red), ("b", Blue), ("c", White)]
map2 = Map.fromList [("a", Yellow), ("b", Yellow), ("c", Green), ("d", Purple)]

mergeMap :: (Semigroup v, Ord k) => Map.Map k v -> Map.Map k v -> Map.Map k v
mergeMap lhs rhs = Map.foldrWithKey f lhs rhs
  where
    -- lhs becomes the accumulator and we fold across rhs key-values
    f :: (Semigroup v, Ord k) => k -> v -> Map.Map k v -> Map.Map k v
    f rKey rVal acc = Map.alter (\lVal -> Just (optionCombine rVal lVal)) rKey acc

optionCombine :: Semigroup a => a -> Maybe a -> a
optionCombine lhs Nothing    = lhs
optionCombine lhs (Just rhs) = lhs <> rhs
