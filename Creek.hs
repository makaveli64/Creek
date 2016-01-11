import System.IO
import Text.Printf
{- Here will be code -}
main :: IO ()
main = do
	src <- readFile "creek.txt"
	let d = parse parseDimentions src
	printf "%s" d

{- TODO --> Nie działa wczytywanie -}
{-
parseDimentions :: Parser String
parseDimentions = do char '('
                     x <- digit
                     do char ','
                     y <- digit
                     return (x:y)
item :: Parser Char
item [] = []
item (x:xs) = [(x, xs)]
-}

{- 'algorytm' -}
fill :: [((Int, Int), Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
fill [((a, b)), c] [(d, e)] [(f, g)] = []
fill ilosci zamalowane niezamalowane kandydaci | ciaglosc ((head kandydaci) : zamalowane) & not null kandydaci = fill ilosci ((head kandydaci) : zamalowane) (tail kandydaci)

ciaglosc :: [(Int, Int)] -> Bool

{-
    let pairs   = map (split.words) (lines src)
    let grades  = foldr insert empty pairs
    mapM_ (draw grades) (sort (keys grades))
  where
    insert (s, g) = insertWith (++) s [g]
    split [name,mark] = (name, read mark)
 
draw g s = printf "%s\t%s\tAverage: %f\n" s (show marks) avg
  where
    marks = findWithDefault (error "No such student") s g
    avg   = sum marks / fromIntegral (length marks) :: Double
-}
