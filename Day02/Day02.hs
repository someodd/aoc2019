import Data.Sequence
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import qualified Data.List (filter, groupBy)

puzzleInput :: FilePath
puzzleInput = "puzzle-input.txt"

type Intcode = Seq Int


-- I really want to have an enumeration where Add = 1 and Multiply = 2 etc
data Opcode = Add | Multiply | Finished


toOpcode 1 = Add
toOpcode 2 = Multiply
toOpcode 99 = Finished
toOpcode a = error $ "Invalid opcode: " ++ show a


-- | Raises an error on failure.
elementAtUnsafe :: Intcode -> Int -> String -> Int 
elementAtUnsafe intcode index' errorMessage =
    case intcode !? index' of
        Nothing -> error errorMessage
        Just value -> value


-- | Gets a value at the position described by the intcode at position x.
offset :: Int -> Intcode -> Int
offset x intcode =
    let reference = elementAtUnsafe intcode x $ "Looked for a value at " ++ show x ++ " but intcode too short."
    in elementAtUnsafe intcode reference $ "Looked for actual value at position " ++ show reference ++ ", which was pointed to by value-position at " ++ show x ++ ", but the intcode is too short."


-- more elegant if index first then the intcode
-- start index at 0
-- FIXME: can't i use MaybeT to not have all these cases?!
processIntcode :: Int -> Intcode -> Maybe Intcode
processIntcode index' intcode = do
    let
        opcode = toOpcode . elementAtUnsafe intcode index' $ "Looked for opcode at " ++ show index' ++ " but intcode too short."
        firstVal = offset (index' + 1) intcode
        secondVal = offset (index' + 2) intcode
        putPosition = elementAtUnsafe intcode (index' + 3) $ "Could not get position " ++ show (index' + 3) ++ " (intcode too short)"
    case opcode of
      -- FIXME: duplicate code...
      Add -> processIntcode (index' + 4) (update putPosition (firstVal + secondVal) intcode)
      Multiply -> processIntcode (index' + 4) (update putPosition (firstVal * secondVal) intcode)
      Finished -> Just intcode


-- I could've imported split, but I wanted to write it myself!
-- | Read file in as Intcode and also do modifications it spoke of.
readIntcode :: IO Intcode
readIntcode = fromList . map (\x -> read x :: Int) . Data.List.filter (/= ",") . Data.List.groupBy (\x y -> x /= ',' && y /= ',') <$> readFile puzzleInput


-- | Once you have a working computer, the first step is to restore the gravity assist program (your puzzle input) to the
-- "1202 program alarm" state it had just before the last computer caught fire. To do this, before running the program,
-- replace position 1 with the value 12 and replace position 2 with the value 2.
restoreState :: Intcode -> Intcode
restoreState intcode = update 2 2 $ update 1 12 intcode


main :: IO ()
main = readIntcode >>= (print . processIntcode 0) . restoreState