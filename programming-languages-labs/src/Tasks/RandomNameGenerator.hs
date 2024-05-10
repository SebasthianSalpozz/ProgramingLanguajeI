module RandomNamePick where

import System.IO
import System.Random
import Control.Monad (liftM)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text


type StudentName = String

data Student = Student{
    lastName::String,
    name::StudentName
    } deriving (Show, Eq,Ord)

data SchoolHouse = Griffindor
                    | Hufflepuff
                    | Ravenclaw 
                    | Slytherin 
                    deriving (Eq, Show, Ord, Read)

data SchoolHouseCount = SchoolHouseCount {
    house :: SchoolHouse,
    count :: Int
} deriving (Eq, Show)

assignSchoolHouse :: [SchoolHouseCount] -> IO SchoolHouse
assignSchoolHouse counts = do
    let validHouses = filter (\c -> count c < 6) counts
    house <- pickRandom (map house validHouses)
    return house

incrementCount :: SchoolHouse -> [SchoolHouseCount] -> [SchoolHouseCount]
incrementCount h counts = map (\c -> if house c == h then c { count = count c + 1 } else c) counts

pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1) 
    return (xs !! idx) 

split' :: Eq a => a -> [a] -> [[a]]
split' d [] = []
split' d s = x : split' d (drop 1 y)
             where (x,y) = span (/= d) s

parseLine :: String -> Student
parseLine line =
    let [lastName, name] = split' ',' line
    in Student{lastName=lastName, name=name}

readFileToList :: FilePath -> IO [Student]
readFileToList filePath = do
    contents <- readFile filePath
    let students = map parseLine (lines contents)
    return students

studentsList :: [Student] -> [SchoolHouseCount] -> IO ()
studentsList [] _ = return ()
studentsList (x:xs) counts = do 
    schoolHouse <- assignSchoolHouse counts
    putStrLn $ show (x, schoolHouse)
    let updatedCounts = incrementCount schoolHouse counts
    studentsList xs updatedCounts

main :: IO()
main = do
    records <- readFileToList "/home/fundacion/Quinto Semestre/PrograV/ProgramacionV/slides/list.txt"
    let initialCounts = [SchoolHouseCount Griffindor 0, SchoolHouseCount Hufflepuff 0, SchoolHouseCount Ravenclaw 0, SchoolHouseCount Slytherin 0]
    studentsList records initialCounts

