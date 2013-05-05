import Text.ParserCombinators.Parsec
import Data.Char
import Data.List


delimiter :: Parser Char
delimiter = char ','

wordParseRule :: Parser [Char]
wordParseRule = char '"' >> manyTill anyChar (char '"')

fileParseRule :: Parser [[Char]]
fileParseRule = sepBy wordParseRule delimiter

sumAsciiVals :: [Char] -> Int
sumAsciiVals = sum . map (flip (-) 64 . ord)

ordinalMult :: [Int] -> [Int]
ordinalMult = zipWith (*) [1..]

p22 = do
  names <- parseFromFile fileParseRule "p22_names.txt"
  case names of
    Left err -> print err
    Right li -> print (sum . ordinalMult $ map sumAsciiVals $ sort li)

main = p22