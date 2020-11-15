module RollLib (printRoll, printf) where

import Data.Text (replace, pack, unpack, singleton, Text)

printRoll :: String -> String
printRoll roll =  printf (getFormat (length roll)) (roll >>= getArgs)

-- printf applies each char in list to format with {number} notation
printf :: String -> [Char] -> String
printf = go 1
    where 
        go :: Int -> String -> [Char] -> String
        go _ res [] = res
        go acc format (x:xs) = 
            let 
                replacedStr = unpack $ replace (getTextFormat acc) (singleton x) (pack format) 
            in 
                go (acc+1) replacedStr xs

        getTextFormat :: Int -> Text
        getTextFormat d = pack $ '{' : show d ++ "}"

getArgs :: Char -> [Char]
getArgs '0' = "         "
getArgs '1' = "    X    "
getArgs '2' = "   X X   "
getArgs '3' = "  X X X  "
getArgs '4' = "X X   X X"
getArgs '5' = "X X X X X"
getArgs '6' = "X XX XX X"
getArgs _ = ""

getFormat :: Int -> String
getFormat 1 = "+-----+\n\
              \|{1} {2} {3}|\n\
              \|{4} {5} {6}|\n\
              \|{7} {8} {9}|\n\
              \+-----+"
getFormat 2 = "+-----+ +-----+\n\
              \|{1} {2} {3}| |{10} {11} {12}|\n\
              \|{4} {5} {6}| |{13} {14} {15}|\n\
              \|{7} {8} {9}| |{16} {17} {18}|\n\
              \+-----+ +-----+"
getFormat 3 = "+-----+ +-----+ +-----+\n\
              \|{1} {2} {3}| |{10} {11} {12}| |{19} {20} {21}|\n\
              \|{4} {5} {6}| |{13} {14} {15}| |{22} {23} {24}|\n\
              \|{7} {8} {9}| |{16} {17} {18}| |{25} {26} {27}|\n\
              \+-----+ +-----+ +-----+"
getFormat 4 = "+-----+ +-----+ +-----+ +-----+\n\
              \|{1} {2} {3}| |{10} {11} {12}| |{19} {20} {21}| |{28} {29} {30}|\n\
              \|{4} {5} {6}| |{13} {14} {15}| |{22} {23} {24}| |{31} {32} {33}|\n\
              \|{7} {8} {9}| |{16} {17} {18}| |{25} {26} {27}| |{34} {35} {36}|\n\
              \+-----+ +-----+ +-----+ +-----+"
getFormat 5 = "+-----+ +-----+ +-----+ +-----+ +-----+\n\
              \|{1} {2} {3}| |{10} {11} {12}| |{19} {20} {21}| |{28} {29} {30}| |{37} {38} {39}|\n\
              \|{4} {5} {6}| |{13} {14} {15}| |{22} {23} {24}| |{31} {32} {33}| |{40} {41} {42}|\n\
              \|{7} {8} {9}| |{16} {17} {18}| |{25} {26} {27}| |{34} {35} {36}| |{43} {44} {45}|\n\
              \+-----+ +-----+ +-----+ +-----+ +-----+"
getFormat 6 = "+-----+ +-----+ +-----+ +-----+ +-----+ +-----+\n\
              \|{1} {2} {3}| |{10} {11} {12}| |{19} {20} {21}| |{28} {29} {30}| |{37} {38} {39}| |{46} {47} {48}|\n\
              \|{4} {5} {6}| |{13} {14} {15}| |{22} {23} {24}| |{31} {32} {33}| |{40} {41} {42}| |{49} {50} {51}|\n\
              \|{7} {8} {9}| |{16} {17} {18}| |{25} {26} {27}| |{34} {35} {36}| |{43} {44} {45}| |{52} {53} {54}|\n\
              \+-----+ +-----+ +-----+ +-----+ +-----+ +-----+"
getFormat _ = "error"

