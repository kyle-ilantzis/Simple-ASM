module SimpleAsmArchitecture(
	Word,
	word,
	int,
	word_size,
	op_size,
	obj_size,
	wordJoin,
	wordSplit
) where

word_size = op_size + obj_size
op_size = 2
obj_size = 3

complement = 10 ^ word_size

min_word = complement `div` 2
max_word = min_word - 1

newtype Word = Word { runWord :: Int } deriving (Eq)

tensComplement x = complement - x

word :: Int -> Word
word x = if x < 0 
			then Word . tensComplement . trunc . abs $ x 
			else Word . trunc $ x
		 where trunc = (`mod` complement)
			
int (Word x) = 	if x < min_word
					then x 
					else negate . tensComplement $ x
					
wordJoin :: Int -> Int -> Word					
wordJoin op obj = Word $  (abs op) * 10 ^ obj_size  +  (abs obj) `mod` 10^ obj_size
				 
wordSplit :: Word -> (Int,Int)				 
wordSplit (Word x) = (x `div` 10 ^ obj_size, x `mod` 10 ^ obj_size)				  
			
inspect (Word x) = "Word { runWord = " ++ (show x) ++ " }"

instance Show Word where
	show (Word x) = frmt $ show x			

instance Num Word where
	w1 + w2 = word $ (int w1) + (int w2)
	w1 * w2 = word $ (int w1) * (int w2)
	negate = word . negate . int
	abs = word . abs . int
	signum = word . signum . int
	fromInteger = word . fromIntegral

instance Ord Word where
	w1 `compare` w2 = int w1 `compare` int w2
	
frmt :: String -> String
frmt x 
	| len > word_size = drop (len - word_size) x
	| len < word_size = pad word_size '0' x
	| otherwise = x
	where len = length x

pad p c x = replicate (p - length x) c ++ x
	