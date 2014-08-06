{-

In this exercise and the next one we will write a simple assembler for a hypothetical computer; we are following the assembler in the book The Awk Programming Langauge by Alfred V. Aho, Brian W. Kernighan and Peter J. Weinberger. Our computer has a memory of a thousand five-digit words, a single accumulator, and eleven opcodes:

OPCODE	INSTRUCTION	DESCRIPTION
00	const 		C assembler pseudo-operator to define a constant C
01	get			read a number from the input to the accumulator
02	put			write the number in the accumulator to the output
03	ld 		M	load accumulator with contents of memory location M
04	st 		M	store contents of accumulator in memory location M
05	add 	M	add contents of memory location M to the accumulator
06	sub 	M	subtract contents of memory location M from the accumulator
07	jpos	M	jump to memory location M if the accumulator is positive
08	jz 		M	jump to memory location M if the accumulator is zero
09	j			jump to memory location M, unconditionally
10	halt		stop program execution

-}

module SimpleAsmCpu where

import SimpleAsmArchitecture as A
import Data.Array.IO
import Control.Monad

-- util --
-- chunks 2 [1,2,3,4] = [[1,2],[3,4]]
-- chunks 3 [1,2,3,4] = [[1,2,3],[4]]
chunks n [] = []
chunks n xs = let (c,xs') = takeChunk [] n xs
			  in c : chunks n xs'
			  where
				takeChunk cs 0 xs = (reverse cs,xs)
				takeChunk cs _ [] = (reverse cs,[])
				takeChunk cs n (x:xs) = takeChunk (x:cs) (n-1) xs

maybeRead str = case reads str of
					[(x,"")] -> Just x
					_ -> Nothing
-- --

data Cpu = Cpu{
			 pc :: Int,
			 acc :: A.Word
		   }

main' f = do
			contents <- readFile f
			let words = map (A.word . read) $ chunks A.word_size contents
			memory <- newMemory words
			let start = Cpu 0 (A.word 0)
			run start memory

newMemory ws = do
	arr <- (newArray (0,1000) (A.word 0) :: IO (IOArray Int A.Word))
	mapM_ (\(i,w) -> writeArray arr i w) (zip [0..1000] ws)
	return arr

run :: Cpu -> IOArray Int A.Word -> IO ()
run (Cpu pc acc) memory =
	let
		pc' = pc + 1
		readAt m = readArray memory m
		writeAt m w = writeArray memory m w
		run' cpu = run cpu memory
	in do
		word <- readAt pc
		case (A.wordSplit word) of
			(1,_) -> do -- get
						num <- readNumber
						run' (Cpu pc' num)
			(2,_) -> do -- put
						writeNumber acc
						run' (Cpu pc' acc)
			(3,m) -> do -- load M
						acc' <- readAt m
						run' (Cpu pc' acc')
			(4,m) -> do -- st M
						writeAt m acc
						run' (Cpu pc' acc)
			(5,m) -> do -- add M
						num <- readAt m
						run' (Cpu pc' (acc + num))
			(6,m) -> do -- sub M
						num <- readAt m
						run' (Cpu pc' (acc - num))
			(7,m) -> if acc > 0 -- jpos M
						then run' (Cpu m acc)
						else run' (Cpu pc' acc)
			(8,m) -> if acc == 0 -- jz M
						then run' (Cpu m acc)
						else run' (Cpu pc' acc)
			(9,m) -> run' (Cpu m acc) -- jump M
			(10,_) -> return () -- Halt
			(x,_) -> putStrLn $ "H! Bad opcode " ++ show x

readNumber :: IO Word
readNumber = do
				putStr "H< "
				input <- getLine
				maybe readNumber return $ (maybeRead input :: Maybe Int) >>= (return . A.word)

writeNumber :: Word -> IO ()
writeNumber num = do
					putStr "H> "
					putStrLn $ show . A.int $ num
