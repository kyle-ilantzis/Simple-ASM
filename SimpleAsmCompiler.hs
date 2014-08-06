module SimpleAsmCompiler where

import Parser(runP)
import SimpleAsmParser
import SimpleAsmGrammer as G
import qualified SimpleAsmArchitecture as A
import Data.List(foldl',mapAccumL)
import qualified Data.Map as M	

-- util --
first :: (a -> b) -> Either a c -> Either b c
first f (Left x) = Left $ f x
first _ (Right x) = Right x

second :: (b -> c) -> Either a b -> Either a c
second f (Right x) = Right $ f x
second _ (Left x) = Left x
-- --

data CompileError = ParseError String | UndefinedLabels [Label]

-- main: take input file, produce output "machine code" to output file.
main' f1 f2 = do 
				contents <- readFile f1
				either putCompileError (writeFile f2 . concat . map show) (compiler contents)

compiler :: String -> Either CompileError [A.Word]
compiler str = do 
				p <- first ParseError $ runP parseAsmProgram $ str
				compile $ fst p
				
putCompileError :: CompileError -> IO ()
putCompileError (UndefinedLabels ls) = do
										putStrLn "Undefined labels:"
										mapM_ putStrLn ls
putCompileError (ParseError str) = putStrLn str
				
compile :: AsmProgram -> Either CompileError [A.Word]
compile p = let 
				ss = link . map statement . statements . clean $ p
				undefinedLabels = foldl' (accObjLbls) [] ss
				accObjLbls acc s = maybe acc (:acc) $ G.getSLabel s
			in if null undefinedLabels then 
				Right $ map toWord ss 
			else 
				Left $ UndefinedLabels undefinedLabels
	
-- filter to remove comments, blank lines, and turn negative constants into 10's complement
clean :: AsmProgram -> AsmProgram
clean = AsmProgram . filter (not . \a -> isAComment a || isABlankLine a) . statements
		where
			isAComment (AComment _) = True
			isAComment _ = False
			isABlankLine ABlankLine = True
			isABlankLine _ = False
			
link :: [Statement] -> [Statement]
link ss = map (G.stopmap linkObject) ss
		  where
			m = labels $ locations ss
			linkObject (OLabel lbl) = maybe (OLabel lbl) (OMemLocation . A.word) (M.lookup lbl m)
			linkObject obj = obj			
			
locations :: [Statement] -> [(Int,Statement)]
locations ss = snd $ mapAccumL wordSizePair 0 ss -- the starting address of a program is zero, then the next word starts at the size of the previous word.
				where wordSizePair acc s = let acc' = acc + stWordSize s in (acc',(acc,s))

labels :: [(Int,Statement)] -> M.Map Label Int
labels = foldl' build M.empty
		where
			build m (x, SVariable lbl) = M.insert lbl x m
			build m (x, SConstant lbl _) = M.insert lbl x m
			build m (x, SLabeledOperation lbl _) = M.insert lbl x m
			build m _ = m

-- how many words an instruction takes			
stWordSize = const 1 -- for now all instructions are 1 word. but later they may be more.				

toWord :: Statement -> A.Word
toWord (SVariable _) = A.wordJoin 0 0
toWord (SConstant lbl c) = c
toWord (SOperation op) = opToWord op
toWord (SLabeledOperation _ op) = opToWord op

opToWord :: Operation -> A.Word
opToWord Get = A.wordJoin 1 0
opToWord Put = A.wordJoin 2 0
opToWord (Ld (OMemLocation mem)) = A.wordJoin 3 (A.int mem)
opToWord (St (OMemLocation mem)) = A.wordJoin 4 (A.int mem)
opToWord (Add (OMemLocation mem)) = A.wordJoin 5 (A.int mem)
opToWord (Sub (OMemLocation mem)) = A.wordJoin 6 (A.int mem)
opToWord (Jpos (OMemLocation mem)) = A.wordJoin 7 (A.int mem)
opToWord (Jz (OMemLocation mem)) = A.wordJoin 8 (A.int mem)
opToWord (J (OMemLocation mem)) = A.wordJoin 9 (A.int mem)
opToWord Halt = A.wordJoin 10 0
