-- Parse the input stream and return a structured representation.
-- TODO validate label
-- TODO validate int is within word size limit
module SimpleAsmParser(
	parseAsmProgram
)where

import Parser
import qualified SimpleAsmArchitecture as A
import SimpleAsmGrammer
import Control.Applicative((<$>),(<*>))
import Control.Monad(when)
import Data.Char(isSpace,isDigit)

-- util --
empty :: String -> Bool
empty = and . map isSpace

preview :: String -> String
preview = (++ "...") . take 10

failToken s = fail $ "Unexpected token in stream '" ++ (preview s) ++ "'"
failEmpty = fail "Expected values in stream"

-- Parsing of an AsmProgram is almost like the grammer definition --
parseAsmProgram :: Parser AsmProgram
parseAsmProgram = AsmProgram <$> parseAsmStatements

parseAsmStatements :: Parser AsmStatements
parseAsmStatements = do
						str <- get
						when (null str) failEmpty
						s <- parseAsmStatement
						str' <- get
						if null str' then
							return [s]
						else
							(s:) <$> parseAsmStatements

parseAsmStatement :: Parser AsmStatement
parseAsmStatement = do
						theline <- line
						str <- get
						put theline
						s <- parseAsmStatementLine
						put str
						return s

parseAsmStatementLine :: Parser AsmStatement
parseAsmStatementLine = tryParserOtherwise
							blankOrComment
							(do
								s <- parseStatement
								b_or_c <- blankOrComment
								return' s b_or_c
							)
					where
						blankOrComment = tryParserOtherwise parseABlankLine parseAComment
						return' s ABlankLine = return $ AStatement s
						return' s (AComment c) = return $ AStatementWComment s c

parseABlankLine :: Parser AsmStatement
parseABlankLine = do
					s <- get
					when (not . empty $ s) (failToken s)
					put ""
					return ABlankLine

parseAComment :: Parser AsmStatement
parseAComment = do
					w <- word
					when (head w /= '#') (failToken w)
					s <- get
					return $ AComment (w ++ s)

parseStatement :: Parser Statement
parseStatement = do
					c <- peek
					if isSymbol c then
						(do
							label <- parseLabel
							tryParserOtherwise
								(parseVariable label)
								(parseLabeledOperation label)
						)
					else
						SOperation <$> parseOperation
				where isSymbol = not . isSpace

parseVariable :: Label -> Parser Statement
parseVariable label = do
						w <- word
						when (w /= "const") (failToken w)
						tryParserOtherwise
							(SConstant label <$> parseWord)
							(return $ SVariable label)

parseLabeledOperation :: Label -> Parser Statement
parseLabeledOperation label = SLabeledOperation label <$> parseOperation

parseOperation :: Parser Operation
parseOperation = do
					w <- word
					case w of
						"get" -> return Get
						"put" -> return Put
						"ld" -> Ld <$> parseObject
						"st" -> St <$> parseObject
						"add" -> Add <$> parseObject
						"sub" -> Sub <$> parseObject
						"jpos" -> Jpos <$> parseObject
						"jz" -> Jz <$> parseObject
						"j" -> J <$> parseObject
						"halt" -> return Halt
						_ -> failToken w

parseObject :: Parser Object
parseObject = tryParserOtherwise (OMemLocation <$> parseWord) (OLabel <$> parseLabel)

parseLabel :: Parser Label
parseLabel = do
				w <- word
				-- TODO validate word
				return w

parseWord :: Parser A.Word
parseWord = do
				i <- parseInt
				return $ A.word i

parseInt :: Parser Int
parseInt = do
			w <- word
			-- TODO validate int is within word size limit?
			case reads w :: [(Int,String)] of
				[(i,"")] -> return i
				_ -> failToken w
