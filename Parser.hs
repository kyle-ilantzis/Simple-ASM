module Parser(
	Parser(..),
	get,
	put,
	maybeParser,
	maybeParsers,
	tryParserOtherwise,
	tryParsersOtherwise,
	peek,
	peeks,
	word,
	line	
) where

import Control.Monad(when)
import Control.Applicative
import Data.Char(isSpace)
import Data.Monoid

newtype Parser a = P { runP :: String -> Either String (a,String) }

instance Monad Parser where
	return x = P $ \str -> Right (x,str)
	m >>= f = P $ (\str -> case (runP m $ str) of
						Left s -> Left s
						Right (a, str') -> let m' = f a in runP m' $ str')
	fail s = P $ \str -> Left s 
-- laws?
-- f = \x -> P $ \str -> Right (head str == x, tail str) -- return x >>= k = k a. CHECK
-- g = P $ \str -> Right (init str, [last str]) -- m >>= return = m. CHECK
	
instance Functor Parser where
	fmap g f = f >>= (return . g)
-- laws?
-- 1. fmap id = id
-- 2. fmap (f . g) = fmap f . fmap g = fmap f (fmap g F)

instance Applicative Parser where
	pure = return
	f <*> g = do
				f' <- f
				a <- g
				fmap f' $ return a
-- laws?
-- 1. pure id <*> v = v
-- 2. pure f <*> pure x = pure (f x)
-- 3. u <*> pure y = pure ($ y) <*> u
-- 4. u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

-- g = P $ \str -> if null str then Left "null str! >:(" else Right (init str, [last str])

-- State --
get :: Parser String
get = P $ \s -> Right (s,s)

put :: String -> Parser ()
put s = P $ \s' -> Right ((),s)

-- parsing util --
maybeParser :: Parser a -> Parser (Maybe a)
maybeParser p = do
					s <- get
					either (const $ nothing s) (just) (runP p s)
				where
					nothing s = do
									put s
									return Nothing
					just (a,s) = do
									put s
									return $ Just a

maybeParsers :: [Parser (Maybe a)] -> Parser (Maybe a)
maybeParsers = fmap (getFirst . mconcat. map First) . sequence

tryParserOtherwise :: Parser a -> Parser a -> Parser a
tryParserOtherwise p1 p2 = tryParsersOtherwise [p1] p2

tryParsersOtherwise :: [Parser a] -> Parser a -> Parser a
tryParsersOtherwise ps p = do
							ma <- mp
							maybe p return ma
						where 
							mp = maybeParsers $ map maybeParser ps

-- token parsing util --
peek :: Parser Char
peek = do
		s <- get
		when (null s) (fail "Expected character in stream")
		return $ head s

peeks :: Parser String
peeks = fmap (:[]) peek		
		
word :: Parser String
word = do
		s <- get
		let (w,s') = break isSpace . dropWhile isSpace $ s
		when (null w) (fail "Expected word in stream")
		put s'
		return w
		
line :: Parser String			
line = do
		s <- get
		let (theline,s') = break (=='\n') s
		put $ dropFirstIf (=='\n') s'
		return theline
		where
			dropFirstIf _ [] = []
			dropFirstIf p (s:ss) = if p s then ss else s:ss
		

	  
