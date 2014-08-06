-- Define a structure representation of the opcodes.
module SimpleAsmGrammer(
	AsmProgram(..),
	AsmStatements,
	AsmStatement(..),
	Statement(..),
	Operation(..),
	Object(..),
	Label,
	Constant,
	MemoryLocation,
	Comment,
	
	stmap,
	opmap,
	stopmap,
	objLblMap,
	objMemMap,
	stLblMap,
	stMemMap,
	
	getOperation,
	getObject,
	getLabel,
	getMemLocation,
	getSObject,
	getSLabel,
	getSMemLocation
)where

import qualified SimpleAsmArchitecture as A

data AsmProgram = AsmProgram {
					statements :: AsmStatements
				  } deriving Show

type AsmStatements = [AsmStatement]

data AsmStatement = AStatement { 
						statement :: Statement 
					} |
					AStatementWComment {
						statement :: Statement,
						comment :: Comment
					} |
					ABlankLine |
					AComment { 
						comment :: Comment 
					} deriving Show

data Statement = SVariable { 
					label :: Label
				 } |
				 SConstant {
					label :: Label,
					constant :: Constant
				 } |
				 SOperation {
					operation :: Operation
				 } |
				 SLabeledOperation {
					label :: Label,
					operation :: Operation
				 } deriving Show

data Operation = Get | 
				 Put | 
				 Ld { object :: Object } | 
				 St { object :: Object} |
				 Add { object :: Object} |
				 Sub { object :: Object} |
				 Jpos { object :: Object} |
				 Jz { object :: Object} |
				 J { object :: Object} |
				 Halt
				 deriving Show
				 
data Object = OLabel { olabel :: Label } | 
			  OMemLocation { omemLocation :: MemoryLocation }
			  deriving Show
				 
type Label = String -- [_a-zA-Z]+[_0-9a-zA-Z]{,31}
type Constant = A.Word
type MemoryLocation = A.Word
type Comment = String

-- map a function on a Statement's operation if any, otherwise acts as "id"
stmap :: (Operation -> Operation) -> Statement -> Statement
stmap f (SLabeledOperation lbl op) = SLabeledOperation lbl $ f op
stmap f (SOperation op) = SOperation (f op)
stmap _ s = s

-- map a function on an Operation's object if any, otherwise acts as "id"
opmap :: (Object -> Object) -> Operation -> Operation
opmap _ Get = Get
opmap _ Put = Put
opmap _ Halt = Halt
opmap f (Ld obj) = Ld $ f obj
opmap f (St obj) = St $ f obj
opmap f (Add obj) = Add $ f obj
opmap f (Sub obj) = Sub $ f obj
opmap f (Jpos obj) = Jpos $ f obj
opmap f (Jz obj) = Jz $ f obj
opmap f (J obj) = J $ f obj

-- map a function on an Object's olabel if any, otherwise acts as "id"
objLblMap :: (Label -> Label) -> Object -> Object
objLblMap f (OLabel lbl) = OLabel $ f lbl
objLblMap _ obj = obj

-- map a function on an Object's omemLocation if any, otherwise acts as "id"
objMemMap :: (MemoryLocation -> MemoryLocation) -> Object -> Object
objMemMap f (OMemLocation mem) = OMemLocation $ f mem
objMemMap _ obj = obj

-- map a function on a Statement's Operation's object if any, otherwise acts as "id"
stopmap :: (Object -> Object) -> Statement -> Statement
stopmap = stmap . opmap

-- map a function on a Statement's Operation's Object's olabel if any, otherwise acts as "id"
stLblMap :: (Label -> Label) -> Statement -> Statement
stLblMap = stmap . opmap . objLblMap

-- map a function on a Statement's Operation's Object's omemLocation if any, otherwise acts as "id"
stMemMap :: (MemoryLocation -> MemoryLocation) -> Statement -> Statement
stMemMap = stmap . opmap . objMemMap

getOperation :: Statement -> Maybe Operation
getOperation (SOperation op) = Just op
getOperation (SLabeledOperation _ op) = Just op
getOperation _ = Nothing

getObject :: Operation -> Maybe Object
getObject Get = Nothing
getObject Put = Nothing
getObject Halt = Nothing
getObject op = Just $ object op

getLabel :: Object -> Maybe Label
getLabel (OLabel lbl) = Just lbl
getLabel _ = Nothing

getMemLocation :: Object -> Maybe MemoryLocation
getMemLocation (OMemLocation m) = Just m
getMemLocation _ = Nothing

getSObject :: Statement -> Maybe Object
getSObject s = getOperation s >>= getObject

getSLabel :: Statement -> Maybe Label
getSLabel s = getOperation s >>= getObject >>= getLabel

getSMemLocation :: Statement -> Maybe MemoryLocation
getSMemLocation s = getOperation s >>= getObject >>= getMemLocation
