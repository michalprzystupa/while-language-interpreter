-- Michal Przystupa 
-- interpreter prostego jezyka imperatywnego

import Data.Char
import Control.Monad

-- KLASA TRANSFORMATORA MONAD

class MonadTrans t where
	lift :: (Monad m) => m a -> t m a

-- KOMUNIKACJA Z OTOCZENIEM --------------------------------------------------------

main :: IO()
main = do
			putStrLn "Path of file:\t"
			path <- getLine
			code <- readFile path
			print_ $ lacz code
			
-- Laczy lekser i parser
lacz :: String -> ExcT IO State
lacz str = aux $ apply stm (lekser str)
	where
		aux (Just (tree,[])) = run tree init_state
		aux _ = fail "NotParse"

-- Wypisywanie wyjatkow na konsole		
print_ :: ExcT IO State -> IO()
print_ x = do 
			s <- runExcT x
			case s of 
				Ok _ 				-> putStrLn "Program zakonczony poprawnie"
				Raise (exc,info,_) 	-> putStrLn $ "Wystapil wyjatek: "++exc++" "++info
		
-- DRZEWA WYRAZEN --------------------------------------------------

data Tree = Leaf String
		deriving Show
		
data Aexp = A_Zmienna String | 
			A_Stala Int | 
			A_Node1 String Aexp | 
			A_Node2 String Aexp Aexp 
	deriving Show

data Bexp = B_Stala Bool | 
			B_Node1 String Bexp | 
			B_Node2a String Aexp Aexp |
			B_Node2b String Bexp Bexp
	deriving Show

data Stm = 	Skip | 
			Read String |
			Write String |
			Throw String |
			Try String Stm Stm |
			If Bexp Stm Stm |
			While Bexp Stm |
			Assign Aexp Aexp |
			Bstm [Stm] [Stm]
		deriving Show

-- PAMIEC --------------------------------------------------------

type State = ([(String,Int)],[Int])				
init_state = ([],[])

-- Pobierz wartosc zmiennej
getV :: Monad m => String -> State -> m Int
getV str = find.fst where
	find [] = fail $ "NotDeclaredVariable " ++ str
	find ((s,res):xs)	| s == str = return res
						| otherwise = find xs				

-- Zadeklaruj zmienna						
addV :: Monad m => (String,Int) -> State -> m State
addV x (xs,stos) = return (x:xs,stos)

-- Zadeklaruj zmienne w bloku
addVs :: Monad m => [Stm] -> State -> m State
addVs [] s = return s
addVs ((Assign (A_Zmienna str) aExp):xs) s = do
												val <- aEval aExp s
												s' <- addV (str,val) s
												addVs xs s'												
addVs _ _ = fail "blad implementacji"

-- Przypisz zmiennej wartosc
changeV :: Monad m => (String,Int) -> State -> m State
changeV y xs = aux y [] xs where
aux (str,_) _ ([],_) = fail $ "NotDeclaredVariable " ++ str
aux y@(str,val) akk (x@(s,v):xs,stos)	| str ==s = return (reverse akk ++ y:xs,stos)
										| otherwise = aux y (x:akk) (xs,stos)

wejdz_do_bloku :: Monad m => State -> m State
wejdz_do_bloku (list,xs) = return (list,(length list):xs) 

wyjdz_z_bloku :: Monad m => State -> m State
wyjdz_z_bloku (list,x:xs) = return (drop (length list - x) list,xs)
wyjdz_z_bloku (_,[]) = fail "blad implementacji"

-- 	WYJATKI ---------------------------------------------------

type Exception = String
type Info = String

data Exc a = Raise (Exception,Info,State) | Ok a
	deriving Show

instance Monad Exc where
	return x = Ok x
	(Raise e) >>= q = Raise e
	(Ok x) >>= f = f x

-- LACZENIE MONAD Exc oraz IO ------------------------------------------------------------

newtype ExcT m a = ExcT { runExcT :: m (Exc a) }

instance Monad m => Monad (ExcT m) where
	return = ExcT . return . Ok 
	x >>= f = ExcT $ do
						exc_value <- runExcT x
						case exc_value of
							Raise e -> return $ Raise e
							Ok a -> runExcT $ f a

instance MonadTrans ExcT where
	lift m = ExcT $ m >>= return.Ok

-- Rzucanie wyjatkow
raise :: Monad m => String -> State -> ExcT m a
raise [] s = ExcT . return . Raise $ ([],[],s)
raise str s = 	let xs = words str
				in 	ExcT . return . Raise $ (head$xs,unwords.tail$xs,s)

-- Lapanie wyjatkow
handle :: Monad m => String -> ExcT m State -> (State -> ExcT m State) -> ExcT m State
handle str excT_s f = ExcT $ do
								exc_s <- runExcT excT_s
								case exc_s of 
									Ok s -> return exc_s
									Raise x@(exc,info,s) ->
										if exc == str
										then runExcT $ f s
										else return exc_s
										
-- LEKSER ----------------------------------------------------------

wstaw :: String	-> String
wstaw [] = []
wstaw [x] = [x]
wstaw (x1:x2:xs)| elem [x1,x2] ["!=","==","<=",">=",":=","&&","||"] = x1:wstaw (x2:xs)
				| (not$isAlphaNum x1) || (not$isAlphaNum x2) = x1:' ':wstaw (x2:xs)
				| otherwise = x1:wstaw (x2:xs)

-- Komentarz do konca linii			
komentarze :: String -> String
komentarze [] = []
komentarze ('$':xs) = komentarze.(\xs -> if xs==[] then [] else tail xs) $ dropWhile (/='\n') xs
komentarze (x:xs) = x:komentarze xs

lekser :: String -> [String]
lekser = words.wstaw.komentarze

--	TYP PARSER -----------------------------------------------------
newtype Parser m a = Parser {apply :: [String] -> m(a,[String])}

instance Monad m => Monad (Parser m) where
	return a = Parser(\xs -> return(a,xs))
	p >>= f = Parser (\xs-> do 
								(a,xs') <- apply p xs
								apply (f a) xs')

instance (MonadPlus m) => MonadPlus (Parser m) where
	mzero = Parser(\_ -> fail "not parse")
	p1 `mplus` p2 = Parser(\xs -> (apply p1 xs)`mplus`(apply p2 xs))

(<+>) :: MonadPlus m => Parser m a -> Parser m a -> Parser m a
(<+>) = mplus

-- POMOCNE PARSERY ----------------------------------------------------------

-- Liczba
czyLiczba :: String -> Bool
czyLiczba [] = False
czyLiczba "0" = True
czyLiczba ('-':x:xs)| x/='0' && all (flip elem ['0'..'9']) (x:xs) = True
czyLiczba (x:xs) = x/='0' && all (flip elem ['0'..'9']) (x:xs)

liczba :: MonadPlus m => Parser m Aexp
liczba = Parser f where
	f ("-":x:xs)| czyLiczba x = return (A_Stala$ - (read x),xs)
	f (x:xs)	| czyLiczba x = return (A_Stala$read x,xs)	
	f _ = fail "not parse"

-- Zmienna
czyZmienna :: String -> Bool
czyZmienna [] = False
czyZmienna (x:xs) = elem x ['a'..'z'] && all isAlphaNum xs
	
zmienna :: MonadPlus m => Parser m Aexp
zmienna = Parser f where
	f (x:xs)| czyZmienna x = return (A_Zmienna x,xs)	
	f _ = fail "not parse"

-- Stale logiczne
bool :: MonadPlus m => Parser m Bexp
bool = Parser f where
	f ("TRUE":xs) = return (B_Stala True,xs)
	f ("FALSE":xs) = return (B_Stala False,xs)
	f _ = fail "not parse"

-- Slowo
_string :: MonadPlus m => Parser m String
_string = Parser f where
	f [] = fail "not parse"
	f (x:xs) = return (x,xs)
	
-- Parser prostych znakow
simple :: Monad m => String -> Parser m Tree
simple pattern = Parser f where
	f [] = fail "not parse"
	f (x:xs)| x==pattern = return (Leaf x,xs)
			| otherwise = fail "not parse"

-- Parser zamieniajacy lewostronna rekursje na iteracje
iter :: MonadPlus m => Aexp -> Parser m Aexp -> Parser m Tree -> Parser m Aexp
iter tree p_wyr p_znak =
				( do 
					Leaf znak <- p_znak
					e <- p_wyr
					iter (A_Node2 znak tree e) p_wyr p_znak
				) 
					<+>
				return tree
				
-- WYRAZENIA ARYTMETYCZNE ------------------------------------------

{-
aExp := aExp (+ | -) aExp1 | aExp1
aExp1 := aExp1 (* | / | mod) aExp2 | aExp2
aExp2 := - aExp3 | aExp3
aExp3 := ( aExp ) | LICZBA | ZMIENNA
-}

plus,minus,razy,dziel,modulo,lewy_n,prawy_n :: Monad m => Parser m Tree
plus = simple "+"
minus = simple "-"
razy = simple "*"
dziel = simple "/"
modulo = simple "%"
lewy_n = simple "("
prawy_n = simple ")"

aExp :: MonadPlus m => Parser m Aexp
aExp = do
			tree <- aExp1
			iter tree aExp1 (plus <+> minus)

aExp1 :: MonadPlus m => Parser m Aexp
aExp1 = do
			tree <- aExp2
			iter tree aExp2 (razy <+> dziel <+> modulo)

aExp2 :: MonadPlus m => Parser m Aexp
aExp2 = (
		do
			Leaf znak <- minus
			e <- aExp3
			return (A_Node1 znak e)
		)
			<+>
		aExp3

aExp3 :: MonadPlus m => Parser m Aexp
aExp3 = (
		do
			_ <- lewy_n
			e <- aExp
			_ <- prawy_n
			return e
		)
			<+> liczba 
			<+> zmienna

f :: String -> (Int -> Int -> Int)
f "+" = (+)
f "-" = (-)
f "*" = (*)
f "%" = mod
f "/" = div		

-- WYRAZENIA LOGICZNE -------------------------------------------------------

_not,_and,_or,rozne,rowne,mniejsze,mniejsze_rowne,wieksze,wieksze_rowne :: Monad m => Parser m Tree
_not = simple "NOT"
_and = simple "&&"
_or = simple "||"
rozne = simple "!="
rowne = simple "=="
mniejsze = simple "<"
mniejsze_rowne = simple "<="
wieksze = simple ">"
wieksze_rowne = simple ">="

{-
bExp := bExp1 || bExp | bExp1
bExp1 := bExp2 && bExp1 | bExp2
bExp2 := aExp (<,<=,!=,==,>,>=) aExp | bExp3
bExp3 := not bExp4 | bExp4
bExp4 := ( bExp ) | TRUE | FALSE
-}

bExp :: MonadPlus m => Parser m Bexp
bExp = (
		do
			tree1 <- bExp1
			Leaf znak <- _or
			tree2 <- bExp
			return $ B_Node2b znak tree1 tree2
		)
			<+>
		bExp1
						
bExp1 :: MonadPlus m => Parser m Bexp
bExp1 = (
		do
			tree1 <- bExp2
			Leaf znak <- _and
			tree2 <- bExp1
			return $ B_Node2b znak tree1 tree2
		)
			<+>
		bExp2

bExp2 :: MonadPlus m => Parser m Bexp
bExp2 = (
		do
			tree1 <- aExp
			Leaf znak <- rozne <+> rowne <+> mniejsze <+> mniejsze_rowne <+> wieksze <+> wieksze_rowne
			tree2 <- aExp
			return $ B_Node2a znak tree1 tree2
		)
			<+>
		bExp3

bExp3 :: MonadPlus m => Parser m Bexp
bExp3 = (
		do
			Leaf znak <- _not
			tree <- bExp4
			return $ B_Node1 znak tree
		)
			<+> bExp4
		
		
bExp4 :: MonadPlus m => Parser m Bexp
bExp4 = (
		do
			_ <- lewy_n
			tree <- bExp
			_ <- prawy_n
			return tree
		)
			<+> bool
			
g :: String -> (Bool -> Bool -> Bool)
g "&&" = (&&)
g "||" = (||)

h :: String -> (Int -> Int -> Bool)
h "!=" = (/=)
h "==" = (==)
h "<" = (<)
h "<=" = (<=)
h ">" = (>)
h ">=" = (>=)

-- INSTRUKCJE ---------------------------------------------------------------

_while,_do,_przypisz,_if,_then,_else,_cons,_l,_p,_var,_skip,_throw,_try,_catch,_to,_read,_write,_repeat,_until :: Monad m => Parser m Tree
_while = simple "WHILE"
_do = simple "DO"
_przypisz = simple ":="	
_if = simple "IF"
_then = simple "THEN"
_else = simple "ELSE"	
_cons = simple ";"
_l = simple "{"
_p = simple "}"
_var = simple "VAR"
_skip = simple "SKIP"
_throw = simple "THROW"
_try = simple "TRY"
_catch = simple "CATCH"
_to = simple "TO"
_read = simple "READ"
_write = simple "WRITE"
_repeat = simple "REPEAT"
_until = simple "UNTIL"

skip :: MonadPlus m => Parser m Stm
skip = do
			stm <- _skip
			return Skip
				
read_ :: MonadPlus m => Parser m Stm
read_ = do
			_ <- _read
			str <- _string
			return $ Read str

write_ :: MonadPlus m => Parser m Stm
write_ = do
			_ <- _write
			str <- _string
			return $ Write str
			
throw :: MonadPlus m => Parser m Stm
throw = do
			_ <- _throw
			str <- _string
			return $ Throw str
			
try :: MonadPlus m => Parser m Stm
try = do
			_ <- _try
			_ <- _l
			stm1 <- stm
			_ <- _p
			_ <- _catch
			str <- _string
			_ <- _to
			_ <- _l
			stm2 <- stm
			_ <- _p
			return $ Try str stm1 stm2
			
if_then_else :: MonadPlus m => Parser m Stm
if_then_else = do
					_ <- _if
					b <- bExp
					_ <- _then
					stm1 <- stm
					_ <- _else
					stm2 <- stm
					return $ If b stm1 stm2

while_do :: MonadPlus m => Parser m Stm
while_do = do
				_ <- _while
				b <- bExp
				_ <- _do
				stm <- stm
				return $ While b stm

repeat_until :: MonadPlus m => Parser m Stm
repeat_until = do
				_ <- _repeat
				stm <- stm
				_ <- _until
				bExp <- bExp
				return $ Bstm [] [stm , While (B_Node1 "NOT" bExp) stm]

assign :: MonadPlus m => Parser m Stm
assign = do
		var <- zmienna
		_ <- _przypisz
		aExp <- aExp
		return $ Assign var aExp	

vars :: MonadPlus m => Parser m [Stm]
vars = do
			_ <- _var
			list <- assign_list
			_ <- _cons
			return list
		<+>
			return []

assign_list :: MonadPlus m => Parser m [Stm]
assign_list = do
				stm <- assign
				_cons
				list <- assign_list
				return (stm:list)
				<+>
			do
				stm <- assign
				_cons
				return [stm]

stm_list :: MonadPlus m => Parser m [Stm]
stm_list = 	
			do
				stm <- stm
				_ <- _cons
				list <- stm_list
				return (stm:list)
				<+>
			do
				stm <- stm
				return [stm]
				
stm :: MonadPlus m => Parser m Stm
stm =	skip
			<+>
		read_
			<+>
		write_
			<+>
		throw
			<+>
		try
			<+>
		if_then_else
			<+>
		while_do
			<+>
		repeat_until
			<+>
		assign
			<+>
		(
		do
			_ <- _l
			var_list <- vars
			stm_list <- stm_list
			_ <- _p
			return $ Bstm var_list stm_list
		)

-- FUNKCJE SEMANTYCZNE ----------------------------------

-- Wyrazenia arytmetyczne
aEval :: Monad m => Aexp -> State -> m Int
aEval (A_Zmienna str) s 	= getV str s
aEval (A_Stala x) _ 		= return x

aEval (A_Node1 "-" wyr) s = do 
							eWyr <- aEval wyr s 
							return (-eWyr)
aEval (A_Node2 znak wyr1 wyr2) s = do
									eWyr1 <- aEval wyr1 s
									eWyr2 <- aEval wyr2 s
									if (znak=="/" && eWyr2 == 0)
									then fail "DivByZero"
									else return $ (f znak) eWyr1 eWyr2
aEval _ _ = fail "blad implementacji"	

-- Wyrazenia logiczne
bEval :: Monad m => Bexp -> State -> m Bool
bEval (B_Stala val) _ = return val 
bEval (B_Node1 "NOT" wyr) s = do 
								eWyr <- bEval wyr s 
								return $ not eWyr
bEval (B_Node2b znak wyr1 wyr2) s = do
									eWyr1 <- bEval wyr1 s
									eWyr2 <- bEval wyr2 s
									return $ (g znak) eWyr1 eWyr2
bEval (B_Node2a znak wyr1 wyr2) s = do	
									eWyr1 <- aEval wyr1 s
									eWyr2 <- aEval wyr2 s
									return $ (h znak) eWyr1 eWyr2
bEval _ _ = fail "blad implementacji"

-- Instrukcje
run :: Stm -> State -> ExcT IO State
run Skip s = return s
run (Throw str) s = raise str s
run (Try str stm1 stm2) s = handle str (run stm1 s) (run stm2)
run (Write str) s = do 
						value <- getV str s
						lift $ print value
						return s
run (Read str) s = do
					input <- lift getLine
					if czyLiczba input
					then changeV (str,read input) s
					else raise ("NotParse: "++input) s			
run (If bExp stm1 stm2) s = do 
								bool <- bEval bExp s
								if bool 
								then run stm1 s
								else run stm2 s
run stm'@(While bExp stm) s = do 
								bool <- bEval bExp s
								if bool
								then do
										s' <- run stm s
										run stm' s'
								else return s
run (Assign (A_Zmienna var) aExp) s = do
								val <- aEval aExp s
								changeV (var,val) s
run (Bstm var_list stm_list) s = do
									s' <- wejdz_do_bloku s
									s'' <- addVs var_list s'
									s''' <- foldM (flip run) s'' stm_list
									wyjdz_z_bloku s'''
