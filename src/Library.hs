{-# LANGUAGE TypeOperators #-}
import Data.Monoid
import Data.Foldable



-- ---------- ОСНОВНЫЕ СТРУКТУРЫ ДАННЫХ (ЧАСТЬ 1) ----------
-- Буква (латинское название ноты).
data Letter = C | D | E | F | G | A | B deriving (Show, Eq, Ord)
-- Лад.
data Mode = Major | Minor | Mempty deriving (Show, Eq)
-- Размер такта.
data Size = Size (Int, Int) deriving (Show, Eq)
-- Имя ноты (БУКВА, ОКТАВА).
data Name = Name (Letter, Int) deriving (Show)
-- Продолжительность.
data Duration = Int :/ Int deriving (Show)
instance Num Duration where
	(+) = undefined
	(*) = error "nonsense"
	signum = undefined
	(-) = undefined
	abs =  undefined
	fromInteger n = (1 :/ fromInteger n)



-- ---------- ОСНОВНЫЕ СТРУКТУРЫ ДАННЫХ (ЧАСТЬ 2) ----------
-- Альтерация.
data Alteration = Diez | Bemol | DDiez | DBemol | Bekart | Empty deriving (Show, Eq)

-- Тональность.
data Tonality = Tonality Letter Alteration Mode deriving (Show, Eq)

-- Нота.
data Note = Note Name Alteration deriving (Show)

-- Составляющие такта.
data Seq = NSeq (Maybe Note) Duration  deriving (Show)

-- Такт.
data Bar = Bar [Seq] deriving (Show)

-- Одноголосая партия.
data Party = Party Tonality Size [Bar] | Error deriving (Show)

-- Одноголосая мелодия.
data Melody = Melody [Party] deriving (Show)

-- Многоголосая мелодия.
data Compose = Compose [Melody] deriving (Show)

-- Диатонический интервал.
data DInterval = DInterval IntervalName (Int, Int) deriving (Show)
-- Названия диатонического интервала.
data IntervalName = Prima
				  | Secunda | Tertia 
				  | Quarta | Quinta 
				  | Sexta | Septima 
				  | Octava deriving (Show, Eq, Ord)



-- Такт, мелодия, композиция, партия.
instance Monoid Bar where
	mempty = Bar mempty
	mappend (Bar xs) (Bar ys) = Bar (xs <> ys)

instance Monoid Party where
	mempty = Party (Tonality C Empty Mempty) (Size (1, 1)) mempty
	mappend (Party t1 s1 xs) (Party t2 s2 ys)
		| (t1 == t2) && (s1 == s2) = Party t1 s1 (xs <> ys)
		| otherwise = Error

instance Monoid Melody where
	mempty = Melody mempty
	Melody xs `mappend` Melody ys = Melody (xs <> ys)

instance Monoid Compose where
	mempty = Compose mempty
	mappend (Compose xs) (Compose ys) = Compose (xs <> ys)



-- ---------- ВСПОМОГАТЕЛЬНЫЕ СТРУКТУРЫ ----------
letters = [C, D, E, F, G, A, B]
intervals = [Prima, Secunda, Tertia, Quarta, Quinta, Sexta, Septima, Octava]



-- ---------- ВСПОМОГАТЕЛЬНЫЕ Ф-ЦИИ ----------
-- Индекс элемента в списке.
position :: Eq a => a -> [a] -> Int -> Int
position e (x : xs) n | e == x = n
					  | otherwise = position e xs (n + 1)

-- Элемент списка по индексу.
element :: Int -> [a] -> a
element 0 (x : xs) = x
element n (x : xs) = element (n - 1) xs



-- ---------- ФУНКЦИОНАЛ БАЗЫ ----------
-- Последовательное склеивание нескольких мелодий.
appendMelody :: [Melody] -> Melody
appendMelody = fold
 
-- Вспомогательная функция для склеивания мелодий.
melodyPlus :: Melody -> Melody -> Melody
melodyPlus = mappend



-- Увеличение тональности на интервал.
-- (Tonality = Tonality Letter Alteration Mode)
tonalityUp :: Tonality -> DInterval -> Tonality
tonalityUp (Tonality l a m) (DInterval n (x, y))
	= Tonality new_l new_a m where
		new_l = newLetter l n
		new_a = newAlteration l a n (x, y)

-- Уменьшение тональности на интервал
tonalityDown :: Tonality -> DInterval -> Tonality		
tonalityDown (Tonality l a m) (DInterval n (x, y))
	= Tonality new_l new_a m where
		new_l = newLetter_ l n
		new_a = newAlteration_ l a n (x, y)


-- Получение названия новой ноты.
newLetter :: Letter -> IntervalName -> Letter
newLetter l int
	= element (((position l letters 0) + (position int intervals 0)) `mod` 7) letters

newLetter_ :: Letter -> IntervalName -> Letter
newLetter_ l int
	= element (((position l letters 0) - (position int intervals 0)) `mod` 7) letters

-- Получение альтерации новой ноты.
newAlteration :: Letter -> Alteration -> IntervalName -> (Int, Int) -> Alteration
newAlteration l a (Prima) (n1, n2) = fPrima l a (n1, n2)
newAlteration l a (Secunda) (n1, n2) = fSecunda l a (n1, n2)
newAlteration l a (Tertia) (n1, n2) = fTertia l a (n1, n2)
newAlteration l a (Quarta) (n1, n2) = fQuarta l a (n1, n2)
newAlteration l a (Quinta) (n1, n2) = fQuinta l a (n1, n2)
newAlteration l a (Sexta) (n1, n2) = fSexta l a (n1, n2)
newAlteration l a (Septima) (n1, n2) = fSeptima l a (n1, n2)
newAlteration l a (Octava) (n1, n2) = fOctava l a (n1, n2)

newAlteration_ :: Letter -> Alteration -> IntervalName -> (Int, Int) -> Alteration
newAlteration_ l a (Prima) (n1, n2) = fPrima l a (n1, n2)
newAlteration_ l a (Secunda) (n1, n2) = fSecunda_ l a (n1, n2)
newAlteration_ l a (Tertia) (n1, n2) = fTertia_ l a (n1, n2)
newAlteration_ l a (Quarta) (n1, n2) = fQuarta_ l a (n1, n2)
newAlteration_ l a (Quinta) (n1, n2) = fQuinta_ l a (n1, n2)
newAlteration_ l a (Sexta) (n1, n2) = fSexta_ l a (n1, n2)
newAlteration_ l a (Septima) (n1, n2) = fSeptima_ l a (n1, n2)
newAlteration_ l a (Octava) (n1, n2) = fOctava l a (n1, n2)


fPrima :: Letter -> Alteration -> (Int, Int) -> Alteration
fPrima _ a (0, 0) = a

fSecunda :: Letter -> Alteration -> (Int, Int) -> Alteration
fSecunda l a (1, 2) 
	| (l == E) || (l == B) = a
	| a == Diez = Empty
	| a == Empty = Bemol
	| otherwise = DBemol
fSecunda l a (1, 1) 
	| (l == C) || (l == D) || (l == F) || (l == G) || (l == A) = a
	| a == Diez = DDiez
	| a == Empty = Diez
	| otherwise = Empty

fSecunda_ :: Letter -> Alteration -> (Int, Int) -> Alteration
fSecunda_ l a (1, 2)
	| (l == C) || (l == F) = a
	| a == Empty = Diez
	| a == Diez = DDiez
	| otherwise = Empty
fSecunda_ l a (1, 1)
	| (l == D) || (l == E) || (l == G) || (l == A) || (l == B) = a
	| a == Empty = Bemol
	| a == Diez = Empty
	| otherwise = DBemol

fTertia :: Letter -> Alteration -> (Int, Int) -> Alteration
fTertia l a (3, 2)
	| (l == D) || (l == E) || (l == A) || (l == B) = a
	| a == Diez = Empty
	| a == Empty = Bemol
	| otherwise = DBemol
fTertia l a (2, 1)
	| (l == C) || (l == F) || (l == G) = a
	| a == Diez = DDiez
	| a == Empty = Diez
	| otherwise = Empty

fTertia_ :: Letter -> Alteration -> (Int, Int) -> Alteration
fTertia_ l a (3, 2)	
	| (l == C) || (l == D) || (l == F) || (l == G) = a
	| a == Empty = Diez
	| a == Bemol = Empty
	| otherwise = DDiez
fTertia_ l a (2, 1)
	| (l == E) || (l == A) || (l == B) = a
	| a == Empty = Bemol
	| a == Bemol = Empty
	| otherwise = DBemol

-- (Diez | Bemol | DDiez | DBemol | Bekart | Empty)
fQuarta :: Letter -> Alteration -> (Int, Int) -> Alteration
fQuarta l a (5, 2)
	| l /= F = a
	| a == Diez = Empty
	| a == Empty = Bemol
	| otherwise = DBemol
fQuarta l a (3, 1)
	| l == F = a
	| a == Diez = DDiez
	| a == Empty = Diez
	| otherwise = Empty

fQuarta_ :: Letter -> Alteration -> (Int, Int) -> Alteration
fQuarta_ l a (5, 2)
	| (l /= B) = a
	| a == Empty = Diez
	| a == Diez = DDiez
	| otherwise = Empty
fQuarta_ l a (3, 1)
	| l == B = a
	| a == Empty = Bemol
	| a == Diez = Empty
	| otherwise = DBemol

fQuinta :: Letter -> Alteration -> (Int, Int) -> Alteration
fQuinta l a (3, 1) 
	| l == B = a
	| a == Diez = Empty
	| a == Empty = Bemol
	| otherwise = DBemol
fQuinta l a (7, 2)
	| (l == C) || (l == D) || (l == E) || (l == F) || (l == G) || (l == A) = a
	| a == Diez = DDiez
	| a == Empty = Diez
	| otherwise = Empty

fQuinta_ :: Letter -> Alteration -> (Int, Int) -> Alteration
fQuinta_ l a (3, 1)
	| (l == F) = a
	| a == Empty = Diez
	| a == Diez = DDiez
	| otherwise = Empty
fQuinta_ l a (7, 2)
	| (l /= F) = a
	| a == Empty = Bemol
	| a == Diez = Empty
	| otherwise = DBemol

fSexta :: Letter -> Alteration -> (Int, Int) -> Alteration
fSexta l a (4, 1)
	| (l == E) || (l == A) || (l == B) = a
	| a == Diez = Empty
	| a == Empty = Bemol
	| otherwise = DBemol
fSexta l a (9, 2)
	| (l == C) || (l == D) || (l == F) || (l == G) = a
	| a == Diez = DDiez
	| a == Empty = Diez
	| otherwise = Empty

fSexta_ :: Letter -> Alteration -> (Int, Int) -> Alteration
fSexta_ l a (4, 1)
	| (l == C) || (l == F) || (l == G) = a
	| a == Empty = Diez
	| a == Diez = DDiez
	| otherwise = Empty
fSexta_ l a (9, 2)
	| (l == D) || (l == E) || (l == A) || (l == B) = a
	| a == Empty = Bemol
	| a == Diez = Empty
	| otherwise = DBemol

fSeptima :: Letter -> Alteration -> (Int, Int) -> Alteration
fSeptima l a (5, 1)
	| (l == D) || (l == E) || (l == G) || (l == A) || (l == B) = a
	| a == Diez = Empty
	| a == Empty = Bemol
	| otherwise = DBemol
fSeptima l a (11, 2)
	| (l == C) || (l == F) = a
	| a == Diez = DDiez
	| a == Empty = Diez
	| otherwise = Empty

fSeptima_ :: Letter -> Alteration -> (Int, Int) -> Alteration
fSeptima_ l a (5, 1)
	| (l == C) || (l == D) || (l == F) || (l == G) || (l == A) = a
	| a == Diez = DDiez
	| a == Empty = Diez
	| otherwise = Empty
fSeptima_ l a (11, 2)
	| (l == E) || (l == B) = a
	| a == Empty = Bemol
	| a == Diez = Empty
	| otherwise = DBemol	

fOctava :: Letter -> Alteration -> (Int, Int) -> Alteration
fOctava _ a (0, 0) = a

-- Увеличение ноты на интервал
noteUp :: Note -> DInterval -> Note
noteUp (Note (Name (l, oct)) a) (DInterval int (i, j))
	| (position l letters 0) + (position int intervals 0) >= 7 = (Note (Name (l', oct + 1)) a')
	| otherwise = (Note (Name (l', oct)) a') where
		l' = newLetter l int
		a' = newAlteration l a int (i, j)

-- Уменьшение ноты на интервал
noteDown :: Note ->	 DInterval -> Note
noteDown (Note (Name (l, oct)) a) (DInterval int (i, j))
	| (position l letters 0) + (position int intervals 0) <= 0 = (Note (Name (l', oct - 1)) a')
	| otherwise = (Note (Name (l', oct)) a') where
		l' = newLetter_ l int
		a' = newAlteration_ l a int (i, j)	

-- Увеличение составляющих тактов на интервал
nseqUp :: Seq -> DInterval -> Seq
nseqUp (NSeq (Nothing) d) int = NSeq (Nothing) d
nseqUp (NSeq (Just n) d) int = NSeq (Just (noteUp n int)) d

-- Уменьшение -//-
nseqDown :: Seq -> DInterval -> Seq
nseqDown (NSeq (Nothing) d) int = NSeq (Nothing) d
nseqDown (NSeq (Just n) d) int = NSeq (Just (noteDown n int)) d

-- Увеличение такта на интервал
barUp :: Bar -> DInterval -> Bar
barUp (Bar x) int = Bar (map (\y -> nseqUp y int) x)

-- Уменьшение -//-
barDown :: Bar -> DInterval -> Bar
barDown (Bar x) int = Bar (map (\y -> nseqDown y int) x)

-- Увеличение партии на интервал
partyUp :: Party -> DInterval -> Party
partyUp (Party t s b) int = Party t' s b' where
	t' = tonalityUp t int
	b' = map (\y -> barUp y int) b

-- Уменьшение -//-
partyDown :: Party -> DInterval -> Party
partyDown (Party t s b) int = Party t' s b' where
	t' = tonalityUp t int
	b' = map (\y -> barDown y int) b

-- Увеличение одноголосой мелодии на интервал
melodyUp :: Melody -> DInterval -> Melody 
melodyUp (Melody m) int = Melody (map (\y -> partyUp y int) m)

-- Уменьшение -//-
melodyDown :: Melody -> DInterval -> Melody
melodyDown (Melody m) int = Melody (map (\y -> partyDown y int) m)

-- Увеличение многоголосой мелодии на интервал
composeUp :: Compose -> DInterval -> Compose
composeUp (Compose c) int = Compose (map (\y -> melodyUp y int) c)

-- Уменьшение -//-
composeDown :: Compose -> DInterval -> Compose
composeDown (Compose c) int = Compose (map (\y -> melodyDown y int) c)



-- ---------- ГЕНЕРАТОРЫ ----------
-- Генераторы нот 1-й октавы.
-- ДО
c :: Duration -> Seq
c = NSeq (Just (Note (Name (C, 3)) Empty))

-- РЕ
d :: Duration -> Seq
d = NSeq (Just (Note (Name (D, 3)) Empty))

-- МИ
e :: Duration -> Seq
e = NSeq (Just (Note (Name (E, 3)) Empty))

-- ФА
f :: Duration -> Seq
f = NSeq (Just (Note (Name (F, 3)) Empty))

-- СОЛЬ
g :: Duration -> Seq
g = NSeq (Just (Note (Name (G, 3)) Empty))

-- ЛЯ
a :: Duration -> Seq
a = NSeq (Just (Note (Name (A, 3)) Empty))

-- СИ
b :: Duration -> Seq
b = NSeq (Just (Note (Name (B, 3)) Empty))



-- Генераторы нот произвольной октавы.
-- ДО
c' :: Int -> Duration -> Seq
c' oc = NSeq (Just (Note (Name (C, oc + 2)) Empty))

-- РЕ
d' :: Int -> Duration -> Seq
d' oc = NSeq (Just (Note (Name (D, oc + 2)) Empty))

-- МИ
e' :: Int -> Duration -> Seq
e' oc = NSeq (Just (Note (Name (E, oc + 2)) Empty))

-- ФА
f' :: Int -> Duration -> Seq
f' oc = NSeq (Just (Note (Name (F, oc + 2)) Empty))

-- СОЛЬ
g' :: Int -> Duration -> Seq
g' oc = NSeq (Just (Note (Name (G, oc + 2)) Empty))

-- ЛЯ
a' :: Int -> Duration -> Seq
a' oc = NSeq (Just (Note (Name (A, oc + 2)) Empty))

-- СИ
b' :: Int -> Duration -> Seq
b' oc = NSeq (Just (Note (Name (B, oc + 2)) Empty))



-- Генератор паузы (rest).
r :: Duration -> Seq
r = NSeq Nothing



-- Генератор последовательности тактов.
bar :: [[Seq]] -> [Bar]
bar [] = []
bar (b : bs) = (Bar b) : (bar bs)

-- Генератор одноголосой партии.
party :: (Letter, Alteration, Mode) -> (Int, Int) -> [[Seq]] -> Party
party (l, a, m) s bs = Party (Tonality l a m) (Size s) (bar bs)

-- Генератор одноголосой мелодии.
melody :: [((Letter, Alteration, Mode), (Int, Int), [[Seq]])] -> Melody
melody ps = Melody (party's ps)
-- Генератор последовательности одноголосых партий.
party's  :: [((Letter, Alteration, Mode), (Int, Int), [[Seq]])] -> [Party]
party's  [] = []
party's  ( ((l, a, m), s, b) : ms) = (party (l, a, m) s b) : (party's ms)
		


-- Генераторы диатонических интервалов
-- Чистая прима
ch1 :: DInterval
ch1 = DInterval Prima (0, 0)

-- Малая секунда
m2 :: DInterval
m2 = DInterval Secunda (1, 2)

-- Большая секунда
b2 :: DInterval
b2 = DInterval Secunda (1, 1)

-- Малая терция
m3 :: DInterval
m3 = DInterval Tertia (3, 2)

-- Большая терция
b3 :: DInterval
b3 = DInterval Tertia (2, 1)

-- Чистая кварта
ch4 :: DInterval
ch4 = DInterval Quarta (5, 2)

-- Увеличенная кварта
uv4 :: DInterval
uv4 = DInterval Quarta (3, 1)

-- Уменьшенная квинта
um5 :: DInterval
um5 = DInterval Quinta (3, 1)

-- Чистая квинта
ch5 :: DInterval
ch5 = DInterval Quinta (7, 2)

-- Малая секста
m6 :: DInterval
m6 = DInterval Sexta (4, 1)

-- Большая секста
b6 :: DInterval
b6 = DInterval Sexta (9, 2)

-- Малая септима
m7 :: DInterval
m7 = DInterval Septima (5, 1)

-- Большая септима
b7 :: DInterval
b7 = DInterval Septima (11, 2)

-- Чистая октава
ch8 :: DInterval
ch8 = DInterval Octava (6, 1)



-- ---------- ПРИМЕР: ----------
-- "Маленькой елочке холодно зимой"
song = melody
	[(
		(C, Empty, Major), (2, 4),
		[
			[g 4, e 8, e 8],
		    [g 4, e 8, e 8],
		    [g 8, f 8, e 8, d 8],
		    [c 4, r 4],
		    [a 4, c' 2 8, a 8],
		    [g 4, e 8, e 8],
		    [g 8, f 8, e 8, d 8],
		    [c 4, r 4]
		]
	)]

song1 = melodyUp song b3
