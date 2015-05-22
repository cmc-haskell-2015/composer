{-# LANGUAGE TypeOperators #-}

-- | ???
module Composer.Types where

import Data.Monoid

-- * Основные структуры данных (часть 1)

-- | Буква (латинское название ноты).
data Letter
  = C   -- ^ ???
  | D   -- ^ ???
  | E   -- ^ ???
  | F   -- ^ ???
  | G   -- ^ ???
  | A   -- ^ ???
  | B   -- ^ ???
  | R   -- ^ ???
  deriving (Show, Eq, Ord)

-- | Лад.
data Mode
  = Major     -- ???
  | Minor     -- ???
  | Mempty    -- ???
  deriving (Show, Eq)

-- | Размер такта.
data Size = Size (Int, Int) deriving (Show, Eq)

-- | Имя ноты (БУКВА, ОКТАВА).
data Name = Name (Letter, Int) deriving (Show)

-- | Продолжительность.
data Duration = Int :/ Int deriving (Show)

instance Num Duration where
	(+) = undefined
	(*) = error "nonsense"
	signum = undefined
	(-) = undefined
	abs =  undefined
	fromInteger n = (1 :/ fromInteger n)

-- * Основные структуры данных (часть 2)

-- | Альтерация.
data Alteration = Diez | Bemol | DDiez | DBemol | Bekart | Empty deriving (Show, Eq)

-- | Тональность.
data Tonality = Tonality Letter Alteration Mode deriving (Show, Eq)

-- | Нота.
data Note = Note Name Alteration deriving (Show)

-- | Составляющие такта.
data Seq
  = NSeq (Maybe Note) Duration    -- ^ ???
  | NPlet [Note] Duration         -- ^ ???
  deriving (Show)

-- | Такт.
data Bar = Bar [Seq] deriving (Show)

-- | Одноголосая партия.
data Party = Party Tonality Size [Bar] | Error deriving (Show)

-- | Одноголосая мелодия.
data Melody = Melody [Party] deriving (Show)

-- | Многоголосая мелодия.
data Compose = Compose [Melody] deriving (Show)

-- | Диатонический интервал.
data DInterval = DInterval IntervalName (Int, Int) deriving (Show)

-- | Названия диатонического интервала.
data IntervalName
  = Prima
  | Secunda
  | Tertia 
  | Quarta
  | Quinta 
  | Sexta
  | Septima 
  | Octava
  deriving (Show, Eq, Ord)

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

