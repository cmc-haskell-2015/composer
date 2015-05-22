-- | ???
module Composer.Pretty where

import Composer.Types

-- * Вывод на экран

-- | Вывод ноты.
showNote :: Note -> (Letter, Int, Alteration)
showNote (Note (Name (n, o)) a) = (n, o, a) 

-- | Вывод составляющих такта.
showSeq :: Seq -> ([(Letter, Int, Alteration)], Duration)
showSeq (NSeq (Just n) d) = ([showNote n], d)
showSeq (NSeq (Nothing) d) = ([(R, 0, Empty)], d)
showSeq (NPlet x d) = (map (\y -> showNote y) x, d)

-- | Вывод такта.
showBar :: Bar -> [([(Letter, Int, Alteration)], Duration)]
showBar (Bar x) = map (\y -> showSeq y) x

-- | Вывод партии.
showParty :: Party -> ((Letter, Alteration, Mode), (Int, Int), [[([(Letter, Int, Alteration)], Duration)]])
showParty (Party (Tonality l a m) (Size (i,j)) b) = ((l,a,m), (i,j), map (\y -> showBar y) b)

-- | Вывод одноголосой мелодии.
showMelody :: Melody -> [((Letter, Alteration, Mode), (Int, Int), [[([(Letter, Int, Alteration)], Duration)]])]
showMelody (Melody x) = map (\y -> showParty y) x

-- | Вывод композиции.
showCompose :: Compose -> [[((Letter, Alteration, Mode), (Int, Int), [[([(Letter, Int, Alteration)], Duration)]])]]
showCompose (Compose c) = map (\y -> showMelody y) c

