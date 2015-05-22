module Main where

import Composer.Types
import Composer.Core
import Composer.Pretty

-- | "Маленькой елочке холодно зимой".
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

-- | ???
song1 = melodyUp song b3

-- | ???
song2 = melodyDown song m2

-- | ???
song3 = nPlet [gP Empty, eP Empty, eP Empty] 4

main :: IO ()
main = do
  print (showMelody song)
  print (showMelody song1)
  print (showMelody song2)
  print (showSeq song3)
