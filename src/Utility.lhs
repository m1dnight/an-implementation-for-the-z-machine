> module Utility where
> import Data.Bits hiding (setBit, clearBit)
> import Data.Word
> import Types

We first declare a few constants to use through our code.

> bit0  = BitNumber 0
> bit1  = BitNumber 1
> bit2  = BitNumber 2
> bit3  = BitNumber 3
> bit4  = BitNumber 4
> bit5  = BitNumber 5
> bit6  = BitNumber 6
> bit7  = BitNumber 7
> bit8  = BitNumber 8
> bit9  = BitNumber 9
> bit10 = BitNumber 10
> bit11 = BitNumber 11
> bit12 = BitNumber 12
> bit13 = BitNumber 13
> bit14 = BitNumber 14
> bit15 = BitNumber 15

> size1  = BitSize 1
> size2  = BitSize 2
> size3  = BitSize 3
> size4  = BitSize 4
> size5  = BitSize 5
> size6  = BitSize 6
> size7  = BitSize 7

The following function returns a boolean indicating wheter or not the
asked bit is set.

> fetchBit (BitNumber n) word = (word .&. (1 `shiftL` n)) `shiftR` n == 1

The function clearBit obviously clears a bit. If it is 1, set to 0 and
vice versa.

> clearBit (BitNumber n) word = word .&. (complement (1 `shiftL` n))

setBit sets a bit to 1 if it is not yet set.

> setBit (BitNumber n) word = word .|. (1 `shiftL` n)

setBitTo takes a value and sets that bit to that given value.

> setBitTo n word value = if value == 1 then setBit n word else clearBit n word

fetchBits takes a word and returns the sequence of bits from n to 

 - The mask is computed by shifting -1 to the left for `len`. This
   generates a word with all ones, but `len` zeros on the right.

 - Shifting `word` (high - length + 1) to the right drops the first
   bits. And then the mask is "anded", so we only leave the first
   `len` bits in tact.

> fetchBits (BitNumber high) (BitSize len) word =
>   let mask = complement ((-1 :: Int) `shiftL` len)
>   in
>     (word `shiftR` (high - len + 1)) .&. mask

inRange takes a ByteAddress and checks if it is below a certain
threshold.

> inRange (ByteAddress address) size = 0 <= address && address < size

outOfRange is the inverse of inRange

> outOfRange address size = not $ inRange address size

We need a function to convert a Word8 to an Int, not an Integer. Since
we are following the blog by Eric we want to keep as close to his
implementation as possible.

> intFromWord :: Word8 -> Int
> intFromWord = fromIntegral . toInteger
