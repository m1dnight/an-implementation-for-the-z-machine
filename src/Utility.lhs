> module Utility where
> import Types

> import qualified Data.Map.Strict       as Map
> import           Data.ByteString       as B
> import           Data.Word
> import           Text.Printf
> import qualified Data.ByteString.Char8 as Char
> import           Data.Bits hiding (setBit, clearBit)


We need a function to convert a Word8 to an Int, not an Integer. Since
we are following the blog by Eric we want to keep as close to his
implementation as possible. OCaml has a built-in function to do this
but we do not have this.

> intFromWord :: Word8 -> Int
> intFromWord = fromIntegral . toInteger


> word8ToWord16 :: Word8 -> Word16
> word8ToWord16 w = let integral  = toInteger w
>                       wordeight = fromInteger integral :: Word16
>                   in
>                     wordeight




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

We need functions to do some pointer arithmetic. This are pointers to
W and not to Bytes.

> incByteAddrBy (ByteAddress addr) offset = ByteAddress $ addr + offset
> decByteAddrBy (ByteAddress addr) offset = ByteAddress $ addr - offset

dereferenceString takes an address and a lis tof bytes. It then
returns the byte at the given address.

> dereferenceString address bytes =
>   if outOfRange address (B.length bytes)
>      then error "Deref byte in string out of bounds"
>      else let (ByteAddress addr) = address
>           in
>             intFromWord $ index bytes addr

Snippet from the blog post of Eric Lipper:

Story state is, as we described last time, a big block of memory,
typically more than would fit into an 8 bit machine like the Commodore
64. Remember, you’ve got to have room for the interpreter
implementation in memory as well! Story state size is also typically
more than can be addressed with a 16 bit pointer. In addition, when we
save a game we actually are going to serialize out all the changes
that have been made to memory so far, so it makes sense to keep all
the mutable memory small and close together.

For these reasons memory is divided up into a “dynamic” region and a
“static” region. The dynamic region is at the low addresses, the
static region is at the top. Everything in the dynamic region can be
accessed with a 16 bit pointer, but it is possible that some portions
of static memory will be at addresses larger than 0xFFFF. The
convention for this “high” memory is to use 17 (or, in later versions,
more) bit pointers where the bottom bit is zero. Obviously that thing
can fit into 16 bits. The price you pay is of course that everything
in “high” memory must begin on an even boundary for it to be
addressible by only 16 bits, and that you have to remember to do a
computation on the 16 bit “packed” pointer before you use it as an
actual offset into memory.

I need to be able to say “given the address of a word, what are the
addresses of the low and high bytes?” Note that in the Z-machine,
words are always stored with the high byte at the lower address:

> addressOfHighByte (WordAddress address) = ByteAddress address
> addressOfLowByte  (WordAddress address) = ByteAddress $ address + 1

