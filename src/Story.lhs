> module Story where
> import           Utility
> import           Types
> import qualified ImmutableBytes        as ImmutableBytes

---------------------------------------------------------------------------------

> import qualified Data.Map.Strict       as Map
> import           Data.ByteString       as B
> import           Data.Word
> import           Text.Printf
> import qualified Data.ByteString.Char8 as Char
> import           Data.Bits hiding (setBit, clearBit)

The dynamic region of the memory can be addressed with 16 bit pointers
(2 bytes). This means that the dynamic memory has a maximum of 0xFFFF
addresses. All above that is considered static memory.

The static memory can no longer be addressed by 16-bit pointers so
there is a small convention for this. Each pointer into the static
memory that is not addressable by a 16-bit pointer has to be addressed
using a 17-bit pointer, but we always keep the most-right bit
zero. This means that we can actually still fit that pointer in a
16-bit array.

The downside of this approach is that we can only form even addresses
into the static memory (the right-most bit is 1 in decimal and that is
always zero for static memory).

Also note that the static memory is supposed to be read-only. We only
write changes to the dynamic memory.

A final note, the Z-machine works mostly with 2-byte integers (16 bit)
called "words". So to read a word we have to read 2 bytes from our
dynamic/static memory.

> data T = T { dynamic_memory :: ImmutableBytes.T,
>              static_memory  :: ByteString }


We need a constructor function.

> make :: ImmutableBytes.T -> ByteString -> T
> make dynamic static = Story.T dynamic static

> makeFromByteString :: ByteString -> ByteString -> T
> makeFromByteString dynamic static = Story.T (ImmutableBytes.makeFromByteString dynamic) static

The readByte function simply reads a single byte from the memory of
the VM. Since we have to deal with dynamic and static memory
explicitly we check if the pointer is in the range of the dynamic
memory.

If I reason correctly, this means if the pointer is > 0xFFFF then we
have to fetch it from static memory. But our static memory can also be
addressable by 16-bit pointers so a check is needed based on the size
of the dynamic memory.

> readByte :: T -> ByteAddress -> Int
> readByte story address =
>   let dynamicSize = ImmutableBytes.size $ dynamic_memory story
>   in
>     if inRange address dynamicSize
>        then ImmutableBytes.readByte (dynamic_memory story) address
>        else let staticAddr = decByteAddrBy address dynamicSize
>             in
>               dereferenceString staticAddr (static_memory story)


A word in the VM consists of two bytes. A word is a 16-bit integer,
hence two bytes of 8 bits.

In OCaml the code is a bit more elegant: the return value is `256 *
high + low`.  In our case, we have read two Word8s. So the first thing
we do is convert high to a Word16. Then shift left 8 times and add
them together.

Example:
high = 01010001
low  = 00001111

high to Word16 = 00000000 01010001
low to Word16  = 00000000 00001111
shift high     = 01010001 00000000
high + low     = 01010001 00001111

> readWord :: T -> WordAddress -> Int
> readWord story address =
>   let high = readByte story (addressOfHighByte address)
>       low  = readByte story (addressOfLowByte  address)
>   in
>     256 * high + low

The building block of writing a word is writing a byte.

> writeByte story address value =
>   let dynamicMemory = ImmutableBytes.writeByte (dynamic_memory story) address value
>   in
>     story { dynamic_memory = dynamicMemory }

Writing a word requires us to pick apart the Int. The right-most 8
bits have to into the high address and the left-most 8 bits have to go
into the low address.

> writeWord story address value =
>   let high   = (value `shiftR` 8) .&. 0xFF
>       low    = value .&. 0xFF
>       story' = writeByte story (addressOfHighByte address) high
>   in
>     writeByte story' (addressOfLowByte address) low

The version of the Zork file is encoded in the first byte of the file.

> versionOffset = ByteAddress 0

> version :: T -> Version
> version story = case (readByte story versionOffset) of
>                   1 -> V1
>                   2 -> V2
>                   3 -> V3
>                   4 -> V4
>                   5 -> V5
>                   6 -> V6
>                   7 -> V7
>                   8 -> V8
>                   _ -> error "Unkown version read from story!"

We need a little helper to determine if the version is lower than the
third version. We just derived Eq and Ord in the type and have that
one for free. From version 4 onward objects have more than 1 byte
object numbers, hence the differentiation.

> v3OrLower :: Version -> Bool
> v3OrLower v = v <= V3

The base of the object table begins at byte 10 in the story header.

> objectTableBase story =
>   let objectTableBaseOffset = WordAddress 10
>   in
>     ObjectBase $ readWord story objectTableBaseOffset


The following function reads the 24th byte from memory. It contains a
16 bit pointer into memory where the abbreviation table starts.

> abbreviationsTableBase story =
>   let abbreviationsTableBaseOffset = WordAddress 24
>   in
>     AbbreviationTableBase (readWord story abbreviationsTableBaseOffset)

Each story file (minizork.z3) starts out with a header of 64 bytes.
The first byte of the static memory is stored in a word (2 bytes)
starting at byte 14. So byte 14 and 15 contain the address of the byte
where our static memory starts.

> headerSize = 64
> staticMemoryOffset = WordAddress 14

dictionaryBase reads out the address of the dictionary from the
header. This is stored in the 8th byte.

> dictionaryBase story =
>   let dictionaryBaseOffset = WordAddress 8
>   in
>     DictionaryBase $ readWord story dictionaryBaseOffset




> load :: String -> IO T
> load filename =
>   do file <- B.readFile filename
>      let len  = B.length file
>      if len < headerSize
>         then error $ printf "Not a valid story file: %s" filename
>         else let high   = dereferenceString (addressOfHighByte staticMemoryOffset) file
>                  low    = dereferenceString (addressOfLowByte  staticMemoryOffset) file
>                  dynLen = high * 256 + low
>              in
>                if dynLen > len
>                   then error $ printf "%s is not a valid story file!" filename
>                   else let dynamic = B.take dynLen file
>                            static  = B.take (len - dynLen) (B.drop dynLen file)
>                         in
>                          return $ makeFromByteString dynamic static
