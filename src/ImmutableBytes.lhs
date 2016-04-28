> module ImmutableBytes where
> import Types
> import Utility

> import qualified Data.Map.Strict       as Map
> import           Data.ByteString       as B
> import           Data.Word
> import           Text.Printf
> import qualified Data.ByteString.Char8 as Char

In the blog, Eric uses simple String as the backing store for
bytes. OCaml has 8-bit Char's. However, Haskell's Char's are 2 words
(4 bytes and 8 byte for x86 and x64 respectively). Haskell has a
ByteString type. This is a list of 8-bit bytes (type Word8).

This also means that the tye of the IntMap is Word8.

See https://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString.html
and https://wiki.haskell.org/GHC/Memory_Footprint

> data T = T { original_bytes :: ByteString,
>              edits          :: Map.Map Int Word8 }

A function that will return an empty T.

> make :: String -> T
> make bytes = T (Char.pack bytes) Map.empty

A function that gives us the largest allowed address.

> size = B.length . original_bytes

readByte takes the datastore and returns the byte at the given
address, if it exists. Throws a hard error if it is not available
(i.e., invalid address).

> readByte :: T -> ByteAddress -> Int
> readByte bytes address =
>   if outOfRange address (size bytes)
>      then error $ printf "Error: Reading address out of range: %s" (show address)
>      else let (ByteAddress addr) = address
>               c = Map.lookup addr (edits bytes)
>               word = case c of
>                       Just word -> word
>                       Nothing   -> index (original_bytes bytes) addr in
>           intFromWord word

And of course, a function to write a new value to an address.

> writeByte :: T -> ByteAddress -> Int -> T
> writeByte bytes address value =
>   if outOfRange address (size bytes)
>      then error $ printf "Error: Writing address out of range: %s" (show address)
>      else let (ByteAddress addr) = address
>               b                  = fromIntegral value
>               edits'             = Map.insert addr b (edits bytes) in
>           bytes { edits = edits'}
