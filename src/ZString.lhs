> module ZString where
> import Utility
> import Types
> import Story

> import Text.Printf
> import Debug.Trace

The length of the abbreviation table is always 96 words.

> abbreviationTableLength = 96


The type WordZStringAddress is a compressed pointer. See Types.lhs for
more info. Basically it's the pointer divided by two. So to convert it
to an actual pointer in memory (A ZStringAddresS) we have to multiply
it by two.

> decodeWordAddress (WordZString word_address) = ZString $ word_address * 2

the first abbreviation is obviously zero based. So the address of that
is the base address.

> firstAbbrevAddr (AbbreviationTableBase base) = WordAddress base

To get the nth abbreviation we can use the abbreviationZString
function.

> abbreviationZString story (Abbreviation n) =
>   if n < 0 || n >= abbreviationTableLength
>     then error "Bad offset into abbreviation table!"
>     else let base     = firstAbbrevAddr (abbreviationsTableBase story)
>              abbrAddr = incWordAddrBy base n
>              wordAddr = WordZString $ readWord story abbrAddr in
>          decodeWordAddress wordAddr

> alphabetTable = [ "_", "?", "?", "?", "?", "?", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
>                   "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]


> displayBytes story (ZString addr) =
>   aux (WordAddress addr) ""
>   where aux current acc = let word   = readWord story current
>                               isEnd  = fetchBits bit15 size1 word
>                               zchar1 = fetchBits bit14 size5 word
>                               zchar2 = fetchBits bit9  size5 word
>                               zchar3 = fetchBits bit4  size5 word
>                               s      = printf "%02x %s %02x %s %02x %s "
>                                         zchar1 (alphabetTable !! zchar1)
>                                         zchar2 (alphabetTable !! zchar2)
>                                         zchar3 (alphabetTable !! zchar3)
>                               acc'    = acc ++ s in
>                           if isEnd == 1
>                             then acc'
>                             else aux (incWordAddr current) acc'
