> module ZString where
> import Utility
> import Types
> import Story

> import Text.Printf
> import Debug.Trace
> import Data.Char


The compression algorithm used from version 3 and up is the following
(text from blog by Eric Lippert):

Every 16 bits of characters contain three five-bit chars and a
continuation bit (is there more to come?). Each char can be drawn from
3 different alphabets (alphabetTable).

Each char's meaning (from the three) has a different meaning depending
on the previous char.

If the character is 1, 2, or 3, then the next character indicates an
offset into the abbreviation table; the string wishes to embed an
abbreviation at this point. So now we see why there are 96
abbreviations: three possible lead characters, 32 possible trailing
characters, makes 96 combinations.

If the character is 4 or 5 then the next character is drawn from the
uppercase or symbol alphabets, respectively, unless…

If the character is 5, and the next character is 6 then the following
two five-bit characters should be combined into one ten-bit character,
which is then interpreted as an eight-bit ASCII code. (This is
actually a slight lie; the coding used by the Z-machine does not
exactly match ASCII. But I am going to ignore that fact for the
purposes of this article.)

Otherwise, it’s just an ordinary lowercase code.


> data StringState
>  = Alphabet Int
>  | Abbrev AbbreviationNumber
>  | Leading
>  | Trailing Int

> abbrev0  = Abbrev (Abbreviation 0)
> abbrev32 = Abbrev (Abbreviation 32)
> abbrev64 = Abbrev (Abbreviation 64)


> alphabet0 = Alphabet 0
> alphabet1 = Alphabet 1
> alphabet2 = Alphabet 2

> alphabetTable = [
>   [ " ", "?", "?", "?", "?", "?", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
>     "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ],
>   [ " ", "?", "?", "?", "?", "?", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
>     "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ],
>   [ " ", "?", "?", "?", "?", "?", "?", "\n", "0", "1", "2", "3", "4", "5", "6", "7",
>     "8", "9", ".", ",", "!", "?", "_", "#", "'", "\"", "/", "\\", "-", ":", "(", ")" ] ]

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



> read story (ZString address) =
>   let processZChar (ZChar zchar) state =
>         case (zchar, state) of


If the first char in the zstring was an integer between 1 and 3 then
it means that the next char we are going to read is just an offset
into the abbreviation table.

Recall that we are dealing with 5-bit chars here. So the maximum
integer we can store is 32. Hence the 3 leading characters.

>           (1, Alphabet _) -> ("", abbrev0)
>           (2, Alphabet _) -> ("", abbrev32)
>           (3, Alphabet _) -> ("", abbrev64)

If the previous state was an offset into the abbreviation table we
will catch that now. We read the next char and add that to the
abbreviation offset, that is the identifier of the abbreviation we
like (1 to 96).

When we have read that abbreviation we have a new address to a new
string in memory. So we read in that string via a recursive call and
we obtain the string that has to be put here.

>           (_, Abbrev (Abbreviation a)) ->
>             let abbrv = Abbreviation (a + zchar)
>                 addr  = abbreviationZString story abbrv
>                 str   = ZString.read story addr
>             in
>               (str, alphabet0)

If the character is 4 or 5 then the next character is drawn from the
uppercase or symbol alphabets, respectively, unless…

>           (4, Alphabet _) -> ("", alphabet1)
>           (5, Alphabet _) -> ("", alphabet2)


The previous state could have been a five. And that means the current
state is alphabet2. But if the current char is a 6 then we have to
combine the next two chars together!

So if we encounter a 6 we go into the state "Leading" which means that
the next char is the leading half.

If we read a zchar and we are in the leading state then we know that
we have just read the first part (the high part) so we have to pass
that along in the Trailing state.

If we then read the next char we are in the trailing state, we just
read the second half. We combine the two chars together into a ten-bit
character and go back to state alphabet0.

>           (6, Alphabet 2) -> ("", Leading)
>           (_, Leading)    -> ("", (Trailing zchar))
>           (_, Trailing high) ->
>             let s = stringOfChar $ chr (high * 32 + zchar)
>             in
>               (s, alphabet0)

The last case just reads a char from the alphabet table and goes back
to alphabet0 as a state and returns the char from the table.

>           (_, Alphabet a) -> (((alphabetTable !! a) !! zchar), alphabet0)

The aux function reads in the word at the given address and adds it to
the accumulator ("" for the initial call).

>       aux acc state1 currentAddress =
>         let zcharBitSize = size5
>             word         = readWord story currentAddress
>             isEnd        = fetchBit bit15 word
>             zchar1       = ZChar $ fetchBits bit14 zcharBitSize word
>             zchar2       = ZChar $ fetchBits bit9  zcharBitSize word
>             zchar3       = ZChar $ fetchBits bit4  zcharBitSize word
>             (text1, state2)    = processZChar zchar1 state1
>             (text2, state3)    = processZChar zchar2 state2
>             (text3, stateNext) = processZChar zchar3 state3
>             newAcc             = acc ++ text1 ++ text2 ++ text3
>         in
>           if isEnd
>            then newAcc
>            else aux newAcc stateNext (incWordAddr currentAddress)
>   in

read takes a storyfile and a ZString. The first thing it does is read
in the word at that address. We know that word contains 3 chars and a
continue bit.

in aux we first read out the three chars and then process them. The
result of processZChar is a text and a new state.

>     aux "" alphabet0 (WordAddress address)


> displayBytes story (ZString addr) =
>   let aux current acc =
>         let word   = readWord story current
>             isEnd  = fetchBits bit15 size1 word
>             zchar1 = fetchBits bit14 size5 word
>             zchar2 = fetchBits bit9  size5 word
>             zchar3 = fetchBits bit4  size5 word
>             s      = printf "%02x %02x %02x" zchar1 zchar2 zchar3
>             acc    = acc ++ s
>         in
>           if isEnd == 1
>            then acc
>            else aux (incWordAddr current) acc
>   in
>     aux (WordAddress addr) ""



















-- > displayBytes story (ZString addr) =
-- >   aux (WordAddress addr) ""
-- >   where aux current acc = let word   = readWord story current
-- >                               isEnd  = fetchBits bit15 size1 word
-- >                               zchar1 = fetchBits bit14 size5 word
-- >                               zchar2 = fetchBits bit9  size5 word
-- >                               zchar3 = fetchBits bit4  size5 word
-- >                               s      = printf "%02x %s %02x %s %02x %s "
-- >                                         zchar1 (alphabetTable !! zchar1)
-- >                                         zchar2 (alphabetTable !! zchar2)
-- >                                         zchar3 (alphabetTable !! zchar3)
-- >                               acc'    = acc ++ s in
-- >                           if isEnd == 1
-- >                             then acc'
-- >                             else aux (incWordAddr current) acc'
