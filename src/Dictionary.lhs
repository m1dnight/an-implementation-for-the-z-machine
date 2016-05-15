> module Dictionary where
> import Utility
> import Types
> import Story
> import ZString

> import Text.Printf

Format of the dictionary:

Byte containing number of word seperators
Byte seperators, one per byte.
Byte giving the number of bytes in each dictionary entry
Word giving the number of table entries which follow

Each dictionary entry is 4 or 6 bytes of zstring data, followed by
bytes to make up the size of the dictionary entry.

> wordSeperatorsBase (DictionaryBase base) = ByteAddress base

To know how many seperators there are we simply read in the first byte
at the start of the dictionary.

> wordSeperatorsCount story =
>   let dictBase = Story.dictionaryBase story
>       wsBase   = wordSeperatorsBase dictBase
>   in
>     Story.readByte story wsBase

The first dictionary entry is simple. We compute the first address of
the dictionary, add the number of seperators (each 1 byte long) and
add one to that address.

> entryBase story =
>   let dictBase = Story.dictionaryBase story
>       wsCount  = wordSeperatorsCount story
>       wsBase   = wordSeperatorsBase dictBase
>   in
>     incByteAddrBy wsBase (wsCount + 1)

The length of each entry is encoded in the first byte after the
seperators.

> entryLength story = Story.readByte story (entryBase story)

The total number of entries is stores after the length of each entry.

> entryCount story =
>   let (ByteAddress addr) = incByteAddr (entryBase story)
>   in
>     Story.readWord story (WordAddress addr)

The actual first entry is stored at three bytes further than the base
of the dictionary.

> tableBase story =
>   let (ByteAddress addr) = incByteAddrBy (entryBase story) 3
>   in
>     DictionaryTableBase addr

The address of an entry is the number of the entry (0 ...) added to
the base address of the table, times the length of each entry.

> entryAddress story (Dictionary dictionaryNumber) =
>   let (DictionaryTableBase base) = tableBase story
>       length                     = entryLength story
>   in
>     DictionaryAddress $ base + dictionaryNumber * length


> entry story dictionaryNumber =
>   let (DictionaryAddress addr) = entryAddress story dictionaryNumber
>   in
>     ZString.read story (ZString addr)


> display story =
>   let count      = entryCount story
>       toString i = printf "%s " (entry story (Dictionary i))
>   in
>     accumulateStringsLoop toString 0 count
