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

> wordSeperatorsCount story =
>   let dictBase = Story.dictionaryBase story
>       wsBase   = wordSeperatorsBase dictBase
>   in
>     Story.readByte story wsBase



> entryBase story =
>   let dictBase = Story.dictionaryBase story
>       wsCount  = wordSeperatorsCount story
>       wsBase   = wordSeperatorsBase dictBase
>   in
>     incByteAddrBy wsBase (wsCount + 1)


> entryLength story = Story.readByte story (entryBase story)


> entryCount story =
>   let (ByteAddress addr) = incByteAddr (entryBase story)
>   in
>     Story.readWord story (WordAddress addr)

> tableBase story =
>   let (ByteAddress addr) = incByteAddrBy (entryBase story) 3
>   in
>     DictionaryTableBase addr


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
