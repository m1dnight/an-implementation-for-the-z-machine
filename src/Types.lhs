> module Types where
> import Data.Map.Strict

The BitNumber data type represent a bit index in a bit pattern.

> data BitNumber = BitNumber Int deriving (Show, Eq)

The BitSize data type represents a range of bits. I.e., a number of bits.

> data BitSize = BitSize Int deriving (Show, Eq)

ByteAddress is a simple tagged integer. It is an address in the memory
heap of the z-machine.

> data ByteAddress = ByteAddress Int deriving (Show, Eq)

WordAddress is a tag for word addresses. A word in the VM is 16
bits. So the address of a byte and a word are not the same!

> data WordAddress = WordAddress Int

The abbreviation table is a table in the z-file that contains the most
used pieces of text, for compression.

The address of the abreviation table is stored in a word starting at
byte 24 (remember: a word is two bytes). The contents of this word is
a 16-bit pointer into memory.

> data AbbreviationNumber = Abbreviation Int
> data AbbrevTableBase = AbbreviationTableBase Int

The abbreviation table is 96 words long. That is 192 bytes long. Eeach
word contains a 17 bit pointer. But a word is only 16 bytes long so
the pointers are divided by two to fit in a word. And that pointer,
when multiplied by two, points to a compressed string in memory.

The "compressed" pointers:

> data WordZStringAddress = WordZString Int

The decompressed pointers:

> data ZStringAddress = ZString Int

A single character extraced from ZString encodings.

> data ZChar = ZChar Int


The dictionary of words recognized by the game has a few types.

> data DictionaryBase      = DictionaryBase Int
> data DictionaryTableBase = DictionaryTableBase Int
> data DictionaryAddress   = DictionaryAddress Int
> data DictionaryNumber    = Dictionary Int
> data WordSeperatorNumber = WordSeperatorNumber Int

There are several different version of Zork. In total, there are 8.

> data Version = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 deriving (Eq, Ord)

> data ObjectBase            = ObjectBase Int
> data PropertyDefaultsTable = PropertyDefaultsTable Int
> data ObjectTreeBase        = ObjectTreeBase Int
> data ObjectNumber          = Object Int
> data ObjectAddress         = ObjectAddress Int
> data PropertyHeaderAddress = PropertyHeader Int
