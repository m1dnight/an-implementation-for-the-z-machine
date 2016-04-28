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
