> module Object where
> import Utility
> import Types
> import Story
> import ZString

> import Text.Printf

Snippet from Eric's source:
The object table is laid out as follows:
* The base of the object table is in the header.
* The object table begins with a block of 31 or 63 default property values.
* Following the default property values is the object tree.
* Each entry in the tree is of the same size, and is laid out as follows:
  * 32 or 48 bits of attribute flags
  * the parent, sibling and child object numbers
  * the address of an additional table of variable-sized properties.
* object numbers are one-based, so zero is used as the invalid object.

> invalid_object = Object 0

The object table begins with either 31 or 63 default values for
properties. Each of them is two bytes.

> defaultPropertyTableSize story =
>   if v3OrLower (version story)
>      then 31
>      else 63

> defaultPropertyTableEntrySize = 2

> treeBase :: T -> ObjectTreeBase
> treeBase story =
>   let (ObjectBase base) = objectTableBase story
>       tableSize         = defaultPropertyTableSize story
>   in
>     ObjectTreeBase (base + defaultPropertyTableEntrySize * tableSize)

The size of an entry depends on the version. Version 3 has

 - 4 bytes of attributes
 - 3 bytes for the parent, sibling and child
 - 2 for property headers.

For version 4, it would 6,6 and 2 above.

> entrySize :: T -> Int
> entrySize story = if v3OrLower . version $ story
>                      then 9
>                      else 14

To determine the actual address of an object:
 - Get the base address of the object tree.
 - Compute entry size.
 - The address is the base, plus object number - 1 times the entry size.

> address :: T -> ObjectNumber -> ObjectAddress
> address story (Object obj) =
>   let (ObjectTreeBase treeBas) = treeBase story
>       entrySiz                 = entrySize story
>   in
>     ObjectAddress $ treeBas + (obj - 1) * entrySiz

To determine the parent of a given object we do the following.  From
version 4 and onward the object numbers are 2 bytes. Before that, 1
byte.

Also recall, an object is built as
|attr|attr|attr|attr|parent|sibling|child|props|props|, hence, to get
the parent, we start from the address of the object and skip the first
4 bytes. That's the base of the parent, which is one byte.

> parent :: T -> ObjectNumber -> ObjectNumber
> parent story obj =
>   let (ObjectAddress addr) = address story obj
>   in
>     if v3OrLower $ version story
>       then Object $ readByte story (ByteAddress (addr + 4))
>       else Object $ readWord story (WordAddress (addr + 6))

> sibling :: T -> ObjectNumber -> ObjectNumber
> sibling story obj =
>   let (ObjectAddress addr) = address story obj
>   in
>     if v3OrLower $ version story
>       then Object $ readByte story (ByteAddress (addr + 5))
>       else Object $ readWord story (WordAddress (addr + 8))

> child :: T -> ObjectNumber -> ObjectNumber
> child story obj =
>   let (ObjectAddress addr) = address story obj
>   in
>     if v3OrLower $ version story
>       then Object $ readByte story (ByteAddress (addr + 6))
>       else Object $ readWord story (WordAddress (addr + 10))

To get the properties we need to compute the offset, and then we read
in the word (two bytes).

> propertyHeaderAddress :: T -> ObjectNumber -> PropertyHeaderAddress
> propertyHeaderAddress story obj =
>   let objectPropertyOffset = if v3OrLower $ version story
>                                 then 7
>                                 else 12
>       (ObjectAddress addr) = address story obj
>   in
>     PropertyHeader $ readWord story (WordAddress (addr + objectPropertyOffset))

In the property word, the first byte containing the length of the name
of the object. After that follows a z-string-encoded name of that
length.

> name :: T -> ObjectNumber -> String
> name story n =
>   let (PropertyHeader addr) = propertyHeaderAddress story n
>       length                = readByte story (ByteAddress addr)
>   in
>     if length == 0
>       then "<unnamed>"
>       else ZString.read story (ZString (addr +1))

*Snip from Eric*: In practice, every story file has the property
 header for the properties of object 1 immediately following the last
 object entry. There is of course no requirement that the property
 block for any object be anywhere, but this convention is consistently
 followed so let’s take advantage of it:

> count :: T -> Int
> count story =
>   let (ObjectTreeBase tableStart) = treeBase story
>       (PropertyHeader tableEnd)   = propertyHeaderAddress story (Object 1)
>       size                        = entrySize story
>   in
>     (tableEnd - tableStart) `div` size
>
> displayObjectTable :: T -> [Char]
> displayObjectTable story =
>   let count'     = count story
>       toString i = let current = (Object i)
>                        (Object par) = parent story current
>                        (Object sib) = sibling story current
>                        (Object cld) = child story current
>                        name'        = name story current
>                    in
>                      printf "%02x: %02x %02x %02x %s\n" i par sib cld name'
>   in
>     accumulateStringsLoop toString 1 (count' + 1)
>
>
> roots :: T -> [ObjectNumber]
> roots story =
>   let aux (Object obj) acc =
>         let current = (Object obj)
>         in
>           if current == invalid_object
>              then acc
>              else if (parent story current) == invalid_object
>                      then aux (Object $ obj - 1) (current:acc)
>                      else aux (Object $ obj - 1) acc
>   in
>     aux (Object $ count story) []
>
>
> displayObjectTree story =
>   let aux acc indent obj =
>         if obj == invalid_object
>          then acc
>          else let name'        = name story obj
>                   child'       = child story obj
>                   sibling'     = sibling story obj
>                   objectText   = printf "%s%s\n" (indent :: String) (name' :: String)
>                   withObject   = acc ++ objectText
>                   newIndent    = "    " ++ indent
>                   withChildren = aux withObject newIndent child'
>               in
>                 aux withChildren indent sibling'
>       toString obj = aux "" "" obj
>   in
>      accumulateStrings toString (roots story)
