type SimpleType = int
type AA = sink string
type
    CrazyGeneric*[A, B, F: string, D; E] = tuple[a, c: A, b: B; d: B]
    Pair[A, B] = tuple[a: A, b: B]
    OtherType* = string
    MoreType = seq[SimpleType]
type LastType* = distinct MoreType

type
    RefersToNextType = Union
    # in a type section, a type is allowed to refer to the another later type in the block
    Union = OtherType | LastType | MoreType
    R* = 0..5
    NestedGeneric* = seq[Pair[R, RefersToNextType]]


