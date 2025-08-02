include include_test_helper

type MainType* = IncludedType
type CombinedType* = tuple[main: MainType, other: AnotherType]

include "include_test_helper2", "include_test_helper3"