type
  # Test basic magic generics - no type definitions, just declarations with pragmas
  `ptr`*[T] {.magic: Pointer.}
  range*[T] {.magic: "Range".}
  seq*[T] {.magic: "Seq".}
  
  # Test magic generic with different backtick style
  `ref`*[T] {.magic: Pointer.}
  
  # Test unknown magic generic
  CustomMagicGeneric*[T] {.magic: SomeUnknownMagic.}
  
  # Test magic generic with multiple type parameters
  array*[I, T] {.magic: "Array".}
