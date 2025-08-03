type
  Slice*[T] = HSlice[T, T]
  HSlice*[T, U] = object
    a*: T               
    b*: U               

type SS = Slice[int]

var a: SS
echo a
