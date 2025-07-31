import sets
import std/[strutils, hashes], sequtils

discard sets.defaultInitialSize
discard strutils.Whitespace
discard hashes.hash("a")
discard sequtils.cycle[int]([], 1)
