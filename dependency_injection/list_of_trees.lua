example = LNonempty {
  first = LLeaf { value = 3 },
  rest = LNonempty {
    first = LLeaf { value = 2 },
    rest = LNonempty {
      first = LLeaf { value = 1 },
      rest = LEmpty {}
    }
  }
}
