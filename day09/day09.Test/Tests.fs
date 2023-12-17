module Tests

open Day09
open Xunit

[<Fact>]
let ``Test for sum of extrapolated values`` () =
    let histories =
        [ [ 0; 3; 6; 9; 12; 15 ]
          [ 1; 3; 6; 10; 15; 21 ]
          [ 10; 13; 16; 21; 30; 45 ] ]
    let result = sumExtrapolatedValues nextValue histories
    Assert.Equal(114, result)

[<Fact>]
let ``Get previous history value`` () =
    Assert.Equal(5, previousValue [ 10; 13; 16; 21; 30; 45 ])
    Assert.Equal(
        0,
        previousValue [ 1
                        3
                        6
                        10
                        15
                        21
                        28 ]
    )
    Assert.Equal(-3, previousValue [ 0; 3; 6; 9; 12; 15 ])

[<Fact>]
let ``Get next history value`` () =
    Assert.Equal(18, nextValue [ 0; 3; 6; 9; 12; 15 ])
    Assert.Equal(28, nextValue [ 1; 3; 6; 10; 15; 21 ])
    Assert.Equal(68, nextValue [ 10; 13; 16; 21; 30; 45 ])

[<Fact>]
let ``Get reduced histories`` () =
    let history = [ 0; 3; 6; 9; 12; 15 ]
    let result = reduceHistory history

    Assert.Equal<int list list>(
        [ [ 0; 3; 6; 9; 12; 15 ]
          [ 3; 3; 3; 3; 3 ]
          [ 0; 0; 0; 0 ] ],
        result
    )

[<Fact>]
let ``Test for differences between values`` () =
    let history = [ 0; 3; 6; 9; 12; 15 ]
    let result = differences history
    Assert.Equal<int list>([ 3; 3; 3; 3; 3 ], result)
