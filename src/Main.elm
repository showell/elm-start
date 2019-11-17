module Main exposing (main)

import Benchmark exposing (benchmark, compare)
import Benchmark.Runner exposing (program)



-- MODEL / INIT


main =
    program suite


suite =
    Benchmark.compare "math"
        "someMath"
        (\_ -> someMath 1000 10 5)
        "someMath2"
        (\_ -> someMath2 1000 10 5)


three =
    3


fac n =
    List.product (List.range 1 n)


someMath a b c =
    let
        double x =
            x * 2

        timesThree x =
            x * three

        compute =
            double a + timesThree b + fac c
    in
    compute


someMath2 a b c =
    let
        double x =
            x * 2

        timesThree x =
            x * three

        compute =
            double a + timesThree b + fac c
    in
    compute
