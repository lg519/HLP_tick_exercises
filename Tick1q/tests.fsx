//computes n! = n * (n-1) * ... * 2 * 1
let fact n =
    if n = 0 then
        1.0
    else
        List.reduce (*) [ 1.0 .. float n ]

/// computes the number of odd numbers in range [0,R]
let number_of_odd R =
    let N = R + 1

    if N % 2 = 0 then
        let odd_numbers = N / 2
        odd_numbers
    else
        let odd_numbers = ((N - 1) / 2)
        odd_numbers

/// computes the number of even numbers in range [0,R]
let number_of_even R =
    let N = R + 1
    N - number_of_odd R


/// computes the taylor expansion of cos(x) up to order n
let cosine (n: int) (x: float) =
    let number_of_terms = number_of_even n

    let term (i: int) =
        ((float -1) ** i * x ** (float (2 * i)))
        / fact (2 * i)

    let last_i_in_list = number_of_terms - 1

    List.map term [ 0 .. last_i_in_list ]
    |> List.reduce (+)

/// computes the taylor expansion of sin(x) up to order n
let sine (n: int) (x: float) =
    let number_of_terms = number_of_odd n

    let term (i: int) =
        (float -1) ** i * x ** (float (2 * i + 1))
        / fact (2 * i + 1)

    let last_i_in_list = number_of_terms - 1

    List.map term [ 0 .. last_i_in_list ]
    |> List.reduce (+)
