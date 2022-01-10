// computes the number of odd numbers in range [0,R]
let number_of_odd R =
    let N = R + 1

    //R is odd
    if N % 2 = 0 then
        let odd_numbers = N / 2
        let even_numbers = N / 2
        odd_numbers
    //R is even
    else
        let odd_numbers = ((N - 1) / 2)
        let even_numbers = N - odd_numbers
        odd_numbers


let number_of_even R =
    let N = R + 1
    N - number_of_odd R
