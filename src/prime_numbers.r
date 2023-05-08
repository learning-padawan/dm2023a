print_primes <- function(start, end) {
    # Check if the start value is less than 2 (since 2 is the first prime number)
    if (start < 2) {
        start <- 2
    }

    # Loop through each number in the range
    for (i in start:end) {
        # Check if the number is prime
        is_prime <- TRUE
        for (j in 2:(i - 1)) {
            if (i %% j == 0) {
                is_prime <- FALSE
                break
            }
        }

        # Print the number if it's prime
        if (is_prime) {
            print(i)
        }
    }
}

print_primes(185003, 185503)



