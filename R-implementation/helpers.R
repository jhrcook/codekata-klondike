# Helper functions for Klondike


# Length of each vector in a list.
list_lengths <- function(x) {
    purrr::map_dbl(x, length)
}


# Count the total number of cards.
count_number_of_cards <- function(...) {
    sum(list_lengths(c(...)))
}


# Are there the expected number of cards?
check_number_of_cards <- function(..., n = 52) {
    count_number_of_cards(...) == n
}
