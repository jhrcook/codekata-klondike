# Functions for setting up the game of Klondike.

source("helpers.R")


# Create a deck of cards (not shuffled).
make_deck <- function() {
    deck_df <- expand.grid(RANKS, SUITS)
    deck <- apply(deck_df, 1, function(r) {
        return(Card(as.numeric(r[[1]]), r[[2]]))
    })
    return(deck)
}


# Create a shuffled deck of cards.
make_shuffled_deck <- function() {
    deck <- make_deck()
    idx <- sample(seq(1, length(deck)))
    deck <- deck[idx]
    return(deck)
}


# Make tableaus using a deck of cards.
make_tableaus <- function(deck, n) {
    tableaus <- rep(NA, n)
    
    for (i in seq(1, n)) {
        idx <- sample(1:length(deck), i)
        tableaus[i] <- list(deck[idx])
        deck <- deck[-idx]
    }
    
    return(list(
        deck = deck,
        tableaus = tableaus
    ))
}
