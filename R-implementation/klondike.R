# CodeKata 20: Klondike
# http://codekata.com/kata/kata20-klondike/

# Libraries
library(testthat)


# Constants
SUITS <- c("H", "D", "S", "C")
RANKS <- seq(1:13)
NUM_TABLEUS <- 7


# Load functions from others scripts.
source("Card.R")
source("helpers.R")
source("klondike-setup.R")


# Get the color of the card based on its suit.
get_card_color <- function(card) {
    suit_color_map <- c(
        "H" = "red", "D" = "red",
        "S" = "black", "C" = "black"
    )
    return(suit_color_map[[card@suit]])
}


# Are the suits different colors?
suits_are_different_color <- function(card1, card2) {
    card1_col <- get_card_color(card1)
    card2_col <- get_card_color(card2)
    return(card1_col != card2_col)
}


# Can the `card` be placed on the `pile`?
can_put_card_on_pile <- function(card, pile) {
    pile <- unlist(pile)
    
    if(is.null(card)) return(FALSE)

    if (length(pile) == 0) {
        if (card@rank == 1) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
    
    top_card <- tryCatch(
        { pile[[1]] },
        error = function(x) { browser() }
    )
    if (card@suit == top_card@suit) {
        if (top_card@rank + 1 == card@rank) {
            return(TRUE)
        }
    }
    return(FALSE)
}


# Can the `card` go on the `tablaeu`?
can_put_card_on_tableau <- function(card, tbl) {
    if(is.null(card)) return(FALSE)
    
    # If the tableau is empty, only a King can go on top.
    if (length(tbl) == 0) {
        if (card@rank == 13) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
    
    top_card <- tbl[[1]]
    if (suits_are_different_color(card, top_card)) {
        if (top_card@rank + 1 == card@rank) {
            return(TRUE)
        }
    }
    return(FALSE)
}


# Flip a card from the deck to discard.
flip_card_to_discard <- function(deck, discard) {
    new_discard <- c(deck[1], discard)
    new_deck <- deck[-1]
    return(list(
        discard = new_discard,
        deck = new_deck
    ))
}



testthat::test_file("tests.R")


# Set-up game ----

set.seed(0)

deck <- make_shuffled_deck()
discard <- c()
piles <- list(
    pile1 <- c(),
    pile2 <- c(),
    pile3 <- c(),
    pile4 <- c()
)


x <- make_tableaus(deck, NUM_TABLEUS)
deck <- x$deck
tableaus <- x$tableaus

# Put first card from deck into discard pile.
discard <- c(discard, deck[[length(deck)]])
deck <- deck[-length(deck)]

# Check number of cards in game.
stopifnot(check_number_of_cards(deck, discard, piles, tableaus))
stopifnot(check_number_of_cards(deck, discard, tableaus))

counter <- 0
while (TRUE) {
    counter <- counter + 1
    cat("round", counter, "\n")
    
    print_game(deck, discard, piles, tableaus)
    
    # If the discard is empty, get a new card from the deck.
    if (length(discard) == 0) {
        X <- flip_card_to_discard(deck, discard)
        discard <- X$discard
        deck <- X$deck
    }
    
    # While possible, put discard into piles.
    old_length <- length(discard)
    for (i in 1:length(piles)) {
        pile <- unlist(piles[i])
        while (can_put_card_on_pile(discard[[1]], pile)) {
            print("Adding card to pile.")
            pile <- c(discard[1], pile)
            piles[i] <- list(pile)
            discard <- discard[-1]
        }
    }
    if (length(discard) != old_length) next
    
    
    # While possible, put tableau cards onto piles.
    
    
    # Try to put tableau cards onto other tableaus.
    
    
    # Try to put discard card onto tableaus.
    
    
    
    
    
    # Flip next card or swap discard and deck.
    if (length(deck) > 0) {
        discard <- c(deck[1], discard)
        deck <- deck[-1]
    } else {
        deck <- rev(discard)
        discard <- deck[1]
        deck <- deck[-1]
    }
    
    # Check number of cards in game.
    stopifnot(check_number_of_cards(deck, discard, piles, tableaus))
    
    if (counter == 30) break
}

