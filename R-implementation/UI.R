# Print the current state of the game.


# Prints the current state of the game.
print_game <- function(deck, discard, piles, tableaus) {
    cat("cards in deck:", length(deck), "\n")
    cat("cards in discard:", length(discard), "\n")
    
    cat("PILES:\n")
    for (pile in piles) {
        if (length(pile) > 0) {
            cat("  ")
            for (card in pile) {
                cat(card@rank, card@suit, ", ", sep = "")
            }
        } else {
            cat("  < empty pile >")
        }
        cat("\n")
    }
    
    cat("Tableau lengths:\n")
    cat(list_lengths(tableaus), "\n")
    cat(paste0(rep("=", 80), collapse = ""), "\n")
}
