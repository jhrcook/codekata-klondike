# The `Card` class for Klondike.


setClass(
    "Card", 
    slots = c(rank = "numeric",
              suit = "character")
)

Card <- function(rank, suit) {
    new("Card", rank = rank, suit = stringr::str_to_upper(suit))
}


setMethod("print", "Card", function(x) {
    print(paste(x@rank, x@suit, sep = "-"))
})
