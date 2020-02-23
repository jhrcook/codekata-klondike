library(testthat)

# Helpers ----
test_that("List length function works.", {
    a <- seq(1:10)
    expect_equal(list_lengths(a), rep(1, 10))
    
    a <- lapply(1:10, function(i) sample(1:100, i))
    expect_equal(list_lengths(a), seq(1, 10))
    
    expect_equal(list_lengths(c()), numeric())
    expect_equal(list_lengths(list()), numeric())
})


# Set up ----
test_that("Game setup works",  {
    expect_equal(length(make_deck()), 52)
    expect_equal(length(make_shuffled_deck()), 52)
    
    deck <- make_deck()
    tbls <- make_tableaus(deck, 7)
    
    expect_equal(names(tbls), c("deck", "tableaus"))
    
    deck <- tbls$deck
    tbls <- tbls$tableaus
    expect_equal(length(tbls), 7)
    expect_equal(length(deck), 52 - sum(seq(1, 7)))
    expect_equal(list_lengths(tbls), seq(1, 7))
})


# Game play ----
test_that("Get correct card color.", {
    expect_equal(get_card_color(Card(12, "H")), "red")
    expect_equal(get_card_color(Card(12, "D")), "red")
    expect_equal(get_card_color(Card(12, "C")), "black")
    expect_equal(get_card_color(Card(12, "S")),  "black")
})

test_that("Suits discriminator works.", {
    c1 <- Card(1, "H")
    c2 <- Card(1, "D")
    c3 <- Card(1, "S")
    c4 <- Card(1, "C")
    expect_true(suits_are_different_color(c1, c3))
    expect_true(suits_are_different_color(c1, c4))
    expect_true(suits_are_different_color(c2, c3))
    expect_true(suits_are_different_color(c1, c4))
    
    expect_false(suits_are_different_color(c1, c1))
    expect_false(suits_are_different_color(c1, c2))
    expect_false(suits_are_different_color(c2, c2))
    expect_false(suits_are_different_color(c3, c3))
    expect_false(suits_are_different_color(c3, c4))
    expect_false(suits_are_different_color(c4, c4))
})


test_that("Pile stacking checker works.", {
    pile <- c(
        Card(2, "S"),
        Card(1, "H")
    )
    
    expect_false(can_put_card_on_pile(NULL, pile))
    
    expect_true(can_put_card_on_pile(Card(3, "S"), pile))
    expect_false(can_put_card_on_pile(Card(3, "H"), pile))
    expect_false(can_put_card_on_pile(Card(4, "S"), pile))
    
    deck <- make_shuffled_deck()
    idx <- unlist(lapply(deck, can_put_card_on_pile, pile = pile))
    expect_equal(sum(idx), 1)
    
    expect_false(can_put_card_on_pile(Card(2, "D"), c()))
    expect_true(can_put_card_on_pile(Card(1, "D"), c()))
})


test_that("Tableau stacking checker works.", {
    tbl <- c(
        Card(2, "S"),
        Card(1, "H")
    )
    
    expect_false(can_put_card_on_tableau(NULL, tbl))
    
    expect_true(can_put_card_on_tableau(Card(3, "H"), tbl))
    expect_true(can_put_card_on_tableau(Card(3, "D"), tbl))
    expect_false(can_put_card_on_tableau(Card(3, "S"), tbl))
    expect_false(can_put_card_on_tableau(Card(3, "C"), tbl))
    
    for (val in c(2, 4)) {
        for (suit in c("H", "D", "S", "C")) {
            expect_false(can_put_card_on_tableau(Card(val, suit), tbl))
        }
    }
    
    expect_false(can_put_card_on_tableau(Card(1, "D"), c()))
    expect_false(can_put_card_on_tableau(Card(12, "D"), c()))
    expect_true(can_put_card_on_tableau(Card(13, "D"), c()))
})
