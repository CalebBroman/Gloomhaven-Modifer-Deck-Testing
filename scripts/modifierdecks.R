#This script creates and works with an attack modifier deck.
#The functions in modifierfunctions.R are required for this script to run correctly.

baseModifierDeck <- c(`+0`,`+0`,`+0`,`+0`,`+0`,`+0`,`+1`,`+1`,`+1`,`+1`,`+1`,`+2`,
                     `-1`,`-1`,`-1`,`-1`,`-1`,`-2`,`x0`,`x2`)

#Brute deck with all perks active:
bruteModifierDeck <- c(`+0`,`+0`,`+0`,`+0`,`+0`,`+0`,`+1`,`+1`,`+1`,`+1`,`+1`,`+1`,`+1`,`+1`,`+1`,
               `+1`,`+1`,`+2`,`+3`,`push1r`,`push1r`,`push1r`,`push1r`,`push1r`,`push1r`,
               `pierce3r`,`pierce3r`,`stunr`,`stunr`,`disarmr`,`muddler`,
               `addtargetr`,`addtargetr`,`+1shieldself`,`-1`,`-1`,`-2`,`x0`,`x2`)



shuffle <- function(deck) {
  deck <- sample(deck, length(deck), replace = F)
  deck
}

