#This script creates and works with an attack modifier deck.

baseModifierDeck <- c(`+0`,`+0`,`+0`,`+0`,`+0`,`+0`,`+1`,`+1`,`+1`,`+1`,`+1`,`+2`,
                     `-1`,`-1`,`-1`,`-1`,`-1`,`-2`,`x0`,`x2`)

#Brute deck with all perks active:
BruteDeck <- c(`+0`,`+0`,`+0`,`+0`,`+0`,`+0`,`+1`,`+1`,`+1`,`+1`,`+1`,`+1`,`+1`,`+1`,`+1`,
               `+1`,`+1`,`+2`,`+3`,`push1r`,`push1r`,`push1r`,`push1r`,`push1r`,`push1r`,
               `pierce3r`,`pierce3r`,`stunr`,`stunr`,`disarmr`,`muddler`,
               `addtargetr`,`addtargetr`,`+1shield`,`-1`,`-1`,`-2`,`x0`,`x2`)


shuffle <- function(deck) {
  deck <- sample(deck, length(deck), replace = F)
  deck
}

