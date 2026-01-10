#This script creates and works with an attack modifier deck.

modifierDeck <- list(`+0`,`+0`,`+0`,`+0`,`+0`,`+0`,`+1`,`+1`,`+1`,`+1`,`+1`,`+2`,
                     `-1`,`-1`,`-1`,`-1`,`-1`,`-2`,`x0`,`x2`)

shuffledDeck <- sample(modifierDeck, length(modifierDeck), replace = F)

