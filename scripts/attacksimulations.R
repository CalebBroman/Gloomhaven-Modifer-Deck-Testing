#This script will be for running larger simulations for generating data.
#Simulation probably isn't the best word for this right now, but I can't really think of a better term

#Number of times to run the simulation
n <- 100
#The deck chosen to run the simulation on
deck <- baseModifierDeck

#Base attack values: (without poisoned target considered)
baseAttack <- list(roll = T, shuffle = F, damage = 3, 
                   push = 0, pull = 0, pierce = 0, target = 1, poison = F, 
                   wound = F, immobilize = F, disarm = F, stun = F, muddle = F, curse = F)

#Target values:
baseTarget <- list(health = 7, shield = 1, retaliate = 0, poison = F, wound = F,
                   immobilize = F, disarm = F, stun = F, muddle = F)

for (i in 1:n) {
  
  #shuffle the base deck
  shuffledDeck <- shuffle(deck)
  
  
  
}

