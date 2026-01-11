#This script will be for running larger simulations for generating data.
#Simulation probably isn't the best word for this right now, but I can't really think of a better term

#Number of times to run the simulation
n <- 10000
#The deck chosen to run the simulation on
deck <- BruteDeck

#Base attack values: (without poisoned target considered)
baseAttack <- list(roll = T, shuffle = F, damage = 3, 
                   push = 0, pull = 0, pierce = 0, target = 1, poison = F, 
                   wound = F, immobilize = F, disarm = F, stun = F, muddle = F, curse = F)

#Target values:
baseTarget <- list(health = 7, shield = 1, retaliate = 0, poison = F, wound = F,
                   immobilize = F, disarm = F, stun = F, muddle = F)

#Data table for the generated data
attackstibble <- tibble(iteration = numeric(),
                        attackType = factor(levels = c("disadvantage","standard", "advantage")),
                        health = numeric(), damage = numeric(), push = numeric(), pull = numeric(),
                        target = numeric(), poison = logical(), wound = logical(), 
                        immobilize = logical(), disarm = logical(), stun = logical(), 
                        muddle = logical(), curse = logical())


for (i in 1:n) {
  #shuffle the base deck
  shuffledDeck <- shuffle(deck)
  
  #run a standard attack with the shuffled deck
  attack <- standardAttackAction(baseAttack, shuffledDeck, baseTarget)
  #add the attack info to the table
  attackstibble <- add_row(attackstibble, iteration = i, attackType = "standard", health = attack$health, 
          damage = attack$damage, push = attack$push, pull = attack$pull, 
          target = attack$target, poison = attack$poison, 
          wound = attack$wound, immobilize = attack$immobilize,
          disarm = attack$disarm, stun = attack$disarm,
          muddle = attack$muddle, curse = attack$curse)
  
  #run an advantaged attack with the same shuffled deck
  attack <- advantageAttackActionV1(baseAttack, shuffledDeck, baseTarget)
  #add the attack info to the table
  attackstibble <- add_row(attackstibble, iteration = i, attackType = "advantage", health = attack$health, 
          damage = attack$damage, push = attack$push, pull = attack$pull, 
          target = attack$target, poison = attack$poison, 
          wound = attack$wound, immobilize = attack$immobilize,
          disarm = attack$disarm, stun = attack$disarm,
          muddle = attack$muddle, curse = attack$curse)
  
  #run a desadvantaged attack with the same shuffled deck
  attack <- disadvantageAttackActionV1(baseAttack, shuffledDeck, baseTarget)
  #add the attack info to the table
  attackstibble <- add_row(attackstibble, iteration = i, attackType = "disadvantage", health = attack$health, 
          damage = attack$damage, push = attack$push, pull = attack$pull, 
          target = attack$target, poison = attack$poison, 
          wound = attack$wound, immobilize = attack$immobilize,
          disarm = attack$disarm, stun = attack$disarm,
          muddle = attack$muddle, curse = attack$curse)
}

