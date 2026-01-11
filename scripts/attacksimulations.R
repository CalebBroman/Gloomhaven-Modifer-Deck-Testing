#This script will be for running larger simulations for generating data.
#Simulation probably isn't the best word for this right now, but I can't really think of a better term

#Number of times to run the simulation
n <- 10000
#The deck chosen to run the simulation on
deck <- bruteModifierDeck
#effect weights for weighted damage
weights <- list(push = 0.5, pull = 0.5, target = 1, poison = 2, wound = 2, immobilize = 1, 
                disarm = 2, stun = 3, muddle = 1, curse = 1)

#Base attack values: (without poisoned target considered)
baseAttack <- list(roll = T, shuffle = F, damage = 4, 
                   push = 0, pull = 0, pierce = 0, target = 1, poison = F, 
                   wound = F, immobilize = F, disarm = F, stun = F, muddle = F, curse = F)

#Target values:
baseTarget <- list(health = 7, shield = 0, retaliate = 0, poison = F, wound = F,
                   immobilize = F, disarm = F, stun = F, muddle = F)

#Data table for the generated data
attackstibble <- tibble(iteration = numeric(),
                        attackType = factor(levels = c("standard", "advantage", "disadvantage",
                                                       "houseAdvantage", "houseDisadvantage")),
                        health = numeric(), damage = numeric(), weightedDamage = numeric(), 
                        push = numeric(), pull = numeric(),
                        target = numeric(), poison = logical(), wound = logical(), 
                        immobilize = logical(), disarm = logical(), stun = logical(), 
                        muddle = logical(), curse = logical())

#account for poison
if (baseTarget$poison) {baseAttack$damage <- baseAttack$damage + 1}

for (i in 1:n) {
  #shuffle the base deck
  shuffledDeck <- shuffle(deck)
  
  #run a standard attack with the shuffled deck
  attack <- standardAttackAction(baseAttack, shuffledDeck, baseTarget)
  #calculate weighted damage
  weightedDamage <- attack$damage + ceiling(sum((as.numeric(attack[3:12]) - 
                                                   as.numeric(c(0, 0, 0, baseTarget[4:9], 0))) * 
                                                  as.numeric(weights))) - weights$target
  #add the attack info to the table
  attackstibble <- add_row(attackstibble, iteration = i, attackType = "standard", 
                           health = attack$health, damage = attack$damage, 
                           weightedDamage = weightedDamage, push = attack$push, pull = attack$pull, 
                           target = attack$target, poison = attack$poison, wound = attack$wound, 
                           immobilize = attack$immobilize, disarm = attack$disarm, 
                           stun = attack$stun, muddle = attack$muddle, curse = attack$curse)
  
  #run an advantaged attack with the same shuffled deck
  attack <- advantageAttackActionV1(baseAttack, shuffledDeck, baseTarget)
  #calculate weighted damage
  weightedDamage <- attack$damage + ceiling(sum((as.numeric(attack[3:12]) - 
                                                   as.numeric(c(0, 0, 0, baseTarget[4:9], 0))) * 
                                                  as.numeric(weights))) - weights$target
  #add the attack info to the table
  attackstibble <- add_row(attackstibble, iteration = i, attackType = "advantage", 
                           health = attack$health, damage = attack$damage, 
                           weightedDamage = weightedDamage, push = attack$push, pull = attack$pull, 
                           target = attack$target, poison = attack$poison, wound = attack$wound, 
                           immobilize = attack$immobilize, disarm = attack$disarm, 
                           stun = attack$stun, muddle = attack$muddle, curse = attack$curse)
  
  #run a disadvantaged attack with the same shuffled deck
  attack <- disadvantageAttackActionV1(baseAttack, shuffledDeck, baseTarget)
  #calculate weighted damage
  weightedDamage <- attack$damage + ceiling(sum((as.numeric(attack[3:12]) - 
                                                   as.numeric(c(0, 0, 0, baseTarget[4:9], 0))) * 
                                                  as.numeric(weights))) - weights$target
  #add the attack info to the table
  attackstibble <- add_row(attackstibble, iteration = i, attackType = "disadvantage",
                           health = attack$health, damage = attack$damage, 
                           weightedDamage = weightedDamage, push = attack$push, pull = attack$pull, 
                           target = attack$target, poison = attack$poison, wound = attack$wound, 
                           immobilize = attack$immobilize, disarm = attack$disarm, 
                           stun = attack$stun, muddle = attack$muddle, curse = attack$curse)
  
  #run an advantaged attack with the same shuffled deck, with the house rule
  attack <- advantageAttackActionHouseRule(baseAttack, shuffledDeck, baseTarget, weights)
  #calculate weighted damage
  weightedDamage <- attack$damage + ceiling(sum((as.numeric(attack[3:12]) - 
                                                   as.numeric(c(0, 0, 0, baseTarget[4:9], 0))) * 
                                                  as.numeric(weights))) - weights$target
  #add the attack info to the table
  attackstibble <- add_row(attackstibble, iteration = i, attackType = "houseAdvantage", 
                           health = attack$health, damage = attack$damage, 
                           weightedDamage = weightedDamage, push = attack$push, pull = attack$pull, 
                           target = attack$target, poison = attack$poison, wound = attack$wound, 
                           immobilize = attack$immobilize, disarm = attack$disarm, 
                           stun = attack$stun, muddle = attack$muddle, curse = attack$curse)
  
  #run a disadvantaged attack with the same shuffled deck, with the house rule
  attack <- disadvantageAttackActionHouseRule(baseAttack, shuffledDeck, baseTarget, weights)
  #calculate weighted damage
  weightedDamage <- attack$damage + ceiling(sum((as.numeric(attack[3:12]) - 
                                                   as.numeric(c(0, 0, 0, baseTarget[4:9], 0))) * 
                                                  as.numeric(weights))) - weights$target
  #add the attack info to the table
  attackstibble <- add_row(attackstibble, iteration = i, attackType = "houseDisadvantage",
                           health = attack$health, damage = attack$damage, 
                           weightedDamage = weightedDamage, push = attack$push, pull = attack$pull, 
                           target = attack$target, poison = attack$poison, wound = attack$wound, 
                           immobilize = attack$immobilize, disarm = attack$disarm, 
                           stun = attack$stun, muddle = attack$muddle, curse = attack$curse)
}

