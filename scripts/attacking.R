#This script will calculate attacks.
#This script requires the comparison function from attackcomparisons.R 
#for the advantage and disadvantage calculations

#Prints an attack in a more human readable format, uses stringr to combine strings
printAttack <- function(attack) {
  print(str_c("damage: ", attack$damage))
  print(str_c("targets: ", attack$target))
  if (attack$push != 0) {print(str_c("push: ", attack$push))}
  if (attack$pull != 0) {print(str_c("pull: ", attack$pull))}
  if (attack$pierce != 0) {print(str_c("pierce: ", attack$pierce))}
  if (attack$poison == T) {print("poison")}
  if (attack$wound == T) {print("wound")}
  if (attack$immobilize == T) {print("immobilize")}
  if (attack$disarm == T) {print("disarm")}
  if (attack$stun == T) {print("stun")}
  if (attack$muddle == T) {print("muddle")}
  if (attack$curse == T) {print("curse")}
  if (attack$shuffle == T) {print("shuffle after turn")}
}

#attacks require the base attack values, the modifier deck to be drawn from, 
#and the target's current state
standardAttackAction <- function(base, deck, target) {
  attack <- base
  while(attack$roll == T) {
    card <- deck[[1]]
    deck <- deck[2:length(deck)]
    attack <- card(attack)
  }
  return(targetAttacked(attack, target))
}

#Attacking with advantage, by the rules written in Gloomhaven v1
advantageAttackActionV1 <- function(base, deck, target) {
  #draw two cards, and check if they are rolling or not
  card1 <- deck[[1]]
  card2 <- deck[[2]]
  deck <- deck[3:length(deck)]
  card1roll <- card1(list(roll = T))$roll
  card2roll <- card1(list(roll = T))$roll
  if (card1roll && card2roll) {
    #if both cards roll, then apply both and continue drawing like normal
    attack <- card2(card1(base))
    while(attack$roll == T) {
      card <- deck[[1]]
      deck <- deck[2:length(deck)]
      attack <- card(attack)
    }
    return(targetAttacked(attack, target))
  } else if (card1roll && !card2roll) {
    #if card 1 rolls but not 2, apply card 1 then card 2
    return(targetAttacked(card2(card1(base)), target))
  } else if (!card1roll && card2roll) {
    #if card 2 rolls but not 1, apply card 2 then card 1
    return(targetAttacked(card1(card2(base)), target))
  } else {
    #if neither card rolls, compare the effects of each, and use the better one.
    #if ambiguous, use the first.
    compVal <- attackComparison(card1(base), card2(base), target)
    if (compVal == 2) {
      return(targetAttacked(card2(base), target))
    } else {
      return(targetAttacked(card1(base), target))
    }
  }
}

#Attacking with disadvantage, by the rules written in Gloomhaven v1
disadvantageAttackActionV1 <- function(base, deck, target) {
  #draw two cards, and check if they are rolling or not
  card1 <- deck[[1]]
  card2 <- deck[[2]]
  deck <- deck[3:length(deck)]
  card1roll <- card1(list(roll = T))$roll
  card2roll <- card1(list(roll = T))$roll
  if (card1roll && card2roll) {
    #if both cards roll, continue drawing till you find one that doesn't
    cardroll <- T
    while(cardroll == T) {
      card <- deck[[1]]
      cardroll <- card(list(roll = T))$roll
      deck <- deck[2:length(deck)]
    }
    #apply just the card that doesn't roll
    return(targetAttacked(card(base), target))
  } else if (card1roll && !card2roll) {
    #if card 1 rolls but card 2 doesn't, apply just card 2
    return(targetAttacked(card2(base), target))
  } else if (!card1roll && card2roll) {
    #if card 2 rolls but card 1 doesn't, apply just card 1
    return(targetAttacked(card1(base), target))
  } else {
    #if neither card rolls, compare the effects of each, and use the worse one.
    #if ambiguous, use the first.
    compVal <- attackComparison(card1(base), card2(base), target)
    if (compVal == 1) {
      return(targetAttacked(card2(base), target))
    } else {
      return(targetAttacked(card1(base), target))
    }
  }
}

#This is a house rule that I used when playing the game due to a misunderstanding of the rules
#basically, for both advantage and disadvantage, you would do two individual standard draws, 
#and choose the better/worse one to use for the attack.

#for these functions, better/worse will be determined by a weighted version of damage, which
#quantifies the value of effects and conditions to make choosing easier. 
#If there's a tie, the first draw will be used.

#Here's an example of a valid weight input:
# weights <- list(push = 0.5, pull = 0.5, target = 1, poison = 2, wound = 2, immobilize = 1, 
#                 disarm = 2, stun = 3, muddle = 1, curse = 1)

advantageAttackActionHouseRule <- function(base, deck, target, weights) {
  attack1 <- base
  while(attack1$roll == T) {
    card <- deck[[1]]
    deck <- deck[2:length(deck)]
    attack1 <- card(attack1)
  }
  attack2 <- base
  while(attack2$roll == T) {
    card <- deck[[1]]
    deck <- deck[2:length(deck)]
    attack2 <- card(attack2)
  }
  weightedDamage1 <- attack1$damage + ceiling(sum((as.numeric(attack1[3:12]) - 
                                                   as.numeric(c(0, 0, 0, target[4:9], 0))) * 
                                                  as.numeric(weights))) - weights$target
  weightedDamage2 <- attack2$damage + ceiling(sum((as.numeric(attack2[3:12]) - 
                                                   as.numeric(c(0, 0, 0, target[4:9], 0))) * 
                                                  as.numeric(weights))) - weights$target
  dif <- weightedDamage1 - weightedDamage2
  if (dif < 0) return(targetAttacked(attack2, target))
  else return(targetAttacked(attack1, target))
}

disadvantageAttackActionHouseRule <- function(base, deck, target, weights) {
  attack1 <- base
  while(attack1$roll == T) {
    card <- deck[[1]]
    deck <- deck[2:length(deck)]
    attack1 <- card(attack1)
  }
  attack2 <- base
  while(attack2$roll == T) {
    card <- deck[[1]]
    deck <- deck[2:length(deck)]
    attack2 <- card(attack2)
  }
  weightedDamage1 <- attack1$damage + ceiling(sum((as.numeric(attack1[3:12]) - 
                                                     as.numeric(c(0, 0, 0, target[4:9], 0))) * 
                                                    as.numeric(weights))) - weights$target
  weightedDamage2 <- attack2$damage + ceiling(sum((as.numeric(attack2[3:12]) - 
                                                     as.numeric(c(0, 0, 0, target[4:9], 0))) * 
                                                    as.numeric(weights))) - weights$target
  dif <- weightedDamage1 - weightedDamage2
  if (dif > 0) return(targetAttacked(attack2, target))
  else return(targetAttacked(attack1, target))
}