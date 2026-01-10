#This script is meant to calculate and compare attacks

target <- list(health = 7, shield = 1, retaliate = 0, poison = F, wound = F,
               immobilize = F, disarm = F, stun = F, muddle = F)

exampleAttack1 <- list(roll = F, shuffle = F, damage = 6, 
                       push = 0, pull = 0, pierce = 2, target = 1, poison = F, 
                       wound = F, immobilize = F, disarm = F, stun = F, muddle = F, curse = F)

exampleAttack2 <- list(roll = F, shuffle = F, damage = 3, 
                       push = 0, pull = 0, pierce = 0, target = 1, poison = F, 
                       wound = F, immobilize = F, disarm = F, stun = F, muddle = F, curse = F)

#calculates the state of the target after the attack
#all negative conditions are set to 1 if the target dies
targetAttacked <- function(attack, target) {
  if (attack$pierce > target$shield) {
    actualDamage <- attack$damage
  } else {
    actualDamage <- attack$damage + attack$pierce - attack$shield
    if (actualDamage < 0) actualDamage <- 0
  }
  health <- target$health - actualDamage
  if (health < 0) health <- 0
  list(health = health, damage = actualDamage,
       push = attack$push, pull = attack$pull, 
       target = attack$target, poison = target$poison || attack$poison || !health, 
       wound = target$wound || attack$wound || !health, 
       immobilize = target$immobilize || attack$immobilize || !health, 
       disarm = target$disarm || attack$disarm || !health, 
       stun = target$stun || attack$stun || !health, 
       muddle = target$muddle || attack$muddle || !health, curse = attack$curse || !health)
}


printAttackEffects <- function(enemyState) {
  print(str_c("damage: ", enemyState$damage))
  print(str_c("health remaining: ", enemyState$health))
  if (enemyState$health) {
    if (enemyState$push != 0) {print(str_c("push: ", enemyState$push))}
    if (enemyState$pull != 0) {print(str_c("pull: ", enemyState$pull))}
    if (enemyState$pierce != 0) {print(str_c("pierce: ", enemyState$pierce))}
    if (enemyState$poison == T) {print("poison")}
    if (enemyState$wound == T) {print("wound")}
    if (enemyState$immobilize == T) {print("immobilize")}
    if (enemyState$disarm == T) {print("disarm")}
    if (enemyState$stun == T) {print("stun")}
    if (enemyState$muddle == T) {print("muddle")}
    if (enemyState$curse == T) {print("curse")}
  }
}

#compares two attacks, returns 1 if the first is better, 
#   2 if the second is better, and -1 if ambiguous
attackComparison <- function(attack1, attack2, target) {
  
}


