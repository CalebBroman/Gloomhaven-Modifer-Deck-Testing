#This script is meant to calculate and compare attacks

#calculates the state of the target after the attack
#all negative conditions are set to 1 if the target dies
targetAttacked <- function(attack, target) {
  if (attack$pierce >= target$shield) {
    actualDamage <- attack$damage
  } else {
    actualDamage <- attack$damage + attack$pierce - target$shield
  }
  if (actualDamage < 0) {actualDamage <- 0}
  if (actualDamage > target$health) {actualDamage <- target$health}
  health <- target$health - actualDamage
  list(health = health, damage = actualDamage,
       push = attack$push || !health, pull = attack$pull || !health, 
       target = attack$target, poison = target$poison || attack$poison || !health, 
       wound = target$wound || attack$wound || !health, 
       immobilize = target$immobilize || attack$immobilize || !health, 
       disarm = target$disarm || attack$disarm || !health, 
       stun = target$stun || attack$stun || !health, 
       muddle = target$muddle || attack$muddle || !health, curse = attack$curse || !health, 
       shieldSelf = attack$shieldSelf)
}

#Prints the state of the enemy after an attack in a more human readable format, 
#uses stringr to combine strings
printAttackEffects <- function(enemyState) {
  print(str_c("damage: ", enemyState$damage))
  print(str_c("health remaining: ", enemyState$health))
  if (enemyState$health) {
    if (enemyState$push != 0) {print(str_c("push: ", enemyState$push))}
    if (enemyState$pull != 0) {print(str_c("pull: ", enemyState$pull))}
    if (enemyState$poison == T) {print("poison")}
    if (enemyState$wound == T) {print("wound")}
    if (enemyState$immobilize == T) {print("immobilize")}
    if (enemyState$disarm == T) {print("disarm")}
    if (enemyState$stun == T) {print("stun")}
    if (enemyState$muddle == T) {print("muddle")}
    if (enemyState$curse == T) {print("curse")}
  }
  if (enemyState$shieldSelf != 0) {print(str_c("shield self: ", enemyState$shieldSelf))}
}

#compares two attacks, returns 1 if the first is better, 
#   2 if the second is better, 0 if equivalent, and -1 if ambiguous
attackComparison <- function(attack1, attack2, target) {
  eState1 <- as.numeric(targetAttacked(attack1, target))
  if (eState1[10] == 1) {eState1[c(8,9)] <- 1}
  eState2 <- as.numeric(targetAttacked(attack2, target))
  if (eState2[10] == 1) {eState2[c(8,9)] <- 1}
  dif <- (eState1-eState2)[2:length(eState1)]
  if (sum(dif != 0) == 0) return(0)
  else if (sum(dif < 0) == 0) return(1)
  else if (sum(dif > 0) == 0) return(2)
  else return(-1)
}


