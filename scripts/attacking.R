#This script will calculate attacks.
#This script requires the comparison function from attackcomparisons.R 
#for the advantage and disadvantage calculations

#Prints an attack in a more human readable format
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

standardAttackAction <- function(base, deck) {
  attack <- base
  while(attack$roll == T) {
    card <- deck[[1]]
    deck <- deck[2:length(deck)]
    attack <- card(attack)
  }
  attack
}

advantageAttackActionV1 <- function(base, deck) {
  
}

disadvantageAttackActionV1 <- function(base, deck) {
  
}

