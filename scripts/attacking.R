#This script will calculate attacks.

baseAttack <- list(roll = T, shuffle = F, damage = 3, 
                shield = 0, retaliate = 0, push = 0, pull = 0, pierce = 0, target = 1,
                invisible = F, strengthen = F, regenerate = F, bless = F, poison = F, wound = F,
                immobilize = F, disarm = F, stun = F, muddle = F, curse = F)

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
  if (attack$shield != 0) {print(str_c("shield self: ", attack$shield))}
  if (attack$retaliate != 0) {print(str_c("retaliate self: ", attack$retaliate))}
  if (attack$invisible == T) {print("invisible self")}
  if (attack$strengthen == T) {print("strengthen self")}
  if (attack$regenerate == T) {print("regenerate self")}
  if (attack$bless == T) {print("bless self")}
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