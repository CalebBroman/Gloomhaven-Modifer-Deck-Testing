#This script will calculate attacks.

baseAttack <- list(roll = T, shuffle = F, damage = 3, 
                shield = 0, retaliate = 0, push = 0, pull = 0, pierce = 0, target = 1,
                invisible = F, strengthen = F, regenerate = F, bless = F, poison = F, wound = F,
                immobilize = F, disarm = F, stun = F, muddle = F, curse = F)

attackAction <- function(base, deck) {
  attack <- base
  while(attack$roll == T) {
    card <- deck[[1]]
    deck <- deck[2:length(deck)]
    attack <- card(attack)
  }
  attack
}