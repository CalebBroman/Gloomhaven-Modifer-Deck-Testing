# This script defines each attack modifier card.
# Each card will have an associated function returning its effects.
# Each function will take in and return a list in the form:
# 
# (roll = T, shuffle = F, damage, 
#  push = 0, pull = 0, pierce = 0, target = 1, poison = F, wound = F,
#  immobilize = F, disarm = F, stun = F, muddle = F, curse = F)

#Universal Modifiers:
"+0" <- function(x) {x$roll <- F; x}
"+1" <- function(x) {x$roll <- F; x$damage <- x$damage + 1; x}
"+2" <- function(x) {x$roll <- F; x$damage <- x$damage + 2; x}
"-1" <- function(x) {x$roll <- F; x$damage <- x$damage - 1; x}
"-2" <- function(x) {x$roll <- F; x$damage <- x$damage - 2; x}
"x0" <- function(x) {x$roll <- F; x$shuffle <- T; x$damage <- 0; x}
"x2" <- function(x) {x$roll <- F; x$shuffle <- T; x$damage <- x$damage * 2; x}

#Brute Modifiers:
"+3" <- function(x) {x$roll <- F; x$damage <- x$damage + 3; x}
"push1r" <- function(x) {x$roll <- T; x$push <- x$push + 1; x}
"pierce3r" <- function(x) {x$roll <- T; x$pierce <- x$pierce + 3; x}
"stunr" <- function(x) {x$roll <- T; x$stun <- T; x}
"disarmr" <- function(x) {x$roll <- T; x$disarm <- T; x}
"muddler" <- function(x) {x$roll <- T; x$muddle <- T; x}
"addtargetr" <- function(x) {x$roll <- T; x$target <- x$target + 1; x}
"+1shield" <- function(x) {x$roll <- F; x$damage <- x$damage + 1;
                          #I don't want to deal with positive effects yet
                          #x$shield <- x$shield + 1; 
                          x}





