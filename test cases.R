source('play.R')

#Test 1: Certain Doom.  The EVONC should be -57.
hand1 <- c(7,8,9,10,11,12)
deck1 <- c(12,12,12,12,12,12,12,12,12,12,12)
play(hand1,deck1)
#-57

#Test 2: Probable Doom.  The EVONC should be -57*.8+(6+15)*.2=-41.4.
hand2 <- c(7,8,9,10,11,12)
deck2 <- c(6,12,12,12,12)
play(hand2,deck2)
#-41.4

#Test 3: Certain Doom Earlier In. The EVONC should be -50*.8+(7+-57)*.2=-50.
hand3 <- c(8,9,10,11,12)
deck3 <- c(7,12,12,12,12)
play(hand3,deck3)
#-50

#Test 4: Probably Doom Earlier In. The EVONC should be (6+7+15)*2*.2*.25+(-50)*.6+(6-56)*.2*.75+(7-57)*.2*.75=-42.2.
hand4 <- c(8,9,10,11,12)
deck4 <- c(6,7,12,12,12)
play(hand4,deck4)

hand <- hand4
deck <- deck4
