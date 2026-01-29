#The play function returns the Expected Value Of the Next Card (EVONC). Presumedly,
#a rational player would hit whenever EVONC is positive but never otherwise.
#The function currently puts the 7-card bonus into the EVONC only when you
#have six cards but not for smaller hand counts. Relatedly, EVONC ought to account
#for the fact that receiving a safe card on your next draw allows you the option to
#draw at least one more time, but it does not currently do this. Special cards are
#not currently accounted for. The cards in front of your opponents are not currently
#accounted for, nor are cards played from the deck in a previous round (and not yet
#shuffled back in).
#Even if all these complications were implemented, it would not fully resolve
#the optimal strategy. Remaining questions would be: whom to freeze, whom to three-flip,
#and whom to gift a second chance.
#Accounting for special cards is complex, since we need to account for the fact that 
#you might be frozen or three-flipped or second-chanced at any time, and the 
#probability of such depends on the scoreboard and other players' strategies. Moreover,
#the calculation of EVONC is changed when you are the last active player, since
#you could have to three-flip yourself.
#Modifier cards consist of +2, +4, +6, +8, +10, and x2. Action cards consist 
#of 3 freezes, 3 flip threes, and 3 second chances.

remover <- function(cards,deck) {
  indices <- match(cards,deck)
  return(deck[-indices])
}

buster <- function(hand,deck) { #returns the probability of successfully making it to 7 cards, starting from a given hand
  m <- 7-length(hand)
  if(m > 0 & m < 7) {
    futures_ <- if(length(deck)>1) {combn(deck,m)} else{deck}
    futures <- rbind(matrix(hand,length(hand),choose(length(deck),m)),futures_)
    return(1-mean(!apply(!apply(futures,FUN=duplicated,MARGIN=2),FUN=prod,MARGIN=2)))
  } else if(m == 7) {
    0
  } else {
    futures <- hand
    return(1-mean(!apply(!apply(futures,FUN=duplicated,MARGIN=2),FUN=prod,MARGIN=2)))
  }
}

aggregator <- function(hand,deck) { #returns the total of all additional points available in a subtree, scaled by their probabilities, starting from a given hand
  if(all(hand > 0)) {
    n <- length(hand)
    m <- 7-length(hand)
    score <- sum(hand)
    if(m > 0) {
      futures_ <- if(length(deck)>1) {combn(deck,m)} else{deck}
      bonus <- if(length(deck)>1) {apply(futures_,FUN=sum,MARGIN=2)} else {futures_}
      futures <- rbind(matrix(hand,length(hand),choose(length(deck),m)),futures_)
      bonus[!apply(!apply(futures,FUN=duplicated,MARGIN=2),FUN=prod,MARGIN=2)] <- -score #TODO: Shouldn't this 0 be -score instead?
      return(sum(bonus)/ifelse(length(deck)>1,ncol(futures_),1))
    } else {
      return(0)
    }
  } else {
    return(0)
  }
}

play <- function(hand,deck) {
  score <- sum(hand)
  deck_ <- deck
  #Reframe the deck in terms of its value to this player, given their hand
  deck_[deck %in% hand] <- -score #Duplicate cards cancel out your score
  hands <- rbind(matrix(hand,length(hand),length(deck_)),deck_) #Consider all possible hands that result from hitting once
  #I need to modify the deck for each of the `hands` scenarios
  ind <- deck_>0 #Determine which ones are successful
  ind2 <- !duplicated(hands,MARGIN=2) #Determine which ones are not redundant
  hands_ab <- hands[,(ind & ind2),drop=FALSE] #Abridge out unsuccessful ones and successful redundant ones for a moment
  deck__ <- deck_[(ind & ind2)] #Abridge deck_ to correspond to the abridged hands for the next two functions
  deck_[ind & ind2] <- deck_[ind & ind2] + apply(hands_ab,MARGIN=2,FUN=aggregator,deck=deck__) #Add to the number the present value of future hits
  if(dim(hands_ab)[2]<=1) reward <- buster(hands_ab,deck__) else reward <- apply(hands_ab,MARGIN=2,FUN=buster,deck=deck__)
  deck_[ind & ind2] <- deck_[ind & ind2] + 15*reward #Add to the number the present value of the 15 point bonus
  if(any(ind & !ind2)) deck_[ind & !ind2] <- rep(deck_[which(ind2)],times=rle(as.vector(ind2))$lengths[-which(rle(as.vector(ind2))$lengths==1)]) #Copy the adjusted numbers to the successful redundant hands TODO: how does this line interact with unsuccessful hits (ind=TRUE)?
  evonc <- sum(deck_/length(deck_))
  return(evonc)
}

whole_deck <- c(rep(12,12),rep(11,11),rep(10,10),rep(9,9),rep(8,8),rep(7,7),rep(6,6),
                rep(5,5),rep(4,4),rep(3,3),rep(2,2),1,0)
