draw<- function() {
  plot (NULL, xlim=c(1,26), ylim=c(1,10), xlab="X",ylab="Y", axes=F)
  box()
  xleft<- c(1,6,11,16,21)
  ybottom<- c(1,6)
  xright<- c(5,10,15,20,25)
  ytop<- c(5,10)
  rect(xleft,ybottom,xright,ytop)
}

random <- function(roll,saved){
  if (roll == 1){
    dice <<- saved
    diceDraw()
  }
  if (roll == 2){
    dice <<- saved
    diceDraw()
  }
  if (roll == 3){
    dice <<- saved
    diceDraw()
  }
}
  
diceDraw <- function() {
  #first dice draw
  if(dice[1]==1){
    x<-c(3)
    y<-c(3)
    points(x,y,pch=19)
  }
  if(dice[1]==2){    
    x<-c(2,4)
    y<-c(2,4)
    points(x,y,pch=19)
  }
  if(dice[1]==3){    
    x<-c(2,3,4)
    y<-c(2,3,4)
    points(x,y,pch=19)
  }
  if(dice[1]==4){    
    x<-c(2,2,4,4)
    y<-c(2,4,2,4)
    points(x,y,pch=19)
  }
  if(dice[1]==5){    
    x<-c(2,2,3,4,4)
    y<-c(2,4,3,2,4)
    points(x,y,pch=19)
  }
  if(dice[1]==6){    
    x<-c(2,2,2,4,4,4)
    y<-c(2,3,4,2,3,4)
    points(x,y,pch=19)
  }
  #second dice draw
  if(dice[2]==1){
    x<-c(3)+5
    y<-c(3)+5
    points(x,y,pch=19)
  }
  if(dice[2]==2){    
    x<-c(2,4)+5
    y<-c(2,4)+5
    points(x,y,pch=19)
  }
  if(dice[2]==3){    
    x<-c(2,3,4)+5
    y<-c(2,3,4)+5
    points(x,y,pch=19)
  }
  if(dice[2]==4){    
    x<-c(2,2,4,4)+5
    y<-c(2,4,2,4)+5
    points(x,y,pch=19)
  }
  if(dice[2]==5){    
    x<-c(2,2,3,4,4)+5
    y<-c(2,4,3,2,4)+5
    points(x,y,pch=19)
  }
  if(dice[2]==6){    
    x<-c(2,2,2,4,4,4)+5
    y<-c(2,3,4,2,3,4)+5
    points(x,y,pch=19)
  }
  #third dice draw
  if(dice[3]==1){
    x<-c(3)+10
    y<-c(3)
    points(x,y,pch=19)
  }
  if(dice[3]==2){    
    x<-c(2,4)+10
    y<-c(2,4)
    points(x,y,pch=19)
  }
  if(dice[3]==3){    
    x<-c(2,3,4)+10
    y<-c(2,3,4)
    points(x,y,pch=19)
  }
  if(dice[3]==4){    
    x<-c(2,2,4,4)+10
    y<-c(2,4,2,4)
    points(x,y,pch=19)
  }
  if(dice[3]==5){    
    x<-c(2,2,3,4,4)+10
    y<-c(2,4,3,2,4)
    points(x,y,pch=19)
  }
  if(dice[3]==6){    
    x<-c(2,2,2,4,4,4)+10
    y<-c(2,3,4,2,3,4)
    points(x,y,pch=19)
  }
  #fourth dice draw
  if(dice[4]==1){
    x<-c(3)+15
    y<-c(3)+5
    points(x,y,pch=19)
  }
  if(dice[4]==2){    
    x<-c(2,4)+15
    y<-c(2,4)+5
    points(x,y,pch=19)
  }
  if(dice[4]==3){    
    x<-c(2,3,4)+15
    y<-c(2,3,4)+5
    points(x,y,pch=19)
  }
  if(dice[4]==4){    
    x<-c(2,2,4,4)+15
    y<-c(2,4,2,4)+5
    points(x,y,pch=19)
  }
  if(dice[4]==5){    
    x<-c(2,2,3,4,4)+15
    y<-c(2,4,3,2,4)+5
    points(x,y,pch=19)
  }
  if(dice[4]==6){    
    x<-c(2,2,2,4,4,4)+15
    y<-c(2,3,4,2,3,4)+5
    points(x,y,pch=19)
  }
  #fifth dice draw
  if(dice[5]==1){
    x<-c(3)+20
    y<-c(3)
    points(x,y,pch=19)
  }
  if(dice[5]==2){    
    x<-c(2,4)+20
    y<-c(2,4)
    points(x,y,pch=19)
  }
  if(dice[5]==3){    
    x<-c(2,3,4)+20
    y<-c(2,3,4)
    points(x,y,pch=19)
  }
  if(dice[5]==4){    
    x<-c(2,2,4,4)+20
    y<-c(2,4,2,4)
    points(x,y,pch=19)
  }
  if(dice[5]==5){    
    x<-c(2,2,3,4,4)+20
    y<-c(2,4,3,2,4)
    points(x,y,pch=19)
  }
  if(dice[5]==6){    
    x<-c(2,2,2,4,4,4)+20
    y<-c(2,3,4,2,3,4)
    points(x,y,pch=19)
  }
}
  
  
  
  

gamePlayer <- function() {
  pNum<<-2
  pNum<<-readline("how many players: ")
  if(pNum<1 && pNum>8){stop("number of player must be an integer from 1 to 8")}
  if (pNum==1) {
    player1<<-readline("Player 1 name:")
    player<<-c(player1)
    }
  if (pNum==2) {
    player1<<-readline("Player 1 name:")
    player2<<-readline("Player 2 name:")
    player<<-c(player1,player2)
    
    }
  if (pNum==3) {
    player1<<-readline("Player 1 name:")
    player2<<-readline("Player 2 name:")
    player3<<-readline("Player 3 name:")
    player<<-c(player1,player2,player3)
    }
  if (pNum==4) {
    player1<<-readline("Player 1 name:")
    player2<<-readline("Player 2 name:")
    player3<<-readline("Player 3 name:")
    player4<<-readline("Player 4 name:")
    player<<-c(player1,player2,player3,player4)
    }
  if (pNum==5) {
    player1<<-readline("Player 1 name:")
    player2<<-readline("Player 2 name:")
    player3<<-readline("Player 3 name:")
    player4<<-readline("Player 4 name:")
    player5<<-readline("Player 5 name:")
    player<<-c(player1,player2,player3,player4,player5)
    }
  if (pNum==6) {    
    player1<<-readline("Player 1 name:")
    player2<<-readline("Player 2 name:")
    player3<<-readline("Player 3 name:")
    player4<<-readline("Player 4 name:")
    player5<<-readline("Player 5 name:")
    player7<<-readline("Player 6 name:")
    player<<-c(player1,player2,player3,player4,player5,player6)
    }
  if (pNum==7) {
    player1<<-readline("Player 1 name:")
    player2<<-readline("Player 2 name:")
    player3<<-readline("Player 3 name:")
    player4<<-readline("Player 4 name:")
    player5<<-readline("Player 5 name:")
    player6<<-readline("Player 6 name:")
    player7<<-readline("Player 7 name:")
    player<<-c(player1,player2,player3,player4,player5,player6,player7)
    }
  if (pNum==8) {
    player1<<-readline("Player 1 name:")
    player2<<-readline("Player 2 name:")
    player3<<-readline("Player 3 name:")
    player4<<-readline("Player 4 name:")
    player5<<-readline("Player 5 name:")
    player6<<-readline("Player 6 name:")
    player7<<-readline("Player 7 name:")
    player8<<-readline("Player 8 name:")
    player<<-c(player1,player2,player3,player4,player5,player6,player7,player8)
  }
  
}

#TESTING PURPOSES
dice <<- c(1,3,4,5,6)
#point<<-0
check <-function(no) {
  if (no==TRUE){
    stop("Can't choose the same category twice")
    gamePoints(dice)
  }
}


?stopifnot
gamePoints<-function(dice) {
  noAces <<- FALSE
  noTwos <<- FALSE
  noThrees <<- FALSE
  noFours <<- FALSE
  noFive <<- FALSE
  noSix <<- FALSE
  noTOK <<- FALSE
  noFOK <<- FALSE
  noFH <<- FALSE
  noLarge <<- FALSE
  noSmall <<- FALSE
  noYaht<<- FALSE
  noChance <<- FALSE
  cat("choose one of the category")
  category <-
    readline(
      "Aces, Twos, Threes, Fours, Fives, Sixes, three of a kind\n, four of a kind, Full House, Small, Large, yahtzee, Chance :"
    )
  
  if (category == "Aces") {
    check(noAces)
    for (i in 1:5) {
      if (dice[i] == 1) {
        point <<- point + 1
        noAces <<- TRUE
      }
    }
    
  }
  if (category == "Twos") {
    check(noTwos)
    for (i in 1:5) {
      if (dice[i] == 2) {
        point <<- point + 2
        noTwos <<- TRUE
      }
    }
  }
  if (category == "Threes") {
    check(noThrees)
    for (i in 1:5) {
      if (dice[i] == 3) {
        point <<- point + 3
        noThrees <<- TRUE
      }
    }
  }
  if (category == "Fours") {
    check(noFours)
    for (i in 1:5) {
      if (dice[i] == 4) {
        point <<- point + 4
        noFours <<- TRUE
      }
    }
  }
  if (category == "Fives") {
    check(noFives)
    for (i in 1:5) {
      if (dice[i] == 5) {
        point <<- point + 5
        noFive <<- TRUE
      }
    }
  }
  if (category == "Sixes") {
    check(noSixes)
    for (i in 1:5) {
      if (dice[i] == 6) {
        point <<- point + 6
        noSix <<- TRUE
      }
    }
  }
  if (category == "three of a kind") {
    check(noATOK)
    j <- 1
    counter <- 0
    for (i in 1:5) {
      if (dice[j] == dice[i]) {
        counter <- counter + 1
        
      }
      else if (dice[j] != i) {
        j <- j + 1
      }
      
    }
    if (counter == 3) {
      point <<- sum(dice)
      noTOK <<- TRUE
    }
  }
  
  
  if (category == "four of a kind") {
    check(noFOK)
    j <- 1
    counter <- 0
    for (i in 1:5) {
      if (dice[j] == dice[i]) {
        counter <- counter + 1
      }
      else if (dice[j] != i) {
        j <- j + 1
      }
    }
    if (counter == 4) {
      point <<- sum(dice)
      noFOK <<- TRUE
    }
  }
  
  if (category == "Full House") {
    check(noFH)
    j <- 1
    f <- 5
    counter1 <- 0
    counter2 <- 0
    for (i in 1:5) {
      if (dice[j] == dice[i]) {
        counter1 <- counter1 + 1
      }
      else if (dice[f] == dice[i]) {
        counter2 <- counter2 + 1
      }
      else if (dice[j] != i) {
        j <- j + 1
      }
    }
    if (counter1 == 3 &&
        counter2 == 2 || counter1 == 2 && counter2 == 3) {
      point <<- point + 25
      noFH <<- TRUE
    }
  }
  if (category == "Large") {
    check(noLarge)
    if (dice[1] == 1 &&
        dice[2] == 2 &&
        dice[3] == 3 &&
        dice[4] == 4 &&
        dice[5] == 5 ||
        dice[1] == 2 &&
        dice[2] == 3 &&
        dice[3] == 4 &&
        dice[4] == 5 &&
        dice[5] == 6) {
      point <<- point + 40
      noLarge <<- TRUE
    }
    
  }
  if (category == "Small") {
    check(noSmall)
    if (dice[1] == 1 &&
        dice[2] == 2 &&
        dice[3] == 3 &&
        dice[4] == 4 ||
        dice[1] == 2 &&
        dice[2] == 3 &&
        dice[3] == 4 &&
        dice[4] == 5 ||
        dice[1] == 3 &&
        dice[2] == 4 &&
        dice[3] == 5 &&
        dice[4] == 6) {
      point <<- point + 30
      noSmall <<- TRUE
    }
  }
  
  if (category == "Yahtzee") {
    check(noYaht)
    j <- 1
    counter <- 0
    for (i in 1:5) {
      if (dice[j] == dice[i]) {
        counter <- counter + 1
      }
      else if (dice[j] != i) {
        j <- j + 1
      }
    }
    if (counter == 5) {
      point <- point + 50
      noYaht<<- TRUE
    }
  }
  
  if (category == "Chance") {
    check(noChance)
    point <<- sum(dice)
    noChance <<- TRUE
  }
}

Record<- function(savedDice) {
  
  if (savedDice == 1) {
    cat("which dice do you want to save? ")
    value<<-c(scan(nmax = savedDice))
    saved<<-c(value)
    rand<<- sample(dice,4)
    dice<<-as.numeric(append(saved,rand))
  }
  if (savedDice == 2) {
    cat("which dice do you want to save? ")
    value<<-c(scan(nmax = savedDice))
    saved<<-c(value)
    rand<<- sample(dice,3)
    dice<<-as.numeric(append(saved,rand))
  }
  if (savedDice == 3) {
    cat("which dice do you want to save? ")
    value<<-c(scan(nmax = savedDice))
    saved<<-c(value)
    rand<<- sample(dice,2)
    dice<<-as.numeric(append(saved,rand))
  }
  if (savedDice == 4) {
    cat("which dice do you want to save? ")
    value<<-c(scan(nmax = savedDice))
    saved<<-c(value)
    rand<<- sample(dice,1)
    dice<<-as.numeric(append(saved,rand))
  }
  if (savedDice == 5) {
    cat("which dice do you want to save? ")
    value<<-c(scan(nmax = savedDice))
    saved<<-c(value)
    dice<<- saved
    gamePoints(dice)
  }
}

Leaderborad<-function(player,point) {
  cat("LeaderBoard")
  board <<- data.frame("player"= player, "Points" = point)
  for(i in 1:length(player)){
    if (board$point[i]>board$point[i+1])
    winner <<- board$player[i]
  }
  if (board$point[i]<board$point[i+1]){
      winner <<- board$player[i+1]
  }
  if(board$point[i]==board$point[i+1]){
    cat("Game is tied")
  }
  
}

yahtzee<-function(){
  point<<-0
  round<-1
  cat("welcome to the game of yahtzee!!\n")
  start<-readline("Please insert s to start the game")
  gamePlayer()    
  for(i in 1:length(player))
    player<<-player[i]
    while (round!=13) {
      draw()
      random(1, sample(c(1,2,3,4,5,6),5))
      savedDice<<- readline("How many dice do you want to save? ")
      Record(savedDice)
      draw()
      random(2, dice)
      savedDice<<- readline("How many dice do you want to save? ")
      Record(savedDice)
      gamePoints(dice)
      draw()
      random(3, dice)
      round<-round+1
    }
  Leaderboard(player,point)
  }
  
yahtzee()

