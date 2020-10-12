#### @author: Guna Pemmaraju

library(tidyverse)
library(scales)
library(RColorBrewer)

DIRECTIONS<-c("North", "South", "East", "West")


getCardDeck <- function(){
  cardVals <- c(2:9, "T", "J", "Q", "K", "A")
  suits <- c("Club", "Diamond", "Heart", "Spade")
  
  deck <- tibble(Suit=rep(suits, each=13), Face=rep(cardVals, 4))
  
}


dealIndices <- function(){
  NorthIndex <- sample(1:52, 13)
  remDeck <- setdiff(1:52, NorthIndex)
  
  SouthIndex <- sample(remDeck, 13)
  remDeck <- setdiff(1:52, c(NorthIndex, SouthIndex))
  
  EastIndex <- sample(remDeck, 13)
  WestIndex <- setdiff(1:52, c(NorthIndex, SouthIndex, EastIndex))
  
  list(NorthIndex=NorthIndex, SouthIndex=SouthIndex, EastIndex=EastIndex, WestIndex=WestIndex, AllIndex=c(NorthIndex, SouthIndex, EastIndex, WestIndex))
  
}

dealCards <- function(){
  dealIdx <- dealIndices()
  deck <- getCardDeck()
  tibble(Player=rep(DIRECTIONS, each=13), Suit=deck$Suit[dealIdx$AllIndex], Face=deck$Face[dealIdx$AllIndex])%>%
    arrange(Player, desc(Suit), Face)
}

getDealStats<-function(myDeal, dealId){
  print(dealId)
  result <- myDeal%>%
    group_by(Player)%>%
    do(getHandStats(.))%>%
    as_tibble()
  result$DealId<-dealId
  result
}  

getHandStats <- function(hand){
  
  suitWiseHolding <- hand%>%group_by(Suit)%>%
    summarise(SuitCount=n(), .groups="drop")
  
  
  
  numVoids=4-nrow(suitWiseHolding)
  
  numSingletons = nrow(suitWiseHolding%>%filter(SuitCount==1))
  res<-tibble(Player=hand$Player[1], NumVoids=numVoids, NumSingletons=numSingletons)
  res
}


runDealSimulations <- function(n=100000){
  res <- map_df(paste0("Deal:", 1:n), function(dealId){
    getDealStats(dealCards(), dealId)
  })
  
  result <- res%>%
    group_by(DealId)%>%
    summarise(SingleTonsInDeal=sum(NumSingletons), VoidsInDeal=sum(NumVoids), .groups="drop")
  
  table(result%>%select(-DealId))
  
}