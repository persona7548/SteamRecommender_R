setRepositories()
library(recommenderlab)
library(tidyverse)
library(stringr)
library(arules)
library(data.table)
library(dplyr)
library(MASS)
library(fBasics)
library(RColorBrewer)
library(wordcloud)

bindf<- function(N){
  file <- paste0("C:/Users/KTH/Desktop/Capstone/",N)
  setwd(file)
  filelist = list.files(file)
  b <- data.frame()
  for (i in 1:length(filelist)){
    a <- read.csv(filelist[i],sep=",")
    a <- unique(a)
    Id <- substr(names(a)[2],0,20)
    userId <- rep(Id,nrow(a))
    a <- cbind(userId,a)
    names(a)[3] <- c("PlayTime")
    b <-rbind(b,a)
  }
  setwd("C:/Users/KTH/Desktop/Capstone")
  Save <- paste0(N,".csv")
  write.csv(b,Save,row.names =FALSE)
}#각 파일 합치기

UserInfo <-function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  rawfile <- read.csv("raw.csv")
  rawuserinfo <- rawfile %>%group_by(userId) %>%summarise(gameCount = n(),totalPlaytime = sum(PlayTime))
  write.csv(rawuserinfo,"userInfo.csv",row.names =FALSE)
}#양념 각 유저에 대한 정보
delUserInfo <-function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  InfoFile <- read.csv("userInfo.csv")
  rawfile <- read.csv("filetUser.csv")
  rawuserinfo <- rawfile %>%group_by(userId) %>%summarise(meanPoint = mean(V4))
  rawuserinfo <- merge(rawuserinfo,InfoFile)
  write.csv(rawuserinfo,"delUserInfo.csv",row.names =FALSE)
  }
delgamef <- function(){#50명 미만이 한 게임 제거
  setwd("C:/Users/KTH/Desktop/Capstone")
  UserDB <- read.csv("raw.csv")
  userInfo <- read.csv("userInfo.csv")
  UserDB <- unique(UserDB)
  
  lowUserGame = UserDB %>%group_by(GameName) %>%summarise(n = n())
  lowUserGame <- lowUserGame[lowUserGame$n<50,]
  for(i in 1:as.numeric(nrow(lowUserGame))){
    lowGame <- as.character(lowUserGame[i,1,drop=TRUE])
    x<-which(UserDB$GameName==lowGame)
    UserDB <- UserDB[- x,]
  }
  
  userInfo <-userInfo[userInfo$totalPlaytime>43800,]#플레이 시간 Outlier 제거(5년)
  userInfo <- userInfo[1]
  for(i in 1:as.numeric(nrow(userInfo))){
    outliear <- as.character(userInfo[i,1,drop=TRUE])
    x<-which(UserDB$userId==outliear)
    UserDB <- UserDB[- x,]
  }
  
  write.csv(UserDB,"delLowgame.csv",row.names =FALSE)
}#50명 미만이 플레이한 게임 제거
feactureGame <- function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  UserDB <- read.csv("delLowgame.csv")
  UserDB <- UserDB[-1]

  Gamelist <- UserDB %>%group_by(GameName) %>%summarise(gameCount = n(),length(unique(PlayTime)),(length(unique(PlayTime))/n())
                                                        ,sum(PlayTime),skewness(log2(PlayTime)),kurtosis(log2(PlayTime)),log2(mean(PlayTime)),
                                                        mean(PlayTime),sd(log2(PlayTime)), var(log2(PlayTime)),median(log2(PlayTime)),
                                                        max(PlayTime),min(PlayTime),mad(log2(PlayTime)),IQR(log2(PlayTime))) 
 
  for(k in seq(0.1,5,by = 0.5)){
    Gamelist2 <- UserDB[UserDB$PlayTime<=k,]%>%group_by(GameName)%>%summarise(n())
    names(Gamelist2)[2] <- c(paste0("Playtime",k*10))
    Gamelist <- merge(Gamelist,Gamelist2,all=TRUE)
  }
  for(k in seq(5,100,by = 5)){
    Gamelist2 <- UserDB[UserDB$PlayTime>k,]%>%group_by(GameName)%>%summarise(n())
    names(Gamelist2)[2] <- c(paste0("Playtime",k*10))
    Gamelist <- merge(Gamelist,Gamelist2,all=TRUE)
  }
  Gamelist[is.na(Gamelist)] <- 0
  GameCount <-(UserDB %>%group_by(GameName) %>%summarise(gameCount = n()))
  for(k in 17:46)
   { Gamelist[k] <-Gamelist[k]/GameCount[2]}
  write.csv(Gamelist,"SummaryGame.csv" ,row.names = FALSE)
}#Outlier 게임 선정
delOutgamef <- function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  UserDB<- read.csv("delLowgame.csv")
  GameOutlier <- read.csv("SummaryGame.csv",check.names=FALSE,sep = ",")
  GameOutlierList <-subset(GameOutlier,`length(unique(PlayTime))` <0.1|`mad(log2(PlayTime))` == 0
                           |`var(log2(PlayTime))`==0|`log2(mean(PlayTime))` <0)
  
  for(i in 1:as.numeric(nrow(GameOutlierList))){
    lowGame <- as.character(GameOutlierList[i,1,drop=TRUE])
    x<-which(UserDB$GameName==lowGame)
    UserDB <- UserDB[- x,]
  }
  
  write.csv(UserDB,"delLowgame.csv",row.names =FALSE)
}#Outlier 게임 제거
givePointf<- function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  UserDB <- read.csv("delLowgame.csv")
  UserDB[[4]] <- log2(UserDB[[3]])
  tempDB<-data.frame()
  gameList = UserDB %>%group_by(GameName) %>%summarise(n = n())
  setwd("C:/Users/KTH/Desktop/Capstone/rankPicture")
  for(i in 1:nrow(gameList))
  {
    gameGroup <- as.character(gameList[i,1,drop=TRUE])
    temp <- subset(UserDB, gameGroup == GameName)
    
    temp[4] <-transform((temp[[4]]-min(temp[[4]]))/(max(temp[[4]])-min(temp[[4]])))
    tempDB <- rbind(tempDB,temp)
    saveName <- paste0(i,".png")
    png(filename =saveName,width = 800,height = 600 )
    hist(temp[[4]],breaks = seq(0,1,by=0.1),xlab = "rank",main = gameGroup)
    dev.off()
  }
  setwd("C:/Users/KTH/Desktop/Capstone")
  names(tempDB)
  write.csv(tempDB,"rankData.csv",row.names = FALSE)
}#[0-1]점수 부여
deluserf <- function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  UserDB <- read.csv("rankData.csv")
  lowGameUser <- UserDB %>%group_by(userId) %>%summarise(n = n())
  lowGameUser <- lowGameUser[lowGameUser$n<20,]
  
  for(i in 1:as.numeric(nrow(lowGameUser))){
    lowUser <- as.character(lowGameUser[i,1,drop=TRUE])
    x<-which(UserDB$userId==lowUser)
    UserDB <- UserDB[- x,]
  }

  write.csv(UserDB,"filetUser.csv",row.names = FALSE)
  
}#20개 미만 플레이 유저 제거
UBCF <- function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  UserDB <- read.csv("filetUser.csv")
  UserDB <- UserDB[-3]

  UserDB <- as(UserDB,"realRatingMatrix")
  index <- sample(1:nrow(UserDB), size = nrow(UserDB) * 0.7) 
  train <- UserDB[index, ] 

  recomm_model <- Recommender(data = train, method = "UBCF",parameter = "Pearson")
  pred <- predict(recomm_model, newdata = UserDB, n = 10)
  write.csv(as(pred,"list"),"predRecom.csv")
  }#점수 기반 게임 추천

pointCount <- function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  UserDB <- read.csv("filetUser.csv")
  userName <- UserDB %>%group_by(userId) %>%summarise
  setwd("C:/Users/KTH/Desktop/Capstone/UserHist")
  for (i in 1:as.numeric(nrow(userName))){
    ID <- as.character(userName[i,1,drop=TRUE])
    temp <- subset(UserDB,userId == ID) #list of each userID
    saveName <- paste0(ID,".png")
    png(filename =saveName,width = 800,height = 600 )
    hist(temp[[4]],breaks = seq(0,1,by=0.1),prob=T,xlab = "플레이 시간",main = ID)
    dev.off()
  }
  setwd("C:/Users/KTH/Desktop/Capstone")
}#각 유저의 점수 부여 정보
wordcloudf <- function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  userDB <- read.csv("filetUser.csv")
  gametag <- read.csv("gametag.csv")
  userDB <-userDB[1:2]


#display.brewer.all()
color <- brewer.pal(9, "Blues")
windowsFonts(font=windowsFont("a한글사랑L"))

tagDB <- merge(userDB, gametag,by='GameName')
tagDB <- tagDB[-1]
userName <- userDB %>%group_by(userId) %>%summarise
setwd("C:/Users/KTH/Desktop/Capstone/WordCloud")
likeTag <- data.frame(c(1:10))
for (i in 1:as.numeric(nrow(userName))){
  ID <- as.character(userName[i,1,drop=TRUE])
  temp <- subset(tagDB,userId == ID)
  temp <- temp[-1]
  wordcount_top <-head(sort(table(temp), decreasing = T),10)
  wordcount <-as.data.frame(wordcount_top)
  wordcount <- wordcount[1]
  names(wordcount) <- ID
  likeTag <- cbind(likeTag,wordcount)
  saveName <- paste0(ID,".png")
  png(filename =saveName,width = 800,height = 600 )
  wordcloud(names(wordcount_top), wordcount_top, scale=c(5,1),random.order = F, random.color = FALSE,rot.per = 0, colors = color, family = "font")
  dev.off()
}
setwd("C:/Users/KTH/Desktop/Capstone")
write.csv(likeTag,"LikeTag.csv",row.names = F)
}#게임 태그 기반 유저 선호 취향
histLogGame<-function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  UserDB <- read.csv("delLowgame.csv")
  UserDB[3] <- log2(UserDB[[3]])
  gameList = UserDB %>%group_by(GameName) %>%summarise(n = n())
  setwd("C:/Users/KTH/Desktop/Capstone/picture")
  for(i in 1:nrow(gameList) ){
    gameGroup <- as.character(gameList[i,1,drop=TRUE])
    temp <- subset(UserDB, gameGroup == GameName)
    hist(temp[[3]])
    saveName <- paste0(i,".png")
    png(filename =saveName,width = 800,height = 600 )
    hist(temp[[3]],xlab = "log2(PlayTime)", main = gameGroup,prob=T)
    dev.off()
  }
  setwd("C:/Users/KTH/Desktop/Capstone")
}#게임 플레이 시간 Histogram
CorList<-function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  
  UserDB <- read.csv("filetUser.csv",sep=",")
  userTable <- table(UserDB[1:2])
  gameTable <- table(UserDB[2:1])
  write.csv(userTable,"UserTable.csv")
  write.csv(gameTable,"GameTable.csv")
  
  userTable <- read.csv("UserTable.csv",check.names=FALSE,sep = ",")
  gameTable <- read.csv("GameTable.csv",sep = ",")
  
  gamecor <- cor(userTable[-1],method = "pearson") 
  usercor <- cor(gameTable[-1],method = "pearson") 
  
  write.csv(usercor,"userCor.csv")
  write.csv(gamecor,"gameCor.csv")
  
  usercor <- read.csv("userCor.csv",check.names=FALSE)
  gamecor <- read.csv("gameCor.csv",check.names=FALSE)
 
  GamecorList<- c("GameName",c(1:10))
  for(i in 1:length(gamecor)){
    temp <-as.data.frame(gamecor[i,])
    GamecorList <- rbind(GamecorList,c(colnames(sort(temp[-1],decreasing = T))[1:11]))
  }
  write.csv((GamecorList),"GamecorList.csv",row.names = F,col.names =  F)
  
  
  usercorList<- c("userId",c(1:10))
  for(i in 1:length(usercor)){
    temp <-as.data.frame(usercor[i,])
    usercorList <- rbind(usercorList,colnames(sort(temp[-1],decreasing = T))[1:11])
  }
  write.csv((usercorList),"usercorList.csv",row.names = F,col.names =  F)
  }#유저-유저/게임-게임 관계리스트 
RecommenderF <- function(){
  setwd("C:/Users/KTH/Desktop/Capstone")
  gameCorList <- read.csv("GamecorList.csv",sep = ",",check.names=FALSE)
  usercorList <- read.csv("usercorList.csv",sep = ",",check.names=FALSE)
  UserDB <- read.csv("filetUser.csv")
  GoodUserDB <- UserDB[UserDB$V4>=0.55,] # 다른사람보다 플레이시간이 많은 게임 리스트
  GoodUserDB <- GoodUserDB[1:2]
  userName <- UserDB %>%group_by(userId) %>%summarise()
  
  goodGameList <- data.frame(c(1:10))
  for (i in 1:nrow(userName)){ 
    ID <- as.character(userName[i,1,drop=TRUE])
    temp <- subset(GoodUserDB,userId == ID) 
    tmp <- as.data.frame(temp[[2]])
    ubcfTmp <- subset(usercorList,userId == ID)
    recomList<- data.frame()
    
    #view(temp)
    #view(t(ubcfTmp))
    if(nrow(tmp) !=0){
      for (j in 1:nrow(tmp)) #Item-Based Recommend
      {
        corGameList <- subset(gameCorList,gameCorList$GameName == as.character(tmp[j,1,drop=T]))
        #view(t(corGameList))
        corGameList <-as.data.frame(t(corGameList[2:6]))
        names(corGameList) <- "List"
        recomList<- rbind(recomList,(corGameList))
      }
    }
   # view(recomList)
    for (j in 2:nrow(ubcfTmp)) #User-Based Recommend
    {  
      temp <- subset(GoodUserDB,userId == t(ubcfTmp)[j])
     # view(temp)
      tmp <- as.data.frame(temp[[2]])
      names(tmp) <- "List"
      recomList<- rbind(recomList,tmp)
    }
    #view(recomList)
    userGameList <- subset(UserDB,userId == ID)
    userGameList <- userGameList[2]
    
    recomList <- (table(recomList))
    recomList <-as.data.frame(recomList)
    recomList <- recomList[recomList$Freq>0,]

    
    for(j in 1:nrow(userGameList)) #추천리스트중 보유한 게임 제거
    {
      lowGame <- as.character(userGameList[j,1,drop=TRUE])
      x<-which( recomList$recomList==lowGame)
      if(length(x) != 0)
        recomList <- recomList[-x,]
    }
    recomList <- recomList[order(-recomList$Freq),]
    topList <-head(recomList,n=10)
    topList <- topList[1]
    #view(topList)
    if(nrow(topList)<10)
    {
      for(j in nrow(topList)+1:(10-nrow(topList)))
        topList <- rbind(topList,c("NA"))
    }
    names(topList) <-ID
    goodGameList <- cbind(goodGameList,topList)
  }
  write.csv(goodGameList,"MyRecommender.csv",row.names = F)
}#자체 추천 알고리즘 

bindf("PC")
bindf("EC2")
bindf("Azure")
bindf("Azure2")
a <-read.csv("PC.csv")
b <-read.csv("EC2.csv")
c <-read.csv("Azure.csv")
d <-read.csv("Azure2.csv")

a <-rbind(a,b)
c <-rbind(c,d)
a <-rbind(a,c)

x <- which(duplicated(a[,c("userId","GameName")]))
for (i in length(x):1)
{
  num <- as.numeric(x[i])
  a<- a[- num,]
}

write.csv(a,"raw.csv",row.names =FALSE)

UserInfo()
delgamef()
feactureGame()
delOutgamef()

feactureGame()

givePointf()
deluserf()
delUserInfo()

CorList()
RecommenderF()
UBCF()
pointCount()
histLogGame()
wordcloudf()

