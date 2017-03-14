#Set working directory----------------
setwd("C:\\Users\\mattd\\Dropbox\\FFL\\CFB\\CFB Play-by-Play Database\\play")

#Load Data-----------------
# Get the files names
files = list.files(pattern="*.csv")

# First apply read.csv, then rbind
train = do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Change Play Type to factor
train$Play.Type <- as.factor(train$Play.Type)
train$Previous.Play.Type<-as.factor(train$Previous.Play.Type)

#Single Season data loads
#train <- read.csv("play 11.csv")


#Clean data--------------------------------
#Train cleaning
train <- train[,c(3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20)]
#Only save Runs and Passes
train <- droplevels(train[!(train$Play.Type %in% c("ATTEMPT", "FIELD_GOAL", "KICKOFF", "PENALTY", "PUNT", "TIMEOUT", "SACK")),])
#4th Quarter Models
#train <- train[which(train$Period.Number == '1'& train$Offense.Points>train$Defense.Points),]
#train4 <- train[which(train$Offense.Team.Code == '703' & train$Year %in% c(2005,2006,2007,2008,2009,2010)&train$Period.Number =='4'&train$Offense.Points<train$Defense.Points),]
#train5 <- train[which(train$Offense.Team.Code == '312' & train$Year %in% c(2012,2013)&train$Period.Number =='4'&train$Offense.Points<train$Defense.Points),]

#Other Quarter Models
#train <- train[which(train$Period.Number == '1'),]
#train1 <- train[which(train$Offense.Team.Code == '648' ),]
#train2 <- train[which(train$Offense.Team.Code == '648' & train$Defense.Team.Code %in% c(196,257,726,736,37,334,430,694)),]
train4 <- train[which(train$Offense.Team.Code == '129' & train$Year %in% c(2005,2006)&train$Period.Number=='1'),]
train5 <- train[which(train$Offense.Team.Code == '140' & train$Year %in% c(2007,2008,2009)&train$Period.Number=='1'),]
train6 <- train[which(train$Offense.Team.Code == '513' & train$Year %in% c(2010,2011,2012,2014)&train$Period.Number=='1'),]
train7 <- test[which(test$Offense.Team.Code=='513'&test$Defense.Team.Code == '703'&test$Period.Number=='1'),]
#Combine different team data
train <- rbind(train4,train5,train6,train7)


#Test Cleaning
test <- read.csv("2015 NEW WEEK 3.csv")
test <- test[,c(3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20)]
#Only save run and pass
test <- droplevels(test[!(test$Play.Type %in% c("ATTEMPT", "FIELD_GOAL",  "KICKOFF", "PENALTY", "PUNT",   "TIMEOUT", "SACK")),])
#Select specific game to test on
test <- test[which(test$Offense.Team.Code == '513'),]
test <- test[which(test$Defense.Team.Code == '746'),]
#4th Quarter model
#test <- test[which(test$Period.Number=='4'&test$Offense.Points<test$Defense.Points),]
#Other Quarter Models
test <- test[which(test$Period.Number=='1'),]
#levels(test$Previous.Play.Type) <- levels(train$Previous.Play.Type)
test[test$Previous.Play.Type == "SACK",]$Previous.Play.Type="PASS"

#Make levels the same for Train and Test
#levels(test$Play.Type) <- levels(train$Play.Type)

str(train)
#Random Forest--------------------
install.packages("randomForest")
library(randomForest)
myNtree = 501
myMtry = 4
myImportance = TRUE
set.seed(415)
#Model set up and run
playFit.1rf <- randomForest(Play.Type ~ Period.Number + Clock + Offense.Team.Code +
                             Defense.Team.Code + Offense.Points + Defense.Points +
                             Down + Distance + Spot + Drive.Number + Interceptions +
                             Fumbles + Point.Differential + DownXDistance + Previous.Play.Type + Year
                             ,data = train, ntree = myNtree, mtry = myMtry, importance = myImportance)

#Shiny Test
playFit.1rf <- randomForest(Play.Type ~ Period.Number + Clock + Offense.Points + Defense.Points +
                              Down + Distance + Spot + Interceptions +
                              Fumbles + DownXDistance + Point.Differential + Year
                            ,data = train, ntree = myNtree, mtry = myMtry, importance = myImportance)



#Plot error
#plot(playFit.rf, log="y")
save(playFit.3rf, file = "Qtr4tieNotreDame.RData")

#Plot variable importance
varImpPlot(playFit.4tierf)

#Create predictions in test tables
test$pred.PlayType.rf <- predict(playFit.1rf, test)
table(test$Play.Type, test$pred.PlayType.rf)

#Conditional Inference Tree-----------------
library(party)
playFit.ctree <- ctree(Play.Type ~ Play.Number + Period.Number +  
                         Offense.Points + Defense.Points + Down + 
                         Distance + Spot + Drive.Number + Home + Away + Stadium + Site +
                         Attendance + Capacity , data = train)

test$pred.PlayType.ctree <- predict(playFit.ctree, test)
table(test$Play.Type, test$pred.PlayType.ctree)
prop.table(table(test$Play.Type, test$pred.PlayType.ctree),1)

#Linear Discriminant Analysis---------------
library(MASS)
playFit.lda <- lda(Play.Type ~ Play.Number + Period.Number +  
                     Offense.Points + Defense.Points + Down + 
                     Distance + Spot + Drive.Number + Home + Away + Stadium + Site +
                     Attendance + Capacity, data = train)

test$pred.PlayType.lda <- predict(playFit.lda, test[,1:12])$class
table(test$Play.Type, test$pred.PlayType.lda)
prop.table(table(test$Play.Type, test$pred.PlayType.lda),1)

#Neural Net-------------------
install.packages("neuralnet")
library(nnet)
library(caret)
?expand.grid

my.grid <- expand.grid(.decay = c(0.5,0.1), .size = c(5,6,7))
playFit.nn <- train(Play.Type ~ Play.Number + Period.Number +  
                      Offense.Points + Defense.Points + Down + 
                      Distance + Spot + Drive.Number + Home + Away + Stadium + Site +
                      Attendance + Capacity,data = train, 
                    method = "nnet", maxit=1000,tuneGrid=my.grid,trace=F,linout=0)
test$pred.PlayType.nn <- predict(playFit.nn, test)
table(test$Play.Type, test$pred.PlayType.nn)
test.look <- test[which(test$Down == '1' & test$Distance == '10'),]
table(test.look$Play.Type, test.look$pred.PlayType.nn)
test.nn <- test[,c(1,2,3,4,5,6,7,8,9,10,11,13)]

#Logistic Regression - NEEDS WORK----------------
install.packages("MASS")
library(MASS)
playFit.lr <- glm(Play.Type ~ Period.Number + Clock +  Offense.Points + Defense.Points +
                    Down + Distance + Spot + Drive.Number + Interceptions +
                    Fumbles + Point.Differential + DownXDistance + Previous.Play.Type + Year,
                  family = binomial(logit),data = train)
test$Play.Prob <- predict(playFit.lr, test, type="response")
#predictions <- apply(probabilities, 1, which.max)
test$Pred.Play.Type <- ifelse(test$Play.Prob>.50, "RUSH", "PASS")
table(test$Pred.Play.Type, test$Play.Type)

#K Nearest Neighbors - NEEDS WORK----------------
library(class)
playFit.knn <- knn(train=train_n, test=test_n, cl=train_n[,10], k=101)
table(test_n$Play.Type, playFit.knn)

#DBSCAN Clustering - NEEDS WORK--------------
install.packages("fpc")
library(fpc)
DBSCAN.train_n <- train_n[-c(10,11)]
DBSCAN.test_n <- test_n[-c(10,11)]
playFit.DBSCAN <- dbscan(DBSCAN.train_n, eps=0.42, MinPts=5)
table(playFit.DBSCAN$cluster, train_n$Play.Type)
test_n$pred.Play.Type <- predict(playFit.DBSCAN, DBSCAN.train_n, DBSCAN.test_n)
table(playFit.DBSCAN$pred.Play.Type, test_n$Play.Type)

getTree(playFit.rf,k=1,labelVar=TRUE)

str(train)
str(test)
