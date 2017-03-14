#Set the working directory
setwd("C:\\Users\\mattd\\Dropbox\\FFL\\CFB\\CFB Play-by-Play Database\\play")

# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
#Change Play Type to factor
myfiles$Play.Type <- as.factor(myfiles$Play.Type)
#Only save "RUSH" and "PASS"
myfiles <- myfiles[!(myfiles$Play.Type %in% c("ATTEMPT", "FIELD_GOAL", "KICKOFF", "PENALTY", "PUNT", "TIMEOUT")),]
#Deselect the time column
myfiles <- myfiles[,c(1:11)]
#Omit all NAs
myfiles <- na.omit(myfiles)

#Write/Read files into/out of R
myfiles <- read.csv("05-13 Play.csv")
#write.csv(myfiles, file = "05-13 Play.csv")

#Look at data characteristics
head(myfiles, 10)
str(myfiles)
head(myfiles_n)
str(myfiles_n)

#Randomize order
set.seed(1000000)
gp <- runif(nrow(myfiles))
myfiles <- myfiles[order(gp),]

#Normalize integer data
normalize <- function(x) {
  return((x-min(x))/(max(x) - min(x)))} 
myfiles_n <- as.data.frame(lapply(myfiles[,c(1,2,3,4,5,6,7,8,9,11)], normalize))

#Set train/test subsets of data
myfiles_train <- myfiles_n[1:985895,]
myfiles_test <- myfiles_n[985896:995895,]
myfiles_train_target <- myfiles[1:985895,10]
myfiles_test_target <- myfiles[985896:995895,10]

#Used to find k from max(rows)
sqrt(995895)

#Set model parameters
library(class)
model1 <- knn(train=myfiles_train, test=myfiles_test, cl=myfiles_train_target, k=499)
model1

#Compare model to real data
table(myfiles_test_target, model1)
