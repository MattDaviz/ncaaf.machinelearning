#Set libraries ----------
library(readr); library(dplyr); library(randomForest)

#Set working directory----------------
setwd(".\\data")

#Load Data-----------------
# Get the files names
files <-  list.files(pattern="*.csv")

# First apply read.csv, then rbind
data <- do.call("rbind", lapply(files, function(x) read_csv(x)))

# Create new data frame for analysis
colnames(data) <- make.names(colnames(data), unique=TRUE)

#Clean data--------------------------------
#Change Play Type to factor and remove all except run/pass and grab only teams coached by Brian Kelly
bk.df <- data %>% 
  mutate(Play.Type = as.factor(Play.Type),
         Previous.Play.Type = as.factor(Previous.Play.Type)) %>% 
  filter(Play.Type %in% c('PASS', 'RUSH')) %>% 
  select(Period.Number:Drive.Number, Interceptions:Year) %>% 
  filter(Offense.Team.Code == '129' & Year %in% c(2005, 2006) |
           Offense.Team.Code == '140' & Year %in% c(2007, 2008, 2009) |
           Offense.Team.Code == '513' & Year %in% c(2010, 2011, 2012, 2014))

test <- bk.df %>% 
  filter(Year >= 2014)

train <- bk.df %>% 
  filter(Year <=)

train$Play.Type <- factor(train$Play.Type) 
test$Play.Type <- factor(test$Play.Type)

#Random Forest--------------------
myNtree = 501
myMtry = 4
myImportance = TRUE
set.seed(415)
#Model set up and run
bk.model <- randomForest(Play.Type ~ Period.Number + Clock + Offense.Team.Code +
                             Defense.Team.Code + Offense.Points + Defense.Points +
                             Down + Distance + Spot + Drive.Number + Interceptions +
                             Fumbles + Point.Differential + DownXDistance + Previous.Play.Type + Year, 
                         data = train, ntree = myNtree, mtry = myMtry, importance = myImportance)

#Plot error
plot(bk.model, log="y")
save(bk.model, file = "BrianKelly.RData")

#Plot variable importance
varImpPlot(bk.model)

#Create predictions in test tables
test$pred.Play.Type <- predict(bk.model, test)
bk.model.results <- data.frame(test$Play.Type, test$pred.Play.Type)

# Analyze accuracy
bk.model.results %>% 
  mutate(correct = ifelse(test.Play.Type == test.pred.Play.Type, 1, 0)) %>% 
  summarize(accuracy = sum(correct, na.rm = TRUE) / n())