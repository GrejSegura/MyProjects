
#PREDICTING THE FIELD GOALS MADE IN NBA USING XGBOOST

'Basketball is really fun that is why I had my eyes wide opened when I saw this dataset. 
Upon examining the data I almost immediately came up with an idea of predicting the field goal made.
The idea of having a model that predicts such event is really exciting to me.
Add to that, I am very excited of how the renowned xgboost would fare in this type of dataset.
However, I should first state clear that this analysis is just for fun and any commentaries regarding my analysis and codes are welcome.

To start with, let us load the necessary libraries and the dataset.'

rm(list = ls())
library(xgboost)
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(DT)
library(data.table)
library(GGally)


nba_shots <- fread("./dta/nba_shots.csv")

dim(nba_shots)
str(nba_shots)


## DATA ANALAYSIS AND VISUALIZATION

'Before proceeding to building a model, I would like to examine the data by creating charts.
I would like to know a couple of things first to have an idea about the field goals made by these nba players.

1. Compare the shots by Home and Away teams.
2. Who are the best non-post shooters for the first quarter and last quarter of the game? This gives us an idea who are the best to start and end the game.
- In this analysis, non-post shot is defined as a shot 5 feet away from the basket.'


### 1. SHOT COMPARISON FOR HOME AND AWAY TEAMS




home_away <- nba_shots %>% group_by(LOCATION) %>% summarize(PERCENTAGE = (sum(FGM)/length(FGM))*100)

ggplot(data = home_away, aes(x = LOCATION, y = PERCENTAGE, fill = LOCATION)) + geom_bar(stat = 'identity')







'The home team has a slight edge in the field goal percentage. This is expected as home teams usually wins more games as shown below.
'



wins <- nba_shots %>% group_by(GAME_ID, LOCATION) %>% filter(W == 'W', FGM == 1)

ggplot(data = wins, aes(LOCATION, fill = factor(W)) )+ geom_bar()




'Home teams are obviously winning more games than the away team based on the chart above.

## 2. BEST STARTER AND FINISHER - NON POST SHOOTERS (5ft+ away from the basket)

Below are the top 15 players who has the best perimeter shooting field goals for the first quarter.
The data is filtered to a minimum of 150 attempts to eliminate the deceiving numbers of those who attempted far less and has high percentage.'

###FIRST QUARTER HEROES



first_q <- nba_shots %>% filter(PERIOD == 1, SHOT_DIST > 5) %>% group_by(player_name) %>% 
	summarise(made = sum(FGM), 
		  total_attempts = length(FGM), 
		  ave_touch = mean(TOUCH_TIME),
		  ave_distance = mean(SHOT_DIST)) %>%
	mutate(percentage = made/total_attempts) %>%
	arrange(desc(percentage)) %>% filter(total_attempts > 150)

best_1st <- data.frame(first_q)

datatable(best_1st[1:15,])


'Unsurprisingly here, Nowitzki is ranked number 1 among perimeter shooters with 51.7%. 
What is surprising is Vucevic is ranked number 2 even if he usually works in the paint as a center.
However, he has the least average shot distance among all rankers.

Below are the top 15 players who has the best perimeter shooting  field goals for the fourth quarter.
The data is again filtered to a minimum of 150 attempts to eliminate the deceiving numbers of those who attempted far less and has high percentage.'

###FOURTH QUARTER HEROES


fourth_q <- nba_shots %>% filter(PERIOD == 4, SHOT_DIST > 5) %>% group_by(player_name) %>% 
	summarise(made = sum(FGM), 
		  total_attempts = length(FGM), 
		  ave_touch = mean(TOUCH_TIME),
		  ave_distance = mean(SHOT_DIST)) %>%
	mutate(percentage = made/total_attempts) %>%
	arrange(desc(percentage)) %>% 
	filter(total_attempts > 150)

best_4th <- data.frame(fourth_q)

datatable(best_4th[1:15,])

'As for the fourth quarter, what is noticeable is a drop in the percentages of the players. 
Wesley Matthews is ranked first with 45.5% aaccuracy. Top players of the league are also ranked higher.
'
## EXPLORATORY DATA ANALYSIS

### DATA PREPARATION

'First, as a standard procedure, we have to determine the features with NAs. The table below shows the list of features that has NAs in it.
'

colSums(is.na(nba_shots))

'As we can see, only the shot clock has an NA. Thus the next step would be to replace the NAs for this feature.
Logically, we can not replace the shot clock by its mean. The most logical assumption of having an NA to the shot clock is that it could be the last possession of the quarter or the game.
Hence we replace the NAs with 0.


nba_shots$SHOT_CLOCK[is.na(nba_shots$SHOT_CLOCK)] <- 0


Next is we examine the structure of the data to see its different types.
'

str(nba_shots)


'Seeing the result, we can notice that the Game clock is of type factor which should not be the case.
The numbers should be converted to seconds as it is more useful in that form. So we next convert the said feature to seconds with type integer.'


GAME_CLOCK <- strptime(nba_shots$GAME_CLOCK, format = '%M:%S')

clock <- GAME_CLOCK$min * 60 + GAME_CLOCK$sec


'Another thing is that the data has features that are redundant and we can immediately determine that even if you are just an average fan of basketball.
One example is the shot result which is basically just a duplicate information of the field goals made or missed.
In addition, there are also features that to me does not provide any information to the model I am trying to build.
These are the game id,  match up (this is debatable), W (win), player id (both defender and shooter) and final margin.
Hence we will remove these feature in the next step.'


nba_data <- nba_shots[,-c(1,2,4,5,8,14,15,16,19,21)]


'Next is to convert all the features type to its correct form.
Period, pts_type and FGM should all be of type factors. '


names(nba_shots)

# FURTHER EXPLORATORY DATA ANALYSIS

shots_data <- nba_shots[, -c(1,2,3,4,5,8,16,18,19,21)]
shots_data$clock <- clock
names(shots_data)
shots_data$PERIOD <- as.factor(shots_data$PERIOD)
shots_data$PTS_TYPE <- as.factor(shots_data$PTS_TYPE)

sample <- sample(1:nrow(shots_data), 2000)
ggpairs(shots_data[sample, -c(9,11)], aes(color = SHOT_RESULT, alpha = 0.75)) + theme_bw()



# PREDICTIVE MODEL

'In order to create an xgboost model, the data should be one hot coded.
'
nba_shots$clock <- clock
dummy <- dummyVars(~.-1, nba_data)

dummy_nba <- predict(dummy, nba_data)


# join the clock(GAME_CLOCK) to dummy_nba

dummy2 <- cbind(dummy_nba, clock)


'Finally, remove the FGM.1.'


dummy2 <- dummy2[,-19]


'Then divide the data into training and testing sets while also segregating the labels.
'

ind <- 1:nrow(dummy2)

index <- sample(ind, round(nrow(dummy2)*.75))

train_d <- dummy2[index, -18]
test_d <- dummy2[-index,-18]

# create a train and label data

train_l <- dummy2[index, 18]
test_l <- dummy2[-index, 18]

### MODEL TRAINING

'The data is now ready for model training. The following setup needs further optimization as the parameter values are just handpicked.
The booster I used is the gbtree while the objective is softmax.
'
xgb_nba <- xgboost(as.matrix(train_d), train_l, 
booster = 'gbtree',
objective = 'multi:softmax',
num_class = 2,
max.depth = 5,
eta = 0.1,
nthread = 4,
nrounds = 500,
min_child_weight = 1,
subsample = 0.5, 
colsample_bytree = 1, 
num_parallel_tree = 1,
missing = 'NAN',
verbose = 0)


'The training error is approximately 36% which is quite high but still viable for its purpose.
After training the model, we have to check its prediction strength by running it into the test dataset.
A confusion table below is made to determine the accuracy of the prediction in the test data.
'
pred_xgb_nba1 <- predict(xgb_nba, as.matrix(test_d), missing = 'NAN')

pred_xgb_nba1 <- ifelse(pred_xgb_nba1 > 0.5, 1, 0)

confusion1 <- table(as.factor(pred_xgb_nba1), as.factor(test_l))

confusion1


'The table shows there are quite a number of misclassified data. To see the exact percentage of the model accuracy, the below codes are implemented.
'

# compute accuracy

wrong1 <- ifelse(abs(test_l - pred_xgb_nba1) > 0, 1, 0)

accuracy <- 1 - sum(wrong1) / length(as.factor(test_l))

accuracy


'The accuracy is indeed relatively low at 62%. However, this model is still subject to optimization.


###CONCLUSION

The model we came up with this analysis only gives us a 62% accuracy rate. 
This is just an average classifier which is not enough. 
The model however is far from its optimal form as the parameters are just subjectively chosen.
The best model are yet to be determined. One such move to optimize this is to apply the train function in caret package to find the best parameter grid.
'