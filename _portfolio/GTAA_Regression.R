setwd("/Users/wsweet/Documents/CSE6242")
library(Metrics)
library(MASS)
library(dplyr)
library(leaps)
library(caret)

data=read.csv("StubHub2015-2017.csv", header = TRUE)
opponents=read.csv("W-Lthrough2017_clemsonrival.csv", header = TRUE)
###capacity=read.csv("seat_capacity.csv", header = TRUE)

### Expand Seat Block into Section, Row and Seat
data = data.frame(data)
data$FirstSeatBlock=as.character(data$FirstSeatBlock)
data.split=strsplit(data$FirstSeatBlock, split=":")
tmp=do.call(rbind, data.split)
data=data.frame(data, tmp)
colnames(data)[43:46]=c("Number", "Section", "Row", "Seat(s)")

rm("tmp", "data.split")

### Merge Opponents with Stubhub data
merged_data <- merge(x=data,y=opponents[,c(3,5,8:18,20,21,24)],
                     by = c("EventCode"), all.x = TRUE)

attach(merged_data)
rm("data", "opponents")

### Partition the 2013- 2017 data randomly
set.seed(33)
split<-createDataPartition(y = log(merged_data$ResaleValue.Ticket), p = 0.7, list = FALSE)
training_set=merged_data[split,]
test_set=merged_data[-split,]




### Initial Model with all variables, no interaction
initial_model=lm(ResaleValue.Ticket~Section+Day+Time+Prior.Year.Wins+Conference+
                   Rival+GTPriorYearWins+OppTwitterFollowers+MilesToATL+UndergradPop+
                   GameMonth+FalconsHome+NatlChampBinary+Tgiving_or_labor+
                   GeorgiaBinary, data=training_set)
summary(initial_model)

plot(initial_model$residuals)
qqnorm(initial_model$residuals)
qqline(initial_model$residuals)


ctrl=trainControl(method="repeatedcv", number=10, 3)
initial_model_train=train(ResaleValue.Ticket~Section+Day+Time+Prior.Year.Wins+Conference+
                            Rival+GTPriorYearWins+OppTwitterFollowers+MilesToATL+UndergradPop+
                            GameMonth+FalconsHome+NatlChampBinary+Tgiving_or_labor+
                            GeorgiaBinary, data=training_set, method="lm", 
                            trControl=ctrl, metric="Rsquared")
summary(initial_model_train)

test_set=test_set[!test_set$Section=="TT1",]

test_set$prediction=predict(initial_model_train, test_set)
initial_model_mse=mse(test_set$ResaleValue.Ticket, test_set$prediction)
initial_model_rmse=rmse(test_set$ResaleValue.Ticket, test_set$prediction)
initial_model_mae=mae(test_set$ResaleValue.Ticket, test_set$prediction)

varImp(initial_model_train)

initial_model_train$results
initial_model_train$resample
rm("initial_model")

### Transform Response Variable to meet normality assumption
initial_model_tr=lm(log(ResaleValue.Ticket)~Section+Day+Time+Prior.Year.Wins+Conference+
                      Rival+GTPriorYearWins+OppTwitterFollowers+MilesToATL+UndergradPop+
                      GameMonth+FalconsHome+NatlChampBinary+Tgiving_or_labor+
                      GeorgiaBinary, data=training_set)
summary(initial_model_tr)

plot(initial_model_tr$residuals)
qqnorm(initial_model_tr$residuals)
qqline(initial_model_tr$residuals)

ctrl=trainControl(method="repeatedcv", number=10, 3)
initial_model_tr_train=train(log(ResaleValue.Ticket)~Section+Day+Time+Prior.Year.Wins+Conference+
                            Rival+GTPriorYearWins+OppTwitterFollowers+MilesToATL+UndergradPop+
                            GameMonth+FalconsHome+NatlChampBinary+Tgiving_or_labor+
                            GeorgiaBinary, data=training_set, method="lm", 
                          trControl=ctrl, metric="Rsquared")
summary(initial_model_tr_train)

test_set$prediction=exp(predict(initial_model_tr_train, test_set))
initial_model_tr_mse=mse(test_set$ResaleValue.Ticket, test_set$prediction)
initial_model_tr_rmse=rmse(test_set$ResaleValue.Ticket, test_set$prediction)
initial_model_tr_mae=mae(test_set$ResaleValue.Ticket, test_set$prediction)

varImp(initial_model_tr_train)

initial_model_tr_train$results
initial_model_tr_train$resample
rm("initial_model_tr")

### Create interaction term for all variables and Conference
full_model=lm(log(ResaleValue.Ticket)~Section+Day+Time+Prior.Year.Wins+Conference
              +Rival+GTPriorYearWins+OppTwitterFollowers+MilesToATL+UndergradPop+
                GameMonth+FalconsHome+NatlChampBinary+Tgiving_or_labor+
                GeorgiaBinary+Conference*(Section+Day+Time+Prior.Year.Wins+Conference+
                Rival+GTPriorYearWins+OppTwitterFollowers+MilesToATL+UndergradPop+
                GameMonth+FalconsHome+NatlChampBinary+Tgiving_or_labor+GeorgiaBinary), data=training_set)
summary(full_model)

null_model=lm(log(ResaleValue.Ticket)~1, data=training_set)
summary(null_model)

ctrl=trainControl(method="repeatedcv", number=10, 3)
full_model_train=train(log(ResaleValue.Ticket)~Section+Day+Time+Prior.Year.Wins+Conference
                       +Rival+GTPriorYearWins+OppTwitterFollowers+MilesToATL+UndergradPop+
                         GameMonth+FalconsHome+NatlChampBinary+Tgiving_or_labor+
                         GeorgiaBinary+Conference*(Section+Day+Time+Prior.Year.Wins+Conference+
                         Rival+GTPriorYearWins+OppTwitterFollowers+MilesToATL+UndergradPop+
                         GameMonth+FalconsHome+NatlChampBinary+Tgiving_or_labor+
                         GeorgiaBinary), data=training_set, method="lm", 
                       trControl=ctrl, metric="Rsquared")
summary(full_model_train)

test_set$prediction=exp(predict(full_model_train, test_set))
full_model_mse=mse(test_set$ResaleValue.Ticket, test_set$prediction)
full_model_rmse=rmse(test_set$ResaleValue.Ticket, test_set$prediction)
full_model_mae=mae(test_set$ResaleValue.Ticket, test_set$prediction)

varImp(full_model_train)

full_model_train$results
full_model_train$resample


### Perform Forward Selection
#step(null_model, scope=list(lower=null_model, upper=full_model_train), direction="forward")

forward_model=lm(log(ResaleValue.Ticket) ~ NatlChampBinary + Section + 
                   Conference + UndergradPop + GameMonth + Day + GTPriorYearWins + 
                   MilesToATL + Tgiving_or_labor + Time + FalconsHome + OppTwitterFollowers + 
                   Rival + Prior.Year.Wins + Conference:GTPriorYearWins + Section:Conference + 
                   Conference:GameMonth + Conference:Time + Conference:Prior.Year.Wins, data = training_set)
summary(forward_model)

ctrl=trainControl(method="repeatedcv", number=10, 3)
forward_model_train=train(log(ResaleValue.Ticket) ~ NatlChampBinary + Section + 
                            Conference + UndergradPop + GameMonth + Day + GTPriorYearWins + 
                            MilesToATL + Tgiving_or_labor + Time + FalconsHome + OppTwitterFollowers + 
                            Rival + Prior.Year.Wins + Conference:GTPriorYearWins + Section:Conference + 
                            Conference:GameMonth + Conference:Time + Conference:Prior.Year.Wins, data=training_set, method="lm", 
                          trControl=ctrl, metric="Rsquared")
summary(forward_model_train)

test_set$prediction=exp(predict(forward_model_train, test_set))
forward_model_mse=mse(test_set$ResaleValue.Ticket, test_set$prediction)
forward_model_rmse=rmse(test_set$ResaleValue.Ticket, test_set$prediction)
forward_model_mae=mae(test_set$ResaleValue.Ticket, test_set$prediction)

varImp(forward_model_train)

forward_model_train$results$Rsquared
forward_model_train$resample



### Perform Backward Selection
#step(full_model, data=training_set, direction = "backward")

backward_model=lm(log(ResaleValue.Ticket) ~ Section + Day + Time + 
                    Prior.Year.Wins + Conference + Rival + GTPriorYearWins + 
                    OppTwitterFollowers + MilesToATL + UndergradPop + GameMonth + 
                    FalconsHome + NatlChampBinary + Section:Conference + Time:Conference + 
                    Prior.Year.Wins:Conference + Conference:GTPriorYearWins, data = training_set)
summary(backward_model)

ctrl=trainControl(method="repeatedcv", number=10, 3)
backward_model_train=train(log(ResaleValue.Ticket) ~ Section + Day + Time + 
                             Prior.Year.Wins + Conference + Rival + GTPriorYearWins + 
                             OppTwitterFollowers + MilesToATL + UndergradPop + GameMonth + 
                             FalconsHome + NatlChampBinary + Section:Conference + Time:Conference + 
                             Prior.Year.Wins:Conference + Conference:GTPriorYearWins, data=training_set, method="lm", 
                          trControl=ctrl, metric="Rsquared")
summary(backward_model_train)

test_set$prediction=exp(predict(backward_model_train, test_set))
backward_model_mse=mse(test_set$ResaleValue.Ticket, test_set$prediction)
backward_model_rmse=rmse(test_set$ResaleValue.Ticket, test_set$prediction)
backward_model_mae=mae(test_set$ResaleValue.Ticket, test_set$prediction)

varImp(backward_model_train)

backward_model_train$results
backward_model_train$resample

### Intuition Model
model=lm(log(ResaleValue.Ticket)~Day+Time:Conference+Prior.Year.Wins:Conference+
           Rival+Section+GTPriorYearWins+GeorgiaBinary+OppTwitterFollowers+
           NatlChampBinary+MilesToATL, data=training_set)
summary(model)

ctrl=trainControl(method="repeatedcv", number=10, 3)
model_train=train(log(ResaleValue.Ticket)~Day+Time:Conference+Prior.Year.Wins:Conference
              +Rival+Section+GTPriorYearWins+GeorgiaBinary+OppTwitterFollowers+NatlChampBinary
              +MilesToATL, data=training_set, method="lm", trControl=ctrl, metric="Rsquared")
summary(model_train)

test_set$prediction=exp(predict(model_train, test_set))
model_mse=mse(test_set$ResaleValue.Ticket, test_set$prediction)
model_rmse=rmse(test_set$ResaleValue.Ticket, test_set$prediction)
model_mae=mae(test_set$ResaleValue.Ticket, test_set$prediction)

varImp(model_train)

### Accuracy Summary
Models=c('initial_model', 'initial_model_tr', 'full_model', 'forward_model', 'backward_model', 'model')
R_Squared=c(initial_model_train$results$Rsquared, initial_model_tr_train$results$Rsquared, full_model_train$results$Rsquared, forward_model_train$results$Rsquared, backward_model_train$results$Rsquared, model_train$results$Rsquared)
MSE=c(initial_model_mse, initial_model_tr_mse, full_model_mse, forward_model_mse, backward_model_mse, model_mse)
RMSE=c(initial_model_rmse, initial_model_tr_rmse, full_model_rmse, forward_model_rmse, backward_model_rmse, model_rmse)
MAE=c(initial_model_mae, initial_model_tr_mae, full_model_mae, forward_model_mae, backward_model_mae, model_mae)
Model_Overview=data.frame(Models, R_Squared, MSE, RMSE, MAE)


rm("backward_model", "forward_model", "full_model", "model")
rm("backward_model_train", "forward_model_train", "full_model_train", "initial_model_train", "model_train")
rm("initial_model_tr", "initial_model_tr_train", "null_model", "ctrl", "training_set", "test_set")



### Predict Selected model on Entire Dataset
forward_model=lm(log(ResaleValue.Ticket) ~ NatlChampBinary + Section + 
                   Conference + UndergradPop + GameMonth + Day + GTPriorYearWins + 
                   MilesToATL + Tgiving_or_labor + Time + FalconsHome + OppTwitterFollowers + 
                   Rival + Prior.Year.Wins + Conference:GTPriorYearWins + Section:Conference + 
                   Conference:GameMonth + Conference:Time + Conference:Prior.Year.Wins, data = merged_data)
summary(forward_model)

pred.interval=predict(forward_model, merged_data, interval = "prediction", level=.9)
merged_data$prediction=exp(pred.interval[,1])
merged_data$lower_bound=exp(pred.interval[,2])
merged_data$upper_bound=exp(pred.interval[,3])

summary_by_section <- merged_data %>%
  dplyr::select(EventCode, Opponent, Day, Time, Section, Conference, prediction, ResaleValue.Ticket, lower_bound, upper_bound) %>%
  group_by(EventCode, Opponent, Day, Time, Conference, Section) %>%
  summarise(prediction = mean(prediction), ResaleValue.Ticket=mean(ResaleValue.Ticket), lower_bound=mean(lower_bound), upper_bound=mean(upper_bound))


### Use Selected Model to make predictions on 2018 Game Data
prediction=read.csv("2018predictions.csv", header = TRUE)

pred.interval=predict(forward_model, prediction, interval = "prediction", level=.9)
prediction$prediction=exp(pred.interval[,1])
prediction$lower_bound=exp(pred.interval[,2])
prediction$upper_bound=exp(pred.interval[,3])

export=prediction[c(1,3:5,18:20)]

export=write.csv(export, file="export.csv")
