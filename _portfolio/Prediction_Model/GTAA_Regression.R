library(Metrics)
library(MASS)
library(dplyr)
library(leaps)
library(caret)
library(car)

### Reads in StubHub data
data <- read.csv("import_data.csv",header = TRUE)

### Reads in Game data from 2013-2017 and information about opponent
opponents <- read.csv("W-Lthrough2017_final.csv", header = TRUE)

### Merge Opponents with Stubhub data
merged_data <- merge(x=data,y=opponents[,c(3,5,8:17,19,20,24)],
                     by = c("EventCode"), all.x = TRUE)

### Remove Tech Terrace Sections as GTTA does not need those in this analysis
merged_data <- merged_data[!merged_data$Section %in% c("TT1","TT2","TT3","LWO"),]
merged_data$Section <- as.factor(as.character(merged_data$Section))

rm("data", "opponents")

### Model with all considered variables
model <- lm(ResaleValue.Ticket ~Section + Day_and_Time + Prior.Year.Wins +
              Conference + Rival + GTPriorYearWins + OppTwitterFollowers + MilesToATL + 
              UndergradPop + GameMonth + NatlChampBinary + Tgiving_or_labor + PriorYearACCChamp,  data = merged_data)
summary(model)

### Check Residuals for normality
plot(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)

### Residuals do not show normality, data needs to be transformed
transformed_model <- lm(log(ResaleValue.Ticket) ~Section + Day_and_Time + Prior.Year.Wins +
                          Conference + Rival + GTPriorYearWins + OppTwitterFollowers + MilesToATL + 
                          UndergradPop + GameMonth + NatlChampBinary + Tgiving_or_labor + PriorYearACCChamp,  data=merged_data)
summary(transformed_model)
vif(transformed_model)

### Very high multicollinearity. Remove GameMonth, Conference, Tgiving_or_labor, and Rival
model <- lm(log(ResaleValue.Ticket) ~ Section + Day_and_Time + Prior.Year.Wins +
                                        GTPriorYearWins + OppTwitterFollowers + MilesToATL + 
                                        UndergradPop + NatlChampBinary + PriorYearACCChamp,  data=merged_data)
summary(model)
vif(model)

### Less multicollinearity
### Check and Remove Outliers
cook <- cooks.distance(model)
plot(cook, type="h", lwd=3, col="red", ylab="Cook's Distance")
bool_dis <- cook > 4/length(cook)
bool_dis <- cook > .002
outliers <- cook[bool_dis]
merged_data <- merged_data[!bool_dis,]

rm("transformed_model", "model", "cook", "outliers", "bool_dis")

### We now have our final data set to train and test on
### Partition the 2013- 2017 data randomly
set.seed(33)
split <- createDataPartition(y = log(merged_data$ResaleValue.Ticket), p = 0.7, list = FALSE)
training_set <- merged_data[split,]
test_set <- merged_data[-split,]

### Initial Model with all remaining variables on transformed response
initial_model <- lm(log(ResaleValue.Ticket) ~ Section + Day_and_Time + Prior.Year.Wins +
                      GTPriorYearWins + OppTwitterFollowers + MilesToATL + 
                      UndergradPop + NatlChampBinary + PriorYearACCChamp, data = training_set)
summary(initial_model)

plot(initial_model$residuals)
qqnorm(initial_model$residuals)
qqline(initial_model$residuals)

#Cross Validation on initial model
ctrl <- trainControl(method="repeatedcv", number=10, 3)
initial_model_train <- train(log(ResaleValue.Ticket) ~ Section + Day_and_Time + Prior.Year.Wins +
                               GTPriorYearWins + OppTwitterFollowers + MilesToATL + 
                               UndergradPop + NatlChampBinary + PriorYearACCChamp,
                               data=training_set, method="lm", trControl=ctrl, metric="Rsquared")
summary(initial_model_train)

test_set$prediction <- exp(predict(initial_model_train, test_set))
initial_model_train_mse <- mse(test_set$ResaleValue.Ticket, test_set$prediction)
initial_model_train_rmse <- rmse(test_set$ResaleValue.Ticket, test_set$prediction)
initial_model_train_mae <- mae(test_set$ResaleValue.Ticket, test_set$prediction)
initial_model_train_rsquared <- initial_model_train$results$Rsquared

### Perform Forward Selection- removes OppTwitterFollowers
null_model <- lm(log(ResaleValue.Ticket)~1, data=training_set)

forward_model <- step(null_model, scope=list(lower=null_model, upper=initial_model), direction="forward", data = training_set)
summary(forward_model)

### Cross Validation on forward model
forward_model_train <- train(log(ResaleValue.Ticket) ~ Section + Day_and_Time + Prior.Year.Wins +
                                GTPriorYearWins + MilesToATL + UndergradPop + NatlChampBinary + PriorYearACCChamp,
                                data = training_set, method="lm", trControl=ctrl, metric="Rsquared")
summary(forward_model_train)

test_set$prediction2 <- exp(predict(forward_model_train, test_set))
forward_model_train_mse <- mse(test_set$ResaleValue.Ticket, test_set$prediction2)
forward_model_train_rmse <- rmse(test_set$ResaleValue.Ticket, test_set$prediction2)
forward_model_train_mae <- mae(test_set$ResaleValue.Ticket, test_set$prediction2)
forward_model_train_rsquared <- forward_model_train$results$Rsquared

### Perform Backward Selection - Outputs same model as forward selection so no need to continue
backward_model <- step(initial_model, data=training_set, direction = "backward")
summary(backward_model)

### Checks variable importance of forward_model_train, finds NatlChampBinary to be most important
varImp(forward_model_train)

### Create model with interaction term for all variables from initial model and NatlChampBinary
model_interactions <- lm(log(ResaleValue.Ticket) ~ Section + Day_and_Time + Prior.Year.Wins + 
                                GTPriorYearWins + OppTwitterFollowers + MilesToATL + UndergradPop + 
                                NatlChampBinary + PriorYearACCChamp + 
                                NatlChampBinary*(Section + Day_and_Time + Prior.Year.Wins +
                                GTPriorYearWins + OppTwitterFollowers + MilesToATL + UndergradPop + 
                                PriorYearACCChamp), data=training_set)
summary(model_interactions)

#Cross validation on Interactions model
model_interactions_train <- train(log(ResaleValue.Ticket) ~ Section + Day_and_Time + Prior.Year.Wins + 
                                         GTPriorYearWins + OppTwitterFollowers + MilesToATL + UndergradPop + 
                                         NatlChampBinary + PriorYearACCChamp + 
                                         NatlChampBinary*(Section + Day_and_Time + Prior.Year.Wins +
                                         GTPriorYearWins + OppTwitterFollowers + MilesToATL + UndergradPop + 
                                         PriorYearACCChamp), data=training_set, method="lm", 
                                         trControl=ctrl, metric="Rsquared")
summary(model_interactions_train)

test_set$prediction3 <- exp(predict(model_interactions_train, test_set))
model_interactions_mse <- mse(test_set$ResaleValue.Ticket, test_set$prediction3)
model_interactions_rmse <- rmse(test_set$ResaleValue.Ticket, test_set$prediction3)
model_interactions_mae <- mae(test_set$ResaleValue.Ticket, test_set$prediction3)
model_interactions_rsquared <- model_interactions_train$results$Rsquared

### Perform Forward Selection on the interactions - kept all variables, nothing further needed to be done
forward_model_interactions <- step(null_model, scope=list(lower = null_model, upper = model_interactions_train), direction = "forward")
summary(forward_model_interactions)

### Perform Backward Selection - removes many variables
backward_model_interactions <- step(full_model_interactions, data = training_set, direction = "backward")
summary(backward_model_interactions)

### Cross Validation on selected Backward model
backward_model_interactions_train <- train(log(ResaleValue.Ticket) ~ Section + Day_and_Time + Prior.Year.Wins + 
                                             GTPriorYearWins + OppTwitterFollowers + 
                                             NatlChampBinary, data = training_set, method="lm", 
                                             trControl=ctrl, metric="Rsquared")
summary(backward_model_interactions_train)

test_set$prediction4 <- exp(predict(backward_model_interactions_train, test_set))
backward_model_interactions_mse <- mse(test_set$ResaleValue.Ticket, test_set$prediction4)
backward_model_interactions_rmse <- rmse(test_set$ResaleValue.Ticket, test_set$prediction4)
backward_model_interactions_mae <- mae(test_set$ResaleValue.Ticket, test_set$prediction4)
backward_model_interactions_rsquared <- backward_model_interactions_train$results$Rsquared

rm('null_model','ctrl')
rm('backward_model', 'backward_model_interactions', 'backward_model_interactions_train')
rm('forward_model', 'forward_model_train', 'forward_model_interactions')
rm('model_interactions', 'model_interactions_train')
rm('initial_model','initial_model_train')

### Accuracy Summary
Models=c('initial_model_train', 'forward_model_train', 'model_interactions', 'backward_model_interactions')
R_Squared=c(initial_model_train_rsquared, forward_model_train_rsquared, model_interactions_rsquared, backward_model_interactions_rsquared)
MSE=c(initial_model_train_mse, forward_model_train_mse, model_interactions_mse, backward_model_interactions_mse)
RMSE=c(initial_model_train_rmse, forward_model_train_rmse, model_interactions_rmse, backward_model_interactions_rmse)
MAE=c(initial_model_train_mae, forward_model_train_mae, model_interactions_mae, backward_model_interactions_mae)
Model_Overview=data.frame(Models, R_Squared, MSE, RMSE, MAE)

### Predict our selected model (initial model) on entire dataset
final_model <- lm(log(ResaleValue.Ticket) ~ Section + Day_and_Time + Prior.Year.Wins +
                    GTPriorYearWins + OppTwitterFollowers + MilesToATL + 
                    UndergradPop + NatlChampBinary + PriorYearACCChamp, data = merged_data)
summary(final_model)

pred.interval <- predict(final_model, merged_data, interval = "prediction", level=.9)
merged_data$prediction <- exp(pred.interval[,1])
merged_data$lower_bound <- exp(pred.interval[,2])
merged_data$upper_bound <- exp(pred.interval[,3])

### Summarize how predictions fit to merged_data (the training set in this case)
summary_by_section <- merged_data %>%
  dplyr::select(EventCode, Opponent, Day_and_Time, Section, Conference, prediction, ResaleValue.Ticket, lower_bound, upper_bound) %>%
  group_by(EventCode, Opponent, Day_and_Time, Conference, Section) %>%
  summarise(prediction = mean(prediction), ResaleValue.Ticket=mean(ResaleValue.Ticket), lower_bound=mean(lower_bound), upper_bound=mean(upper_bound))

### Percent of time outside Prediction Interval
mean((summary_by_section$ResaleValue.Ticket > summary_by_section$upper_bound) | 
       (summary_by_section$ResaleValue.Ticket < summary_by_section$lower_bound))

### Use Selected Model to make predictions on 2018 Game Data
prediction <- read.csv("2018predictions.csv", header = TRUE)

pred.interval <- predict(final_model, prediction, interval = "prediction", level=.9)
prediction$Wtp <- exp(pred.interval[,1])
prediction$lower_bound <- exp(pred.interval[,2])
prediction$upper_bound <- exp(pred.interval[,3])
prediction$Net <- prediction$Wtp-prediction$Face

### Final table to export as csv
export <- prediction[c(1,3:4,17:21)]
write.csv(export, file="export.csv")
write.csv(Model_Overview, "Model_Overview.csv")
