#*******Prediction Model for Churn in voice phone Customers*****

# Step 1 : Source in the file as CSV
source_data <- read.csv("C:/Users/SARANG/Desktop/Churn.csv")
source_data
head(source_data)

# Step 2 : Get the idea of how data is structured
str(source_data)

# Step 3 : Visualize the current distribution of the dependent variable
churn_yes <- source_data[source_data$Churn == 1,]
nrow(churn_yes)

churn_no <- source_data[source_data$Churn == 0,]
nrow(churn_no)


text(barplot(table(source_data$Churn),main = 'Bar plot of Churn variable in source data'),0,
     table(source_data$Churn),cex = 1,pos = 3)

# Step 4 : Check for any correlated variables
library(corrplot)
cor <- cor(source_data[sapply(source_data,is.numeric)])
corrplot(cor)

# Step 5 : We remove the highly correlated variables and the other categorical variables..
# ..which don't contribute to behaviour of dependent variable

source_data$Account.Length <-NULL
source_data$VMail.Message <-NULL
source_data$Day.Mins <-NULL
source_data$Eve.Mins <-NULL
source_data$Night.Mins <-NULL
source_data$Intl.Mins <-NULL
source_data$CustServ.Calls <-NULL
source_data$Int.l.Plan <-NULL
source_data$Day.Calls <-NULL
source_data$Eve.Calls <-NULL
source_data$Night.Calls <-NULL
source_data$Intl.Calls <-NULL

source_data$Area.Code <-NULL
source_data$State <-NULL
source_data$Phone <-NULL

# Step 6 : Split the source data into 80:20 ratio as mentioned in the assignment
# To ensure a uniform start point for generating the random nos. we use set.seed function

source_data
require(caTools)
set.seed(123)
partition <- sample.split(source_data,SplitRatio = 0.80)
partition

# Step 7 : Use this partition for alloting the rows to Training and Testing datasets

train <- subset(source_data,partition == TRUE)
train
test <- subset(source_data,partition == FALSE)
test
str(train)

nrow(train)
nrow(test)

# Step 8 : Run the Logistics Regression model over the training set with Churn as the dependent variable

log_model <- glm(source_data$Churn ~.,train,family = "binomial")
log_model
summary(log_model)

# Step 9 : Predict the Churn values by deploying this model ovet the test data

result <- predict (log_model,test,type = "response")
result

# Step 10 : Let's use ROC curve to substantiate the strength of the model
# The curve yields a cut-off of 0.25

install.packages("ROCR")
library(ROCR)

result<-predict(log_model,train,type = "response")
roc_pred <- prediction(result,train$Churn)
roc_perf <- performance(roc_pred,"tpr","fpr")
plot(roc_perf,colorize=TRUE,print.cutoffs.at=seq(0.05,by=0.05))

# Step 11 : Create a confusion matrix

result_test <- predict(log_model,test,type = "response")
table(Observed = test$Churn,Predicted = result_test > 0.25)
accuracy <- (857+73)/(857+75+73+106)
accuracy
