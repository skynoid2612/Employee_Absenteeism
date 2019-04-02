###################Setting directory and files###################
setwd('D:/Data Science/EDWISOR/2_PORTFOLIO/project 1')
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','readxl')
lapply(x, require, character.only = TRUE)
absent_data = read_xls('Absenteeism_at_work_Project.xls',sheet = 1,col_names = TRUE)
anyNA(absent_data)
absent_data= as.data.frame(absent_data)
col_names = colnames(absent_data)
categorical_col = c('Reason for absence', 'Month of absence', 'Day of the week',
                    'Seasons', 'Disciplinary failure', 'Education', 'Social drinker',
                    'Social smoker')

###################KNN imputation###################
knn_process_data=knnImputation(absent_data,k = 3)
knn_process_data=as.data.frame(knn_process_data)
#Created an knn_process_data by applying KNN imputation algorithm

for(i in colnames(knn_process_data)){
  knn_process_data[,i]=as.integer(knn_process_data[,i])
}

for (i in categorical_col){
  knn_process_data[,i]=as.factor(knn_process_data[,i])
}
###################Feature Selection###################
#corrgram(knn_process_data[,numeric_index],order = FALSE,
#         upper.panel = panel.pie,text.panel = panel.txt,main= 'Correlation Plot')

knn_process_data = subset(knn_process_data,select = -c(Weight,Height))
knn_process_data= as.data.frame(knn_process_data)


###################Featrue Scaling###################

scaling_col = c("Transportation expense", "Distance from Residence to Work","Service time",
                "Work load Average/day" ,"Hit target","Body mass index")
for (i in scaling_col){
  print(i)
  knn_process_data[,i]= (knn_process_data[,i]-min(knn_process_data[,i]))/(max(knn_process_data[,i])-min(knn_process_data[,i]))
}

###################Modeling###################
rmExcept(c("knn_process_data","absent_data"))
set.seed(121)
train_index=createDataPartition(knn_process_data$`Absenteeism time in hours`,p = 0.9,list = FALSE)
train=knn_process_data[train_index,]
test=knn_process_data[-train_index,]
train$`Absenteeism time in hours`=as.factor(train$`Absenteeism time in hours`)
test$`Absenteeism time in hours`=as.factor(test$`Absenteeism time in hours`)


#KNN Algo
library(class)
train$`Absenteeism time in hours`=as.numeric(train$`Absenteeism time in hours`)
knn_model = knnreg(train[,-19],train$`Absenteeism time in hours`,k = 3)
predict_knn =predict(knn_model,test[,-19])
measureMAE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_knn))
measureRSQ(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_knn))
measureRMSE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_knn))
summary(knn_model)
measureMSE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_knn))


#Linear Regession
col = colnames(train)
pre = col[19]
col=col[-19]
train$`Absenteeism time in hours`=as.numeric(train$`Absenteeism time in hours`)
linreg =  lm(`Absenteeism time in hours`~ .,data = train)
predict_linreg = predict(linreg,test[,1:18])

measureMAE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_linreg))
measureMSE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_linreg))
measureRMSE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_linreg))


#Decision Tree
Dec_model = rpart(`Absenteeism time in hours`~ .,data = train)
summary(Dec_model)
predict_Dec = predict(Dec_model,test[,-19])
measureMAE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_Dec))
measureMSE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_Dec))
measureRMSE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_Dec))


#Random Forest
rf_model = randomForest(x=train[,-19],y=train$`Absenteeism time in hours`,importance = TRUE,ntree = 500)
summary(rf_model)
predict_rf = predict(rf_model,test[,-19])
RMSE(pred = as.numeric(predict_rf), obs = as.numeric(test$`Absenteeism time in hours`))
importance(rf_model,type = 1)
measureMAE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_rf))
measureRSQ(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_rf))
measureRMSE(as.numeric(test$`Absenteeism time in hours`),as.numeric(predict_rf))


