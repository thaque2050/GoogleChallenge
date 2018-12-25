library(lubridate)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(car)
library(keras)
library(MlBayesOpt)
library(xgboost)
library(caret)
library(rBayesianOptimization)
library(MASS)
library(randomForest)
library(e1071)


#Read Data
read.csv("train.csv",stringsAsFactors = FALSE,nrows = 10)%>%ncol()
read.csv("test.csv",stringsAsFactors = FALSE,nrows = 10)%>%ncol()
dataformat<-c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")
train_data<-read.csv("train.csv",stringsAsFactors = FALSE,colClasses = dataformat)
test_data<-read.csv("test.csv",stringsAsFactors = FALSE,colClasses = dataformat)
dtrain<-train_data
dtrain<-dtrain%>%filter(!is.na(fullVisitorId))


#TRANSAFORM DATA

#Parse JASON Data
tr_device <- paste("[", paste(dtrain$device, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_geoNetwork<- paste("[", paste(dtrain$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_total<- paste("[", paste(dtrain$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_trafficsource<-paste("[",paste(dtrain$trafficSource,collapse = ","),"]")%>%fromJSON(flatten=TRUE)
dtrain<-cbind(dtrain,tr_device,tr_geoNetwork,tr_total,tr_trafficsource)

#Remove not required columns
dtrain$device<-NULL
dtrain$geoNetwork<-NULL
dtrain$totals<-NULL
dtrain$trafficSource<-NULL
dtrain$adwordsClickInfo.gclId<-NULL
dtrain$sessionId<-NULL
dtrain$visitId<-NULL
dtrain$visits<-NULL
dtrain$newVisits<-NULL

#Remove certain strings with NAs
dtrain[dtrain=="not available in demo dataset"]<-NA
dtrain[dtrain=="(not set)"]<-NA
dtrain[dtrain=="(not provided)"]<-NA
dtrain[which(dtrain$networkDomain=="unknown.unknown"),]<-NA

#Replace NAs with appropriate values
dtrain$adwordsClickInfo.page[is.na(dtrain$adwordsClickInfo.page)]<-"0"
dtrain$adwordsClickInfo.slot[is.na(dtrain$adwordsClickInfo.slot)]<-"Others"
dtrain$adwordsClickInfo.adNetworkType[is.na(dtrain$adwordsClickInfo.adNetworkType)]<-"Others"
dtrain$adwordsClickInfo.isVideoAd[is.na(dtrain$adwordsClickInfo.isVideoAd)]<-TRUE
dtrain$isTrueDirect[is.na(dtrain$isTrueDirect)]<-FALSE
dtrain$transactionRevenue[is.na(dtrain$transactionRevenue)]<-0


#Convert variable type
dtrain$date<-as.Date(as.character(dtrain$date),"%Y%m%d")
dtrain$visitStartTime<-as.POSIXct(dtrain$visitStartTime,tz="UTC",origin='1970-01-01')
dtrain$hits<-dtrain$hits%>%as.numeric()
dtrain$bounces<-dtrain$bounces%>%as.numeric()
dtrain$pageviews<-dtrain$pageviews%>%as.numeric()
dtrain$transactionRevenue<-as.numeric(dtrain$transactionRevenue)
dtrain$transactionRevenue<-dtrain$transactionRevenue/1e6

#Convert character variables to factors
#dtrain$adwordsClickInfo.adNetworkType<-dtrain$adwordsClickInfo.adNetworkType%>%as.factor()
#dtrain$adwordsClickInfo.page<-dtrain$adwordsClickInfo.page%>%as.factor()
#dtrain$adwordsClickInfo.slot<-dtrain$adwordsClickInfo.slot%>%as.factor()
#dtrain$adwordsClickInfo.isVideoAd<-dtrain$adwordsClickInfo.isVideoAd%>%as.factor()
#dtrain$channelGrouping<-dtrain$channelGrouping%>%as.factor()
#dtrain$socialEngagementType<-dtrain$socialEngagementType%>%as.factor()
#dtrain$browser<-dtrain$browser%>%as.factor()
#dtrain$operatingSystem<-dtrain$operatingSystem%>%as.factor()
#dtrain$adContent<-dtrain$adContent%>%as.factor()
#dtrain$campaign<-dtrain$campaign%>%as.factor()

#Create new variables
dtrain$start_hour<-hour(dtrain$visitStartTime)
dtrain$month<-month(dtrain$visitStartTime)
dtrain$day<-day(dtrain$visitStartTime)
dtrain$weekday<-weekdays(dtrain$visitStartTime)
dtrain$netdomain<-sub('.*\\.', '', dtrain$networkDomain)
dtrain$networkDomain<-NULL
dtrain$visitStartTime<-NULL
dtrain$date<-NULL

#Remove columns with more than 90% missing entries
full_data<-data.frame(sapply(dtrain,function(x)(round(sum(is.na(x))/length(x)*100,digits=2))))
Variable_Names<-rownames(full_data)
full_data<-cbind(Variable_Names,full_data)
rownames(full_data)<-NULL
colnames(full_data)[2]<-"Percent_Missing"

Include_Variable<-full_data%>%filter(Percent_Missing<100)%>%pull(Variable_Names)%>%as.character()
dtrain<-dtrain%>%select(Include_Variable)



#EXPLORATORY ANALYSIS OF DATA
pdf("plots.pdf")

#Identify variables with no missing entries
full_data<-data.frame(sapply(dtrain,function(x)(round(sum(is.na(x))/length(x)*100,digits=2))))
Variable_Names<-rownames(full_data)
full_data<-cbind(Variable_Names,full_data)
rownames(full_data)<-NULL
colnames(full_data)[2]<-"Percent_Missing"

C1<-full_data%>%filter(Percent_Missing<100)%>%ggplot()+geom_col(aes(x=Variable_Names,y=Percent_Missing))+
  theme(axis.text.x = element_text(angle=90))
C2<-full_data%>%filter(Percent_Missing<100)%>%ggplot()+geom_dotplot(aes(x=Percent_Missing),
                                  color="orange",fill="orange",binwidth = 2,binaxis = "x")

grid.arrange(C1,C2,nrow=2)


#Visits and revenue vs domain (.edu etc.)
dtrain%>%group_by(netdomain)%>%summarise(total_rev=sum(transactionRevenue,na.rm=TRUE),total_count=n())%>%
  arrange(desc(total_rev))%>%top_n(10)%>%ggplot()+geom_col(aes(x=netdomain,y=total_rev))

#Analysis of TransactionRevenue
c1<-dtrain%>%ggplot()+
  geom_histogram(aes(x=log(transactionRevenue),y=..density..),fill="orange",bins = 40,na.rm=TRUE)+
  geom_density(aes(x=log(transactionRevenue)), color="pink",fill="pink",alpha=0.5,na.rm=TRUE)+
  ggtitle("Density plot of log(revenue)")+
  theme(plot.title = element_text(hjust=0.5))

c2<-dtrain%>%ggplot()+geom_histogram(aes(x=log(transactionRevenue)),na.rm=TRUE,bins = 20)+
  stat_bin(aes(x=log(transactionRevenue),label=..count..),geom = "text",bins = 20,na.rm = TRUE,vjust=0)

grid.arrange(c1,c2,nrow=2)


#Log transaction revenue Per Users
c1<-dtrain%>%group_by(fullVisitorId)%>%
  summarise(num_visits=n(),total_rev=sum(transactionRevenue,na.rm=TRUE),num_trans=length(transactionRevenue))%>%
  arrange(desc(total_rev))%>%data.frame()%>%
  ggplot+geom_histogram(aes(x=log(total_rev),y=..density..),bins = 50,na.rm=TRUE,fill="blue")+
  geom_density(aes(x=log(total_rev)),na.rm=TRUE)+
  ggtitle("Log(Total Revenue) by User")+
  theme(plot.title = element_text(hjust=0.5))

c2<-dtrain%>%group_by(fullVisitorId)%>%
  summarise(num_visits=n(),total_rev=sum(transactionRevenue,na.rm=TRUE),num_trans=length(transactionRevenue))%>%
  arrange(desc(total_rev))%>%data.frame()%>%
  ggplot()+geom_histogram(aes(x=num_trans,y=..density..),bins = 50,na.rm=TRUE,fill="blue")+
  geom_density(aes(x=num_trans),na.rm=TRUE)+
  ggtitle("Number of Transaction (rev>0) by User")+
  theme(plot.title = element_text(hjust=0.5))

grid.arrange(c1,c2,nrow=2)


#Analysis of transaction revenue by country and continent
dtrain$country<-as.factor(dtrain$country)
dtrain$continent<-as.factor(dtrain$continent)
country_table<-dtrain%>%group_by(country,continent)%>%
  summarise(total_revenue=sum(transactionRevenue,na.rm=TRUE))%>%arrange(desc(total_revenue))%>%data.frame

country_table[1:5,]%>%ggplot()+
  geom_col(aes(x=continent,y=total_revenue,fill=country))+
  geom_text_repel(aes(x=continent,y=total_revenue,label=round(total_revenue,digits = 2)),size=3)+
  ggtitle("Total Revenue by Continent")+
  theme(plot.title = element_text(hjust=0.5),legend.text = element_text(size=8))

#Plot daily visits and revenue: hourly, daily, weekday, monthly
b1<-dtrain%>%group_by(weekday)%>%summarise(total_rev=mean(transactionRevenue,na.rm=TRUE))%>%
  ggplot()+geom_col(aes(x=weekday,y=total_rev))+ggtitle("Mean Weekday Revenue")

b2<-dtrain%>%group_by(start_hour)%>%summarise(avg_rev=mean(transactionRevenue,na.rm=TRUE))%>%
  ggplot()+geom_col(aes(x=start_hour,y=avg_rev))+
  ggtitle("Mean Hourly Revenue")

b3<-dtrain%>%group_by(day)%>%summarise(avg_rev=mean(transactionRevenue,na.rm=TRUE))%>%
  ggplot()+geom_col(aes(x=day,y=avg_rev))+ggtitle("Mean DateDay Revenue")

b4<-dtrain%>%group_by(month)%>%summarise(avg_rev=mean(transactionRevenue,na.rm=TRUE))%>%
  ggplot()+geom_col(aes(x=month,y=avg_rev))+ggtitle("Mean Monthly Revenue")

grid.arrange(b1,b2,b3,b4,ncol=2)

#Density plot of mobile vs non-mobile revenue
dtrain%>%ggplot()+geom_density(aes(x=log(transactionRevenue),fill=isMobile),na.rm=TRUE,alpha=0.5)+
  scale_fill_manual(values = c('steelblue', 'orange'))
  
#VisitNumber Vs Total and Average Revenue
dtrain%>%group_by(visitNumber)%>%
  summarise(avg_rev=mean(transactionRevenue,na.rm=TRUE))%>%
  arrange(desc(avg_rev))%>%data.frame()%>%ggplot()+geom_col(aes(x=visitNumber,y=avg_rev))

#Transaction revenue grouped by channel
dtrain%>%group_by(channelGrouping)%>%
  summarise(tot_obs=n(),total_rev=sum(transactionRevenue,na.rm=TRUE))%>%arrange(desc(total_rev))%>%data.frame()%>%
  ggplot()+geom_col(aes(x=channelGrouping,y=total_rev),fill="steelblue")+
  geom_text_repel(aes(x=channelGrouping,y=total_rev,label=total_rev), vjust=1, size=3)+
  theme(axis.text.x = element_text(angle=90),legend.position = "none")

#Transaction revenue by device operating system
dtrain%>%group_by(operatingSystem)%>%
  summarise(avg_rev=round(mean(transactionRevenue,na.rm=TRUE),digits=1),total_obs=n())%>%
  arrange(desc(avg_rev))%>%ggplot()+geom_col(aes(x=operatingSystem,y=avg_rev))+
  geom_text_repel(aes(x=operatingSystem,y=avg_rev,label=avg_rev),angle=90, vjust=1,hjust=0.5)+
  theme(legend.position = "None",axis.text.x = element_text(angle=90))

#Revenue by device category
dtrain%>%group_by(deviceCategory)%>%summarise(total_rev=sum(transactionRevenue,na.rm=TRUE))%>%
  arrange(desc(total_rev))%>%data.frame()%>%ggplot()+geom_col(aes(x="",y=total_rev,fill=deviceCategory))+
  geom_text_repel(aes(x="",y=total_rev,label=paste(deviceCategory,"\n",round(total_rev/sum(total_rev)*100,digit=1),"%")),size=3,hjust=1,vjust=0)+
  coord_polar("y")

#Revenue vs medium/traffic source
dtrain%>%group_by(source)%>%summarise(avg_rev=mean(transactionRevenue,na.rm=TRUE))%>%
  ggplot()+geom_col(aes(x=source,y=avg_rev))+theme(axis.text.x = element_text(angle=90))

dev.off()



# BUILD MODEL
dtrain$fullVisitorId<-NULL
train_xgb<-dtrain%>%mutate_if(is.character, as.factor)%>%mutate_if(is.factor, as.integer)
train_xgb$adwordsClickInfo.isVideoAd<-as.integer(train_xgb$adwordsClickInfo.isVideoAd)
train_xgb$isTrueDirect<-as.integer(train_xgb$isTrueDirect)
train_xgb$isMobile<-as.integer(train_xgb$isMobile)
train_xgb<-train_xgb%>%filter(transactionRevenue<10)


set.seed(1234)
ind<-sample(2, nrow(train_xgb), replace=TRUE, prob=c(0.7,0.3))
Training_Data=train_xgb[ind==1,]
Validation_Data=train_xgb[ind==2,]


#XGBOOST
Y_train<-as.matrix(Training_Data$transactionRevenue)
X_train<-as.matrix(Training_Data[,!(names(Training_Data) %in% c("transactionRevenue"))])
train_matrix<-xgb.DMatrix(data=X_train,label=Y_train)


p <- list(objective = "reg:linear",
          booster = "gbtree",
          eval_metric = "rmse",
          nthread = 4,
          eta = 0.05,
          max_depth = 7,
          min_child_weight = 5,
          gamma = 0,
          subsample = 0.8,
          colsample_bytree = 0.7,
          colsample_bylevel = 0.6,
          nrounds = 100)

set.seed(0)
m_xgb <- xgb.train(p, train_matrix, p$nrounds)

Y_validation<-as.matrix(Validation_Data$transactionRevenue)
X_validation<-as.matrix(Validation_Data[,!(names(Validation_Data) %in% c("transactionRevenue"))])
validation_matrix<-xgb.DMatrix(data=X_validation,label=Y_validation)

test_predict<-predict(m_xgb,X_validation)

RMSE(test_predict,Y_validation)




#Hyperparameter Optimization (unchecked)

res0 <- xgb_cv_opt(data = Training_Data,
                   label = transactionRevenue,
                   objectfun = "reg:linear",
                   evalmetric = "rmse",
                   n_folds = 5)



#??

cv_folds <- KFold(Training_Data$transactionRevenue, nfolds = 5,stratified = TRUE, seed = 0)

xgb_cv_bayes <- function(eta,gamma,colsample_bytree,max_delta_step,lambda,alpha,
                         max_depth, min_child_weight, subsample) {
  cv <- xgb.cv(params = list(booster = "gbtree",
                             eta = eta,
                             max_depth = max_depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, 
                             colsample_bytree = colsample_bytree,
                             lambda = lambda,
                             alpha = alpha,
                             gamma=gamma,
                             max_delta_step=max_delta_step,
                             objective = "reg:linear",
                             eval_metric = "rmse"),
               data = train_matrix, nrounds=105,folds = cv_folds, prediction = TRUE, 
               showsd = TRUE,early_stopping_rounds = 5, maximize = TRUE, verbose = 0)
  list(Score = cv$evaluation_log$test_error_mean[cv$best_iteration],
       Pred = cv$pred)
}


OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max_depth = c(2L,6L),
                                              min_child_weight = c(1L,10L),
                                              subsample = c(0, 1.0),
                                              eta=c(0,1.0),
                                              colsample_bytree = c(0,1.0),
                                              lambda = c(0,1.0),
                                              alpha = c(0,1.0),
                                              gamma=c(0,20),
                                              max_delta_step=c(0,10)),
                                init_grid_dt = NULL, init_points = 10, n_iter = 60,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)



#RANDOMFORREST
dtrain$fullVisitorId<-NULL
train_rf<-dtrain%>%mutate_if(is.character, as.factor)%>%mutate_if(is.factor, as.integer)
train_rf$adwordsClickInfo.isVideoAd<-as.integer(train_rf$adwordsClickInfo.isVideoAd)
train_rf$isTrueDirect<-as.integer(train_rf$isTrueDirect)
train_rf$isMobile<-as.integer(train_rf$isMobile)
train_rf<-train_rf%>%filter(transactionRevenue<10)


set.seed(1234)
ind<-sample(2, nrow(train_rf), replace=TRUE, prob=c(0.7,0.3))
Training_Data=train_rf[ind==1,]
Validation_Data=train_rf[ind==2,]

rfimpoted<-rfImpute(transactionRevenue~.,Training_Data)

tree.rev<-randomForest(transactionRevenue~.,data = Training_Data, na.action = na.roughfix)

Validation_Data_rf<-Validation_Data[,!(names(Validation_Data) %in% c("transactionRevenue"))]

predict_rf<-predict(tree.rev,Training_Data[,-17],na.action=na.roughfix)

RMSE(predict_rf,Training_Data[,17])



#SVM
train_svm<-dtrain%>%mutate_if(is.character, as.factor)

level=0
for (i in 1:ncol(Training_Data)){
  level[i,1]=nlevels(Training_Data[,i])
  level[i,2]=colnames(Training_Data[,i])
}


train_svm$adwordsClickInfo.isVideoAd<-as.integer(train_svm$adwordsClickInfo.isVideoAd)
train_svm$isTrueDirect<-as.integer(train_svm$isTrueDirect)
train_svm$isMobile<-as.integer(train_svm$isMobile)
train_svm<-train_svm%>%filter(transactionRevenue<10)
dummy_channel<-model.matrix(~channelGrouping-1,data = train_svm)
dummy_social<-model.matrix(~socialEngagementType-1,data=train_svm)

set.seed(1234) 
ind<-sample(2, nrow(train_svm), replace=TRUE, prob=c(0.7,0.3))
Training_Data=train_svm[ind==1,]
Validation_Data=train_svm[ind==2,]


model_svm_tune<-svm(transactionRevenue~.,data=Training_Data)




#OLS
model_lm<-lm(transactionRevenue~.,data = Training_Data)
