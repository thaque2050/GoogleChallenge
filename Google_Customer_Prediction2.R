#Read Data
read.csv("test.csv/test.csv",stringsAsFactors = FALSE,nrows = 10)%>%ncol()
dataformat<-c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")
test_data<-read.csv("test.csv",stringsAsFactors = FALSE,colClasses = dataformat)
dtest<-test_data


#TRANSAFORM DATA

#Parse JASON Data
ts_device <- paste("[", paste(dtest$device, collapse = ","), "]") %>% fromJSON(flatten = T)
ts_geoNetwork<- paste("[", paste(dtest$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
ts_total<- paste("[", paste(dtest$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
ts_trafficsource<-paste("[",paste(dtest$trafficSource,collapse = ","),"]")%>%fromJSON(flatten=TRUE)
dtest<-cbind(dtest,ts_device,ts_geoNetwork,ts_total,ts_trafficsource)

#Remove not required columns
dtest$device<-NULL
dtest$geoNetwork<-NULL
dtest$totals<-NULL
dtest$trafficSource<-NULL
dtest$adwordsClickInfo.gclId<-NULL
dtest$sessionId<-NULL
dtest$visitId<-NULL
dtest$visits<-NULL
dtest$newVisits<-NULL

#Remove certain strings with NAs
dtest[dtest=="not available in demo dataset"]<-NA
dtest[dtest=="(not set)"]<-NA
dtest[dtest=="(not provided)"]<-NA
dtest[which(dtest$networkDomain=="unknown.unknown"),]<-NA

#Replace NAs with appropriate values
dtest$adwordsClickInfo.page[is.na(dtest$adwordsClickInfo.page)]<-"0"
dtest$adwordsClickInfo.slot[is.na(dtest$adwordsClickInfo.slot)]<-"Others"
dtest$adwordsClickInfo.adNetworkType[is.na(dtest$adwordsClickInfo.adNetworkType)]<-"Others"
dtest$adwordsClickInfo.isVideoAd[is.na(dtest$adwordsClickInfo.isVideoAd)]<-TRUE
dtest$isTrueDirect[is.na(dtest$isTrueDirect)]<-FALSE


#Convert variable type
dtest$date<-as.Date(as.character(dtest$date),"%Y%m%d")
dtest$visitStartTime<-as.POSIXct(dtest$visitStartTime,tz="UTC",origin='1970-01-01')
dtest$hits<-dtest$hits%>%as.numeric()
dtest$bounces<-dtest$bounces%>%as.numeric()
dtest$pageviews<-dtest$pageviews%>%as.numeric()


#Create new variables
dtest$start_hour<-hour(dtest$visitStartTime)
dtest$month<-month(dtest$visitStartTime)
dtest$day<-day(dtest$visitStartTime)
dtest$weekday<-weekdays(dtest$visitStartTime)
dtest$netdomain<-sub('.*\\.', '', dtest$networkDomain)
dtest$networkDomain<-NULL
dtest$visitStartTime<-NULL
dtest$date<-NULL

#Remove columns with more than 90% missing entries
Include_Variable2<-Include_Variable[!Include_Variable %in% ("transactionRevenue")]
dtest<-dtest%>%select(Include_Variable2)




#PREDICT USING XGBOOST
dtest_xgb<-dtest[,-c("fullVisitorId")]
dtest_xgb<-dtest%>%mutate_if(is.character, as.factor)%>%mutate_if(is.factor, as.integer)
dtest_xgb$adwordsClickInfo.isVideoAd<-as.integer(dtest_xgb$adwordsClickInfo.isVideoAd)
dtest_xgb$isTrueDirect<-as.integer(dtest_xgb$isTrueDirect)
dtest_xgb$isMobile<-as.integer(dtest_xgb$isMobile)

X_test<-as.matrix(dtest_xgb)
test_predict<-predict(m_xgb,X_test)
test_predict<-test_predict%>%as.data.frame()
colnames(test_predict)<-"pred_rev"


test_predict%>%ggplot+geom_histogram(aes(x=pred_rev),bins = 50)
 


#SUBMIT DATA
test_predict<-cbind()








