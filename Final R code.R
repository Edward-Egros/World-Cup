df_orig<- read.csv("D:/saahi/Documents/Stat 6366/Project/dataset/football_1.csv")
df<-df_orig
require(lubridate)
df$year<-year(mdy(df$date))
df<- df[which(df$year>=2003),]

result<- df$home_score-df$away_score
df$result<-ifelse(result<0,df$result<- -1,ifelse(result==0,df$result<-0,df$result<-1))
df$result<-as.factor(df$result)
df$home_res<-result

df<-df[which(abs(df$home_res)<3),]

deg2rad<-function(deg){
  return(deg*pi/180)
}

distcoor<- function(lat1,lat2,lon1,lon2){
  R<- 6371
  dlat<-deg2rad(lat2-lat1)
  dlon<-deg2rad(lon2-lon1)
  a<- sin(dlat/2)*sin(dlat/2)+cos(deg2rad(lat1))*cos(deg2rad(lat2))*sin(dlon/2)*sin(dlon/2)
  c<- 2*atan2(sqrt(a),sqrt(1-a))
  d<- R*c
  return(d)
}

df$home_dist<-distcoor(df$Coordinate_match1,df$Coordinate_home1,df$Coordinate_match2,df$Coordinate_home2)
df$away_dist<-distcoor(df$Coordinate_match1,df$Coordinate_away1,df$Coordinate_match2,df$Coordinate_away2)

require(dplyr)

## to check which tournaments have significant observations
y<-group_by(df,tournament)
y1<-summarize(y,freq=n())
y1<-as.data.frame(y1)
y1<-y1[which(y1$freq>50),]

## Removing insignificant tournaments
df<-df[df$tournament %in% y1$tournament,]
df$tournament<-as.character(df$tournament)
df$tournament<-as.factor(df$tournament)

## Average home team performance
hometeamrecord<- group_by(df,home_team)
summ<-summarize(hometeamrecord,result=mean(as.numeric(result)-2))
summ<-as.data.frame(summ)
summ[order(summ$result,decreasing = F),]

df<-merge(df,summ,by = "home_team")
colnames(df)[18]<-"result"
colnames(df)[22]<-"homeavgperformance"


## Average away team performance
awayteamrecord<- group_by(df,away_team)
summ2<-summarize(awayteamrecord,result=mean(as.numeric(result)-2)*-1)
summ2<-as.data.frame(summ2)
summ2[order(summ2$result,decreasing = F),]

df<-merge(df,summ2,by = "away_team")
colnames(df)[18]<-"result"
colnames(df)[23]<-"awayavgperformance"

## use this distribution for grouping
hist(df$homeavgperformance,breaks = 40)
hist(df$awayavgperformance,breaks = 40)

## gouping home teams
home_team_group<- function(team_performance){
  team_group<-ifelse(team_performance< -0.9,1,
                     ifelse(team_performance< -0.7,2,
                            ifelse(team_performance< -0.4,3,
                                   ifelse(team_performance< -0.3,4,
                                          ifelse(team_performance< -0.2,5,
                                                 ifelse(team_performance< -0.1,6,
                                                        ifelse(team_performance< 0,7,
                                                               ifelse(team_performance< 0.1,8,
                                                                      ifelse(team_performance< 0.15,9,
                                                                             ifelse(team_performance< 0.175,10,
                                                                                    ifelse(team_performance< 0.2,11,
                                                                                           ifelse(team_performance< 0.225,12,
                                                                                                  ifelse(team_performance< 0.275,13,
                                                                                                         ifelse(team_performance< 0.3,14,
                                                                                                                ifelse(team_performance< 0.325,15,
                                                                                                                       ifelse(team_performance< 0.35,16,
                                                                                                                              ifelse(team_performance< 0.375,17,
                                                                                                                                     ifelse(team_performance< 0.4,18,
                                                                                                                                            ifelse(team_performance< 0.425,19,
                                                                                                                                                   ifelse(team_performance< 0.45,20,
                                                                                                                                                          ifelse(team_performance< 0.475,21,
                                                                                                                                                                 ifelse(team_performance< 0.5,22,
                                                                                                                                                                        ifelse(team_performance< 0.525,23,
                                                                                                                                                                               ifelse(team_performance< 0.55,24,
                                                                                                                                                                                      ifelse(team_performance< 0.575,25,
                                                                                                                                                                                             ifelse(team_performance< 0.6,26,                                 
                                                                                                                                                                                                    27
                                                                                                                                                                                             ))))))))))))))))))))))))))
  return(team_group)
}


away_team_group<- function(team_performance){
  team_group<-ifelse(team_performance< -0.9,1,
                     ifelse(team_performance< -0.8,2,
                            ifelse(team_performance< -0.75,3,
                                   ifelse(team_performance< -0.7,4,
                                          ifelse(team_performance< -0.65,5,
                                                 ifelse(team_performance< -0.6,6,
                                                        ifelse(team_performance< -0.5,7,
                                                               ifelse(team_performance< -0.45,8,
                                                                      ifelse(team_performance< -0.4,9,
                                                                             ifelse(team_performance< 0.3,10,
                                                                                    ifelse(team_performance< -0.25,11,
                                                                                           ifelse(team_performance< -0.2,12,
                                                                                                  ifelse(team_performance< -0.15,13,
                                                                                                         ifelse(team_performance< -0.1,14,
                                                                                                                ifelse(team_performance< -0.05,15,
                                                                                                                       ifelse(team_performance< 0,16,
                                                                                                                              ifelse(team_performance< 0.1,17,
                                                                                                                                     ifelse(team_performance< 0.15,18,
                                                                                                                                            ifelse(team_performance< 0.2,19,
                                                                                                                                                   ifelse(team_performance< 0.3,20,
                                                                                                                                                          ifelse(team_performance< 0.4,21,
                                                                                                                                                                 22
                                                                                                                                                          )))))))))))))))))))))
  return(team_group)
}


df$home_team_group<-home_team_group(df$homeavgperformance)
df$away_team_group<-away_team_group(df$awayavgperformance)

df$home_team_group<- as.factor(df$home_team_group)
df$away_team_group<- as.factor(df$away_team_group)

df$month<- month(mdy(df$date))
head(df)

df$diff<-df$Home.team.value-df$Away.team.value

X_train<-df[,c(6,20,21,24,25,26,27)]
Y_train<-as.factor(df[,18])

require(gbm)
out.boost<- gbm(Y_train~.,data=X_train,
                distribution="multinomial",
                n.trees=1000,
                shrinkage=0.01,
                interaction.depth=3,
                bag.fraction = 0.3,
                n.minobsinnode = 10,
                cv.folds = 10,
                keep.data=TRUE,
                verbose=TRUE,
                n.cores=8)
sum.boost<-summary(out.boost)

pred<-predict(out.boost,X_train,type = "response")

p.pred<-apply(pred,1,which.max)
prob.pred<-apply(pred,1,max)

valid_set<- data.frame(df$home_res,df$result,p.pred-2,prob.pred)
valid_set$pred_goal<-ifelse(valid_set$prob.pred>0.5,2*valid_set$p.pred...2,valid_set$p.pred...2)
valid_set$pred_goal<-ifelse(valid_set$prob.pred>0.8,3*valid_set$p.pred...2,valid_set$pred_goal)
valid_set

df_test_orig<-read.csv("D:/saahi/Documents/Stat 6366/Project/dataset/WorldCupMatches.csv")
df_test<-df_test_orig[1:48,]


df_test<-merge(df_test,summ,by = "home_team")
colnames(df_test)[13]<-"homeavgperformance"
df_test<-merge(df_test,summ2,by = "away_team")
colnames(df_test)[14]<-"awayavgperformance"

df_test$home_team_group<-home_team_group(df_test$homeavgperformance)
df_test$away_team_group<-away_team_group(df_test$awayavgperformance)

df_test$home_team_group<- as.factor(df_test$home_team_group)
df$away_team_group<- as.factor(df$away_team_group)

df_test$month<-rep(6,dim(df_test)[1])
colnames(df_test)[5]<-'Coordinate_match1'
colnames(df_test)[6]<-'Coordinate_match2'

df_test$home_dist<-distcoor(df_test$Coordinate_match1,df_test$Coordinate_home1,df_test$Coordinate_match2,df_test$Coordinate_home2)
df_test$away_dist<-distcoor(df_test$Coordinate_match1,df_test$Coordinate_away1,df_test$Coordinate_match2,df_test$Coordinate_away2)

df_test$tournament<-rep('FIFA World Cup',dim(df_test)[1])

df_test$diff<-df_test$Home.team.value-df_test$Away.team.value

X_test<-df_test[,c(5,6,15,16,17,18,19,20,21)]

pred<-predict(out.boost,X_test,type = "response")

p.pred<-apply(pred,1,which.max)
prob.pred<-apply(pred,1,max)

WCPred<-data.frame(df_test$home_team,df_test$away_team,pred,p.pred-2,prob.pred)
WCPred$pred_goal<-ifelse(WCPred$prob.pred>0.5,2*WCPred$p.pred...2,WCPred$p.pred...2)
WCPred$pred_goal<-ifelse(WCPred$prob.pred>0.8,3*WCPred$p.pred...2,WCPred$pred_goal)
colnames(WCPred)<-c('Home Team','Away Team','Probability of loss','Probability of tie','Probability of win','Predicted result','Probability','Goals')
WCPred

write.csv(WCPred,file = "D:/saahi/Documents/Stat 6366/Project/dataset/predictions.csv")
