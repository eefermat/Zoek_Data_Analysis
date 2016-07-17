rm(list=ls())
options(scipen=999)
require(XLConnect)
require(magrittr)
require(jsonlite)
require(dplyr)
require(lubridate)

setwd("~/Desktop/ZOEK/BI/Data_Analysis/App")

#===============Read Data =================
member=fromJSON("member.json")
member_fb=fromJSON("member_fb.json")
branch=fromJSON("branch.json")
account=fromJSON("account.json")
product=fromJSON("product.json")
product_type=fromJSON("product_type.json")
orders=fromJSON("orders.json")
sales=fromJSON("sales.json")
userlog=fromJSON("userlog.json")
userlog_2=fromJSON("userlog_201606.json")

userlog = rbind(userlog,userlog_2)

#Only Funnow order
orders$amount<-as.numeric(orders$amount)
orders$type<-as.numeric(orders$type)
orders%<>%filter(type==0&amount>100)

#Remove Zoekers
zoeker=read.csv("zoeker.csv",stringsAsFactor=F,header=T)
orders=orders[orders$uid%in%zoeker$uid==F,]
member=member[member$uid%in%zoeker$uid==F,]
userlog=userlog[userlog$uid%in%zoeker$uid==F,]

#=====================Userlog Process ================

userlog_branch=userlog[userlog$eventname=="branch/search",]
userlogid=userlog[userlog$eventname!="branch/search",1]
userloguid=userlog[userlog$eventname!="branch/search",2]
userlog2=userlog[userlog$eventname!="branch/search",4]
userlog3=userlog2[1:(length(userlog2)-1)]
userlog3=paste(userlog3,sep=","," ")
userlog3=paste(userlog3, collapse ="")
userlog3=paste(userlog3,sep="",userlog2[length(userlog2)])
userlog3=paste("[",userlog3,sep="")
userlog3=paste(userlog3,"]",sep="")
userlog3=fromJSON(userlog3)
 
userlog3$id=userlogid
#userlog3=userlog3[,-2]
userlog%<>%select(id,eventname,createtime)%>%filter(eventname!="branch/search")
userlog=merge(userlog,userlog3,by="id",all.x=T)
#userlog%<>%filter(is.na(bid))

rm(userlog2,userlogid,userloguid,userlog3)
#===============Pid->Ptid->bid->pid =================
product%<>%select(pid,ptid)
branch%<>%select(bid,branchname,area,lat,lng,type,createtime)
product_type%<>%select(ptid,bid,productname,duration,facility)
sales%<>%select(sid,pid,discountratio,promostart,promoend)

temp<-merge(product_type,product,by="ptid",all.x = T)
temp<-merge(temp,select(branch,bid,branchname,area,lat,lng,type),by="bid",all.x = T)

sales<-merge(sales,temp,by="pid",all.x = T)
orders<-merge(orders,sales,by="sid",all.x = T)

#===============Member Data===============

#Merge two files to get create time
account%<>%select(uid,createtime)
member=member[,colnames(member)!="createtime"]
member=merge(member,account,by="uid",all.x=T)
member=member[-1,]# remove first one (invalid)

#Extract Date
member$cd=as.Date(member$createtime)
member$createtime=as.POSIXct(member$createtime)
member%<>%filter((createtime>as.POSIXct("2015-11-04")))


orders$cd=as.Date(orders$createtime)
orders$createtime=as.POSIXct(orders$createtime)
orders%<>%filter((createtime>as.POSIXct("2015-11-04")))


sales%<>%filter(promostart!="0000-00-00 00:00:00")
sales$cd=as.Date(sales$promostart)
sales$promostart=as.POSIXct(sales$promostart)
sales%<>%filter((promostart>as.POSIXct("2015-11-04")))


userlog$cd=as.Date(userlog$createtime)
userlog$createtime=as.POSIXct(userlog$createtime)
#userlog%<>%filter((cd<max(cd)))

#Add weekday& weekend
time.lub <- ymd_hms(userlog$createtime)
hour.lub<-as.numeric(hour(time.lub))
Time_D<-hour.lub>5&hour.lub<19
userlog$DN<-0
userlog$DN[Time_D]<-"Day"
userlog$DN[!Time_D]<-"Night"
userlog$TW<-weekdays(ymd_hms(userlog$createtime))
TW_weekend<-userlog$TW=="周六"|userlog$TW=="周日"|userlog$TW=="周五"
userlog$Weekday<-0
userlog$Weekday[!TW_weekend]<-"Weekday"
userlog$Weekday[TW_weekend]<-"Weekend"
userlog$TW<- factor(userlog$TW, levels= c("周一", "周二", "周三", "周四", "周五", "周六","周日"))

time.lub <- ymd_hms(orders$bookingtime)
hour.lub<-as.numeric(hour(time.lub))
Time_D<-hour.lub>5&hour.lub<19
orders$DN<-0
orders$DN[Time_D]<-"Day"
orders$DN[!Time_D]<-"Night"
orders$TW<-weekdays(ymd_hms(orders$bookingtime))
TW_weekend<-orders$TW=="周六"|orders$TW=="周日"|orders$TW=="周五"
orders$Weekday<-0
orders$Weekday[!TW_weekend]<-"Weekday"
orders$Weekday[TW_weekend]<-"Weekend"
orders$TW<- factor(orders$TW, levels= c("周一", "周二", "周三", "周四", "周五", "周六","周日"))

#===============Order Data===============
orders$week=as.integer(floor((orders$cd-as.Date("2015-11-04"))/7)+1)
orders$status_name<-0
orders$status_name[orders$status==2]<-"Paid"
orders$status_name[orders$status!=2]<-"Intention"

orders_member<-merge(orders,member,by="uid",all.x=T)
orders_member$gender<-factor(orders_member$gender,levels=c(0,1),labels=c("Female","Male"))
orders_member<-orders_member[!(orders_member$birthday=="0000-00-00")& !is.na(orders_member$birthday),]
orders_member$birthday<-as.Date(orders_member$birthday)
orders_member$age<-as.numeric((as.Date(Sys.time())-orders_member$birthday)/365)
orders_member$age<- cut(orders_member$age, 
            breaks = c(-Inf, 18, 22, 25, 30, 35,40,45, Inf), 
            labels = c("<Age 18", "Age 18-22", "Age 22-25", "Age 25-30", "Age 30-35", "Age 35-40","Age 40-45",">Age 45"), 
            right = FALSE)
orders_member$os=toupper(orders_member$os)
orders_member%<>%select(uid,week,gender,age,os,DN,TW,Weekday,branchname,status_name)
names(orders_member)<-c("uid","Create_Time","Gender","Age","Operating_System","Day_Night","Mon_to_Sun","Weekday_Weekend","branchname","status_name")
orders_member%<>%filter(status_name=="Paid")
orders_member<-orders_member[order(orders_member$Create_Time),]

orders_member$Rep<-"First"
rep_uid<-unique(orders_member[duplicated(orders_member$uid),]%$%uid)
for (i in 1:length(rep_uid)){
  orders_member$Rep[orders_member$uid==rep_uid[i]]<-"Rep"
  orders_member$Rep[min(which(orders_member$uid==rep_uid[i]))]<-"First"
}
#===============Member===============
member$week_create=as.integer(floor((member$cd-as.Date("2015-11-04"))/7)+1)
member$os=toupper(member$os)
member%<>%select(uid,birthday,cd,regType,gender,os,week_create,beinvitedcode,createtime)
member$regType=as.factor(member$regType)
member$gender=factor(member$gender,levels=c(0,1),labels=c("Female","Male"))
member$os=as.factor(member$os)
member$sign=0
member$sign[member$regType!="GUEST"]=1
member$sign=factor(member$sign,levels=c(0,1),labels=c("Not sign-up","Sign-up"))
names(member)<-c("uid","birthday","Create_Time","Register_Type","Gender","Operating_System","week_create","Invite_Code","Account_Create_Time","Sign_Up")
member_birth<-member[!(member$birthday=="0000-00-00")& !is.na(member$birthday),]
member_birth$age<-as.numeric((as.Date(Sys.time())-as.Date(member_birth$birthday))/365)
member_birth$age<- cut(member_birth$age, 
                        breaks = c(-Inf, 18, 22, 25, 30, 35,40,45, Inf), 
                        labels = c("<Age 18", "Age 18-22", "Age 22-25", "Age 25-30", "Age 30-35", "Age 35-40","Age 40-45",">Age 45"), 
                        right = FALSE)
#===============Branch Data===============
branch$area_detail<-branch$area
branch$area<-substr(branch$area,1,3)
branch$category<-factor(branch$type,levels=c(0,1,2,3,4,5,6,7,8),labels=c("休憩","休憩","休憩","美甲美睫","密室","桌遊","主題酒吧","按摩","中華職棒"))
branch$type<-factor(branch$type,levels=c(0,1,2,3,4,5,6,7,8),labels=c("摩鐵","湯屋","商旅","美甲美睫","密室","桌遊","主題酒吧","按摩","中華職棒"))
branch_gps<-select(branch,lng,lat,category)
branch$week<-as.integer(floor((as.Date(branch$createtime)-as.Date("2015-11-04"))/7)+1)

#============ Viewing data
user_cat<-userlog%>%filter(eventname=="product/home")%>%select(lat,lng,rpgid,start,forMap,createtime,uid)
user_cat$createtime<-as.Date(user_cat$createtime)
user_cat$lat<-as.numeric(user_cat$lat)
user_cat$lng<-as.numeric(user_cat$lng)
user_pt<-userlog%>%filter(eventname=="product_type/detail")%>%select(lat,lng,ptid,createtime,uid)
user_pt$createtime<-as.Date(user_pt$createtime)
user_pt$lat<-as.numeric(user_pt$lat)
user_pt$lng<-as.numeric(user_pt$lng)
user_search<-select(userlog_branch,uid,description,createtime)
user_cat<-na.omit(user_cat)
user_pt<-na.omit(user_pt)
user_search<-na.omit(user_search)

colnames(user_cat)<-c("lat","lng","rpgid","start","forMap","createtime","uid")
colnames(user_pt)<-c("lat","lng","ptid","createtime","uid")
colnames(user_search)<-c("uid","search","createtime")

user_pt<-user_pt%>%merge(.,select(product_type,ptid,bid),by="ptid")%>%merge(.,select(branch,bid,branchname),by="bid",all.x = T)
user_pt%<>%merge(select(branch,branchname,area_detail,category),by="branchname")
user_cat$rpgid[user_cat$rpgid==5]<-1
user_cat$rpgid[user_cat$rpgid==6]<-2
user_cat$rpgid=factor(user_cat$rpgid,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,14),labels=c("泡湯x休憩","晚鳥過夜","美甲x美睫","桌遊x密室","泡湯x休憩","晚鳥過夜","桌遊x吧x密室","主題酒吧","中信兄弟","按摩紓壓","台中享樂摩鐵","台中晚鳥過夜","台中中信兄弟"))
user_cat$forMap=factor(user_cat$forMap,levels=c(0,1),labels=c("List","Map"))
user_cat$week=as.integer(floor((as.Date(user_cat$createtime)-as.Date("2015-11-04"))/7)+1)

#===============login Data===============
userlog$week=as.integer(floor((userlog$cd-as.Date("2015-11-04"))/7)+1)
userlog$Login<-0
userlog$Login[userlog$eventname=="logout"]<-"Logout"
userlog$Login[userlog$eventname!="logout"]<-"Login"
userlog=filter(userlog,Login=="Login")
userlog<-merge(userlog,select(member,uid,Operating_System),by="uid")
userlog=select(userlog,uid,week,DN,TW,Weekday,cd,eventname,Operating_System)
names(userlog)<-c("uid","Create_Time","Day_Night","Mon_to_Sun","Weekday_Weekend","cd","eventname","os")


#===============Sales Data===============
time.lub <- ymd_hms(sales$promostart)
hour.lub<-as.numeric(hour(time.lub))
Time_D<-hour.lub>5&hour.lub<19
sales$start_DN<-0
sales$start_DN[Time_D]<-"Day"
sales$start_DN[!Time_D]<-"Night"

time.lub <- ymd_hms(sales$promoend)
hour.lub<-as.numeric(hour(time.lub))
Time_D<-hour.lub>5&hour.lub<19
sales$end_DN<-0
sales$end_DN[Time_D]<-"Day"
sales$end_DN[!Time_D]<-"Night"

sales$TW<-weekdays(ymd_hms(sales$promostart))
TW_weekend<-sales$TW=="周六"|sales$TW=="周日"|sales$TW=="周五"
sales$Weekday<-0
sales$Weekday[!TW_weekend]<-"Weekday"
sales$Weekday[TW_weekend]<-"Weekend"
sales$TW<- factor(sales$TW, levels= c("周一", "周二", "周三", "周四", "周五", "周六","周日"))

sales$DN<-0
sales$DN[sales$start_DN=="Day"&sales$end_DN=="Night"]<-"Day_Night"
sales$DN[sales$start_DN=="Night"&sales$end_DN=="Day"]<-"Day"
sales$DN[sales$start_DN=="Day"&sales$end_DN=="Day"]<-"Day"
sales$DN[sales$start_DN=="Night"&sales$end_DN=="Night"]<-"Night"

#========== Create Month===========
userlog$create_month<-substr(userlog$cd,1,7)
member$create_month<-substr(member$Create_Time,1,7)
orders$create_month<-substr(orders$cd,1,7)
#shorten list

orders<-select(orders,uid,createtime,create_month,cd,Weekday,DN,week,bookingtime,discountratio,branchname,status_name,amount,bonus)
names(orders)<-c("uid","createtime","create_month","cd","Weekday","DN","Create_Time","bookingtime","discountratio","branchname","status_name","amount","bonus")

orders$bonus[is.na(orders$bonus)]<-0
orders$bonus<-as.integer(orders$bonus)

orders$time_diff<-as.numeric(difftime(orders$bookingtime,orders$createtime,unit="hours"))
orders$time_diff<- cut(orders$time_diff, 
                       breaks = c(-Inf, 1, 3, 6, 9, 12, 24,Inf), 
                       labels = c("<1 hour", "1-3 hours", "3-6 hours", "6-9 hours", "9-12 hours", "12-24 hours",">24 hours"), 
                       right = FALSE)
#========== First Shopping===========
first_shopping<-merge(select(orders,uid,createtime,status_name),select(member,uid,Account_Create_Time),by="uid")
first_shopping%<>%filter(status_name=="Paid"&as.Date(Account_Create_Time)>=as.Date("2015-11-04"))
first_shopping<-first_shopping[order(first_shopping$createtime),]
first_shopping<-first_shopping[!duplicated(first_shopping$uid),]
first_shopping$time_diff<-difftime(first_shopping$createtime,first_shopping$Account_Create_Time,units="days")
first_shopping$browse_count<-0
for (i in 1:length(first_shopping$uid)){
  temp_id<-first_shopping$uid[i]
  temp_date<-first_shopping$createtime[i]
  temp<-userlog%>%filter(uid==temp_id&cd<=as.Date(temp_date)&eventname=="product/home")
  first_shopping$browse_count[i]<-nrow(temp)
}


#================ process data ===============
#Take out non_member
temp<-userlog[userlog$uid%in%(member%>%filter(Sign_Up=="Sign-up"&Create_Time>=as.Date(Sys.Date()-90))%$%uid),]

temp$os<-NULL
orders<-orders[!is.na(orders$branchname),]
userlog_member<-merge(temp,select(member,uid,week_create,Operating_System),by="uid",all.x=T)
userlog_member<-userlog_member[!is.na(userlog_member$week_create),]

#order data
orders<-merge(orders,select(member,uid,Operating_System),by="uid")
orders<-merge(orders,select(branch,branchname,type),by="branchname")
orders$hours<-"1-3"
time.lub <- ymd_hms(orders$bookingtime)
hour.lub<-as.numeric(hour(time.lub))
Time_D<-hour.lub>=4&hour.lub<=6
orders$hours[Time_D]<-"4-6"
Time_D<-hour.lub>=7&hour.lub<=9
orders$hours[Time_D]<-"7-9"
Time_D<-hour.lub>=10&hour.lub<=12
orders$hours[Time_D]<-"10-12"
Time_D<-hour.lub>=13&hour.lub<=15
orders$hours[Time_D]<-"13-15"
Time_D<-hour.lub>=16&hour.lub<=18
orders$hours[Time_D]<-"16-18"
Time_D<-hour.lub>=19&hour.lub<=21
orders$hours[Time_D]<-"19-21"
Time_D<-hour.lub>=22|hour.lub==0
orders$hours[Time_D]<-"22-0"
#MAU Data
MAU_userlog<-merge(select(userlog,uid,eventname,cd),select(member,uid,Create_Time,Register_Type,Operating_System),by="uid")
MAU_userlog%<>%filter(cd!=Create_Time)
date<-Sys.Date()-(0:90)
dates_vector <- as.Date(date[1:(length(date))],
                        format = "%Y-%m-%d")
#All
MAU<-data.frame(dates_vector,
                matrix(data = 0,
                       nrow = length(date),
                       ncol = 3)) 

colnames(MAU) <- c("Date","MAU_count", "DAU_count", "member")

for (i in 1:91){
  MAU[i,2]<-length(unique(filter(MAU_userlog,cd<=(Sys.Date()-i+1)&cd>=(Sys.Date()-i+1-30)&Register_Type!="GUEST")%$%uid))
  MAU[i,3]<-length(unique(filter(MAU_userlog,cd==(Sys.Date()-i+1)&Register_Type!="GUEST")%$%uid))
  MAU[i,4]<-length(filter(member,Create_Time<=(Sys.Date()-i+1)&Register_Type!="GUEST")%$%uid)
}
MAU$MAU_percentage<-MAU$MAU_count/MAU$member
MAU$DAU_MAU_percentage<-MAU$DAU_count/MAU$MAU_count
MAU$funnel<-"retention"

temp<-data.frame(dates_vector,
                matrix(data = 0,
                       nrow = length(date),
                       ncol = 3)) 

colnames(temp) <- c("Date","MAU_count", "DAU_count", "member")

for (i in 1:91){
  temp[i,2]<-length(unique(filter(orders,cd<=(Sys.Date()-i+1)&cd>=(Sys.Date()-i+1-30)&status_name=="Paid")%$%uid))
  temp[i,3]<-length(unique(filter(orders,cd==(Sys.Date()-i+1)&status_name=="Paid")%$%uid))
  temp[i,4]<-length(filter(member,Create_Time<=(Sys.Date()-i+1)&Register_Type!="GUEST")%$%uid)
}
temp$MAU_percentage<-temp$MAU_count/temp$member
temp$DAU_MAU_percentage<-temp$DAU_count/temp$MAU_count
temp$funnel<-"conversion"
MAU_all<-rbind(MAU,temp)

#iOS
MAU_userlog_IOS<-filter(MAU_userlog,Operating_System=="IOS")

for (i in 1:91){
  MAU[i,2]<-length(unique(filter(MAU_userlog_IOS,cd<=(Sys.Date()-i+1)&cd>=(Sys.Date()-i+1-30)&Register_Type!="GUEST")%$%uid))
  MAU[i,3]<-length(unique(filter(MAU_userlog_IOS,cd==(Sys.Date()-i+1)&Register_Type!="GUEST")%$%uid))
  MAU[i,4]<-length(filter(member,Create_Time<=(Sys.Date()-i+1)&Register_Type!="GUEST"&Operating_System=="IOS")%$%uid)
}
MAU$MAU_percentage<-MAU$MAU_count/MAU$member
MAU$DAU_MAU_percentage<-MAU$DAU_count/MAU$MAU_count
MAU$funnel<-"retention"


for (i in 1:91){
  temp[i,2]<-length(unique(filter(orders,cd<=(Sys.Date()-i+1)&cd>=(Sys.Date()-i+1-30)&status_name=="Paid"&Operating_System=="IOS")%$%uid))
  temp[i,3]<-length(unique(filter(orders,cd==(Sys.Date()-i+1)&status_name=="Paid"&Operating_System=="IOS")%$%uid))
  temp[i,4]<-length(filter(member,Create_Time<=(Sys.Date()-i+1)&Register_Type!="GUEST"&Operating_System=="IOS")%$%uid)
}
temp$MAU_percentage<-temp$MAU_count/temp$member
temp$DAU_MAU_percentage<-temp$DAU_count/temp$MAU_count
temp$funnel<-"conversion"

MAU_IOS<-rbind(MAU,temp)

#Android
MAU_userlog_ANDROID<-filter(MAU_userlog,Operating_System=="ANDROID")
for (i in 1:91){
  MAU[i,2]<-length(unique(filter(MAU_userlog_ANDROID,cd<=(Sys.Date()-i+1)&cd>=(Sys.Date()-i+1-30)&Register_Type!="GUEST")%$%uid))
  MAU[i,3]<-length(unique(filter(MAU_userlog_ANDROID,cd==(Sys.Date()-i+1)&Register_Type!="GUEST")%$%uid))
  MAU[i,4]<-length(filter(member,Create_Time<=(Sys.Date()-i+1)&Register_Type!="GUEST"&Operating_System=="ANDROID")%$%uid)
}
MAU$MAU_percentage<-MAU$MAU_count/MAU$member
MAU$DAU_MAU_percentage<-MAU$DAU_count/MAU$MAU_count
MAU$funnel<-"retention"

for (i in 1:91){
  temp[i,2]<-length(unique(filter(orders,cd<=(Sys.Date()-i+1)&cd>=(Sys.Date()-i+1-30)&status_name=="Paid"&Operating_System=="ANDROID")%$%uid))
  temp[i,3]<-length(unique(filter(orders,cd==(Sys.Date()-i+1)&status_name=="Paid"&Operating_System=="ANDROID")%$%uid))
  temp[i,4]<-length(filter(member,Create_Time<=(Sys.Date()-i+1)&Register_Type!="GUEST"&Operating_System=="ANDROID")%$%uid)
}
temp$MAU_percentage<-temp$MAU_count/temp$member
temp$DAU_MAU_percentage<-temp$DAU_count/temp$MAU_count
temp$funnel<-"conversion"

MAU_ANDROID<-rbind(MAU,temp)

#set area
member$area<-"台北"
orders<-merge(orders,select(branch,lat,branchname),by="branchname")
Taichung_uid<-aggregate(user_cat["lat"],by=list(user_cat$uid),FUN=mean)%>%filter(lat<24.53&lat>20)
member[member$uid%in%(Taichung_uid$Group.1),"area"]<-"台中"
orders$area<-"台北"
orders[orders$lat<24.53,"area"]<-"台中"

#Save file
saveRDS(userlog,"login")
saveRDS(member,"member_data")
saveRDS(user_cat,"user_cat")
saveRDS(user_pt,"user_pt")
saveRDS(user_search,"user_search")
saveRDS(orders_member,"orders_member")
saveRDS(orders,"orders")
saveRDS(branch,"branch")
saveRDS(sales,"sales")
saveRDS(first_shopping,"first_shopping")
saveRDS(userlog_member,"userlog_member")
saveRDS(member_birth,"member_birth")
saveRDS(MAU_all,"MAU_all")
saveRDS(MAU_IOS,"MAU_IOS")
saveRDS(MAU_ANDROID,"MAU_ANDROID")


#jdasiod

