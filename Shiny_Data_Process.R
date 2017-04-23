rm(list=ls())
ptm <- proc.time()

options(scipen=999)
require(XLConnect)
require(magrittr)
require(jsonlite)
require(dplyr)
require(lubridate)

#setwd("~/Desktop/ZOEK/BI/Data_Analysis/App")

#===============Read Data =================
member=fromJSON("member.json")
branch=fromJSON("branch.json")
account=fromJSON("account.json")
product=fromJSON("product.json")
product_type=fromJSON("product_type.json")
orders=fromJSON("orders.json")
sales=fromJSON("sales.json")
userlog=fromJSON("userlog_201701.json")
userlog_2=fromJSON("userlog_201702.json")
#userlog_3=fromJSON("userlog_201610.json")
#userlog_4=fromJSON("userlog_201611.json")
#userlog_5=fromJSON("userlog_201612.json")

userlog=rbind(userlog,userlog_2)
#userlog=rbind(userlog,userlog_4)
#userlog=rbind(userlog,userlog_5)
#rm(userlog_2,userlog_3,userlog_4,userlog_5)
#Only Funnow order
orders$amount<-as.numeric(orders$amount)
orders$type<-as.numeric(orders$type)
orders%<>%filter(type==0&amount>60)

#Remove Zoekers
zoeker=read.csv("zoeker.csv",stringsAsFactor=F,header=T)
#orders=orders[orders$uid%in%zoeker$uid==F,]
#member=member[member$uid%in%zoeker$uid==F,]
#userlog=userlog[userlog$uid%in%zoeker$uid==F,]

#===============Branch Data===============
branch$area_detail<-branch$area
branch$category<-factor(branch$type,levels=c(0,1,2,3,4,5,6,7,8,9,10,11),labels=c("休憩","休憩","休憩","美甲美睫","密室","桌遊","主題酒吧","按摩","中華職棒","電影","咖啡","親子"))
branch$type<-factor(branch$type,levels=c(0,1,2,3,4,5,6,7,8,9,10,11),labels=c("摩鐵","湯屋","商旅","美甲美睫","密室","桌遊","主題酒吧","按摩","中華職棒","電影","咖啡","親子"))
branch_gps<-select(branch,lng,lat,category)
branch$week<-as.integer(floor((as.Date(branch$createtime)-as.Date("2015-11-02"))/7)+1)
branch$lat<-as.numeric(branch$lat)
branch$lng<-as.numeric(branch$lng)

branch$area<-"台北"
branch[branch$lat<24.53,"area"]<-"台中"
#=====================Userlog Process ================
userlog$cd=as.Date(userlog$createtime)
userlog$createtime=as.POSIXct(userlog$createtime)
userlog$Create_Time=as.integer(floor((userlog$cd-as.Date("2015-11-02"))/7)+1)

#============ Viewing data
userlog_branch=userlog[userlog$eventname=="branch/search",]

temp=userlog[userlog$eventname=="product/home",]
userlogid=temp[,1]
userloguid=temp[,2]
userlog2=temp[,4]
userlog3=userlog2[1:(length(userlog2)-1)]
userlog3=paste(userlog3,sep=","," ")
userlog3=paste(userlog3, collapse ="")
userlog3=paste(userlog3,sep="",userlog2[length(userlog2)])
userlog3=paste("[",userlog3,sep="")
userlog3=paste(userlog3,"]",sep="")
userlog3=fromJSON(userlog3)
userlog3$id=userlogid
userlog3=merge(temp%>%select(id,eventname,createtime,cd,Create_Time),userlog3,by="id",all.x=T)

user_cat<-userlog3%>%filter(eventname=="product/home")%>%select(lat,lng,rpgid,start,forMap,createtime,uid)
user_cat$createtime<-as.Date(user_cat$createtime)
user_cat$lat<-as.numeric(user_cat$lat)
user_cat$lng<-as.numeric(user_cat$lng)

temp=userlog[userlog$eventname=="product_type/detail",]
userlogid=temp[,1]
userloguid=temp[,2]
userlog2=temp[,4]
userlog3=userlog2[1:(length(userlog2)-1)]
userlog3=paste(userlog3,sep=","," ")
userlog3=paste(userlog3, collapse ="")
userlog3=paste(userlog3,sep="",userlog2[length(userlog2)])
userlog3=paste("[",userlog3,sep="")
userlog3=paste(userlog3,"]",sep="")
userlog3=fromJSON(userlog3)
userlog3$id=userlogid
userlog3=merge(temp%>%select(id,eventname,createtime,cd,Create_Time),userlog3,by="id",all.x=T)


user_pt<-userlog3%>%filter(eventname=="product_type/detail")%>%select(lat,lng,ptid,createtime,uid)
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
user_search$createtime<-as.Date(user_search$createtime)

user_pt<-user_pt%>%merge(.,select(product_type,ptid,bid,ot_flag),by="ptid")%>%merge(.,select(branch,bid,branchname,type,area,area_detail),by="bid",all.x = T)
user_pt$category<-user_pt$type
user_pt$category<-factor(user_pt$type,levels=c("桌遊","密室","摩鐵","商旅","休憩","晚鳥過夜","美甲美睫","桌遊x密室","主題酒吧","按摩","中華職棒","電影","咖啡","親子"))
user_pt$category[(user_pt$type=="摩鐵"|user_pt$type=="湯屋"|user_pt$type=="商旅")&user_pt$ot_flag==0]<-"休憩"
user_pt$category[(user_pt$type=="摩鐵"|user_pt$type=="湯屋"|user_pt$type=="商旅")&user_pt$ot_flag==1]<-"晚鳥過夜"
user_cat$rpgid[user_cat$rpgid==5]<-1
user_cat$rpgid[user_cat$rpgid==6]<-2
user_cat$rpgid=factor(user_cat$rpgid,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),labels=c("泡湯x休憩","晚鳥過夜","美甲x美睫","桌遊x密室","泡湯x休憩","晚鳥過夜","桌遊x吧x密室","主題酒吧","中信兄弟","按摩紓壓","台中享樂摩鐵","台中晚鳥過夜","台中按摩紓壓","台中中信兄弟","週末派對","首購彩蛋","來場電影","特色咖啡"))
user_cat$forMap=factor(user_cat$forMap,levels=c(0,1),labels=c("List","Map"))
user_cat$week=as.integer(floor((as.Date(user_cat$createtime)-as.Date("2015-11-02"))/7)+1)

user_cat%<>%filter(lat>=25.061579|lat<=25.0566701|lng>=121.5261216|lng<=121.5207947)
user_pt%<>%filter(lat>=25.061579|lat<=25.0566701|lng>=121.5261216|lng<=121.5207947)
user_cat$TW<-weekdays((user_cat$createtime))
TW_weekend<-user_cat$TW=="Saturday"|user_cat$TW=="Sunday"|user_cat$TW=="Friday"
user_cat$Weekday<-0
user_cat$Weekday[!TW_weekend]<-"Weekday"
user_cat$Weekday[TW_weekend]<-"Weekend"

user_cat_old<-readRDS("user_cat")
user_cat<-rbind(user_cat_old,user_cat)
saveRDS(user_cat,"user_cat")

user_pt_old<-readRDS("user_pt")
user_pt<-rbind(user_pt_old,user_pt)
saveRDS(user_pt,"user_pt")
rm(user_pt,userlog_branch,user_pt_old,user_cat_old)

user_search_old<-readRDS("user_search")
user_search<-rbind(user_search_old,user_search)
saveRDS(user_search,"user_search")
rm(temp,userlog2,userlogid,userloguid,userlog3,user_search,user_search_old,userlog_2)


#===============Pid->Ptid->bid->pid =================
product%<>%select(pid,ptid)
branch%<>%select(bid,branchname,area,lat,lng,type,createtime,area_detail,week,category)
product_type%<>%select(ptid,bid,productname,duration,facility,ot_flag)
sales%<>%select(sid,pid,discountratio,promostart,promoend,createtime,lastmodifiedtime,lasteditor,deleted)

temp<-merge(product_type,product,by="ptid",all.x = T)
temp<-merge(temp,select(branch,bid,branchname,area,lat,lng,type,area_detail),by="bid",all.x = T)

sales<-merge(sales,temp,by="pid",all.x = T)
drops<-c("createtime","lastmodifiedtime","lasteditor","deleted")
sales_short<-sales[,!(names(sales) %in% drops)]
orders<-merge(orders,sales_short,by="sid",all.x = T)
rm(product)
#===============Member Data===============

#Merge two files to get create time
account%<>%select(uid,createtime,email)
member=member[,colnames(member)!="createtime"]
member=merge(member,account,by="uid",all.x=T)
member=member[-1,]# remove first one (invalid)
rm(account)
#Extract Date
member$cd=as.Date(member$createtime)
member$createtime=as.POSIXct(member$createtime)
member%<>%filter((createtime>as.POSIXct("2015-11-02")))


orders$cd=as.Date(orders$createtime)
orders$createtime=as.POSIXct(orders$createtime)
orders%<>%filter((createtime>as.POSIXct("2015-11-02")))


sales%<>%filter(promostart!="0000-00-00 00:00:00")
sales$cd=as.Date(sales$promostart)
sales$promostart=as.POSIXct(sales$promostart)
sales%<>%filter((promostart>as.POSIXct("2015-11-02")))

time.lub <- ymd_hms(orders$bookingtime)
hour.lub<-as.numeric(hour(time.lub))
Time_D<-hour.lub>5&hour.lub<19
orders$DN<-0
orders$DN[Time_D]<-"Day"
orders$DN[!Time_D]<-"Night"
orders$TW<-weekdays(ymd_hms(orders$bookingtime))
TW_weekend<-orders$TW=="Saturday"|orders$TW=="Sunday"|orders$TW=="Friday"
orders$Weekday<-0
orders$Weekday[!TW_weekend]<-"Weekday"
orders$Weekday[TW_weekend]<-"Weekend"
orders$TW<- factor(orders$TW,levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

#===============Order Data===============
orders$week=as.integer(floor((orders$cd-as.Date("2015-11-02"))/7)+1)
member$member_week_create=as.integer(floor((member$cd-as.Date("2015-11-02"))/7)+1)

orders$status_name<-0
orders$status_name[orders$status==2|orders$status==7]<-"Paid"
orders$status_name[orders$status!=2&orders$status!=7]<-"Intention"

orders_member<-merge(orders,member,by="uid",all.x=T)
orders_member$gender<-factor(orders_member$gender,levels=c(0,1),labels=c("Female","Male"))
orders_member[(orders_member$birthday=="0000-00-00")|is.na(orders_member$birthday),]$birthday<-"1990-01-01"
orders_member$birthday<-as.Date(orders_member$birthday)
orders_member$age<-as.numeric((as.Date(Sys.time())-orders_member$birthday)/365)
orders_member$age<- cut(orders_member$age, 
            breaks = c(-Inf, 18, 22, 25, 30, 35,40,45, Inf), 
            labels = c("<Age 18", "Age 18-22", "Age 22-25", "Age 25-30", "Age 30-35", "Age 35-40","Age 40-45",">Age 45"), 
            right = FALSE)
orders_member$os=toupper(orders_member$os)
orders_member%<>%select(uid,week,member_week_create,regType,gender,age,os,DN,TW,Weekday,branchname,status_name)
names(orders_member)<-c("uid","week","member_week_create","regType","Gender","Age","Operating_System","Day_Night","Mon_to_Sun","Weekday_Weekend","branchname","status_name")
orders_member%<>%filter(status_name=="Paid")

# Orders_Segmentation
orders_member<-orders_member[order(orders_member$week),]
orders_member$rep<-"Rep"
orders_member$purchase<-1
rep_uid<-unique(orders_member[duplicated(orders_member$uid),]%$%uid)
for (i in 1:length(rep_uid)){
  orders_member$purchase[orders_member$uid==rep_uid[i]]<-2
  orders_member$purchase[min(which(orders_member$uid==rep_uid[i]))]<-1
}

orders_member_old<-orders_member[orders_member$member_week_create!=orders_member$week,]
orders_member_old%<>%filter(purchase==1)%>%select(uid,week)
orders_member_old$week_old<- orders_member_old$week
orders_member_old%<>%select(uid,week_old)
orders_member<-merge(orders_member,orders_member_old,by="uid",all=TRUE)
orders_member$rep[orders_member$member_week_create!=orders_member$week&orders_member$week==orders_member$week_old]<-"First not same week"

orders_member$rep[orders_member$member_week_create==orders_member$week]<-"First"
stats<-orders_member%>%group_by(week,rep,uid)%>%summarise(n=n())

head_stats<-data.frame(matrix(data = 0,
                              nrow = length(unique(orders_member$week))*length(unique(orders_member$rep)),
                              ncol = 3)) 
row<-1
for (i in unique(orders_member$week)){
  for (j in unique(orders_member$rep)){
    head_stats[row,1]<-i
    head_stats[row,2]<-j
    head_stats[row,3]<-nrow(stats%>%filter(week==i&rep==j))
    row<-row+1
  }
}

colnames(head_stats)<-c("create_week","Type","Head_Counts")
stats<-orders_member%>%group_by(week,rep)%>%summarise(n=n())
colnames(stats)<-c("create_week","Type","Counts")
head_stats<-head_stats[order(head_stats$create_week),]
stats<-merge(head_stats,stats,by=c("create_week","Type"),all = T)
stats[is.na(stats$Counts),"Counts"]<-0

member_first<-member%>%filter(regType!="GUEST")%>%group_by(member_week_create)%>%summarise(members=n())
colnames(member_first)<-c("create_week","members")
member_first$Type<-"First"
member_rep<-data.frame(matrix(data = 0,
                              nrow = length(unique(orders_member$week)),
                              ncol = 2))
row<-1
for (i in unique(orders_member$week)){
  member_rep[row,1]<-i
  member_rep[row,2]<-length(unique(orders_member%>%filter(week<i)%$%uid))
  row<-row+1
}
colnames(member_rep)<-c("create_week","members")
member_rep$Type<-"Rep"
member_rep<-member_rep[order(member_rep$create_week),]

member_first_not_same<-data.frame(matrix(data = 0,
                              nrow = length(unique(member$member_week_create)),
                              ncol = 2))
row<-1
for (i in unique(orders_member$week)){
  member_first_not_same[row,1]<-i
  member_first_not_same[row,2]<-length(unique(member%>%filter(member_week_create<i&regType!="GUEST")%$%uid))
  row<-row+1
}
colnames(member_first_not_same)<-c("create_week","members")

member_first_not_same$Type<-"First not same week"
member_first_not_same<-member_first_not_same[order(member_first_not_same$create_week),]
member_first_not_same$members<-member_first_not_same$members-member_rep$members
member_stats<-do.call("rbind", list(member_first, member_first_not_same, member_rep))
stats<-merge(stats,member_stats,by=c("create_week","Type"))
stats<-stats[order(stats$create_week),]
stats$conversion<-stats$Head_Counts/stats$members

stats%<>%filter(create_week>=2)

#Month


#===============Member===============
colnames(member)[colnames(member)=="member_week_create"]<-"week_create"
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


#===============login Data===============
#Take out non_member
temp<-userlog[userlog$uid%in%(member%>%filter(Sign_Up=="Sign-up"&Create_Time>=as.Date(Sys.Date()-90))%$%uid),]

temp$os<-NULL
orders<-orders[!is.na(orders$branchname),]
userlog_member<-merge(temp,select(member,uid,week_create,Operating_System),by="uid",all.x=T)
rm(temp)
userlog_member<-userlog_member[!is.na(userlog_member$week_create),]
saveRDS(userlog_member,"userlog_member")
rm(userlog_member)
MAU_userlog<-merge(select(userlog,uid,eventname,cd),select(member,uid,Create_Time,Register_Type,Operating_System),by="uid")

userlog$create_month<-substr(userlog$cd,1,7)
time.lub <- ymd_hms(userlog$createtime)
hour.lub<-as.numeric(hour(time.lub))
Time_D<-hour.lub>5&hour.lub<19
userlog$Day_Night<-0
userlog$Day_Night[Time_D]<-"Day"
userlog$Day_Night[!Time_D]<-"Night"
userlog$Mon_to_Sun<-weekdays(ymd_hms(userlog$createtime))
TW_weekend<-userlog$Mon_to_Sun=="Saturday"|userlog$Mon_to_Sun=="Sunday"|userlog$Mon_to_Sun=="Friday"
userlog$Weekday_Weekend<-0
userlog$Weekday_Weekend[!TW_weekend]<-"Weekday"
userlog$Weekday_Weekend[TW_weekend]<-"Weekend"
userlog$Mon_to_Sun<- factor(userlog$Mon_to_Sun, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

old_login<-readRDS("login")
userlog<-rbind(old_login,userlog)
saveRDS(userlog,"login")
rm(userlog,old_login)

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
TW_weekend<-sales$TW=="Saturday"|sales$TW=="Sunday"|sales$TW=="Friday"
sales$Weekday<-0
sales$Weekday[!TW_weekend]<-"Weekday"
sales$Weekday[TW_weekend]<-"Weekend"
sales$TW<- factor(sales$TW, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

sales$DN<-0
sales$DN[sales$start_DN=="Day"&sales$end_DN=="Night"]<-"Day_Night"
sales$DN[sales$start_DN=="Night"&sales$end_DN=="Day"]<-"Day"
sales$DN[sales$start_DN=="Day"&sales$end_DN=="Day"]<-"Day"
sales$DN[sales$start_DN=="Night"&sales$end_DN=="Night"]<-"Night"
sales<-na.omit(sales)
sales$lat<-as.numeric(sales$lat)
sales$lng<-as.numeric(sales$lng)
saveRDS(sales,"sales")
rm(sales)
#========== Create Month===========

member$create_month<-substr(member$Create_Time,1,7)
orders$create_month<-substr(orders$cd,1,7)
#shorten list

orders<-select(orders,uid,createtime,create_month,cd,Weekday,DN,week,bookingtime,discountratio,branchname,status_name,amount,bonus,ot_flag,productname,area_detail,duration)
names(orders)<-c("uid","createtime","create_month","cd","Weekday","DN","Create_Time","bookingtime","discountratio","branchname","status_name","amount","bonus","ot_flag","productname","area_detail","duration")

orders$bonus[is.na(orders$bonus)]<-0
orders$bonus<-as.integer(orders$bonus)

orders$time_diff<-as.numeric(difftime(orders$bookingtime,orders$createtime,unit="hours"))
orders$time_diff<- cut(orders$time_diff, 
                       breaks = c(-Inf, 1, 3, 6, 9, 12, 24,Inf), 
                       labels = c("<1 hour", "1-3 hours", "3-6 hours", "6-9 hours", "9-12 hours", "12-24 hours",">24 hours"), 
                       right = FALSE)
#========== First Shopping===========
# first_shopping<-merge(select(orders,uid,createtime,status_name),select(member,uid,Account_Create_Time),by="uid")
# first_shopping%<>%filter(status_name=="Paid"&as.Date(Account_Create_Time)>=as.Date("2015-11-02"))
# first_shopping<-first_shopping[order(first_shopping$createtime),]
# first_shopping<-first_shopping[!duplicated(first_shopping$uid),]
# first_shopping$time_diff<-difftime(first_shopping$createtime,first_shopping$Account_Create_Time,units="days")
# first_shopping$browse_count<-0
# for (i in 1:length(first_shopping$uid)){
#   temp_id<-first_shopping$uid[i]
#   temp_date<-first_shopping$createtime[i]
#   temp<-userlog%>%filter(uid==temp_id&cd<=as.Date(temp_date)&eventname=="product/home")
#   first_shopping$browse_count[i]<-nrow(temp)
# }
# 

#================ process data ===============

#order data
orders<-merge(orders,select(member,uid,Operating_System),by="uid",all.x = TRUE)
orders<-merge(orders,select(branch,branchname,type),by="branchname",all.x =TRUE)
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
saveRDS(MAU_IOS,"MAU_IOS")
rm(MAU_IOS)
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
saveRDS(MAU_ANDROID,"MAU_ANDROID")
rm(MAU_ANDROID)

#set area
member$area<-"台北"
orders<-merge(orders,select(branch,lat,branchname),by="branchname")
Taichung_uid<-aggregate(user_cat["lat"],by=list(user_cat$uid),FUN=mean)%>%filter(lat<24.53&lat>20)
member[member$uid%in%(Taichung_uid$Group.1),"area"]<-"台中"
orders$area<-"台北"
orders[orders$lat<24.53,"area"]<-"台中"

#category
orders$category<-orders$type
orders$category<-factor(orders$type,levels=c("桌遊","密室","摩鐵","商旅","休憩x湯屋","晚鳥過夜","美甲美睫","桌遊x密室","主題酒吧","按摩","中華職棒","電影","咖啡","親子"))
orders$category[(orders$type=="摩鐵"|orders$type=="湯屋"|orders$type=="商旅")&orders$ot_flag==0]<-"休憩x湯屋"
orders$category[(orders$type=="摩鐵"|orders$type=="湯屋"|orders$type=="商旅")&orders$ot_flag==1]<-"晚鳥過夜"
orders$category[(orders$type=="桌遊"|orders$type=="密室")]<-"桌遊x密室"



#Save file
saveRDS(member,"member_data")
saveRDS(orders_member,"orders_member")
saveRDS(orders,"orders")
saveRDS(branch,"branch")
saveRDS(member_birth,"member_birth")
saveRDS(MAU_all,"MAU_all")
saveRDS(stats,"conversion_stats")
proc.time() -ptm


#jdasiod

