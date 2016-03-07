rm(list=ls())
options(scipen=999)
require(XLConnect)
require(magrittr)
require(jsonlite)
require(dplyr)
require(lubridate)

setwd("~/Desktop/ZOEK/BI/Analysis/App")


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
notification<-fromJSON("notification.json")
notice=fromJSON("notices_list.json")
zoeker=read.csv("zoeker.csv",stringsAsFactor=F,header=T)
branch$lng%<>%as.numeric()
branch$lat%<>%as.numeric()

#Notice List
List= c(45,46,68,69,72,73,76,77,86,87,112,113,120,121,123,128,132,134,136,146,147,156,157)
notice<-notice[notice$ntfid%in%List,]%>%select(ntfid,subject,readmark,senttime)
notice$senttime<-as.Date(notice$senttime)
notification<-notification[notification$ntfid%in%List,]%>%select(ntfid,to_value)
notification_stat<-merge(notice,notification,by="ntfid")
notification_stat$to_value%<>%substr(6,8)
#Only Funnow order
orders$amount<-as.numeric(orders$amount)
orders$type<-as.numeric(orders$type)
orders%<>%filter(type==0&amount>100)
#Remove Zoekers
orders=orders[orders$uid%in%zoeker$uid==F,]
member=member[member$uid%in%zoeker$uid==F,]
userlog%<>%select(uid,eventname,createtime,description)
userlog=userlog[userlog$uid%in%zoeker$uid==F,]

#================Manual Notification==================
# notification<-notification[c(6,14,21,29,44,63,69),] 
# notification_stat<-data.frame()
# noti_row<-1
# for (i in 1:length(notification$createtime)){
#   temp<-as.numeric(difftime(userlog$createtime,notification$createtime[i],unit="hours"))
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<-1
#   notification_stat[noti_row,3]<-sum(temp<1&temp>=0)
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<--1
#   notification_stat[noti_row,3]<-sum(temp<0&temp>=(-1))
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<-3
#   notification_stat[noti_row,3]<-sum(temp<3&temp>=1)
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<--3
#   notification_stat[noti_row,3]<-sum(temp<(-1)&temp>=(-3))
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<-5
#   notification_stat[noti_row,3]<-sum(temp<5&temp>=3)
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<--5
#   notification_stat[noti_row,3]<-sum(temp<(-3)&temp>=(-5))
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<-7
#   notification_stat[noti_row,3]<-sum(temp<7&temp>=5)
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<--7
#   notification_stat[noti_row,3]<-sum(temp<(-5)&temp>=(-7))
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<-9
#   notification_stat[noti_row,3]<-sum(temp<9&temp>=7)
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<--9
#   notification_stat[noti_row,3]<-sum(temp<(-7)&temp>=(-9))
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<-12
#   notification_stat[noti_row,3]<-sum(temp<12&temp>=9)
#   noti_row<-noti_row+1
#   notification_stat[noti_row,1]<-i
#   notification_stat[noti_row,2]<--12
#   notification_stat[noti_row,3]<-sum(temp<(-9)&temp>=(-12))
#   noti_row<-noti_row+1
# }
# names(notification_stat)<-c("Event","Hours","Count")
# notification_stat$Event=factor(notification_stat$Event,levels=c(1,2,3,4,5,6,7),labels=c("美式上海","五折美麗心","輕設計旅店下殺","頂級設計","洗車卷","薇閣六折限時三天","薇閣六折倒數三天"))
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
member$cd=as.POSIXct(substr(member$createtime,1,10))
member$createtime=as.POSIXct(member$createtime)
member%<>%filter((createtime>as.POSIXct("2015-11-04")))
member$cd=as.Date(member$cd)

orders$cd=as.POSIXct(substr(orders$createtime,1,10))
orders$createtime=as.POSIXct(orders$createtime)
orders%<>%filter((createtime>as.POSIXct("2015-11-04")))
orders$cd=as.Date(orders$cd)

sales%<>%filter(promostart!="0000-00-00 00:00:00")
sales$cd=as.POSIXct(substr(sales$promostart,1,10))
sales$promostart=as.POSIXct(sales$promostart)
sales%<>%filter((promostart>as.POSIXct("2015-11-04")))
sales$cd=as.Date(sales$cd)

userlog$cd=as.POSIXct(substr((userlog$createtime),1,10))
userlog$createtime=as.POSIXct(userlog$createtime)
userlog%<>%filter((createtime>=as.POSIXct("2015-11-04")))
userlog$cd=as.Date(userlog$cd)

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
orders$status_name[orders$status==2zok]<-"Paid"
orders$status_name[orders$status!=2]<-"Intention"

orders_member<-merge(subset(orders,!duplicated(uid)),member,by="uid",all.x=T)
orders_member$gender=factor(orders_member$gender,levels=c(0,1),labels=c("Female","Male"))
orders_member<-orders_member[!(orders_member$birthday=="0000-00-00")& !is.na(orders_member$birthday),]
orders_member$birthday<-as.Date(as.POSIXct(orders_member$birthday))
orders_member$age<-as.numeric((as.Date(Sys.time())-orders_member$birthday)/365)
orders_member$age<- cut(orders_member$age, 
            breaks = c(-Inf, 15, 20, 25, 30, 35,40,45, Inf), 
            labels = c("<Age 15", "Age 15-20", "Age 20-25", "Age 25-30", "Age 30-35", "Age 35-40","Age 40-45",">Age 45"), 
            right = FALSE)
orders_member$os=toupper(orders_member$os)
orders_member%<>%select(week,gender,age,os,DN,TW,Weekday,branchname,status_name)
names(orders_member)<-c("Create_Time","Gender","Age","Operating_System","Day_Night","Mon_to_Sun","Weekday_Weekend","branchname","status_name")
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
                        breaks = c(-Inf, 15, 20, 25, 30, 35,40,45, Inf), 
                        labels = c("<Age 15", "Age 15-20", "Age 20-25", "Age 25-30", "Age 30-35", "Age 35-40","Age 40-45",">Age 45"), 
                        right = FALSE)

#===============GPS Data===============
branch$lng%<>%as.numeric()
branch$lat%<>%as.numeric()

user_gps<-data.frame()
user_gps_row<-1
user_cat<-data.frame()
user_cat_row<-1
user_pt<-data.frame()
user_pt_row<-1
user_search<-data.frame()
user_search_row<-1
for (i in 1:length(userlog$description)){
  if (userlog$eventname[i]=="auth"){
    if(!is.null(fromJSON(userlog$description[i])$lat)){
      user_gps[user_gps_row,1]<-as.numeric(fromJSON(userlog$description[i])$lat)
      user_gps[user_gps_row,2]<-as.numeric(fromJSON(userlog$description[i])$lng)
      user_gps[user_gps_row,3]<-as.Date((userlog$createtime[i]))
      user_gps_row%<>%+1
    }
  }
  if (userlog$eventname[i]=="product/home"){
    if(!is.null(fromJSON(userlog$description[i])$rpgid)){
      user_cat[user_cat_row,1]<-as.numeric(fromJSON(userlog$description[i])$lat)
      user_cat[user_cat_row,2]<-as.numeric(fromJSON(userlog$description[i])$lng)
      user_cat[user_cat_row,3]<-as.numeric(fromJSON(userlog$description[i])$rpgid)
      user_cat[user_cat_row,4]<-as.numeric(fromJSON(userlog$description[i])$start)
      user_cat[user_cat_row,5]<-as.numeric(fromJSON(userlog$description[i])$forMap)
      user_cat[user_cat_row,6]<-as.Date((userlog$createtime[i]))
      user_cat[user_cat_row,7]<-as.numeric((userlog$uid[i]))
      user_cat_row%<>%+1
    }
  }
  if (userlog$eventname[i]=="product_type/detail"){
    if(!is.null(fromJSON(userlog$description[i])$ptid)){
      user_pt[user_pt_row,1]<-as.numeric(fromJSON(userlog$description[i])$lat)
      user_pt[user_pt_row,2]<-as.numeric(fromJSON(userlog$description[i])$lng)
      user_pt[user_pt_row,3]<-as.numeric(fromJSON(userlog$description[i])$ptid)
      user_pt[user_pt_row,4]<-as.Date((userlog$createtime[i]))
      user_pt[user_pt_row,5]<-as.numeric((userlog$uid[i]))
      user_pt_row%<>%+1
    }
  }
  if (userlog$eventname[i]=="branch/search"){
    user_search[user_search_row,1]<-userlog$uid[i]
    user_search[user_search_row,2]<-userlog$description[i]
    user_search[user_search_row,3]<-as.Date((userlog$createtime[i]))

    user_search_row%<>%+1
  }
}


user_gps<-na.omit(user_gps)
user_cat<-na.omit(user_cat)
user_pt<-na.omit(user_pt)
user_search<-na.omit(user_search)

colnames(user_gps)<-c("lat","lng","createtime")
colnames(user_cat)<-c("lat","lng","rpgid","start","forMap","createtime","uid")
colnames(user_pt)<-c("lat","lng","ptid","createtime","uid")
colnames(user_search)<-c("uid","search","createtime")

user_pt<-user_pt%>%merge(.,select(product_type,ptid,bid),by="ptid")%>%merge(.,select(branch,bid,branchname),by="bid",all.x = T)
user_cat$rpgid[user_cat$rpgid==5]<-1
user_cat$rpgid[user_cat$rpgid==6]<-2
user_cat$rpgid[user_cat$rpgid==7]<-4
user_cat$rpgid=factor(user_cat$rpgid,levels=c(1,2,3,4,5,6,7),labels=c("泡湯x休憩","晚鳥過夜","美甲x美睫","桌遊x吧x密室","泡湯x休憩","晚鳥過夜","桌遊x吧x密室"))
user_cat$forMap=factor(user_cat$forMap,levels=c(0,1),labels=c("List","Map"))
user_cat$week=as.integer(floor((user_cat$createtime-as.numeric(as.Date("2015-11-04")))/7)+1)

user_gps%<>%filter(lat>=25.061579|lat<=25.0566701|lng>=121.5261216|lng<=121.5207947)

#buyers
buyers_gps<-data.frame()
buyers_gps_row<-1
buyers_list<-orders%>%filter(status_name=="Paid")%>%select(uid)

for (i in 1:length(buyers_list$uid)){
  temp<-filter(userlog,uid==buyers_list$uid[i])
  if(length(temp$uid)!=0){
    for (j in 1:length(temp$uid)){
      if (temp$eventname[j]=="auth"){
        if(!is.null(fromJSON(temp$description[j])$lat)){
          buyers_gps[buyers_gps_row,1]<-as.numeric(fromJSON(temp$description[j])$lat)
          buyers_gps[buyers_gps_row,2]<-as.numeric(fromJSON(temp$description[j])$lng)
          buyers_gps[buyers_gps_row,3]<-as.Date((temp$createtime[j]))
          buyers_gps_row%<>%+1
        }
      }
    }
  }
}

colnames(buyers_gps)<-c("lat","lng","createtime")
buyers_gps%<>%filter(lat>=25.061579|lat<=25.0566701|lng>=121.5261216|lng<=121.5207947)

#repeat buyers
rep_buyers_gps<-data.frame()
rep_buyers_gps_row<-1
rep_buyers_list<-orders%>%group_by(uid)%>%summarise(count=n())%>%filter(count>=2)

for (i in 1:length(rep_buyers_list$uid)){
  temp<-filter(userlog,uid==rep_buyers_list$uid[i])
  if(length(temp$uid)!=0){
    for (j in 1:length(temp$uid)){
      if (temp$eventname[j]=="auth"){
        if(!is.null(fromJSON(temp$description[j])$lat)){
          rep_buyers_gps[rep_buyers_gps_row,1]<-as.numeric(fromJSON(temp$description[j])$lat)
          rep_buyers_gps[rep_buyers_gps_row,2]<-as.numeric(fromJSON(temp$description[j])$lng)
          rep_buyers_gps[rep_buyers_gps_row,3]<-as.Date((temp$createtime[j]))
          rep_buyers_gps_row%<>%+1
        }
      }
    }
  }
}

colnames(rep_buyers_gps)<-c("lat","lng","createtime")
rep_buyers_gps%<>%filter(lat>=25.061579|lat<=25.0566701|lng>=121.5261216|lng<=121.5207947)

#===============login Data===============
userlog$week=as.integer(floor((userlog$cd-as.Date("2015-11-04"))/7)+1)
userlog$Login<-0
userlog$Login[userlog$eventname=="logout"]<-"Logout"
userlog$Login[userlog$eventname!="logout"]<-"Login"
userlog=filter(userlog,Login=="Login")
userlog=select(userlog,uid,week,DN,TW,Weekday,cd,eventname)
names(userlog)<-c("uid","Create_Time","Day_Night","Mon_to_Sun","Weekday_Weekend","cd","eventname")

#===============Branch Data===============
branch$area_detail<-branch$area
branch$area<-substr(branch$area,1,3)
branch$category=factor(branch$type,levels=c(0,1,2,3,4,5,6,7,8),labels=c("休憩","休憩","休憩","美甲美睫","密室桌遊bar","密室桌遊bar","密室桌遊bar","密室桌遊bar","按摩"))
branch$type=factor(branch$type,levels=c(0,1,2,3,4,5,6,7,8),labels=c("摩鐵","湯屋","商旅","美甲美睫","密室","桌遊","飛鏢bar","運動bar","按摩"))
branch_gps<-select(branch,lng,lat,category)
branch$week<-as.integer(floor((as.Date(branch$createtime)-as.Date("2015-11-04"))/7)+1)
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
orders<-select(orders,uid,createtime,create_month,cd,Weekday,DN,week,bookingtime,discountratio,branchname,status_name)
names(orders)<-c("uid","createtime","create_month","cd","Weekday","DN","Create_Time","bookingtime","discountratio","branchname","status_name")

#========== First Shopping===========
first_shopping<-merge(select(orders,uid,createtime,status_name),select(member,uid,Account_Create_Time),by="uid")
first_shopping%<>%filter(status_name=="Paid"&as.Date(Account_Create_Time)>=as.Date("2015-11-04"))
first_shopping<-first_shopping[order(first_shopping$createtime),]
first_shopping<-first_shopping[!duplicated(first_shopping$createtime),]
first_shopping$time_diff<-difftime(first_shopping$createtime,first_shopping$Account_Create_Time,units="days")
first_shopping$browse_count<-0
for (i in 1:length(first_shopping$uid)){
  temp_id<-first_shopping$uid[i]
  temp_date<-first_shopping$createtime[i]
  temp<-userlog%>%filter(uid==temp_id&cd<=as.Date(temp_date)&eventname=="auth")
  first_shopping$browse_count[i]<-nrow(temp)
}


#================ process data ===============

#Take out non_member
userlog=userlog[userlog$uid%in%(member%>%filter(Sign_Up=="Sign-up"&Create_Time>=as.Date("2015-11-04"))%$%uid),]


orders<-orders[!is.na(orders$branchname),]
userlog_member<-merge(userlog,select(member,uid,week_create),by="uid",all.x=T)
userlog_member<-userlog_member[!is.na(userlog_member$week_create),]
# Sales Funnel
total_count<-member%>%filter((week_create>=1))%>%group_by(week_create)%>%summarise(n=n())
total_count$Type<-"Total"
member_count<-member%>%filter(Sign_Up=="Sign-up"&week_create>=1)%>%group_by(week_create)%>%summarise(n=n())
member_count$Type<-"Member"
retention_count<-data.frame()
for (i in 1: max(member$week_create)){
  retention_count[i,1]<-i
  retention_count[i,2]<-sum(member$uid[member$week_create==i]%in%unique(filter(userlog_member,Create_Time!=week_create)[,1]))
}
colnames(retention_count)<-c("week_create","n")
retention_count$Type<-"Retention"
paid_count<-data.frame()
for (i in 1: max(member$week_create)){
  paid_count[i,1]<-i
  paid_count[i,2]<-sum(member$uid[member$week_create==i]%in%unique(filter(orders,status_name=="Paid")$uid))
}
colnames(paid_count)<-c("week_create","n")
paid_count$Type<-"Paid"

referral_count<-data.frame()
for (i in 1: max(member$week_create)){
  referral_count[i,1]<-i
  referral_count[i,2]<-sum(member$Invite_Code[member$week_create==i]!="")}
colnames(referral_count)<-c("week_create","n")
referral_count$Type<-"Refferal"

funnel_stat<-rbind(member_count,retention_count,paid_count,referral_count)
funnel_stat$n<-funnel_stat$n/total_count$n

orders$time_diff<-as.numeric(difftime(orders$bookingtime,orders$createtime,unit="hours"))
orders$time_diff<- cut(orders$time_diff, 
                       breaks = c(-Inf, 1, 3, 6, 9, 12, 24,Inf), 
                       labels = c("<1 hour", "1-3 hours", "3-6 hours", "6-9 hours", "9-12 hours", "12-24 hours",">24 hours"), 
                       right = FALSE)

#MAU Data
MAU<-member%>%filter(Create_Time>=as.Date("2015-11-04")&Sign_Up=="Sign-up")%>%group_by(create_month)%>%summarise(n=n())%>%mutate(Cumul=cumsum(n))
MAU<-select(MAU,create_month,Cumul)
temp<-unique(userlog$create_month)
for (i in 1:length(unique(userlog$create_month))){
  MAU[MAU$create_month==temp[i],3]<-userlog%>%filter(create_month==temp[i])%>%subset(!duplicated(uid))%>%nrow()
}
temp<-unique(orders$create_month)
for (i in 1:length(unique(orders$create_month))){
  MAU[MAU$create_month==temp[i],4]<-orders%>%filter(create_month==temp[i]&status_name=="Paid")%>%subset(!duplicated(uid))%>%nrow()
}
names(MAU)<-c("month","Total member","MAU Login","MAU Paid")

#WAU Data
WAU<-member%>%filter(Create_Time>=as.Date("2015-11-04")&Sign_Up=="Sign-up")%>%group_by(week_create)%>%summarise(n=n())%>%mutate(Cumul=cumsum(n))
WAU<-select(WAU,week_create,Cumul)
temp<-unique(userlog$Create_Time)
for (i in 1:length(unique(userlog$Create_Time))){
  WAU[WAU$week_create==temp[i],3]<-userlog%>%filter(Create_Time==temp[i])%>%subset(!duplicated(uid))%>%nrow()
}

for (i in 1: length(unique(userlog$Create_Time))){
  WAU[WAU$week_create==temp[i],4]<-userlog_member%>%filter(Create_Time==temp[i]&(Create_Time!=week_create))%>%subset(!duplicated(uid))%>%nrow()
}

temp<-unique(orders$Create_Time)
for (i in 1:length(unique(orders$Create_Time))){
  WAU[WAU$week_create==temp[i],5]<-orders%>%filter(Create_Time==temp[i]&status_name=="Intention")%>%subset(!duplicated(uid))%>%nrow()
}
for (i in 1:length(unique(orders$Create_Time))){
  WAU[WAU$week_create==temp[i],6]<-orders%>%filter(Create_Time==temp[i]&status_name=="Paid")%>%subset(!duplicated(uid))%>%nrow()
}

names(WAU)<-c("week","Total_member","WAU_Login","WAU_Rep_Login","WAU_Intention","WAU_Paid")


#Notification
push_id<-unique(notification_stat$ntfid)
push_list<-data.frame()
for (i in 1:length(push_id)){
  temp<-notification_stat[notification_stat$ntfid==push_id[i],]
  push_list[i,1]<-as.Date(temp$senttime[1] , "%m/%d/%y")
  push_list[i,2]<-temp$subject[1]
  push_list[i,3]<-temp$to_value[1]
  temp<-temp%>%group_by(readmark)%>%summarise(n=n())
  push_list[i,4]<-temp[temp$readmark==1,2]
  push_list[i,5]<-temp[temp$readmark==0,2]
  if(length(temp[temp$readmark==2,2])==1){push_list[i,6]<-0}else{push_list[i,6]<-temp[temp$readmark==2,2]}
  push_list[i,7]<-push_list[i,4]/(push_list[i,4]+push_list[i,5]+push_list[i,6])
}

colnames(push_list)<-c("Date","Subject","OS","Read","Not Read","Delete","Read Rate")
push_list<-push_list[order(push_list$Date),]


#Save file
saveRDS(userlog,"login")
saveRDS(member,"member_data")
saveRDS(user_gps,"User_GPS")
saveRDS(rep_buyers_gps,"rep_buyers_gps")
saveRDS(buyers_gps,"buyers_gps")
saveRDS(branch_gps,"branch_GPS")
saveRDS(user_cat,"user_cat")
saveRDS(user_pt,"user_pt")
saveRDS(user_search,"user_search")
saveRDS(orders_member,"orders_member")
saveRDS(orders,"orders")
saveRDS(branch,"branch")
saveRDS(sales,"sales")
saveRDS(notification_stat,"notification_stat")
saveRDS(first_shopping,"first_shopping")
saveRDS(userlog_member,"userlog_member")
saveRDS(funnel_stat,"funnel_stat")
saveRDS(MAU,"MAU")
saveRDS(WAU,"WAU")
saveRDS(push_list,"push_list")
saveRDS(member_birth,"member_birth")



