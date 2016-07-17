library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
require(ggmap)
require(XLConnect)
require(magrittr)
require(zoo)

setwd("~/Desktop/ZOEK/BI/Data_Analysis/App")

member<-readRDS("member_data")
#member_birth<-readRDS("member_birth")
login<-readRDS("login")
orders_member<-readRDS("orders_member")
orders<-readRDS("orders")
branch<-readRDS("branch")
sales<-readRDS("sales")
user_cat<-readRDS("user_cat")
user_pt<-readRDS("user_pt")
user_search<-readRDS("user_search")
first_shopping<-readRDS("first_shopping")
userlog_member<-readRDS("userlog_member")
MAU<-readRDS("MAU_all")
MAU_IOS<-readRDS("MAU_IOS")
MAU_ANDROID<-readRDS("MAU_ANDROID")

# Shiny Main Program
shinyServer(function(input, output) {
      #===============Data================
      # Member Data
          dataset_member <- reactive({
              {   week_start<-(floor(input$dates[1]-as.Date("2015-11-04"))/7)+1
                  week_end<-(floor(input$dates[2]-as.Date("2015-11-04"))/7)+1
                  if(input$y=="Gender"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end)&(Register_Type!="GUEST"))%>%group_by(week_create,Gender)%>%dplyr::summarise(n=n())%>%group_by(Gender)%>%mutate(Cumul=cumsum(n))
                  }
                  else if(input$y=="Operating_System"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end)&(Register_Type!="GUEST"))%>%group_by(week_create,Operating_System)%>%dplyr::summarise(n=n())%>%group_by(Operating_System)%>%mutate(Cumul = cumsum(n))
                  }
                  else if(input$y=="Register_Type"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end)&(Register_Type!="GUEST"))%>%group_by(week_create,Register_Type)%>%dplyr::summarise(n=n())%>%group_by(Register_Type)%>%mutate(Cumul = cumsum(n))
                  }
                  else if(input$y=="Sign_Up"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end))%>%group_by(week_create,Sign_Up)%>%dplyr::summarise(n=n())%>%group_by(Sign_Up)%>%mutate(Cumul = cumsum(n))
                  }
                  else if(input$y=="Total_Member"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end)&(Register_Type!="GUEST"))%>%group_by(week_create)%>%dplyr::summarise(n=n())%>%mutate(Cumul = cumsum(n))
                  }
              }
           
          })
  # Login Data
          dataset_login <- reactive({
            { week_start<-(floor(input$dates_L[1]-as.Date("2015-11-04"))/7)+1
              week_end<-(floor(input$dates_L[2]-as.Date("2015-11-04"))/7)+1
              if(input$y_L=="Day_Night"){
                  login%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&eventname=="product/home")%>%group_by(Create_Time,Day_Night)%>%dplyr::summarise(n=n())%>%group_by(Day_Night)%>%mutate(Cumul=cumsum(n))
              }
              else if(input$y_L=="Mon_to_Sun"){
                  login%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&eventname=="product/home")%>%group_by(Create_Time,Mon_to_Sun)%>%dplyr::summarise(n=n())%>%group_by(Mon_to_Sun)%>%mutate(Cumul = cumsum(n))
              }
              else if(input$y_L=="Weekday_Weekend"){
                  login%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&eventname=="product/home")%>%group_by(Create_Time,Weekday_Weekend)%>%dplyr::summarise(n=n())%>%group_by(Weekday_Weekend)%>%mutate(Cumul = cumsum(n))
              }
            }
            
          })
          
          # Orders Data
          dataset_orders_member <- reactive({
            { week_start<-(floor(input$dates_O[1]-as.Date("2015-11-04"))/7)+1
            week_end<-(floor(input$dates_O[2]-as.Date("2015-11-04"))/7)+1
            if(input$y_O=="Gender"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Gender)%>%dplyr::summarise(n=n())%>%group_by(Gender)%>%mutate(Cumul=cumsum(n))
            }
            else if(input$y_O=="Day_Night"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Day_Night)%>%dplyr::summarise(n=n())%>%group_by(Day_Night)%>%mutate(Cumul=cumsum(n))
            }
            else if(input$y_O=="Age"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Age)%>%dplyr::summarise(n=n())%>%group_by(Age)%>%mutate(Cumul=cumsum(n))
            }
            else if(input$y_O=="Operating_System"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Operating_System)%>%dplyr::summarise(n=n())%>%group_by(Operating_System)%>%mutate(Cumul=cumsum(n))
            }
            else if(input$y_O=="Mon_to_Sun"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Mon_to_Sun)%>%dplyr::summarise(n=n())%>%group_by(Mon_to_Sun)%>%mutate(Cumul = cumsum(n))
            }
            else if(input$y_O=="Weekday_Weekend"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Weekday_Weekend)%>%dplyr::summarise(n=n())%>%group_by(Weekday_Weekend)%>%mutate(Cumul = cumsum(n))
            }
            else if(input$y_O=="Rep"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Rep)%>%dplyr::summarise(n=n())%>%mutate(Cumul = cumsum(n))
            }
            else if(input$y_O=="Total_Order"){
                merge(orders,select(member,uid))%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time)%>%dplyr::summarise(n=n())%>%mutate(Cumul = cumsum(n))
            }
            }
            
          })
          
          # Merchant Data
          dataset_Merchant_Top <- reactive({
              week_start<-floor((input$dates_M[1]-as.Date("2015-11-04"))/7)+1
              week_end<-floor((input$dates_M[2]-as.Date("2015-11-04"))/7)+1
              temp<-orders%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(branchname)%>%dplyr::summarise(n=n())
              temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
              temp<-temp[order(-temp$n),]
              colnames(temp)<-c("商家","購買次數","地區")
              temp
          })
          dataset_Merchant_Line <- reactive({
              branch%>%group_by(week)%>%dplyr::summarise(n=n())%>%mutate(Cumul = cumsum(n))
          })
          # Sales Data
          dataset_Sales <- reactive({
              temp<-sales%>%filter((cd>=input$dates_M[1])&(cd<=input$dates_M[2]))%$%as.data.frame.matrix(table(Weekday,DN))
              temp$Day%<>%+temp$Day_Night
              temp$Night%<>%+temp$Day_Night
              select(temp,Day,Night)
          })
          dataset_Orders_sub <- reactive({
              orders%>%filter((cd>=input$dates_M[1])&(cd<=input$dates_M[2])&status_name=="Paid")%$%as.data.frame.matrix(table(Weekday,DN))
          })
          # Behavior Data
          dataset_cat<-reactive({
              user_cat%>%filter((createtime>=input$dates_B[1])&(createtime<=input$dates_B[2]))
            })
          dataset_view<-reactive({
              temp<-user_cat%>%filter((createtime>=input$dates_B[1])&(createtime<=input$dates_B[2]))
              mean(as.integer(select(temp,start)[,1]))/10+1
          })
          dataset_pt<-reactive({
              user_pt%>%filter((createtime>=input$dates_B[1])&(createtime<=input$dates_B[2]))
          })
          dataset_search<-reactive({
              user_search%>%filter((createtime>=input$dates_B[1])&(createtime<=input$dates_B[2]))
          })
          order_browsing<-reactive({
            orders%>%filter((cd>=input$dates_B[1])&(cd<=input$dates_B[2]))
          })
          #orders time
          dataset_orders_time <- reactive({
            temp_order_time<-filter(orders,(cd>=input$dates_order_time[1])&(cd<=input$dates_order_time[2])&status_name=="Paid")
            if(input$order_weekday=="ALL"){
              if(input$order_cat=="ALL"){
                temp_order_time
              }else{
                temp_order_time%>%filter(type==input$order_cat)
              }
            } else{
              temp_order_time%<>%filter(Weekday==input$order_weekday)
              if(input$order_cat=="ALL"){
                temp_order_time
              }else{
                temp_order_time%>%filter(type==input$order_cat)
              }
            }
          })
          #===============Table==============
          output$Order_demography<-renderTable({
              orders_member%>%filter(status_name=="Paid")%>%group_by(Gender,Age)%>%dplyr::summarise(n=n()) 
          })
          output$Top_summary<-renderTable({
        
              dataset_Merchant_Top()
          })
          
          output$Sales_summary<-renderTable({
            
              dataset_Sales()
          
          })
          
          output$Orders_sub_summary<-renderTable({
            
              dataset_Orders_sub()
            
          })
          #Cohort
          output$Cohort_plot<-renderTable({
              cohort_date<-data.frame()
              cohort<-data.frame()
              row<-1
              for (i in min(login$Create_Time):max(login$Create_Time)){
                  col<-1
                  for (j in i:max(login$Create_Time)){
                      #temp<-filter(userlog,Create_Time==i)
                      cohort[row,col]<-sum(member$uid[member$week_create==i&member$Sign_Up=="Sign-up"]%in%unique(login[login$Create_Time==j,1]))
                      col%<>%+1
                  }
                  row%<>%+1
              }
              cohort<-(cohort/cohort[,1])
              row<-1
              for(i in min(login$Create_Time):max(login$Create_Time)){
                  cohort_date[row,1]<-paste(as.Date("2015-11-04")+7*(i-1))
                  rownames(cohort_date)[row]<-paste("Week",i,sep=" ")
                  colnames(cohort)[row]<-paste("Week",row,sep=" ")
                  row%<>%+1
              }
              colnames(cohort_date)<-"Date"
              cbind(cohort_date,cohort)
            
          })
          

          
          output$Cohort_Spent<-renderTable({
            cohort_date_spent<-data.frame()
            cohort_spent<-data.frame()
            temp_month<-unique(member$create_month)
            temp_month<-temp_month[order(temp_month)]
            for (i in 1:length(unique(member$create_month))){
              col<-2
              cohort_spent[i,1]<-nrow(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up"))
              for (j in i:length(unique(member$create_month))){
                temp<-orders[orders$uid%in%(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up")%$%uid),]
                if(input$fun_pi){
                  cohort_spent[i,col]<-(sum(temp%>%filter(create_month<=temp_month[j]&status_name=="Paid")%$%amount)*0.15-sum(temp%>%filter(create_month<=temp_month[j]&status_name=="Paid")%$%bonus))
                }else{
                  cohort_spent[i,col]<-(sum(temp%>%filter(create_month<=temp_month[j]&status_name=="Paid")%$%amount))*0.15
                }
                colnames(cohort_spent)[1]<-paste("base")
                colnames(cohort_spent)[j+1]<-paste("month",j,sep=" ")
                col%<>%+1
              }
            }
            cohort_spent_percentage<-cohort_spent
            cohort_spent_percentage[,2:ncol(cohort_spent_percentage)]<-(cohort_spent[,2:ncol(cohort_spent)]/cohort_spent[,1])
            
  
            for (i in 1:length(unique(member$create_month))){
              cohort_date_spent[i,1]<-paste("month",i,sep=" ")
            }
            colnames(cohort_date_spent)<-"Date"
            if(input$LTV){
              cbind(cohort_date_spent,cohort_spent_percentage)
            }else{
              cbind(cohort_date_spent,cohort_spent)
              
            }
            
          })
          
          output$Cohort_conversion_plot<-renderTable({
            
            cohort_date_c<-data.frame()
            cohort_c<-data.frame()
            row<-1
            for (i in min(login$Create_Time):max(login$Create_Time)){
              col<-2
              cohort_c[row,1]<-nrow(filter(member,week_create==i&Sign_Up=="Sign-up"&area==input$cohort_area))
              for (j in i:max(login$Create_Time)){
                temp<-filter(orders,Create_Time<=j&Create_Time>j-1&status_name=="Paid"&area==input$cohort_area)
                cohort_c[row,col]<-sum(member$uid[member$week_create==i&member$Sign_Up=="Sign-up"&member$area==input$cohort_area]%in%temp$uid)
                col%<>%+1
              }
              row%<>%+1
            }
            cohort_c[,2:ncol(cohort_c)]<-(cohort_c[,2:ncol(cohort_c)]/cohort_c[,1])
            
            row<-1
            colnames(cohort_c)[1]<-paste("base")
            for(i in min(login$Create_Time):max(login$Create_Time)){
              cohort_date_c[row,1]<-paste(as.Date("2015-11-04")+7*(i-1))
              rownames(cohort_date_c)[row]<-paste("Week",i,sep=" ")
              colnames(cohort_c)[row+1]<-paste("Week",row,sep=" ")
              row%<>%+1
            }
            
            colnames(cohort_date_c)<-"Date"
            cbind(cohort_date_c,cohort_c)
            
          },digit=3)
          output$Cohort_conversion_month<-renderTable({
            cohort_date_cm<-data.frame()
            cohort_cm<-data.frame()
            temp_month<-unique(member$create_month)
            temp_month<-temp_month[order(temp_month)]
            for (i in 1:length(unique(member$create_month))){
              col<-2
              cohort_cm[i,1]<-nrow(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up"))
              for (j in i:length(unique(member$create_month))){
                temp<-orders[orders$uid%in%(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up")%$%uid),]
                cohort_cm[i,col]<-length(unique(temp%>%filter(create_month==temp_month[j]&status_name=="Paid")%$%uid))
                colnames(cohort_cm)[1]<-paste("base")
                colnames(cohort_cm)[j+1]<-paste("month",j,sep=" ")
                col%<>%+1
              }
            }
            
            if(input$cm_percentage){
              cohort_cm[,2:ncol(cohort_cm)]<-(cohort_cm[,2:ncol(cohort_cm)]/cohort_cm[,1])
            }
            
            for (i in 1:length(unique(member$create_month))){
              cohort_date_cm[i,1]<-paste(temp_month[i])
            }
            colnames(cohort_date_cm)<-"Date"
            
            cbind(cohort_date_cm,cohort_cm)
          },digit=3)
          
          output$Conversion_trend<-renderTable({
            cohort_date_ct<-data.frame()
            cohort_ct<-data.frame()
            member_count<-data.frame()
            temp_month<-unique(member$create_month)
            temp_month<-temp_month[order(temp_month)]
            for (i in 1:length(unique(member$create_month))){
              temp_old<-orders[orders$uid%in%(filter(member,create_month<temp_month[i]&Sign_Up=="Sign-up")%$%uid),]
              temp_new<-orders[orders$uid%in%(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up")%$%uid),]
              member_count[i,1]<-nrow(filter(member,create_month<temp_month[i]&Sign_Up=="Sign-up"))
              member_count[i,2]<-nrow(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up"))
              cohort_ct[i,1]<-length(unique(temp_old%>%filter(create_month==temp_month[i]&status_name=="Paid")%$%uid))
              cohort_ct[i,2]<-length(unique(temp_new%>%filter(create_month==temp_month[i]&status_name=="Paid")%$%uid))
              
            }
            colnames(member_count)<-c("Old","New")
            colnames(cohort_ct)<-c("Old conversion","New conversion")
            if(input$cm_percentage){
              cohort_ct<-(cohort_ct/member_count)
            }
            
            for (i in 1:length(unique(member$create_month))){
              cohort_date_ct[i,1]<-paste(temp_month[i])
            }
            colnames(cohort_date_ct)<-"Date"
            
            cbind(cohort_date_ct,member_count,cohort_ct)
          },digit=3)
          
          output$Cohort_buy_size<-renderTable({
            cohort_date_bs<-data.frame()
            cohort_bs<-data.frame()
            temp_month<-unique(member$create_month)
            temp_month<-temp_month[order(temp_month)]
            for (i in 1:length(unique(member$create_month))){
              col<-2
              cohort_bs[i,1]<-nrow(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up"))
              for (j in i:length(unique(member$create_month))){
                temp<-orders[orders$uid%in%(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up")%$%uid),]
                cohort_bs[i,col]<-(sum(temp%>%filter(create_month==temp_month[j]&status_name=="Paid")%$%amount))/nrow(temp%>%filter(create_month==temp_month[j]&status_name=="Paid"))
                colnames(cohort_bs)[1]<-paste("base")
                colnames(cohort_bs)[j+1]<-paste("month",j,sep=" ")
                col%<>%+1
              }
            }
            
            
            for (i in 1:length(unique(member$create_month))){
              cohort_date_bs[i,1]<-paste(temp_month[i])
            }
            colnames(cohort_date_bs)<-"Date"
            
            cbind(cohort_date_bs,cohort_bs)
          })
          output$Cohort_repeat<-renderTable({
            cohort_date_rep<-data.frame()
            cohort_rep<-data.frame()
            temp_month<-unique(member$create_month)
            temp_month<-temp_month[order(temp_month)]
            for (i in 1:length(unique(member$create_month))){
              col<-2
              cohort_rep[i,1]<-(nrow(orders[orders$uid%in%(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up")%$%uid),]%>%filter(status_name=="Paid")))
              for (j in i:length(unique(member$create_month))){
                temp<-orders[orders$uid%in%(filter(member,create_month==temp_month[i]&Sign_Up=="Sign-up")%$%uid),]
                cohort_rep[i,col]<-(mean((temp%>%filter(create_month==temp_month[j]&status_name=="Paid")%>%group_by(uid)%>%summarise(n=n()))%$%n))
                colnames(cohort_rep)[1]<-paste("orders")
                colnames(cohort_rep)[j+1]<-paste("month",j,sep=" ")
                col%<>%+1
              }
            }
            
            
            for (i in 1:length(unique(member$create_month))){
              cohort_date_rep[i,1]<-paste(temp_month[i])
            }
            colnames(cohort_date_rep)<-"Date"
            
            cbind(cohort_date_rep,cohort_rep)
          })
          
          output$Repeat_trend<-renderTable({
            cohort_date_rt<-data.frame()
            cohort_rt<-data.frame()
            temp_month<-unique(member$create_month)
            temp_month<-temp_month[order(temp_month)]
            for (i in 1:length(unique(member$create_month))){
              col<-2
              cohort_rt[i,1]<-(nrow(orders%>%filter(create_month==temp_month[i]&status_name=="Paid")%>%group_by(uid)%>%summarise(n=n())))
              cohort_rt[i,2]<-(mean((orders%>%filter(create_month==temp_month[i]&status_name=="Paid")%>%group_by(uid)%>%summarise(n=n()))%$%n))
            }
            
            colnames(cohort_rt)<-c("orders","Rep")
            for (i in 1:length(unique(member$create_month))){
              cohort_date_rt[i,1]<-paste(temp_month[i])
            }
            colnames(cohort_date_rt)<-"Date"
            
            cbind(cohort_date_rt,cohort_rt)
          })
          output$Repeat_ratio_trend<-renderTable({
            cohort_date_rtt<-data.frame()
            cohort_rtt<-data.frame()
            temp_month<-unique(member$create_month)
            temp_month<-temp_month[order(temp_month)]
            for (i in 1:length(unique(member$create_month))){
              col<-2
              temp<-orders%>%filter(create_month<=temp_month[i]&status_name=="Paid")%>%group_by(uid)%>%summarise(n=n())
              cohort_rtt[i,1]<-nrow(temp%>%filter(n>=2))/nrow(temp)
            }
            
            colnames(cohort_rtt)<-c("Rep_ratio")
            for (i in 1:length(unique(member$create_month))){
              cohort_date_rtt[i,1]<-paste(temp_month[i])
            }
            colnames(cohort_date_rtt)<-"Date"
            
            cbind(cohort_date_rtt,cohort_rtt)
          })
          # output$Cohort_plot_Man_25<-renderTable({
          #   temp_member<-filter(member_birth,age=="Age 25-30"&Gender=="Male")
          #   cohort<-data.frame()
          #   for (i in 1:max(temp_member$week_create)){
          #       col<-1
          #       for (j in i:max(userlog_member$Create_Time)){
          #         temp<-filter(userlog_member,week_create==i)
          #         cohort[i,col]<-sum(temp_member$uid[temp_member$week_create==i&temp_member$Sign_Up=="Sign-up"]%in%unique(temp[temp$Create_Time==j,1]))
          #         colnames(cohort)[j]<-paste("Week",j,sep=" ")
          #         col%<>%+1
          #       }
          #   }
          #   cohort<-(cohort/cohort[,1])
          #   for (i in 1:max(temp_member$week_create)){
          #     rownames(cohort)[i]<-paste("Week",i,sep=" ")
          #   }
          #   for (i in 1:max(userlog_member$week_create)){
          #     rownames(cohort)[j]<-paste("Week",j,sep=" ")
          #   }
          #   cohort
          #   
          # })
          # 
          # output$Cohort_plot_Man_30<-renderTable({
          #     temp_member<-filter(member_birth,age=="Age 30-35"&Gender=="Male")
          #     cohort<-data.frame()
          #     for (i in 1:max(temp_member$week_create)){
          #       col<-1
          #       for (j in i:max(userlog_member$Create_Time)){
          #         temp<-filter(userlog_member,week_create==i)
          #         cohort[i,col]<-sum(temp_member$uid[temp_member$week_create==i&temp_member$Sign_Up=="Sign-up"]%in%unique(temp[temp$Create_Time==j,1]))
          #         colnames(cohort)[j]<-paste("Week",j,sep=" ")
          #         col%<>%+1
          #       }
          #     }
          #     cohort<-(cohort/cohort[,1])
          #     for (i in 1:max(temp_member$week_create)){
          #       rownames(cohort)[i]<-paste("Week",i,sep=" ")
          #     }
          #     for (i in 1:max(userlog_member$week_create)){
          #       rownames(cohort)[j]<-paste("Week",j,sep=" ")
          #     }
          #     cohort
          #   
          # })
          # 
          # output$Cohort_plot_Female_25<-renderTable({
          #   temp_member<-filter(member_birth,age=="Age 25-30"&Gender=="Female")
          #   cohort<-data.frame()
          #   for (i in 1:max(temp_member$week_create)){
          #     col<-1
          #     for (j in i:max(userlog_member$Create_Time)){
          #       temp<-filter(userlog_member,week_create==i)
          #       cohort[i,col]<-sum(temp_member$uid[temp_member$week_create==i&temp_member$Sign_Up=="Sign-up"]%in%unique(temp[temp$Create_Time==j,1]))
          #       colnames(cohort)[j]<-paste("Week",j,sep=" ")
          #       col%<>%+1
          #     }
          #   }
          #   cohort<-(cohort/cohort[,1])
          #   for (i in 1:max(temp_member$week_create)){
          #     rownames(cohort)[i]<-paste("Week",i,sep=" ")
          #   }
          #   for (i in 1:max(userlog_member$week_create)){
          #     rownames(cohort)[j]<-paste("Week",j,sep=" ")
          #   }
          #   cohort
          #   
          # })
          # 
          # output$Cohort_plot_Female_30<-renderTable({
          #   temp_member<-filter(member_birth,age=="Age 30-35"&Gender=="Female")
          #   cohort<-data.frame()
          #   for (i in 1:max(temp_member$week_create)){
          #     col<-1
          #     for (j in i:max(userlog_member$Create_Time)){
          #       temp<-filter(userlog_member,week_create==i)
          #       cohort[i,col]<-sum(temp_member$uid[temp_member$week_create==i&temp_member$Sign_Up=="Sign-up"]%in%unique(temp[temp$Create_Time==j,1]))
          #       colnames(cohort)[j]<-paste("Week",j,sep=" ")
          #       col%<>%+1
          #     }
          #   }
          #   cohort<-(cohort/cohort[,1])
          #   for (i in 1:max(temp_member$week_create)){
          #     rownames(cohort)[i]<-paste("Week",i,sep=" ")
          #   }
          #   for (i in 1:max(userlog_member$week_create)){
          #     rownames(cohort)[j]<-paste("Week",j,sep=" ")
          #   }
          #   cohort
          #   
          # })
          output$Orders_Sales_summary<-renderTable({
            
            (dataset_Orders_sub()/dataset_Sales()*100)
            
          })

          output$Cat_Top<-renderTable({
              temp<-dataset_cat()%>%filter(start==0)%>%group_by(rpgid)%>%dplyr::summarise(n=n())
              names(temp)<-c("Category","Tot_views")
              temp[order(-temp$Tot_views),]

          })
          
          output$Product_Top_rest<-renderTable({
            temp<-dataset_pt()%>%filter(category=="休憩")%>%group_by(branchname)%>%dplyr::summarise(n=n())
            names(temp)<-c("branchname","Views")
            temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
            temp<-temp[order(-temp$Views),]
            temp$order_count<-0
            temp$intention_count<-0
            for (i in 1:length(temp$branchname)){
              temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
              temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
            }
            temp$order_percentage<-temp$order_count/temp$Views
            temp$intention_percentage<-temp$intention_count/temp$Views
            names(temp)<-c("Merchants","Views","地區","銷售","有意圖","銷售比例","有意圖比例")
            temp
          })
          output$Product_Top_massage<-renderTable({
            temp<-dataset_pt()%>%filter(category=="按摩")%>%group_by(branchname)%>%dplyr::summarise(n=n())
            names(temp)<-c("branchname","Views")
            temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
            temp<-temp[order(-temp$Views),]
            temp$order_count<-0
            temp$intention_count<-0
            for (i in 1:length(temp$branchname)){
              temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
              temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
            }
            temp$order_percentage<-temp$order_count/temp$Views
            temp$intention_percentage<-temp$intention_count/temp$Views
            names(temp)<-c("Merchants","Views","地區","銷售","有意圖","銷售比例","有意圖比例")
            temp[1:30,]
          })
          output$Product_Top_manicure<-renderTable({
            temp<-dataset_pt()%>%filter(category=="美甲美睫")%>%group_by(branchname)%>%dplyr::summarise(n=n())
            names(temp)<-c("branchname","Views")
            temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
            temp<-temp[order(-temp$Views),]
            temp$order_count<-0
            temp$intention_count<-0
            for (i in 1:length(temp$branchname)){
              temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
              temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
            }
            temp$order_percentage<-temp$order_count/temp$Views
            temp$intention_percentage<-temp$intention_count/temp$Views
            names(temp)<-c("Merchants","Views","地區","銷售","有意圖","銷售比例","有意圖比例")
            temp[1:20,]
          })
          output$Product_Top_escape<-renderTable({
            temp<-dataset_pt()%>%filter(category=="密室")%>%group_by(branchname)%>%dplyr::summarise(n=n())
            names(temp)<-c("branchname","Views")
            temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
            temp<-temp[order(-temp$Views),]
            temp$order_count<-0
            temp$intention_count<-0
            for (i in 1:length(temp$branchname)){
              temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
              temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
            }
            temp$order_percentage<-temp$order_count/temp$Views
            temp$intention_percentage<-temp$intention_count/temp$Views
            names(temp)<-c("Merchants","Views","地區","銷售","有意圖","銷售比例","有意圖比例")
            temp[1:10,]
          })
          output$Product_Top_board<-renderTable({
            temp<-dataset_pt()%>%filter(category=="桌遊")%>%group_by(branchname)%>%dplyr::summarise(n=n())
            names(temp)<-c("branchname","Views")
            temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
            temp<-temp[order(-temp$Views),]
            temp$order_count<-0
            temp$intention_count<-0
            for (i in 1:length(temp$branchname)){
              temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
              temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
            }
            temp$order_percentage<-temp$order_count/temp$Views
            temp$intention_percentage<-temp$intention_count/temp$Views
            names(temp)<-c("Merchants","Views","地區","銷售","有意圖","銷售比例","有意圖比例")
            temp[1:10,]
          })
          output$Product_Top_bar<-renderTable({
            temp<-dataset_pt()%>%filter(category=="主題酒吧")%>%group_by(branchname)%>%dplyr::summarise(n=n())
            names(temp)<-c("branchname","Views")
            temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
            temp<-temp[order(-temp$Views),]
            temp$order_count<-0
            temp$intention_count<-0
            for (i in 1:length(temp$branchname)){
              temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
              temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
            }
            temp$order_percentage<-temp$order_count/temp$Views
            temp$intention_percentage<-temp$intention_count/temp$Views
            names(temp)<-c("Merchants","Views","地區","銷售","有意圖","銷售比例","有意圖比例")
            temp[1:20,]
          })
          output$Search_Top<-renderTable({
            temp<-dataset_search()%>%group_by(search)%>%dplyr::summarise(n=n())
            names(temp)<-c("Search","Counts")
            temp<-temp[order(-temp$Counts),]
            temp<-na.omit(temp)
            temp$user_order<-0
            for (i in 1:length(temp$Search)){
              temp$user_order[i]<-sum((dataset_search()%>%filter(search==temp$Search[i])%$%uid)%in%(orders%>%filter(status_name=="Paid")%$%uid))
            }
            temp[1:20,]
            
          })
          output$Ave_items<-renderTable({
            temp<-data.frame()
            temp[1,1]<-(dataset_view())
            names(temp)<-c("Ave pages view")
            temp
          })
          #===============Plot================ 
          output$Member_plot <- renderPlot({
            
            if(input$plot_type=="Line"){
              if(input$y!="Total_Member"){
                if(input$cum==F){
                  p <- ggplot(dataset_member(), aes_string(x=input$x, y="n",color=input$y))+
                        geom_line()+
                        labs(y="Freq",x="week")+
                        theme(panel.grid.minor.x=element_blank())+
                        geom_text(data=dataset_member(),aes(label=n))+scale_x_continuous(breaks=seq(0, 52, 1))
                }
                else if(input$cum==T){
                  p <- ggplot(dataset_member(), aes_string(x=input$x, y="Cumul",color=input$y))+
                    geom_line()+
                    labs(y="Freq",x="week")+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_member(),aes(label=Cumul))+scale_x_continuous(breaks=seq(0, 52, 1))
                }
              }
              else{
                if(input$cum==F){
                  p <- ggplot(dataset_member(), aes_string(x=input$x, y="n")) + geom_line()+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_member(),aes(label=n))+scale_x_continuous(breaks=seq(0, 52, 1))
                }
                else if(input$cum==T){
                  p <- ggplot(dataset_member(), aes_string(x=input$x, y="Cumul"))+
                    geom_line()+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_member(),aes(label=Cumul))+scale_x_continuous(breaks=seq(0, 52, 1))
                }
              }
              p
            } else if (input$plot_type=="Stack_Area"){
              if(input$y!="Total_Member"){
                if(input$cum==F){
                  p <- ggplot(dataset_member(), aes_string(x=input$x,y="n",group=input$y,fill=input$y)) + 
                    geom_area(position="fill")+
                    labs(y="Percentage",x="week")+scale_x_continuous(breaks=seq(0, 52, 1))
                }
                else if(input$cum==T){
                  p <- ggplot(dataset_member(), aes_string(x=input$x,y="Cumul",group=input$y,fill=input$y)) + 
                    geom_area(position="fill")+
                    labs(y="Percentage",x="week")+scale_x_continuous(breaks=seq(0, 52, 1))
                }
              }
              else{
                if(input$cum==F){
                  p <- ggplot(dataset_member(), aes_string(x=input$x, y="n")) + geom_line()+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_member(),aes(label=n))+scale_x_continuous(breaks=seq(0, 52, 1))
                }
                else if(input$cum==T){
                  p <- ggplot(dataset_member(), aes_string(x=input$x, y="Cumul"))+
                    geom_line()+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_member(),aes(label=Cumul))+scale_x_continuous(breaks=seq(0, 52, 1))
                }
              }
              p
            }
            
            
          })
          
          output$Login_plot <- renderPlot({
            
            if(input$plot_type_L=="Line"){
                if(input$cum_L==F){
                  p <- ggplot(dataset_login(), aes_string(x=input$x_L, y="n",color=input$y_L))+
                    geom_line()+
                    labs(y="Freq",x="week")+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_login(),aes(label=n))+
                    theme_grey(base_family = "STKaiti")+scale_x_continuous(breaks=seq(0, 52, 1))
                }
                else if(input$cum_L==T){
                  p <- ggplot(dataset_login(), aes_string(x=input$x_L, y="Cumul",color=input$y_L))+
                    geom_line()+
                    labs(y="Freq",x="week")+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_login(),aes(label=Cumul))+
                    theme_grey(base_family = "STKaiti")+scale_x_continuous(breaks=seq(0, 52, 1))
                }
              p
            } else if (input$plot_type_L=="Stack_Area"){
              
                if(input$cum_L==F){
                  p <- ggplot(dataset_login(), aes_string(x=input$x_L,y="n",group=input$y_L,fill=input$y_L)) + 
                    geom_area(position="fill")+
                    labs(y="Percentage",x="Week")+
                    theme_grey(base_family = "STKaiti")+scale_x_continuous(breaks=seq(0, 52, 1))
                }
                else if(input$cum_L==T){
                  p <- ggplot(dataset_login(), aes_string(x=input$x_L,y="Cumul",group=input$y_L,fill=input$y_L)) + 
                    geom_area(position="fill")+
                    labs(y="Percentage",x="Week")+
                    theme_grey(base_family = "STKaiti")+scale_x_continuous(breaks=seq(0, 52, 1))
                }
              p
            }
            
            
          })
          #Orders Plot
          output$Orders_plot <- renderPlot({
            
            if(input$plot_type_O=="Line"){
              if(input$y_O=="Total_Order"){
                if(input$cum_O==F){
                  p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O, y="n"))+
                    geom_line()+
                    labs(y="Freq",x="week")+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_orders_member(),aes(label=n))+scale_x_continuous(breaks=seq(0, 52, 1))
                }
                else if(input$cum_O==T){
                  p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O, y="Cumul"))+
                    geom_line()+
                    labs(y="Freq",x="week")+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_orders_member(),aes(label=Cumul))+scale_x_continuous(breaks=seq(0, 52, 1))
                }
              } 
              else{
                if(input$cum_O==F){
                  p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O, y="n",color=input$y_O))+
                    geom_line()+
                    labs(y="Freq",x="week")+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_orders_member(),aes(label=n))+
                    theme_grey(base_family = "STKaiti")+scale_x_continuous(breaks=seq(0, 52, 1))
                }
                else if(input$cum_O==T){
                  p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O, y="Cumul",color=input$y_O))+
                    geom_line()+
                    labs(y="Freq",x="week")+
                    theme(panel.grid.minor.x=element_blank())+
                    geom_text(data=dataset_orders_member(),aes(label=Cumul))+
                    theme_grey(base_family = "STKaiti")+scale_x_continuous(breaks=seq(0, 52, 1))
                }
                
                
              }
              p
            } else if (input$plot_type_O=="Stack_Area"){
              
              if(input$cum_O==F){
                p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O,y="n",group=input$y_O,fill=input$y_O)) + 
                  geom_area(position="fill")+
                  labs(y="Percentage",x="Week")+
                  theme_grey(base_family = "STKaiti")+scale_x_continuous(breaks=seq(0, 52, 1))
              }
              else if(input$cum_O==T){
                p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O,y="Cumul",group=input$y_O,fill=input$y_O)) + 
                  geom_area(position="fill")+
                  labs(y="Percentage",x="Week")+
                  theme_grey(base_family = "STKaiti")+scale_x_continuous(breaks=seq(0, 52, 1))
              }
              p
            }
            
            
          })
          output$Merchant_plot <- renderPlot({
            
            temp<-branch%>%group_by(area,type)%>%dplyr::summarise(n=n())
            p<- ggplot(data = temp) + 
                geom_bar(aes(x = "", y = n, fill = type), stat = "identity") +
                facet_wrap(~area) +theme_grey(base_family = "STKaiti")
            p
          })
          output$Merchant_line_plot <- renderPlot({
            if(input$cum_M==F){
              p <- ggplot(dataset_Merchant_Line(), aes(x=week, y=n))+
                geom_line()+
                labs(y="Merchant on board",x="week")+
                theme(panel.grid.minor.x=element_blank())+
                geom_text(data=dataset_Merchant_Line(),aes(label=n))+scale_x_continuous(breaks=seq(-10, 52, 1))
            } 
            else{
              p <- ggplot(dataset_Merchant_Line(), aes(x=week, y=Cumul))+
                geom_line()+
                labs(y="Merchant on board",x="week")+
                theme(panel.grid.minor.x=element_blank())+
                geom_text(data=dataset_Merchant_Line(),aes(label=Cumul))+scale_x_continuous(breaks=seq(-10, 52, 1))
            }
        
            p
          })
          output$Orders_time_plot <- renderPlot({ 

            temp<-orders%>%filter(status_name=="Paid")%>%group_by(time_diff)%>%dplyr::summarise(n=n())
            p<- ggplot(data = temp) + 
              geom_bar(aes(x = "", y = n, fill = time_diff), stat = "identity") +
              coord_polar(theta="y")+theme_grey(base_family = "STKaiti")
            p
            })
          #MAU
          output$MAU_plot <- renderPlot({
            if(input$MAU_OS=="ALL"){
              temp<-filter(MAU,funnel=="retention")
            }else if (input$MAU_OS=="IOS"){
              temp<-filter(MAU_IOS,funnel=="retention")
            }else if(input$MAU_OS=="ANDROID"){
              temp<-filter(MAU_ANDROID,funnel=="retention")
            }
            if(input$MAU_var=="MAU"){
              p <- ggplot(data=temp)+geom_line(aes(x=Date,y=MAU_count))
            }else if(input$MAU_var=="DAU"){
              p <- ggplot(data=temp)+geom_line(aes(x=Date,y=DAU_count))
            }else if(input$MAU_var=="DAU_MAU_ratio"){
              p <- ggplot(data=temp)+geom_line(aes(x=Date,y=DAU_MAU_percentage))
            }
            p
          })
          output$MAU_stacked <- renderPlot({
            if(input$MAU_OS=="ALL"){
              p <- ggplot(MAU, aes(x=Date,y=MAU_percentage,group=funnel,fill=funnel)) + 
                geom_area()+
                labs(y="Percentage",x="Date")
            }else if (input$MAU_OS=="IOS"){
              p <- ggplot(MAU_IOS, aes(x=Date,y=MAU_percentage,group=funnel,fill=funnel)) + 
                geom_area()+
                labs(y="Percentage",x="Date")
            }else if(input$MAU_OS=="ANDROID"){
              p <- ggplot(MAU_ANDROID, aes(x=Date,y=MAU_percentage,group=funnel,fill=funnel)) + 
                geom_area()+
                labs(y="Percentage",x="Date")
            }

            p
          })
          output$MAU_table<- renderTable({
            if(input$MAU_OS=="ALL"){
              MAU
            }else if (input$MAU_OS=="IOS"){
              MAU_IOS
            }else if(input$MAU_OS=="ANDROID"){
              MAU_ANDROID
            }
          },digit=3)
          
          #User Behavior 
          output$Category_browsing <- renderPlot({
            temp<-dataset_cat()%>%filter(start==0)%>%group_by(rpgid,forMap)%>%dplyr::summarise(n=n())
            p<- ggplot(data = temp) + 
              geom_bar(aes(x = "", y = n, fill = forMap), stat = "identity") +
              facet_wrap(~rpgid) +theme_grey(base_family = "STKaiti")
            p
          })
          
          output$Map_Browsing <- renderPlot({
            temp<-dataset_cat()%>%filter(start==0)%>%group_by(rpgid,forMap)%>%dplyr::summarise(n=n())
            p<- ggplot(data = temp) + 
              geom_bar(aes(x = "", y = n, fill = rpgid), stat = "identity") +
              facet_wrap(~forMap) +theme_grey(base_family = "STKaiti")
            p
          })
          output$Cat_Browsing <- renderPlot({
            if(input$Cat_Browsing_variable=="Week"){
              if(input$remove_first==F){
                temp<-dataset_cat()%>%group_by(week,rpgid)%>%dplyr::summarise(n=n())
              }else{
                temp<-merge(dataset_cat(),select(member,uid,Create_Time),by="uid")
                temp%<>%filter(createtime!=Create_Time)%>%group_by(week,rpgid)%>%dplyr::summarise(n=n())
              }
              p <- ggplot(temp, aes(x=week, y=n,color=rpgid))+
                geom_line()+
                labs(y="Freq",x="week")+
                theme(panel.grid.minor.x=element_blank())+
                geom_text(data=temp,aes(label=n))+scale_x_continuous(breaks=seq(0, 52, 1)) +theme_grey(base_family = "STKaiti")
              
            } else if(input$Cat_Browsing_variable=="Day"){
              if(input$remove_first==F){
                temp<-dataset_cat()%>%group_by(createtime,rpgid)%>%dplyr::summarise(n=n())
              }else{
                temp<-merge(dataset_cat(),select(member,uid,Create_Time),by="uid")
                temp%<>%filter(createtime!=Create_Time)%>%group_by(createtime,rpgid)%>%dplyr::summarise(n=n())
              }
              p <- ggplot(temp, aes(x=createtime, y=n,color=rpgid))+
                geom_line()+
                labs(y="Freq",x="week")+
                theme(panel.grid.minor.x=element_blank())+theme_grey(base_family = "STKaiti")
              
            }
            p
          })
          
          output$first_shopping_dist <- renderPlot({
            hist(as.numeric(first_shopping$time_diff),col="red",breaks=50,xlab="Days between account created time and first shopping time")
          })
          output$login_vs_first_time_dist <- renderPlot({
            plot(first_shopping$time_diff,first_shopping$browse_count)  
          })
          #order time
          output$order_time_plot<-renderPlot({
            hours_matrix<-data.frame(matrix(data = 0,nrow = 1,ncol = 2)) 
            colnames(hours_matrix)<-c("hours","n")
            order_time_plot_temp<-dataset_orders_time()
            order_time_plot_temp%<>%group_by(hours)%>%summarise(n=n())
            if(!("1-3"%in%order_time_plot_temp$hours)){
              hours_matrix[1,1]<-"1-3"
              order_time_plot_temp<-rbind(order_time_plot_temp,hours_matrix)
            }
            if(!("4-6"%in%order_time_plot_temp$hours)){
              hours_matrix[1,1]<-"4-6"
              order_time_plot_temp<-rbind(order_time_plot_temp,hours_matrix)
            }
            if(!("7-9"%in%order_time_plot_temp$hours)){
              hours_matrix[1,1]<-"7-9"
              order_time_plot_temp<-rbind(order_time_plot_temp,hours_matrix)
            }
            if(!("10-12"%in%order_time_plot_temp$hours)){
              hours_matrix[1,1]<-"10-12"
              order_time_plot_temp<-rbind(order_time_plot_temp,hours_matrix)
            }
            if(!("13-15"%in%order_time_plot_temp$hours)){
              hours_matrix[1,1]<-"13-15"
              order_time_plot_temp<-rbind(order_time_plot_temp,hours_matrix)
            }
            if(!("16-18"%in%order_time_plot_temp$hours)){
              hours_matrix[1,1]<-"16-18"
              order_time_plot_temp<-rbind(order_time_plot_temp,hours_matrix)
            }
            if(!("19-21"%in%order_time_plot_temp$hours)){
              hours_matrix[1,1]<-"19-21"
              order_time_plot_temp<-rbind(order_time_plot_temp,hours_matrix)
            }
            if(!("22-0"%in%order_time_plot_temp$hours)){
              hours_matrix[1,1]<-"22-0"
              order_time_plot_temp<-rbind(order_time_plot_temp,hours_matrix)
            }
            order_time_plot_temp$hours<- factor(order_time_plot_temp$hours, levels= c("1-3", "4-6", "7-9", "10-12", "13-15", "16-18","19-21","22-0"))
            ggplot(order_time_plot_temp,aes(x=hours,y=n))+geom_point()
          })
          
})

