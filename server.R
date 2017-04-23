library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
require(ggmap)
require(XLConnect)
require(magrittr)
require(jsonlite)

setwd("~/Dropbox/App")

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
# first_shopping<-readRDS("first_shopping")
userlog_member<-readRDS("userlog_member")
MAU<-readRDS("MAU_all")
MAU_IOS<-readRDS("MAU_IOS")
MAU_ANDROID<-readRDS("MAU_ANDROID")
conversion_stats<-readRDS("conversion_stats")
hotspot=read.csv("hotspot.csv",stringsAsFactor=F,header=T,fileEncoding='big5')

branch_bd<-fromJSON("branch_bd.json")
branch_bd<-merge(branch_bd,select(branch,bid,branchname),by="bid")
branch_bd%<>%select(bd_name,branchname)

sales_daily_setting<-fromJSON("sales_daily_setting.json")
sales_daily_setting$cd<-as.Date(sales_daily_setting$lastmodifiedtime)
sales_daily_setting<-merge(sales_daily_setting,select(branch,bid,branchname),by='bid')



# Shiny Main Program
shinyServer(function(input, output) {
      #===============Data================
          
    output$merchant_selector <- renderUI({
      selectInput("merchant_com", "商家名稱:", as.list(dataset_branch_com())) 
    })
    output$hr_selector <- renderUI({

      selectInput("hr_com", "Duration(hr):", as.list(c('All',dataset_hr_com()))) 
    })
    dataset_branch_com <- reactive({
      branch%>%filter(area_detail==input$area_com&category=="休憩")%$%branchname
    })
    dataset_hr_com <- reactive({
      orders%>%filter(branchname==input$merchant_com)%$%duration
    })
      # Member Data
          dataset_member <- reactive({
              {   week_start<-(floor(input$dates[1]-as.Date("2015-11-02"))/7)+1
                  week_end<-(floor(input$dates[2]-as.Date("2015-11-02"))/7)+1
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
            { week_start<-(floor(input$dates_L[1]-as.Date("2015-11-02"))/7)+1
              week_end<-(floor(input$dates_L[2]-as.Date("2015-11-02"))/7)+1
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
            { 
            week_start<-(floor(input$dates_O[1]-as.Date("2015-11-02"))/7)+1
            week_end<-(floor(input$dates_O[2]-as.Date("2015-11-02"))/7)+1
            if(input$y_O=="Total_Order"){
              temp<-conversion_stats%>%filter((create_week>=week_start)&(create_week<=week_end))
              temp<-aggregate(temp$Counts,by=list(temp$create_week),FUN=sum)
              colnames(temp)<-c("create_week","Orders")
              temp
            }
            else{
              conversion_stats%>%filter((create_week>=week_start)&(create_week<=week_end))
            }
            }
            
          })
          # Category data
          dataset_category <- reactive({
            if (input$Category_sel=="all"){
              orders
            } else {
              orders%>%filter(category==input$Category_sel)
            }
          })
          # Merchant Data
          dataset_Merchant_Top <- reactive({
            temp<-orders%>%filter((cd>=input$dates_M[1])&(cd<=input$dates_M[2])&(status_name=="Paid"))%>%group_by(branchname)%>%dplyr::summarise(n=n())
            temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
            temp<-temp[order(-temp$n),]
            colnames(temp)<-c("商家","購買次數","地區")
            temp
          })
          # Merchant stats Data
          dataset_Merchant_stats <- reactive({
            if (input$Category_sel_MS=="all"){
              if(input$Weekday_MS=="all"){
                if(input$Day_MS=="all"){
                  orders%>%filter(cd>=input$dates_MS[1]&cd<=input$dates_MS[2]&status_name=="Paid")
                }else{
                  orders%>%filter(cd>=input$dates_MS[1]&cd<=input$dates_MS[2]&DN==input$Day_MS&status_name=="Paid")
                }
              }else{
                if(input$Day_MS=="all"){
                  orders%>%filter(cd>=input$dates_MS[1]&cd<=input$dates_MS[2]&Weekday==input$Weekday_MS&status_name=="Paid")
                }else{
                  orders%>%filter(cd>=input$dates_MS[1]&cd<=input$dates_MS[2]&Weekday==input$Weekday_MS&DN==input$Day_MS&status_name=="Paid")
                }
              }
            }else{
              if(input$Weekday_MS=="all"){
                if(input$Day_MS=="all"){
                  orders%>%filter(cd>=input$dates_MS[1]&cd<=input$dates_MS[2]&category==input$Category_sel_MS&status_name=="Paid")
                }else{
                  orders%>%filter(cd>=input$dates_MS[1]&cd<=input$dates_MS[2]&DN==input$Day_MS&category==input$Category_sel_MS&status_name=="Paid")
                }
              }else{
                if(input$Day_MS=="all"){
                  orders%>%filter(cd>=input$dates_MS[1]&cd<=input$dates_MS[2]&Weekday==input$Weekday_MS&category==input$Category_sel_MS&status_name=="Paid")
                }else{
                  orders%>%filter(cd>=input$dates_MS[1]&cd<=input$dates_MS[2]&Weekday==input$Weekday_MS&DN==input$Day_MS&category==input$Category_sel_MS&status_name=="Paid")
                }
              }
            }
          })
          output$local_average<-renderText({
            temp_orders<-dataset_Merchant_stats()
            temp<-branch%>%filter(((((lat-hotspot$lat[2])^2+(lng-hotspot$lng[2])^2)^0.5)/0.00000900900901)<hotspot$diameter[2])
            temp<-temp_orders[temp_orders$branchname%in%temp$branchname,]
            paste("Average",mean(temp$amount+temp$bonus),sep=": ")
          })
          dataset_supply_demand_weekday<-reactive({
            if (input$Type_M=="all"){
              supply_temp<-sales%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekday")
              demand_temp<-orders%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekday"&status_name=="Paid")
            } else if(input$Type_M=="休憩"){
              supply_temp<-sales%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekday"&(type=="摩鐵"|type=="商旅"))
              demand_temp<-orders%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekday"&status_name=="Paid"&(type=="摩鐵"|type=="商旅"))
            } else{
              supply_temp<-sales%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekday"&type==input$Type_M)
              demand_temp<-orders%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekday"&status_name=="Paid"&type==input$Type_M)
            }

            supply_demand_table<-data.frame(matrix(data = 0,nrow = nrow(hotspot),ncol = 4)) 
            colnames(supply_demand_table)<-c("Location","Sales","Orders","%")
            for (i in 1:nrow(hotspot)){
              supply_demand_table[i,1]<-hotspot$location[i]
              temp<-supply_temp%>%filter(((((lat-hotspot$lat[i])^2+(lng-hotspot$lng[i])^2)^0.5)/0.00000900900901)<hotspot$diameter[i])
              supply_demand_table[i,2]<-nrow(temp)
              supply_demand_table[i,3]<-nrow(demand_temp[demand_temp$branchname%in%unique(temp$branchname),])
              supply_demand_table[i,4]<-supply_demand_table[i,3]/supply_demand_table[i,2]*100
            }
            supply_demand_table
          })
          dataset_supply_demand_weekend<-reactive({
            if (input$Type_M=="all"){
              supply_temp<-sales%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekend")
              demand_temp<-orders%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekend"&status_name=="Paid")
            } else if(input$Type_M=="休憩"){
              supply_temp<-sales%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekend"&(type=="摩鐵"|type=="商旅"))
              demand_temp<-orders%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekend"&status_name=="Paid"&(type=="摩鐵"|type=="商旅"))
            } else{
              supply_temp<-sales%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekend"&type==input$Type_M)
              demand_temp<-orders%>%filter(cd>=input$dates_M[1]&cd<=input$dates_M[2]&Weekday=="Weekend"&status_name=="Paid"&type==input$Type_M)
            }
            
            supply_demand_table<-data.frame(matrix(data = 0,nrow = nrow(hotspot),ncol = 4)) 
            colnames(supply_demand_table)<-c("Location","Sales","Orders","%")
            for (i in 1:nrow(hotspot)){
              supply_demand_table[i,1]<-hotspot$location[i]
              temp<-supply_temp%>%filter(((((lat-hotspot$lat[i])^2+(lng-hotspot$lng[i])^2)^0.5)/0.00000900900901)<hotspot$diameter[i])
              supply_demand_table[i,2]<-nrow(temp)
              supply_demand_table[i,3]<-nrow(demand_temp[demand_temp$branchname%in%unique(temp$branchname),])
              supply_demand_table[i,4]<-supply_demand_table[i,3]/supply_demand_table[i,2]*100
            }
            supply_demand_table
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
          # dataset_pt_QK<-reactive({
          #   temp<-dataset_pt()%>%filter(category=="休憩"&area=="台北")%>%group_by(branchname)%>%dplyr::summarise(n=n())
          #   names(temp)<-c("branchname","Views")
          #   temp<-merge(temp,select(branch,branchname),by="branchname")
          #   temp$intention_count<-0
          #   temp$order_count<-0
          #   
          #   for (i in 1:length(temp$branchname)){
          #     temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
          #     temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
          #   }
          #   temp$intention_percentage<-temp$intention_count/temp$Views
          #   temp$order_percentage<-temp$order_count/temp$Views
          #   
          #   temp<-merge(temp,branch_bd,by="branchname")
          #   names(temp)<-c("Merchants","Views","有意圖","銷售","有意圖比例","銷售比例","負責BD")
          #   temp[order(-temp$Views),]
          # })
          dataset_pt_mix<-reactive({
            temp<-dataset_pt()%>%group_by(branchname)%>%dplyr::summarise(n=n())
            names(temp)<-c("branchname","Views")
            temp<-merge(temp,select(branch,bid,branchname,area,category),by="branchname")
            temp$intention_count<-0
            temp$order_count<-0
            
            for (i in 1:length(temp$branchname)){
              temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
              temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
            }
            temp$intention_percentage<-temp$intention_count/temp$Views
            temp$order_percentage<-temp$order_count/temp$Views
            
            temp<-merge(temp,branch_bd,by="branchname")
            names(temp)<-c("Merchants","Views","bid","area","cateogry","有意圖","銷售","有意圖比例","銷售比例","負責BD")
            temp[order(-temp$Views),]
          })
          # dataset_pt_massage<-reactive({
          #   temp<-dataset_pt()%>%filter(category=="按摩"&area=="台北")%>%group_by(branchname)%>%dplyr::summarise(n=n())
          #   names(temp)<-c("branchname","Views")
          #   temp<-merge(temp,select(branch,branchname),by="branchname")
          #   temp$intention_count<-0
          #   temp$order_count<-0
          #   
          #   for (i in 1:length(temp$branchname)){
          #     temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
          #     temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
          #   }
          #   temp$intention_percentage<-temp$intention_count/temp$Views
          #   temp$order_percentage<-temp$order_count/temp$Views
          #   
          #   temp<-merge(temp,branch_bd,by="branchname")
          #   names(temp)<-c("Merchants","Views","有意圖","銷售","有意圖比例","銷售比例","負責BD")
          #   temp[order(-temp$Views),]
          #   
          # })
          # dataset_pt_late<-reactive({
          #   temp<-dataset_pt()%>%filter(category=="晚鳥過夜"&area=="台北")%>%group_by(branchname)%>%dplyr::summarise(n=n())
          #   names(temp)<-c("branchname","Views")
          #   temp<-merge(temp,select(branch,branchname),by="branchname")
          #   temp$intention_count<-0
          #   temp$order_count<-0
          #   
          #   for (i in 1:length(temp$branchname)){
          #     temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
          #     temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
          #   }
          #   temp$intention_percentage<-temp$intention_count/temp$Views
          #   temp$order_percentage<-temp$order_count/temp$Views
          #   
          #   temp<-merge(temp,branch_bd,by="branchname")
          #   names(temp)<-c("Merchants","Views","有意圖","銷售","有意圖比例","銷售比例","負責BD")
          #   temp[order(-temp$Views),]
          #   
          # })
          # dataset_pt_manicure<-reactive({
          #   temp<-dataset_pt()%>%filter(category=="美甲美睫"&area=="台北")%>%group_by(branchname)%>%dplyr::summarise(n=n())
          #   names(temp)<-c("branchname","Views")
          #   temp<-merge(temp,select(branch,branchname),by="branchname")
          #   temp$intention_count<-0
          #   temp$order_count<-0
          #   
          #   for (i in 1:length(temp$branchname)){
          #     temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
          #     temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
          #   }
          #   temp$intention_percentage<-temp$intention_count/temp$Views
          #   temp$order_percentage<-temp$order_count/temp$Views
          #   
          #   temp<-merge(temp,branch_bd,by="branchname")
          #   names(temp)<-c("Merchants","Views","有意圖","銷售","有意圖比例","銷售比例","負責BD")
          #   temp[order(-temp$Views),]
          # })
          # dataset_pt_bar<-reactive({
          #   temp<-dataset_pt()%>%filter(category=="主題酒吧"&area=="台北")%>%group_by(branchname)%>%dplyr::summarise(n=n())
          #   names(temp)<-c("branchname","Views")
          #   temp<-merge(temp,select(branch,branchname),by="branchname")
          #   temp$intention_count<-0
          #   temp$order_count<-0
          #   
          #   for (i in 1:length(temp$branchname)){
          #     temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
          #     temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
          #   }
          #   temp$intention_percentage<-temp$intention_count/temp$Views
          #   temp$order_percentage<-temp$order_count/temp$Views
          #   
          #   temp<-merge(temp,branch_bd,by="branchname")
          #   names(temp)<-c("Merchants","Views","有意圖","銷售","有意圖比例","銷售比例","負責BD")
          #   temp[order(-temp$Views),]
          # })
          # 
          # dataset_pt_QK_Taichung<-reactive({
          #   temp<-dataset_pt()%>%filter(category=="休憩"&area=="台中")%>%group_by(branchname)%>%dplyr::summarise(n=n())
          #   names(temp)<-c("branchname","Views")
          #   temp<-merge(temp,select(branch,branchname),by="branchname")
          #   temp$intention_count<-0
          #   temp$order_count<-0
          #   
          #   for (i in 1:length(temp$branchname)){
          #     temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
          #     temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
          #   }
          #   temp$intention_percentage<-temp$intention_count/temp$Views
          #   temp$order_percentage<-temp$order_count/temp$Views
          #   
          #   temp<-merge(temp,branch_bd,by="branchname")
          #   names(temp)<-c("Merchants","Views","有意圖","銷售","有意圖比例","銷售比例","負責BD")
          #   temp[order(-temp$Views),]
          # })
          # 
          # dataset_pt_late_Taichung<-reactive({
          #   temp<-dataset_pt()%>%filter(category=="晚鳥過夜"&area=="台中")%>%group_by(branchname)%>%dplyr::summarise(n=n())
          #   names(temp)<-c("branchname","Views")
          #   temp<-merge(temp,select(branch,branchname),by="branchname")
          #   temp$intention_count<-0
          #   temp$order_count<-0
          #   
          #   for (i in 1:length(temp$branchname)){
          #     temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
          #     temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
          #   }
          #   temp$intention_percentage<-temp$intention_count/temp$Views
          #   temp$order_percentage<-temp$order_count/temp$Views
          #   temp<-merge(temp,branch_bd,by="branchname")
          #   names(temp)<-c("Merchants","Views","有意圖","銷售","有意圖比例","銷售比例","負責BD")
          #   temp[order(-temp$Views),]
          #   
          # })
          
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
          #map data
          dataset_GPS_supply <- reactive({
            if(input$Weekday_DS=="all"){
              if (input$Type_DS=="all"){
                sales%>%filter((cd>=input$dates_DS[1])&(cd<=input$dates_DS[2]))
              } else{
                sales%>%filter((cd>=input$dates_DS[1])&(cd<=input$dates_DS[2])&type==input$Type_DS)
              }
            } else{
              if (input$Type_DS=="all"){
                sales%>%filter((cd>=input$dates_DS[1])&(cd<=input$dates_DS[2])&Weekday==input$Weekday_DS)
              } else {
                sales%>%filter((cd>=input$dates_DS[1])&(cd<=input$dates_DS[2])&type==input$Type_DS&Weekday==input$Weekday_DS)
              }
            }

          })

          dataset_GPS_view <- reactive({
            if(input$Weekday_DS=="all"){
              if (input$Type_DS=="all"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2]))
              } else if(input$Type_DS=="商旅"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="泡湯x休憩")
              } else if(input$Type_DS=="摩鐵"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="泡湯x休憩")
              } else if(input$Type_DS=="美甲美睫"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="美甲x美睫")
              } else if(input$Type_DS=="按摩"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="按摩紓壓")
              } else if(input$Type_DS=="主題酒吧"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="主題酒吧")
              }
            } else{
              if (input$Type_DS=="all"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2]))
              } else if(input$Type_DS=="商旅"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="泡湯x休憩"&Weekday==input$Weekday_DS)
              } else if(input$Type_DS=="摩鐵"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="泡湯x休憩"&Weekday==input$Weekday_DS)
              } else if(input$Type_DS=="美甲美睫"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="美甲x美睫"&Weekday==input$Weekday_DS)
              } else if(input$Type_DS=="按摩"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="按摩紓壓"&Weekday==input$Weekday_DS)
              } else if(input$Type_DS=="主題酒吧"){
                user_cat%>%filter((createtime>=input$dates_DS[1])&(createtime<=input$dates_DS[2])&rpgid=="主題酒吧"&Weekday==input$Weekday_DS)
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
          output$local_stats<-renderTable({
            temp_orders<-dataset_Merchant_stats()
            temp<-branch%>%filter(((((lat-hotspot$lat[2])^2+(lng-hotspot$lng[2])^2)^0.5)/0.00000900900901)<hotspot$diameter[2])
            temp<-temp_orders[temp_orders$branchname%in%temp$branchname,]
            temp$total<-temp$amount+temp$bonus
            temp%>%group_by(branchname,total)%>%summarise(sales=n())
          })
          output$weekday_supply_demand_summary<-renderTable({
            
            dataset_supply_demand_weekday()
            
          })
          output$weekend_supply_demand_summary<-renderTable({
            
            dataset_supply_demand_weekend()
            
          })
          #Cohort
          output$Cohort_plot<-renderTable({
              cohort_date<-data.frame()
              cohort<-data.frame()
              row<-1
              for (i in min(user_cat$week):max(user_cat$week)){
                  col<-1
                  for (j in i:max(user_cat$week)){
                      #temp<-filter(userlog,Create_Time==i)
                      cohort[row,col]<-sum(member$uid[member$week_create==i&member$Sign_Up=="Sign-up"]%in%unique(user_cat[user_cat$week==j,]%$%uid))
                      col%<>%+1
                  }
                  row%<>%+1
              }
              cohort<-(cohort/cohort[,1])*100
              row<-1
              for(i in min(user_cat$week):max(user_cat$week)){
                  cohort_date[row,1]<-paste(as.Date("2015-11-02")+7*(i-1))
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
            for (i in min(user_cat$week):max(user_cat$week)){
              col<-2
              cohort_c[row,1]<-nrow(filter(member,week_create==i&Sign_Up=="Sign-up"&area==input$cohort_area))
              for (j in i:max(user_cat$week)){
                temp<-filter(orders,Create_Time<=j&Create_Time>j-1&status_name=="Paid"&area==input$cohort_area)
                cohort_c[row,col]<-sum(member$uid[member$week_create==i&member$Sign_Up=="Sign-up"&member$area==input$cohort_area]%in%temp$uid)
                col%<>%+1
              }
              row%<>%+1
            }
            cohort_c[,2:ncol(cohort_c)]<-(cohort_c[,2:ncol(cohort_c)]/cohort_c[,1])*100
            
            row<-1
            colnames(cohort_c)[1]<-paste("base")
            for(i in min(user_cat$week):max(user_cat$week)){
              cohort_date_c[row,1]<-paste(as.Date("2015-11-02")+7*(i-1))
              rownames(cohort_date_c)[row]<-paste("Week",i,sep=" ")
              colnames(cohort_c)[row+1]<-paste("Week",row,sep=" ")
              row%<>%+1
            }
            
            colnames(cohort_date_c)<-"Date"
            cbind(cohort_date_c,cohort_c)
            
          },digit=5)
          
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
                if (input$Category_sel_cohort!="all"){
                  temp%<>%filter(category==input$Category_sel_cohort)
                }
                cohort_cm[i,col]<-length(unique(temp%>%filter(create_month==temp_month[j]&status_name=="Paid")%$%uid))
                colnames(cohort_cm)[1]<-paste("base")
                colnames(cohort_cm)[j+1]<-paste("month",j,sep=" ")
                col%<>%+1
              }
            }
            
            if(input$cm_percentage){
              cohort_cm[,2:ncol(cohort_cm)]<-(cohort_cm[,2:ncol(cohort_cm)]/cohort_cm[,1])*100
            }
            
            for (i in 1:length(unique(member$create_month))){
              cohort_date_cm[i,1]<-paste(temp_month[i])
            }
            colnames(cohort_date_cm)<-"Date"
            
            cbind(cohort_date_cm,cohort_cm)
          },digit=3)
          
          output$Conversion_trend<-renderTable({
            member_temp<-member
            member_temp%<>%select(uid,create_month)
            colnames(member_temp)<-c("uid","member_create_month")
            member_orders<-merge(orders%>%filter(status_name=="Paid")%>%select(uid,create_month,category),member_temp,by="uid",all.x=TRUE)
            if (input$Category_sel_cohort!="all"){
              member_orders%<>%filter(category==input$Category_sel_cohort)
            }
            member_orders<-member_orders[order(member_orders$create_month),]
            member_orders$rep<-"Rep"
            member_orders$purchase<-1
            
            rep_uid<-unique(member_orders[duplicated(member_orders$uid),]%$%uid)
            for (i in 1:length(rep_uid)){
              member_orders$purchase[member_orders$uid==rep_uid[i]]<-2
              member_orders$purchase[min(which(member_orders$uid==rep_uid[i]))]<-1
            }
            
            member_orders_old<-member_orders[member_orders$member_create_month!=member_orders$create_month,]
            member_orders_old%<>%filter(purchase==1)%>%select(uid,create_month)
            member_orders_old$month<- member_orders_old$create_month
            member_orders_old%<>%select(uid,month)
            member_orders<-merge(member_orders,member_orders_old,by="uid",all.x=TRUE)
            member_orders$rep[member_orders$member_create_month!=member_orders$create_month&member_orders$create_month==member_orders$month]<-"First not same month"
            
            member_orders$rep[member_orders$member_create_month==member_orders$create_month]<-"First"
            head_stats<-data.frame(matrix(data = 0,
                             nrow = length(unique(member_orders$create_month)),
                             ncol = 4)) 
            member_orders<-member_orders[order(member_orders$create_month),]

            if (input$head_count){
            	row<-1
            	for (i in unique(member_orders$create_month)){
    				head_stats[row,1]<-i
    				head_stats[row,2]<-length(unique(member_orders%>%filter(create_month==i&rep==unique(member_orders$rep)[1])%$%uid))
    				head_stats[row,3]<-length(unique(member_orders%>%filter(create_month==i&rep==unique(member_orders$rep)[3])%$%uid))
    				head_stats[row,4]<-length(unique(member_orders%>%filter(create_month==i&rep==unique(member_orders$rep)[2])%$%uid))
    				row<-row+1
				}
				
				colnames(head_stats)<-c('month',unique(member_orders$rep)[1],unique(member_orders$rep)[3],unique(member_orders$rep)[2])
				head_stats

            } else {
            	if (input$cm_percentage){
              	temp<-table(member_orders$create_month,member_orders$rep)
              	prop.table(temp,1)
            	} else {
              	table(member_orders$create_month,member_orders$rep)
            	}
            }
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
                if (input$Category_sel_cohort!="all"){
              		temp%<>%filter(category==input$Category_sel_cohort)
                }
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
          output$buy_size<-renderTable({
            cohort_date_bs<-data.frame()
            cohort_bs<-data.frame()
            temp_month<-unique(member$create_month)
            temp_month<-temp_month[order(temp_month)]
            for (i in 1:length(unique(member$create_month))){
            	temp<-orders
           		if (input$Category_sel_cohort!="all"){
              		temp%<>%filter(category==input$Category_sel_cohort)
                }
              cohort_bs[i,1]<-(sum(temp%>%filter(create_month==temp_month[i]&status_name=="Paid")%$%amount))/nrow(temp%>%filter(create_month==temp_month[i]&status_name=="Paid"))
              colnames(cohort_bs)<-"Amount"
            }
            
            
            for (i in 1:length(unique(member$create_month))){
              cohort_date_bs[i,1]<-paste(temp_month[i])
            }
            colnames(cohort_date_bs)<-"Date"
            
            cbind(cohort_date_bs,cohort_bs)
          })
          
          
          output$Repeat_ratio_trend<-renderTable({
            cohort_date_rtt<-data.frame()
            cohort_rtt<-data.frame()
            temp_month<-unique(member$create_month)
            temp_month<-temp_month[order(temp_month)]
            for (i in 1:length(unique(member$create_month))){
              col<-2
              if (i==1){
                cohort_rtt[i,1]<-0
              }else{
                if (input$Category_sel_cohort!="all"){
                  order_cat<-orders%>%filter(category==input$Category_sel_cohort)
                } else{
                	order_cat<-orders
                }
                temp<-order_cat%>%filter(create_month==temp_month[i-1]&status_name=="Paid")
                temp_2<-order_cat%>%filter(create_month==temp_month[i]&status_name=="Paid")
                temp_2<-temp_2[temp_2$uid%in%temp$uid,]
                cohort_rtt[i,1]<-length(unique(temp_2$uid))/length(unique(temp$uid))
              }

            }
            
            colnames(cohort_rtt)<-c("Rep_Ratio_over_month")
            for (i in 1:length(unique(member$create_month))){
              cohort_date_rtt[i,1]<-paste(temp_month[i])
            }
            colnames(cohort_date_rtt)<-"Date"
            
            cbind(cohort_date_rtt,cohort_rtt)
          })
        
          output$Orders_Sales_summary<-renderTable({
            
            (dataset_Orders_sub()/dataset_Sales()*100)
            
          })

          output$Cat_Top<-renderTable({
              temp<-dataset_cat()%>%filter(start==0)%>%group_by(rpgid)%>%dplyr::summarise(n=n())
              names(temp)<-c("Category","Tot_views")
              temp[order(-temp$Tot_views),]

          })
          output$Product_Top_mix<-renderTable({
            temp<-dataset_pt_mix()
            temp[,-3]
          })
          
          # output$Product_Top_rest<-renderTable({
          #   temp<-dataset_pt_QK()
          #   temp[1:20,]
          # })
          # output$Product_Top_late<-renderTable({
          #   temp<-dataset_pt_late()
          #   temp[1:20,]
          # })
          # output$Product_Top_massage<-renderTable({
          #   temp<-dataset_pt_massage()
          #   temp[1:20,]
          # })
          # output$Product_Top_manicure<-renderTable({
          #   temp<-dataset_pt_manicure()
          #   temp[1:20,]
          # })
          # 
          # output$Product_Top_rest_Taichung<-renderTable({
          #   temp<-dataset_pt_QK_Taichung()
          #   temp[1:20,]
          # })
          # output$Product_Top_late_Taichung<-renderTable({
          #   temp<-dataset_pt_late_Taichung()
          #   temp[1:20,]
          # })
          # output$Product_Top_escape<-renderTable({
          #   temp<-dataset_pt()%>%filter(category=="密室")%>%group_by(branchname)%>%dplyr::summarise(n=n())
          #   names(temp)<-c("branchname","Views")
          #   temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
          #   temp<-temp[order(-temp$Views),]
          #   temp$intention_count<-0
          #   temp$order_count<-0
          #   
          #   for (i in 1:length(temp$branchname)){
          #     temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
          #     temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
          #   }
          #   temp$intention_percentage<-temp$intention_count/temp$Views
          #   temp$order_percentage<-temp$order_count/temp$Views
          #   
          #   names(temp)<-c("Merchants","Views","地區","有意圖","銷售","有意圖比例","銷售比例")
          #   temp[1:10,]
          # })
          # output$Product_Top_board<-renderTable({
          #   temp<-dataset_pt()%>%filter(category=="桌遊")%>%group_by(branchname)%>%dplyr::summarise(n=n())
          #   names(temp)<-c("branchname","Views")
          #   temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
          #   temp<-temp[order(-temp$Views),]
          #   temp$intention_count<-0
          #   temp$order_count<-0
          #   
          #   for (i in 1:length(temp$branchname)){
          #     temp$order_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Paid")))
          #     temp$intention_count[i]<-as.numeric(sum((order_browsing()$branchname==temp$branchname[i])&(order_browsing()$status_name=="Intention")))
          #   }
          #   temp$intention_percentage<-temp$intention_count/temp$Views
          #   temp$order_percentage<-temp$order_count/temp$Views
          #   
          #   names(temp)<-c("Merchants","Views","地區","有意圖","銷售","有意圖比例","銷售比例")
          #   temp[1:10,]
          # })
          output$Product_Top_bar<-renderTable({
            temp<-dataset_pt_bar()
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
            temp[1:30,]
            
          })
          output$Ave_items<-renderTable({
            temp<-data.frame()
            temp[1,1]<-(dataset_view())
            names(temp)<-c("Ave pages view")
            temp
          })
          output$Category_table<-renderTable({
            Cat_matrix<-data.frame(matrix(data = 0,nrow = length(unique(dataset_category()%$%create_month)),ncol = 3))
            month<-unique(dataset_category()%$%create_month)
            for (i in 1:length(month)){
              Cat_matrix[i,1]<-month[i]
              old_uid<-dataset_category()%>%filter(status_name=="Paid"&create_month<month[i])%$%uid
              temp<-dataset_category()%>%filter(status_name=="Paid"&create_month==month[i])
              temp_old<-temp[temp$uid%in%old_uid,]
              temp<-temp[!(temp$uid%in%old_uid),]
              Cat_matrix[i,2]<-nrow(temp_old)
              Cat_matrix[i,3]<-nrow(temp)
            }
            Cat_matrix[,4]<-Cat_matrix[,2]/(Cat_matrix[,3]+Cat_matrix[,2])
            colnames(Cat_matrix)<-c("month","rep","New","rep ratio")
            Cat_matrix[order(Cat_matrix$month),]
          })
          output$Category_cross_table<-renderTable({
            temp<-orders%>%filter(cd>=input$dates_cat[1]&cd<=input$dates_cat[2]&status_name=="Paid")
            cat_type<-na.omit(unique(temp$category))
            Cat_cross_matrix<-data.frame(matrix(data = 0,nrow = length(cat_type),ncol =  length(cat_type)))
            for (i in 1:length(cat_type)){
              temp_cat<-temp%>%filter(category==cat_type[i])
              for (j in 1:length(cat_type)){
                temp_cat_2<-temp%>%filter(category==cat_type[j])
                Cat_cross_matrix[i,j]<-length(unique(temp_cat_2[temp_cat_2$uid%in%temp_cat$uid,]%$%uid))
              }
            }
            colnames(Cat_cross_matrix)<-cat_type
            rownames(Cat_cross_matrix)<-cat_type
            Cat_cross_matrix
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
            if(input$y_O=="Total_Order"){
              p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O, y="Orders"))+
                geom_line()+
                labs(y="Orders",x="week")+
                theme(panel.grid.minor.x=element_blank())+
                geom_text(data=dataset_orders_member(),aes(label=Orders))+scale_x_continuous(breaks=seq(0, 52, 1))
            }
            else if(input$y_O=="Heads"){
              p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O, y="Head_Counts",color="Type"))+
                geom_line()+
                labs(y="purchase members",x="week")+
                theme(panel.grid.minor.x=element_blank())+
                geom_text(data=dataset_orders_member(),aes(label=Head_Counts))+scale_x_continuous(breaks=seq(0, 52, 1))
            }
            else if(input$y_O=="Orders"){
              p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O, y="Counts",color="Type"))+
                geom_line()+
                labs(y="Orders",x="week")+
                theme(panel.grid.minor.x=element_blank())+
                geom_text(data=dataset_orders_member(),aes(label=Counts))+scale_x_continuous(breaks=seq(0, 52, 1))
            }
            else if(input$y_O=="conversion"){
              p <- ggplot(dataset_orders_member(), aes_string(x=input$x_O, y="conversion",color="Type"))+
                geom_line()+
                labs(y="conversion",x="week")+
                theme(panel.grid.minor.x=element_blank())+
                geom_text(data=dataset_orders_member(),aes(label=sprintf("%.2f%%",conversion*100)))+scale_x_continuous(breaks=seq(0, 52, 1))
            }
            p

          })
          #Category Plot
          output$Category_plot <- renderPlot({
            temp<-orders%>%filter(status_name=="Paid")%>%group_by(create_month,category)%>%summarise(orders=n())
            p <- ggplot(temp, aes_string(x="create_month", y="orders",color="category"))+
              geom_line()+
              labs(y="Orders",x="month")+
              theme(panel.grid.minor.x=element_blank())+
              geom_text(data=temp,aes(label=orders))+theme_grey(base_family = "STKaiti")
              
            p
            
          })
          
          #Merchant  
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
          #Frequency
          output$frequency_plot <- renderPlot({
            temp<-orders
            if (input$frequency_category!="all"){
              temp%<>%filter(type==input$frequency_category)
            }
            orders_stats<-temp%>%filter(status_name=="Paid"&cd>=as.Date("2016-02-01"))%>%group_by(uid)%>%summarise(n=n())%>%filter(n>=2)
            interval<-""
            for (i in 1:nrow(orders_stats)){
              orders_uid<-temp%>%filter(uid==orders_stats$uid[i])
              if (length(unique(orders_uid))!=1){
                new_interval<-as.integer(max(orders_uid$cd)-min(orders_uid$cd))/(length(unique(orders_uid$cd))-1)
                interval<-c(interval,new_interval)
              }

            }
            interval<-as.integer(interval)
            hist(interval,breaks=200,col = 'red',xlim=range(0,100))
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
          # output$Category_browsing <- renderPlot({
          #   temp<-dataset_cat()%>%filter(start==0)%>%group_by(rpgid,forMap)%>%dplyr::summarise(n=n())
          #   p<- ggplot(data = temp) + 
          #     geom_bar(aes(x = "", y = n, fill = forMap), stat = "identity") +
          #     facet_wrap(~rpgid) +theme_grey(base_family = "STKaiti")
          #   p
          # })
          
          # output$Map_Browsing <- renderPlot({
          #   temp<-dataset_cat()%>%filter(start==0)%>%group_by(rpgid,forMap)%>%dplyr::summarise(n=n())
          #   p<- ggplot(data = temp) + 
          #     geom_bar(aes(x = "", y = n, fill = rpgid), stat = "identity") +
          #     facet_wrap(~forMap) +theme_grey(base_family = "STKaiti")
          #   p
          # })
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
          
          # output$first_shopping_dist <- renderPlot({
          #   hist(as.numeric(first_shopping$time_diff),col="red",breaks=50,xlab="Days between account created time and first shopping time")
          # })
          # output$login_vs_first_time_dist <- renderPlot({
          #   plot(first_shopping$time_diff,first_shopping$browse_count)  
          # })
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
          #map
          output$GPS_supply_plot<-renderPlot({
            if (input$area_DS=="台北"){
              gps_lon<-121.5219634
              gps_lat<-25.0389007
            } else if((input$area_DS=="台中")){
              gps_lon<-120.630577
              gps_lat<- 24.1406094
            }
           
            zoom_par<-12
            mapgilbert <- get_map(location = c(lon=gps_lon,lat= gps_lat), zoom = zoom_par,
                                  maptype = "roadmap", scale = 2)
            p<-ggmap(mapgilbert) + 
              stat_density2d(dataset_GPS_supply(), mapping=aes(x=lng, y=lat, fill=..level..), geom="polygon", alpha=0.5)+ 
              scale_fill_gradient(low = "green", high = "red")
            p
          })

          output$GPS_views_plot<-renderPlot({
            if (input$area_DS=="台北"){
              gps_lon<-121.5219634
              gps_lat<-25.0389007
            } else if((input$area_DS=="台中")){
              gps_lon<-120.630577
              gps_lat<- 24.1406094
            }
            zoom_par<-12
            mapgilbert <- get_map(location = c(lon=gps_lon,lat= gps_lat), zoom = zoom_par,
                                  maptype = "roadmap", scale = 2)
            p<-ggmap(mapgilbert) +  
              stat_density2d(dataset_GPS_view(), mapping=aes(x=lng, y=lat, fill=..level..), geom="polygon", alpha=0.5)+
              scale_fill_gradient(low = "green", high = "red")
            p
          })
          
          output$merchant_com_plot_sales<-renderPlot({
            if (input$hr_com=='All'){
              main<-orders%>%filter(branchname==input$merchant_com&cd>=input$date_com[1]&cd<=input$date_com[2])
              if (input$include_com){
                comp<-orders%>%filter(area_detail==input$area_com&cd>=input$date_com[1]&cd<=input$date_com[2])
              }else{
                comp<-orders%>%filter(area_detail==input$area_com&branchname!=input$merchant_com&cd>=input$date_com[1]&cd<=input$date_com[2])
              }
            }
            else{
              main<-orders%>%filter(branchname==input$merchant_com&duration==input$hr_com&cd>=input$date_com[1]&cd<=input$date_com[2])
              if (input$include_com){
                comp<-orders%>%filter(area_detail==input$area_com&duration==input$hr_com&cd>=input$date_com[1]&cd<=input$date_com[2])
              }else{
                comp<-orders%>%filter(area_detail==input$area_com&branchname!=input$merchant_com&duration==input$hr_com&cd>=input$date_com[1]&cd<=input$date_com[2])
              }
            }
            main_stats<-main%>%group_by(hours)%>%summarise(sales=n())
            comp_stats<-comp%>%group_by(hours)%>%summarise(sales=n())
            comp_stats$sales<-comp_stats$sales/(length(unique(comp$branchname)))
            
            main_stats$name<-'本店'
            comp_stats$name<-'比較商家'
            total<-rbind(main_stats,comp_stats)
            total$hours<-factor(total$hours,c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-0"))
            
            
            p <- ggplot(total, aes_string(x="hours", y="sales",color="name"))+
              geom_point()+
              labs(y="Orders",x="hours")+
              theme(panel.grid.minor.x=element_blank())+
              theme_grey(base_family = "STKaiti")
            
            p
          })
          output$merchant_com_plot_sales_amount<-renderPlot({
            if (input$hr_com=='All'){
              main<-orders%>%filter(branchname==input$merchant_com&cd>=input$date_com[1]&cd<=input$date_com[2]&category==input$category_com)
              if (input$include_com){
                comp<-orders%>%filter(area_detail==input$area_com&cd>=input$date_com[1]&cd<=input$date_com[2]&category==input$category_com)
              }else{
                comp<-orders%>%filter(area_detail==input$area_com&branchname!=input$merchant_com&cd>=input$date_com[1]&cd<=input$date_com[2]&category==input$category_com)
              }
            }
            else{
              main<-orders%>%filter(branchname==input$merchant_com&duration==input$hr_com&cd>=input$date_com[1]&cd<=input$date_com[2]&category==input$category_com)
              if (input$include_com){
                comp<-orders%>%filter(area_detail==input$area_com&duration==input$hr_com&cd>=input$date_com[1]&cd<=input$date_com[2]&category==input$category_com)
              }else{
                comp<-orders%>%filter(area_detail==input$area_com&branchname!=input$merchant_com&duration==input$hr_com&cd>=input$date_com[1]&cd<=input$date_com[2]&category==input$category_com)
              }
            }
            main_stats<-aggregate(main$amount,by=list(main$hours),FUN=mean)
            comp_stats<-aggregate(comp$amount,by=list(comp$hours),FUN=mean)
            colnames(main_stats)<-c("hours","amount")
            colnames(comp_stats)<-c("hours","amount")
            
            main_stats$name<-'本店'
            comp_stats$name<-'比較商家'
            total<-rbind(main_stats,comp_stats)
            total$hours<-factor(total$hours,c("1-3","4-6","7-9","10-12","13-15","16-18","19-21","22-0"))
            
            p <- ggplot(total, aes_string(x="hours", y="amount",color="name"))+
              geom_point()+
              labs(y="amount",x="hours")+
              theme(panel.grid.minor.x=element_blank())+
              theme_grey(base_family = "STKaiti")
            
            p
          })
          
          #Download files
          output$downloadData_mix <- downloadHandler(
            filename = function() { 
              paste("mix", '.csv', sep='') 
            },
            content = function(file) {
              temp<-dataset_pt_mix()
              temp<-temp[,-1]
              write.csv( temp,file,fileEncoding = "big5")
            }
          )
          
          # output$downloadData_QK_Taipei <- downloadHandler(
          #   filename = function() { 
          #     paste("Taipei_QK", '.csv', sep='') 
          #   },
          #   content = function(file) {
          #     write.csv(dataset_pt_QK(), file,fileEncoding = "big5")
          #   }
          # )
          # output$downloadData_late_Taipei <- downloadHandler(
          #   filename = function() { 
          #     paste("Taipei_Late", '.csv', sep='') 
          #   },
          #   content = function(file) {
          #     write.csv(dataset_pt_late(), file,fileEncoding = "big5")
          #   }
          # )
          # output$downloadData_massage_Taipei <- downloadHandler(
          #   filename = function() { 
          #     paste("Taipei_massage", '.csv', sep='') 
          #   },
          #   content = function(file) {
          #     write.csv(dataset_pt_massage(), file,fileEncoding = "big5")
          #   }
          # )
          # output$downloadData_manicure_Taipei <- downloadHandler(
          #   filename = function() { 
          #     paste("Taipei_manicure", '.csv', sep='') 
          #   },
          #   content = function(file) {
          #     write.csv(dataset_pt_manicure(), file,fileEncoding = "big5")
          #   }
          # )
          # output$downloadData_bar_Taipei <- downloadHandler(
          #   filename = function() { 
          #     paste("Taipei_bar", '.csv', sep='') 
          #   },
          #   content = function(file) {
          #     write.csv(dataset_pt_bar(), file,fileEncoding = "big5")
          #   }
          # )
          # output$downloadData_late_Taipei <- downloadHandler(
          #   filename = function() { 
          #     paste("Taichung_late", '.csv', sep='') 
          #   },
          #   content = function(file) {
          #     write.csv(dataset_pt_late_Taichung(), file,fileEncoding = "big5")
          #   }
          # )
          # output$downloadData_QK_Taipei <- downloadHandler(
          #   filename = function() { 
          #     paste("Taichung_QK", '.csv', sep='') 
          #   },
          #   content = function(file) {
          #     write.csv(dataset_pt_QK_Taichung(), file,fileEncoding = "big5")
          #   }
          # )
          ##### push
          dataset_push <- reactive({
            temp<-user_cat%>%filter(createtime>=input$dates_push[1]&createtime<=input$dates_push[2]&rpgid==input$dataset_push_category)
            cat_stats<-temp%>%group_by(uid)%>%summarise(n=n())%>%filter(n>=input$view_click)
            paste(unique(cat_stats$uid),collapse=",")
          })
          output$downloadData_push <- downloadHandler(
            filename = function() { 
              paste("push","_",input$dates_push[1],"_",input$dates_push[2]) 
            },
            content = function(file) {
              write(dataset_push(), file)
            }
          )
          
          ##### Sales item edited
          dataset_sie<-reactive({
            temp<-sales%>%filter(cd>=input$dates_sie[1]&cd<=input$dates_sie[2]&createtime!=lastmodifiedtime)
            upload<-sales%>%filter(cd>=input$dates_sie[1]&cd<=input$dates_sie[2])
            sales_upload<-upload%>%group_by(branchname)%>%summarise(upload=n())
            
            sales_edit<-temp%>%group_by(branchname)%>%summarise(edit=n())
            sales_delete<-temp%>%filter(deleted==1)%>%group_by(branchname)%>%summarise(delete=n())
            temp<-merge(sales_upload,sales_edit,by='branchname',all.x=TRUE)
            temp<-merge(temp,sales_delete,by='branchname',all.x=TRUE)
            
            temp2<-sales_daily_setting%>%filter(cd>=input$dates_sie[1]&cd<=input$dates_sie[2]&createtime!=lastmodifiedtime)
            sales_edit<-temp2%>%group_by(branchname)%>%summarise(auto_edit=n())
            sales_delete<-temp2%>%filter(enabled==0)%>%group_by(branchname)%>%summarise(auto_disable=n())
            temp<-merge(temp,sales_edit,by='branchname',all.x=TRUE)
            temp<-merge(temp,sales_delete,by='branchname',all.x=TRUE)
            
            
            temp[order(-temp$edit),]
          })
          output$sie_table<-renderTable({
            temp<-dataset_sie()
            temp[1:20,]
          })
          output$downloadData_sie <- downloadHandler(
            filename = function() { 
              paste("Sales_item_edited_Stats",input$dates_sie[1],"_to_",input$dates_sie[2],".csv") 
            },
            content = function(file) {
              write.csv(dataset_sie(), file,fileEncoding = "big5")
            }
          )
})

