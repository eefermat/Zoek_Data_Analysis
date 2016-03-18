library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
require(ggmap)
require(XLConnect)
require(magrittr)

setwd("C:\\Users\\SzuYuan\\Desktop\\實習\\App")

user_gps <- readRDS("User_GPS")
branch_gps <- readRDS("branch_GPS")
buyers_gps <- readRDS("buyers_gps")
rep_buyers_gps <- readRDS("rep_buyers_gps")
member<-readRDS("member_data")
member_birth<-readRDS("member_birth")
login<-readRDS("login")
orders_member<-readRDS("orders_member")
orders<-readRDS("orders")
branch<-readRDS("branch")
sales<-readRDS("sales")
user_cat<-readRDS("user_cat")
user_pt<-readRDS("user_pt")
user_search<-readRDS("user_search")
notification_stat<-readRDS("notification_stat")
first_shopping<-readRDS("first_shopping")
userlog_member<-readRDS("userlog_member")
funnel_stat<-readRDS("funnel_stat")
MAU<-readRDS("MAU")
MAU_OS<-readRDS("MAU_OS")
WAU<-readRDS("WAU")
push_list<-readRDS("push_list")


# Shiny Main Program
shinyServer(function(input, output) {
      #===============Data================
      # Member Data
          dataset_member <- reactive({
              {   week_start<-(floor(input$dates[1]-as.Date("2015-11-04"))/7)+1
                  week_end<-(floor(input$dates[2]-as.Date("2015-11-04"))/7)+1
                  if(input$y=="Gender"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end)&(Register_Type!="GUEST"))%>%group_by(week_create,Gender)%>%summarise(n=n())%>%group_by(Gender)%>%mutate(Cumul=cumsum(n))
                  }
                  else if(input$y=="Operating_System"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end)&(Register_Type!="GUEST"))%>%group_by(week_create,Operating_System)%>%summarise(n=n())%>%group_by(Operating_System)%>%mutate(Cumul = cumsum(n))
                  }
                  else if(input$y=="Register_Type"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end)&(Register_Type!="GUEST"))%>%group_by(week_create,Register_Type)%>%summarise(n=n())%>%group_by(Register_Type)%>%mutate(Cumul = cumsum(n))
                  }
                  else if(input$y=="Sign_Up"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end))%>%group_by(week_create,Sign_Up)%>%summarise(n=n())%>%group_by(Sign_Up)%>%mutate(Cumul = cumsum(n))
                  }
                  else if(input$y=="Total_Member"){
                      member%>%filter((week_create>=week_start)&(week_create<=week_end)&(Register_Type!="GUEST"))%>%group_by(week_create)%>%summarise(n=n())%>%mutate(Cumul = cumsum(n))
                  }
              }
           
          })
  # Login Data
          dataset_login <- reactive({
            { week_start<-(floor(input$dates_L[1]-as.Date("2015-11-04"))/7)+1
              week_end<-(floor(input$dates_L[2]-as.Date("2015-11-04"))/7)+1
              if(input$y_L=="Day_Night"){
                  login%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&eventname=="auth")%>%group_by(Create_Time,Day_Night)%>%summarise(n=n())%>%group_by(Day_Night)%>%mutate(Cumul=cumsum(n))
              }
              else if(input$y_L=="Mon_to_Sun"){
                  login%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&eventname=="auth")%>%group_by(Create_Time,Mon_to_Sun)%>%summarise(n=n())%>%group_by(Mon_to_Sun)%>%mutate(Cumul = cumsum(n))
              }
              else if(input$y_L=="Weekday_Weekend"){
                  login%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&eventname=="auth")%>%group_by(Create_Time,Weekday_Weekend)%>%summarise(n=n())%>%group_by(Weekday_Weekend)%>%mutate(Cumul = cumsum(n))
              }
            }
            
          })
          
          # Orders Data
          dataset_orders_member <- reactive({
            { week_start<-(floor(input$dates_O[1]-as.Date("2015-11-04"))/7)+1
            week_end<-(floor(input$dates_O[2]-as.Date("2015-11-04"))/7)+1
            if(input$y_O=="Gender"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Gender)%>%summarise(n=n())%>%group_by(Gender)%>%mutate(Cumul=cumsum(n))
            }
            else if(input$y_O=="Day_Night"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Day_Night)%>%summarise(n=n())%>%group_by(Day_Night)%>%mutate(Cumul=cumsum(n))
            }
            else if(input$y_O=="Age"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Age)%>%summarise(n=n())%>%group_by(Age)%>%mutate(Cumul=cumsum(n))
            }
            else if(input$y_O=="Operating_System"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Operating_System)%>%summarise(n=n())%>%group_by(Operating_System)%>%mutate(Cumul=cumsum(n))
            }
            else if(input$y_O=="Mon_to_Sun"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Mon_to_Sun)%>%summarise(n=n())%>%group_by(Mon_to_Sun)%>%mutate(Cumul = cumsum(n))
            }
            else if(input$y_O=="Weekday_Weekend"){
                orders_member%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Weekday_Weekend)%>%summarise(n=n())%>%group_by(Weekday_Weekend)%>%mutate(Cumul = cumsum(n))
            }
            else if(input$y_O=="Rep"){
                temp<-merge(orders,select(member,uid,week_create))
                temp$Rep<-"First"
                temp$Rep[temp$Create_Time!=temp$week_create]<-"Rep"
                temp%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time,Rep)%>%summarise(n=n())%>%mutate(Cumul = cumsum(n))
            }
            else if(input$y_O=="Total_Order"){
                merge(orders,select(member,uid))%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(Create_Time)%>%summarise(n=n())%>%mutate(Cumul = cumsum(n))
            }
            }
            
          })
          
          # Merchant Data
          dataset_Merchant_Top <- reactive({
              week_start<-floor((input$dates_M[1]-as.Date("2015-11-04"))/7)+1
              week_end<-floor((input$dates_M[2]-as.Date("2015-11-04"))/7)+1
              temp<-orders%>%filter((Create_Time>=week_start)&(Create_Time<=week_end)&(status_name=="Paid"))%>%group_by(branchname)%>%summarise(n=n())
              temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
              temp<-temp[order(-temp$n),]
              colnames(temp)<-c("商家","購買次數","地區")
              temp
          })
          dataset_Merchant_Line <- reactive({
              branch%>%group_by(week)%>%summarise(n=n())%>%mutate(Cumul = cumsum(n))
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
          # GPS Data
          dataset_GPS<-reactive({user_gps%>%filter((createtime>=input$dates_map[1])&(createtime<=input$dates_map[2]))})
          dataset_buyer_GPS<-reactive({buyers_gps%>%filter((createtime>=input$dates_map[1])&(createtime<=input$dates_map[2]))})
          dataset_rep_buyer_GPS<-reactive({rep_buyers_gps%>%filter((createtime>=input$dates_map[1])&(createtime<=input$dates_map[2]))})
          
          #===============Table==============
          output$Order_demography<-renderTable({
              orders_member%>%filter(status_name=="Paid")%>%group_by(Gender,Age)%>%summarise(n=n()) 
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
              for (i in 1:max(member$week_create)){
                  col<-1
                  for (j in i:max(userlog_member$Create_Time)){
                      temp<-filter(userlog_member,week_create==i)
                      cohort[i,col]<-sum(member$uid[member$week_create==i&member$Sign_Up=="Sign-up"]%in%unique(temp[temp$Create_Time==j,1]))
                      colnames(cohort)[j]<-paste("Week",j,sep=" ")
                      col%<>%+1
                  }
              }
              cohort<-(cohort/cohort[,1])
        
              for(i in 1:max(member$week_create)){
                  cohort_date[i,1]<-paste(as.Date("2015-11-04")+7*(i-1))
              }
              for (i in 1:max(member$week_create)){
                  rownames(cohort_date)[i]<-paste("Week",i,sep=" ")
              }
              colnames(cohort_date)<-"Date"
              cbind(cohort_date,cohort)
            
          })
          output$Cohort_Spent<-renderTable({
              cohort_spent<-data.frame()
              cohort_date<-data.frame()
              for(i in 1:max(member$week_create)){
                  temp<-filter(member,week_create==i)
                  temp<-orders[orders$uid%in%temp$uid,]
                  temp<-filter(temp,status_name=="Paid")
                  cohort_spent[i,1]<-sum(temp$amount)
              }
              for(i in 1:max(member$week_create)){
                  cohort_date[i,1]<-paste(as.Date("2015-11-04")+7*(i-1))
              }
              for (i in 1:max(member$week_create)){
                  rownames(cohort_date)[i]<-paste("Week",i,sep=" ")
              }
              colnames(cohort_spent)<-"Amount(NTD)"
              colnames(cohort_date)<-"Date"
              cbind(cohort_date,cohort_spent)
          })
          output$Cohort_plot_Man_25<-renderTable({
            temp_member<-filter(member_birth,age=="Age 25-30"&Gender=="Male")
            cohort<-data.frame()
            for (i in 1:max(temp_member$week_create)){
                col<-1
                for (j in i:max(userlog_member$Create_Time)){
                  temp<-filter(userlog_member,week_create==i)
                  cohort[i,col]<-sum(temp_member$uid[temp_member$week_create==i&temp_member$Sign_Up=="Sign-up"]%in%unique(temp[temp$Create_Time==j,1]))
                  colnames(cohort)[j]<-paste("Week",j,sep=" ")
                  col%<>%+1
                }
            }
            cohort<-(cohort/cohort[,1])
            for (i in 1:max(temp_member$week_create)){
              rownames(cohort)[i]<-paste("Week",i,sep=" ")
            }
            for (i in 1:max(userlog_member$week_create)){
              rownames(cohort)[j]<-paste("Week",j,sep=" ")
            }
            cohort
            
          })
          
          output$Cohort_plot_Man_30<-renderTable({
              temp_member<-filter(member_birth,age=="Age 30-35"&Gender=="Male")
              cohort<-data.frame()
              for (i in 1:max(temp_member$week_create)){
                col<-1
                for (j in i:max(userlog_member$Create_Time)){
                  temp<-filter(userlog_member,week_create==i)
                  cohort[i,col]<-sum(temp_member$uid[temp_member$week_create==i&temp_member$Sign_Up=="Sign-up"]%in%unique(temp[temp$Create_Time==j,1]))
                  colnames(cohort)[j]<-paste("Week",j,sep=" ")
                  col%<>%+1
                }
              }
              cohort<-(cohort/cohort[,1])
              for (i in 1:max(temp_member$week_create)){
                rownames(cohort)[i]<-paste("Week",i,sep=" ")
              }
              for (i in 1:max(userlog_member$week_create)){
                rownames(cohort)[j]<-paste("Week",j,sep=" ")
              }
              cohort
            
          })
          
          output$Cohort_plot_Female_25<-renderTable({
            temp_member<-filter(member_birth,age=="Age 25-30"&Gender=="Female")
            cohort<-data.frame()
            for (i in 1:max(temp_member$week_create)){
              col<-1
              for (j in i:max(userlog_member$Create_Time)){
                temp<-filter(userlog_member,week_create==i)
                cohort[i,col]<-sum(temp_member$uid[temp_member$week_create==i&temp_member$Sign_Up=="Sign-up"]%in%unique(temp[temp$Create_Time==j,1]))
                colnames(cohort)[j]<-paste("Week",j,sep=" ")
                col%<>%+1
              }
            }
            cohort<-(cohort/cohort[,1])
            for (i in 1:max(temp_member$week_create)){
              rownames(cohort)[i]<-paste("Week",i,sep=" ")
            }
            for (i in 1:max(userlog_member$week_create)){
              rownames(cohort)[j]<-paste("Week",j,sep=" ")
            }
            cohort
            
          })
          
          output$Cohort_plot_Female_30<-renderTable({
            temp_member<-filter(member_birth,age=="Age 30-35"&Gender=="Female")
            cohort<-data.frame()
            for (i in 1:max(temp_member$week_create)){
              col<-1
              for (j in i:max(userlog_member$Create_Time)){
                temp<-filter(userlog_member,week_create==i)
                cohort[i,col]<-sum(temp_member$uid[temp_member$week_create==i&temp_member$Sign_Up=="Sign-up"]%in%unique(temp[temp$Create_Time==j,1]))
                colnames(cohort)[j]<-paste("Week",j,sep=" ")
                col%<>%+1
              }
            }
            cohort<-(cohort/cohort[,1])
            for (i in 1:max(temp_member$week_create)){
              rownames(cohort)[i]<-paste("Week",i,sep=" ")
            }
            for (i in 1:max(userlog_member$week_create)){
              rownames(cohort)[j]<-paste("Week",j,sep=" ")
            }
            cohort
            
          })
          output$Orders_Sales_summary<-renderTable({
            
            (dataset_Orders_sub()/dataset_Sales()*100)
            
          })
          
          output$Cat_Top<-renderTable({
              temp<-dataset_cat()%>%filter(start==0)%>%group_by(rpgid)%>%summarise(n=n())
              names(temp)<-c("Category","Tot_views")
              temp[order(-temp$Tot_views),]
              
          })
          output$Product_Top<-renderTable({
            temp<-dataset_pt()%>%group_by(branchname)%>%summarise(n=n())
            names(temp)<-c("branchname","Views")
            temp<-merge(temp,select(branch,branchname,area_detail),by="branchname")
            temp<-temp[order(-temp$Views),]
            temp$order_count<-0
            for (i in 1:length(temp$branchname)){
              temp$order_count[i]<-as.numeric(sum((orders$branchname==temp$branchname[i])&(orders$status_name=="Paid")))
            }
            names(temp)<-c("Merchants","Views","地區","銷售")
            temp[1:20,]
          })
          output$Search_Top<-renderTable({
            temp<-dataset_search()%>%group_by(search)%>%summarise(n=n())
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
          output$MAU<-renderTable({
            MAU
          })
          output$MAU_OS<-renderTable({
            MAU_OS
          })
          output$WAU<-renderTable({
            WAU
          })
          #notification
          output$notification_table <- renderTable({
            push_list
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
            
            temp<-branch%>%group_by(area,type)%>%summarise(n=n())
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
            temp<-orders%>%filter(status_name=="Paid")%>%group_by(time_diff)%>%summarise(n=n())
            p<- ggplot(data = temp) + 
              geom_bar(aes(x = "", y = n, fill = time_diff), stat = "identity") +
              coord_polar(theta="y")+theme_grey(base_family = "STKaiti")
            p
            })
          #Sales Funnel 
          output$Funnel_plot <- renderPlot({
            p <- ggplot(funnel_stat, aes( week_create, n))
            p + geom_area(aes(colour = Type, fill= Type), position = 'stack') +scale_x_continuous(breaks=seq(0, 52, 1))
          })
          #WAU_Funnel_plot 
          output$WAU_Funnel_plot <- renderPlot({
        #     temp_L<-select(WAU,week,WAU_Login)
        #     temp_L$Type<-"Login"
        #     names(temp_L)<-c("week","n","Type")
        #     temp_L$n<-temp_L$n/WAU$Total_member
            temp_RL<-select(WAU,week,WAU_Rep_Login)
            temp_RL$Type<-"Rep_Login"
            names(temp_RL)<-c("week","n","Type")
            temp_RL$n<-temp_RL$n/WAU$WAU_Login
            temp_I<-select(WAU,week,WAU_Intention)
            temp_I$Type<-"Intention"
            names(temp_I)<-c("week","n","Type")
            temp_I$n<-temp_I$n/WAU$WAU_Login
            temp_P<-select(WAU,week,WAU_Paid)
            temp_P$Type<-"Paid"
            names(temp_P)<-c("week","n","Type")
            temp_P$n<-temp_P$n/WAU$WAU_Login
            temp<-rbind(temp_RL,temp_I,temp_P)
            p <- ggplot(temp, aes(week, n))
            p + geom_area(aes(colour = Type, fill= Type), position = 'stack') +scale_x_continuous(breaks=seq(0, 52, 1))
          })
          #WAU
          output$WAU_plot<-renderPlot({
            p <- ggplot(WAU, aes(x=week, y=WAU_Login))+
              geom_line()+
              labs(y="WAU",x="week")+
              theme(panel.grid.minor.x=element_blank())+
              geom_text(data=WAU,aes(label=WAU_Login))+scale_x_continuous(breaks=seq(0, 52, 1))
            p
          })
          #User Behavior 
          output$Category_browsing <- renderPlot({
            temp<-dataset_cat()%>%filter(start==0)%>%group_by(rpgid,forMap)%>%summarise(n=n())
            p<- ggplot(data = temp) + 
              geom_bar(aes(x = "", y = n, fill = forMap), stat = "identity") +
              facet_wrap(~rpgid) +theme_grey(base_family = "STKaiti")
            p
          })
          
          output$Map_Browsing <- renderPlot({
            temp<-dataset_cat()%>%filter(start==0)%>%group_by(rpgid,forMap)%>%summarise(n=n())
            p<- ggplot(data = temp) + 
              geom_bar(aes(x = "", y = n, fill = rpgid), stat = "identity") +
              facet_wrap(~forMap) +theme_grey(base_family = "STKaiti")
            p
          })
          output$Cat_Browsing <- renderPlot({
            temp<-dataset_cat()%>%group_by(week,rpgid)%>%summarise(n=n())
            p <- ggplot(temp, aes(x=week, y=n,color=rpgid))+
              geom_line()+
              labs(y="Freq",x="week")+
              theme(panel.grid.minor.x=element_blank())+
              geom_text(data=temp,aes(label=n))+scale_x_continuous(breaks=seq(0, 52, 1)) +theme_grey(base_family = "STKaiti")
            p
          })
          
          output$first_shopping_dist <- renderPlot({
            hist(as.numeric(first_shopping$time_diff),col="red",breaks=50,xlab="Days between account created time and first shopping time")
          })
          output$login_vs_first_time_dist <- renderPlot({
            plot(first_shopping$time_diff,first_shopping$browse_count)  
          })
          
        
          #GPS Plot
          output$GPS_plot <- renderPlot({
            if (input$city=="台北市"){
              gps_lon<-121.5452817
              gps_lat<-25.0672969
              zoom_par<-13
            } else if (input$city=="桃園"){
              gps_lon<-121.2638554
              gps_lat<-24.9991417
              zoom_par<-13
            } else if (input$city=="新北市(板橋,新莊)"){
              gps_lon<-121.4416995
              gps_lat<-25.0159169
              zoom_par<-13
            } else if (input$city=="新北市(新莊,三重)"){
              gps_lon<-121.4756995
              gps_lat<-25.0759169
              zoom_par<-14
            } else if (input$city=="新北市(永和,中和,新店)"){
              gps_lon<-121.5002331
              gps_lat<-24.9757189
              zoom_par<-13
            }
            mapgilbert <- get_map(location = c(lon=gps_lon,lat= gps_lat), zoom = zoom_par,
                                  maptype = "roadmap", scale = 2)
            p<-ggmap(mapgilbert) + 
              geom_point(data=branch_gps, mapping=aes(lng, lat,color=category),size=4) + 
              stat_density2d(dataset_GPS(), mapping=aes(x=lng, y=lat, fill=..level..), geom="polygon", alpha=0.5)+ 
              scale_fill_gradient(low = "green", high = "red")+
              theme_grey(base_family = "STKaiti")
            p
          })
          
          output$buyer_GPS_plot <- renderPlot({
            if (input$city=="台北市"){
              gps_lon<-121.5452817
              gps_lat<-25.0672969
              zoom_par<-13
            } else if (input$city=="桃園"){
              gps_lon<-121.2638554
              gps_lat<-24.9991417
              zoom_par<-13
            } else if (input$city=="新北市(板橋,新莊)"){
              gps_lon<-121.4416995
              gps_lat<-25.0159169
              zoom_par<-13
            } else if (input$city=="新北市(新莊,三重)"){
              gps_lon<-121.4756995
              gps_lat<-25.0759169
              zoom_par<-14
            } else if (input$city=="新北市(永和,中和,新店)"){
              gps_lon<-121.5002331
              gps_lat<-24.9757189
              zoom_par<-13
            }
            mapgilbert <- get_map(location = c(lon=gps_lon,lat= gps_lat), zoom = zoom_par,
                                  maptype = "roadmap", scale = 2)
            p<-ggmap(mapgilbert) + 
              geom_point(data=branch_gps, mapping=aes(lng, lat,color=category),size=4) + 
              stat_density2d(dataset_buyer_GPS(), mapping=aes(x=lng, y=lat, fill=..level..), geom="polygon", alpha=0.5)+ 
              scale_fill_gradient(low = "green", high = "red")+
              theme_grey(base_family = "STKaiti")
            p
          })
          
          output$rep_buyer_GPS_plot <- renderPlot({
            if (input$city=="台北市"){
              gps_lon<-121.5452817
              gps_lat<-25.0672969
              zoom_par<-13
            } else if (input$city=="桃園"){
              gps_lon<-121.2638554
              gps_lat<-24.9991417
              zoom_par<-13
            } else if (input$city=="新北市(板橋,新莊)"){
              gps_lon<-121.4416995
              gps_lat<-25.0159169
              zoom_par<-13
            } else if (input$city=="新北市(新莊,三重)"){
              gps_lon<-121.4756995
              gps_lat<-25.0759169
              zoom_par<-14
            } else if (input$city=="新北市(永和,中和,新店)"){
              gps_lon<-121.5002331
              gps_lat<-24.9757189
              zoom_par<-13
            }
            mapgilbert <- get_map(location = c(lon=gps_lon,lat= gps_lat), zoom = zoom_par,
                                  maptype = "roadmap", scale = 2)
            p<-ggmap(mapgilbert) + 
              geom_point(data=branch_gps, mapping=aes(lng, lat,color=category),size=4) + 
              stat_density2d(dataset_rep_buyer_GPS(), mapping=aes(x=lng, y=lat, fill=..level..), geom="polygon", alpha=0.5)+ 
              scale_fill_gradient(low = "green", high = "red")+
              theme_grey(base_family = "STKaiti")
            p
          })
          dataset_AU <- reactive({
            
            
            
            if(input$AU_variable=="All"){
              if(input$AU_select=="DAU_ratio"){
                member_all=member_AU%>%group_by(date)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%group_by(date)%>%distinct(uid)%>%summarise(active=n())
                member_all=merge(member_all,userlog_all,by="date",all.x = T)
                member_all%<>%mutate(DAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%group_by(date)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by="date",all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all

              }
              else if(input$AU_select=="WAU_ratio"){
                member_all=member_AU%>%group_by(week)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%group_by(week)%>%distinct(uid)%>%summarise(active=n())
                member_all=merge(member_all,userlog_all,by="week",all.x = T)
                member_all%<>%mutate(WAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%group_by(week)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by="week",all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
              else if(input$AU_select=="MAU_ratio"){
                member_all=member_AU%>%group_by(month)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%group_by(month)%>%distinct(uid)%>%summarise(active=n())
                member_all=merge(member_all,userlog_all,by="month",all.x = T)
                member_all%<>%mutate(MAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%group_by(month)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by="month",all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
              
            }
            else if(input$AU_variable=="gender"){
              if(input$AU_select=="DAU_ratio"){
                member_all=member_AU%>%group_by(gender,date)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%group_by(gender,date)%>%distinct(uid)%>%summarise(active=n())
                member_all=merge(member_all,userlog_all,by=c("gender","date"),all.x = T)
                member_all%<>%mutate(DAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%group_by(gender,date)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by=c("gender","date"),all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
                
              
              else if(input$AU_select=="WAU_ratio"){
                member_all=member_AU%>%group_by(gender,week)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%group_by(gender,week)%>%distinct(uid)%>%summarise(active=n())
                member_all=merge(member_all,userlog_all,by=c("gender","week"),all.x = T)
                member_all%<>%mutate(WAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%group_by(gender,week)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by=c("gender","week"),all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
              else if(input$AU_select=="MAU_ratio"){
                member_all=member_AU%>%group_by(gender,month)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%group_by(gender,month)%>%distinct(uid)%>%summarise(active=n())
                member_all=merge(member_all,userlog_all,by=c("gender","month"),all.x = T)
                member_all%<>%mutate(MAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%group_by(gender,month)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by=c("gender","month"),all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
            }
            
            else if(input$AU_variable=="os"){
              if(input$AU_select=="DAU_ratio"){
                member_all=member_AU%>%group_by(os,date)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%group_by(os,date)%>%distinct(uid)%>%summarise(active=n())
                member_all=merge(member_all,userlog_all,by=c("os","date"),all.x = T)
                member_all%<>%mutate(DAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%group_by(os,date)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by=c("os","date"),all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
              else if(input$AU_select=="WAU_ratio"){
                member_all=member_AU%>%group_by(os,week)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%group_by(os,week)%>%distinct(uid)%>%summarise(active=n())
                member_all=merge(member_all,userlog_all,by=c("os","week"),all.x = T)
                member_all%<>%mutate(WAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%group_by(os,week)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by=c("os","week"),all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
              else if(input$AU_select=="MAU_ratio"){
                member_all=member_AU%>%group_by(os,month)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%group_by(os,month)%>%distinct(uid)%>%summarise(active=n())
                member_all=merge(member_all,userlog_all,by=c("os","month"),all.x = T)
                member_all%<>%mutate(MAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%group_by(os,month)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by=c("os","month"),all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
            }
            else if(input$AU_variable=="age"){
              if(input$AU_select=="DAU_ratio"){
                member_all=member_AU%>%filter((age=="(25,30]")|(age=="(30,35]"))%>%group_by(age,date)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%filter(age2=="(25,30]"|age2=="(30,35]")%>%group_by(age2,date)%>%distinct(uid)%>%summarise(active=n())
                colnames(userlog_all)[1]="age"
                member_all=merge(member_all,userlog_all,by=c("age","date"),all.x = T)
                member_all%<>%mutate(DAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%filter((age=="(25,30]")|(age=="(30,35]"))%>%group_by(age,date)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by=c("age","date"),all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
                
              }
              else if(input$AU_select=="WAU_ratio"){
                member_all=member_AU%>%filter((age=="(25,30]")|(age=="(30,35]"))%>%group_by(age,week)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%filter(age2=="(25,30]"|age2=="(30,35]")%>%group_by(age2,week)%>%distinct(uid)%>%summarise(active=n())
                colnames(userlog_all)[1]="age"
                member_all=merge(member_all,userlog_all,by=c("age","week"),all.x = T)
                member_all%<>%mutate(WAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%filter((age=="(25,30]")|(age=="(30,35]"))%>%group_by(age,week)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by=c("age","week"),all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
              else if(input$AU_select=="MAU_ratio"){
                member_all=member_AU%>%filter((age=="(25,30]")|(age=="(30,35]"))%>%group_by(age,month)%>%summarise(member=sum(member))%>%mutate(cumul_member=cumsum(member))
                userlog_all=userlog_AU%>%filter(age2=="(25,30]"|age2=="(30,35]")%>%group_by(age2,month)%>%distinct(uid)%>%summarise(active=n())
                colnames(userlog_all)[1]="age"
                member_all=merge(member_all,userlog_all,by=c("age","month"),all.x = T)
                member_all%<>%mutate(MAU_ratio=active/cumul_member)
                member_all
                if(input$AU_buy){
                  orders_all=orders_AU%>%filter(status_name=="Paid")%>%filter((age=="(25,30]")|(age=="(30,35]"))%>%group_by(age,month)%>%summarise(orders=n())
                  member_all=merge(member_all,orders_all,by=c("age","month"),all.x = T)
                  member_all%<>%mutate(orders_ratio=orders/active)
                  member_all
                }
                else
                  member_all
              }
              
            }
            
            
            
          })
          
          output$AU_table <- renderTable({
            if(input$AU_select=="DAU_ratio"){
              dataset_AU()%>%mutate(date=as.character(date))
            }
            else if(input$AU_select=="WAU_ratio"){
              dataset_AU()
            }
            else if(input$AU_select=="MAU_ratio"){
              dataset_AU()%>%mutate(month=as.character(month))
            }
            
            
          })
          output$AU_plot<-renderPlot({
            if(input$AU_select=="DAU_ratio"){
              if(input$AU_buy){
                p <- ggplot(dataset_AU(), aes_string(x="date",y="orders_ratio"))+geom_line()
              }
              else
                p <- ggplot(dataset_AU(), aes_string(x="date",y="DAU_ratio"))+geom_line()
              if(input$AU_variable!="All"){
                p+aes_string(color=input$AU_variable)
              }
              else
                p
              
            }
            else if(input$AU_select=="WAU_ratio"){
              if(input$AU_buy){
                p <- ggplot(dataset_AU(), aes_string(x="week",y="orders_ratio"))+geom_line()
              }
              else
                p <- ggplot(dataset_AU(), aes_string(x="week",y="WAU_ratio"))+geom_line()
              if(input$AU_variable!="All"){
                p+aes_string(color=input$AU_variable)
              }
              else
                p
              
            }
            else if(input$AU_select=="MAU_ratio"){
              if(input$AU_buy){
                p <- ggplot(dataset_AU(), aes_string(x="month",y="orders_ratio"))+geom_line()
              }
              else
                p <- ggplot(dataset_AU(), aes_string(x="month",y="MAU_ratio"))+geom_line()
              if(input$AU_variable!="All"){
                p+aes_string(color=input$AU_variable)
              }
              else
                p
              
            }
          })
          
})

