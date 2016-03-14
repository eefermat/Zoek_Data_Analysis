library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

shinyUI(fluidPage(
  
  title = "FunNow Data Analysis",
  
  
  
  hr(),
  tabsetPanel(
    tabPanel("Member Stat",
             plotOutput('Member_plot'),
             column(4,dateRangeInput('dates', 'Date range',start="2015-11-04", end=substr(Sys.time(),1,10)),
             checkboxInput('cum', 'Cumulative')),
             column(4,selectInput('x', 'X-Axis', c("week_create"),c("week_create")),
             selectInput('y', 'Y-Axis', c("Gender","Register_Type","Operating_System","Sign_Up","Total_Member"), "Gender")),
             column(4,selectInput('plot_type', 'Plot Type', c("Line","Stack_Area"),c("Line")))
             ),
    tabPanel("Login Stat",
             plotOutput('Login_plot'),
             column(4,dateRangeInput('dates_L', 'Date range',start="2015-11-04", end=substr(Sys.time(),1,10)),
                    checkboxInput('cum_L', 'Cumulative')),
             column(4,selectInput('x_L', 'X-Axis', c("Create_Time"),c("Create_Time")),
                    selectInput('y_L', 'Y-Axis', c("Day_Night","Mon_to_Sun","Weekday_Weekend"), "Day_Night")),
             column(4,selectInput('plot_type_L', 'Plot Type', c("Line","Stack_Area"),c("Line")))
    ),
    tabPanel("Orders Stat",
             plotOutput('Orders_plot'),
             column(4,dateRangeInput('dates_O', 'Date range',start="2015-11-04", end=substr(Sys.time(),1,10)),
                    checkboxInput('cum_O', 'Cumulative')),
             column(4,selectInput('x_O', 'X-Axis', c("Create_Time"),c("Create_Time")),
                    selectInput('y_O', 'Y-Axis', c("Gender","Age","Operating_System","Day_Night","Mon_to_Sun","Weekday_Weekend","Rep","Total_Order"), "Day_Night"),
                    HTML("<br>"),
                    HTML("Order statistics"),
                    tableOutput('Order_demography'),
                    HTML("<br>"),
                    HTML("buying time and book time difference")),
             column(4,selectInput('plot_type_O', 'Plot Type', c("Line","Stack_Area"),c("Line"))),
             plotOutput('Orders_time_plot')
             
             
             
             
             
    ),
    tabPanel("Merchant", 
             plotOutput('Merchant_plot'),
             plotOutput('Merchant_line_plot'),
             checkboxInput('cum_M', 'Cumulative'),
             dateRangeInput('dates_M', 'Date range',start="2015-11-04", end=substr(Sys.time(),1,10)),
             column(5,div(class="span12",tableOutput('Top_summary'))),
             column(4,HTML("Sales Stat"),div(class="span12",tableOutput('Sales_summary')),
                    HTML("Orders Stat"),div(class="span12",tableOutput('Orders_sub_summary')),
                    HTML("Orders/Sales Percentage(%)"),div(class="span12",tableOutput('Orders_Sales_summary')))
             ),
    tabPanel("Cohort/Funnel", 
             HTML("Cohort Analysis"),
             tableOutput('Cohort_plot'),
             HTML("Money Spent"),
             tableOutput('Cohort_Spent'),
             # HTML("Cohort Analysis Man 25-30"),
             # tableOutput('Cohort_plot_Man_25'),
             # HTML("Cohort Analysis Man 30-35"),
             # tableOutput('Cohort_plot_Man_30'),
             # HTML("Cohort Analysis Female 25-30"),
             # tableOutput('Cohort_plot_Female_25'),
             # HTML("Cohort Analysis Female 30-35"),
             # tableOutput('Cohort_plot_Female_30'),
             HTML("Sales Funnel"),
             plotOutput('Funnel_plot'),
             HTML("WAU Funnel"),
             plotOutput('WAU_Funnel_plot'),
             HTML("Weekly Active users"),
             plotOutput('WAU_plot'),
             HTML("Monthly Active users"),
             tableOutput('MAU'),
             HTML("Monthly Active users based on OS"),
             tableOutput('MAU_OS'),
             HTML("Weekly Active users"),
             tableOutput('WAU')
             
             
    ),
    tabPanel("Browsing Behavior", 
             dateRangeInput('dates_B', 'Date range',start="2015-01-22", end=substr(Sys.time(),1,10)),
             HTML("Catorgy vs Map/List"),
             plotOutput('Category_browsing'),
             HTML("Map/List vs Category"),
             plotOutput('Map_Browsing'),
             HTML("Category Borwsing"),
             plotOutput('Cat_Browsing'),
             column(3,div(class="span12",tableOutput('Cat_Top')),div(class="span12",tableOutput('Ave_items'))),
             column(5,tableOutput('Product_Top')),
             column(4,tableOutput('Search_Top'))
             
    ),
    tabPanel("Shopping Behavior",
             HTML("First Time Shopping"),
             plotOutput('first_shopping_dist'),
             HTML("Login before first Time Shopping"),
             plotOutput('login_vs_first_time_dist')
    ),
    tabPanel("Effectiveness", 
             HTML("Push notification"),
             tableOutput('notification_table')
    ),
    tabPanel("Map",
             dateRangeInput('dates_map', 'Date range',start="2015-11-04", end=substr(Sys.time(),1,10)),
             selectInput('city','Select City',c("台北市","新北市(板橋,新莊)","新北市(新莊,三重)","新北市(永和,中和,新店)","桃園"),c("台北市")),
             HTML("User GPS plot"),
             plotOutput('GPS_plot'),
             HTML("Buyer GPS plot"),
             plotOutput('buyer_GPS_plot'),
             HTML("Repeat Buyer GPS plot"),
             plotOutput('rep_buyer_GPS_plot'))
    )

))