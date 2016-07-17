library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
#kasmdksl
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
             HTML("Cohort Conversion Analysis"),
             selectInput('cohort_area', '地區', c("台中","台北"),"台北"),
             tableOutput('Cohort_conversion_plot'),
             HTML("Money Spent"),
             checkboxInput('LTV','LTV'),
             checkboxInput('fun_pi','Fun Pi'),
             tableOutput('Cohort_Spent'),
             HTML("Conversion"),
             checkboxInput('cm_percentage','percentage'),
             tableOutput('Cohort_conversion_month'),
             tableOutput('Conversion_trend'),
             HTML("Average buying size"),
             tableOutput('Cohort_buy_size'),
             HTML("Average rep"),
             tableOutput('Cohort_repeat'),
             tableOutput('Repeat_trend'),
             tableOutput('Repeat_ratio_trend')
    ),
    tabPanel("Browsing Behavior", 
             dateRangeInput('dates_B', 'Date range',start="2016-01-22", end=substr(Sys.time(),1,10)),
             HTML("Catorgy vs Map/List"),
             plotOutput('Category_browsing'),
             HTML("Map/List vs Category"),
             plotOutput('Map_Browsing'),
             HTML("Category Borwsing"),
             selectInput('Cat_Browsing_variable','Select Variable',c("Week","Day")),
             checkboxInput("remove_first","Remove First Day click"),
             plotOutput('Cat_Browsing'),
             column(3,div(class="span12",tableOutput('Cat_Top')),div(class="span12",tableOutput('Ave_items'))),
             column(5,tableOutput('Product_Top_rest'),tableOutput('Product_Top_massage'),tableOutput('Product_Top_bar'),tableOutput('Product_Top_manicure'),tableOutput('Product_Top_escape'),tableOutput('Product_Top_board')),
             column(4,tableOutput('Search_Top'))
             
    ),
    tabPanel("Shopping Behavior",
             HTML("First Time Shopping"),
             plotOutput('first_shopping_dist'),
             HTML("Login before first Time Shopping"),
             plotOutput('login_vs_first_time_dist')
    ),
    tabPanel("AU",
             selectInput('MAU_OS','Variable',c("ALL","IOS","ANDROID"),"ALL"),
             selectInput('MAU_var','Variable',c("MAU","DAU","DAU_MAU_ratio"),"MAU"),
             plotOutput('MAU_plot'),
             plotOutput('MAU_stacked'),
             HTML("MAU"),
             tableOutput('MAU_table')),
    tabPanel("Order time",
             dateRangeInput('dates_order_time', 'Date range',start="2015-11-04", end=substr(Sys.time(),1,10)),
             selectInput('order_weekday','Weekday',c("ALL","Weekday","Weekend"),"ALL"),
             selectInput('order_cat','Category',c("ALL","摩鐵","湯屋","商旅","美甲美睫","密室","桌遊","主題酒吧","按摩"),"ALL"),
             plotOutput('order_time_plot'))
)))