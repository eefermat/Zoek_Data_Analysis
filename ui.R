library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
#kasmdksl
branch<-readRDS("branch")
orders<-readRDS("orders")

shinyUI(fluidPage(
  
  title = "FunNow Data Analysis",
  
  
  
  hr(),
  tabsetPanel(
    tabPanel("Member Stat",
             plotOutput('Member_plot'),
             column(4,dateRangeInput('dates', 'Date range',start="2015-11-02", end=substr(Sys.time(),1,10)),
             checkboxInput('cum', 'Cumulative')),
             column(4,selectInput('x', 'X-Axis', c("week_create"),c("week_create")),
             selectInput('y', 'Y-Axis', c("Gender","Register_Type","Operating_System","Sign_Up","Total_Member"), "Gender")),
             column(4,selectInput('plot_type', 'Plot Type', c("Line","Stack_Area"),c("Line")))
             ),
    tabPanel("Login Stat",
             plotOutput('Login_plot'),
             column(4,dateRangeInput('dates_L', 'Date range',start="2015-11-02", end=substr(Sys.time(),1,10)),
                    checkboxInput('cum_L', 'Cumulative')),
             column(4,selectInput('x_L', 'X-Axis', c("Create_Time"),c("Create_Time")),
                    selectInput('y_L', 'Y-Axis', c("Day_Night","Mon_to_Sun","Weekday_Weekend"), "Day_Night")),
             column(4,selectInput('plot_type_L', 'Plot Type', c("Line","Stack_Area"),c("Line")))
    ),
    tabPanel("Orders Stat",
             plotOutput('Orders_plot'),
             column(4,dateRangeInput('dates_O', 'Date range',start="2015-11-02", end=substr(Sys.time(),1,10)),
                    checkboxInput('cum_O', 'Cumulative')),
             column(4,selectInput('x_O', 'X-Axis', c("create_week"),c("create_week")),
                    selectInput('y_O', 'Y-Axis', c("Heads","Orders","conversion","Total_Order"), "Orders"),
                    HTML("<br>"),
                    HTML("Order statistics"),
                    tableOutput('Order_demography'),
                    HTML("<br>"),
                    HTML("buying time and book time difference")),
             plotOutput('Orders_time_plot')





    ),
    tabPanel("Category",
             plotOutput("Category_plot"),
             selectInput('Category_sel', '類別', c("all","休憩x湯屋","晚鳥過夜","桌遊x密室","美甲美睫","按摩","主題酒吧","中華職棒","電影","咖啡","親子"),"all"),
             tableOutput("Category_table"),
             dateRangeInput('dates_cat', 'Date range',start="2015-11-02", end=substr(Sys.time(),1,10)),
             tableOutput("Category_cross_table")

    ),
    tabPanel("Merchant", 
             plotOutput('Merchant_plot'),
             plotOutput('Merchant_line_plot'),
             checkboxInput('cum_M', 'Cumulative'),
             dateRangeInput('dates_M', 'Date range',start="2015-11-02", end=substr(Sys.time(),1,10)),
             column(4,tableOutput('Top_summary')),
             column(3,HTML("Sales Stat"),tableOutput('Sales_summary'),
                    HTML("Orders Stat"),tableOutput('Orders_sub_summary'),
                    HTML("Orders/Sales Percentage(%)"),tableOutput('Orders_Sales_summary')),
             
             column(4,selectInput('Type_M', '類別', c("all","休憩","美甲美睫","按摩","主題酒吧"),"all"),
                    HTML("Weekday"),tableOutput('weekday_supply_demand_summary'),
                    HTML("Weekend"),tableOutput('weekend_supply_demand_summary'))
             ),
    tabPanel("Merchant_stats", 
             dateRangeInput('dates_MS', 'Date range',start="2015-11-02", end=substr(Sys.time(),1,10)),
             selectInput('Category_sel_MS', '類別', c("all","休憩x湯屋","晚鳥過夜","桌遊x密室","美甲美睫","按摩","主題酒吧","中華職棒","電影","咖啡","親子"),"all"),
             selectInput('Weekday_MS', '平日/週末', c("all","Weekday","Weekend"),"all"),
             selectInput('Day_MS', '白天/晚上', c("all","Day","Night"),"all"),
             HTML("台北車站"),
             textOutput("local_average"),
             tableOutput("local_stats")
             
             
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
             selectInput('Category_sel_cohort', '類別', c("all","休憩x湯屋","晚鳥過夜","桌遊x密室","美甲美睫","按摩","主題酒吧","中華職棒","電影","咖啡","親子"),"all"),
             checkboxInput('cm_percentage','percentage'),
             tableOutput('Cohort_conversion_month'),
             checkboxInput('head_count','head count'),
             tableOutput('Conversion_trend'),
             HTML("Average buying size(cohort)"),
             tableOutput('Cohort_buy_size'),
             HTML("Average buying size"),
             tableOutput('buy_size'),
             tableOutput('Repeat_ratio_trend')
    ),
    tabPanel("Browsing Behavior", 
             dateRangeInput('dates_B', 'Date range',start=(Sys.Date()-30), end=Sys.Date()),
             #HTML("Catorgy vs Map/List"),
             #plotOutput('Category_browsing'),
             #HTML("Map/List vs Category"),
             #plotOutput('Map_Browsing'),
             HTML("Category Borwsing"),
             selectInput('Cat_Browsing_variable','Select Variable',c("Week","Day")),
             checkboxInput("remove_first","Remove First Day click"),
             plotOutput('Cat_Browsing'),
             column(2,div(class="span12",tableOutput('Cat_Top')),div(class="span12",tableOutput('Ave_items'))),
             column(7,HTML("Summary"),downloadButton('downloadData_mix', 'Download'),tableOutput('Product_Top_mix')),
                    # HTML("台北休憩"),downloadButton('downloadData_QK_Taipei', 'Download'),tableOutput('Product_Top_rest'),
                    # HTML("台北晚鳥"),downloadButton('downloadData_late_Taipei', 'Download'),tableOutput('Product_Top_late'),
                    # HTML("台北按摩"),downloadButton('downloadData_massage_Taipei', 'Download'),tableOutput('Product_Top_massage'),
                    # HTML("台北酒吧"),downloadButton('downloadData_bar_Taipei', 'Download'),tableOutput('Product_Top_bar'),
                    # HTML("台北美甲美睫"),downloadButton('downloadData_manicure_Taipei', 'Download'),tableOutput('Product_Top_manicure'),
                    # HTML("台中休憩"),downloadButton('downloadData_QK_Taichung', 'Download'),tableOutput('Product_Top_rest_Taichung'),
                    # HTML("台中晚鳥"),downloadButton('downloadData_late_Taichung', 'Download'),tableOutput('Product_Top_late_Taichung')),
                    #HTML("密室"),downloadButton('downloadData_escape', 'Download'),tableOutput('Product_Top_escape'),
                    #HTML("桌遊"),downloadButton('downloadData', 'Download'),tableOutput('Product_Top_board')),
             column(2,tableOutput('Search_Top'))
             
    ),
    tabPanel("Frequency", 
             selectInput('frequency_category','類別',c("all","摩鐵","商旅","美甲美睫","按摩","主題酒吧"),"all"),
             plotOutput('frequency_plot')
             
    ),
    tabPanel("Demand & Supply",
             dateRangeInput('dates_DS', 'Date range',start="2016-01-22", end=substr(Sys.time(),1,10)),
             selectInput('area_DS', '地區', c("台北","台中"),"台北"),
             selectInput('Weekday_DS', '平日/週末', c("all","Weekday","Weekend"),"all"),
             selectInput('Type_DS', '類別', c("all","摩鐵","商旅","美甲美睫","按摩","主題酒吧"),"all"),
             column(6,HTML("Sales plot"),plotOutput('GPS_supply_plot')),
             column(6,HTML("View plot"),plotOutput('GPS_views_plot'))
      
    ),
    # tabPanel("Shopping Behavior",
    #          HTML("First Time Shopping"),
    #          plotOutput('first_shopping_dist'),
    #          HTML("Login before first Time Shopping"),
    #          plotOutput('login_vs_first_time_dist')
    #),
    tabPanel("AU",
             selectInput('MAU_OS','Variable',c("ALL","IOS","ANDROID"),"ALL"),
             selectInput('MAU_var','Variable',c("MAU","DAU","DAU_MAU_ratio"),"MAU"),
             plotOutput('MAU_plot'),
             plotOutput('MAU_stacked'),
             HTML("MAU"),
             tableOutput('MAU_table')),
    tabPanel("Order time",
             dateRangeInput('dates_order_time', 'Date range',start="2015-11-02", end=substr(Sys.time(),1,10)),
             selectInput('order_weekday','Weekday',c("ALL","Weekday","Weekend"),"ALL"),
             selectInput('order_cat','Category',c("ALL","摩鐵","湯屋","商旅","美甲美睫","密室","桌遊","主題酒吧","按摩"),"ALL"),
             plotOutput('order_time_plot')),
    
    tabPanel("Merchant Comparison", 
             dateRangeInput('date_com', 'Date range',start="2015-11-02", end=substr(Sys.time(),1,10)),
             selectInput('category_com','類別',c("休憩x湯屋","晚鳥過夜"),"休憩x湯屋"),
                    selectInput('area_com','地區',unique(branch$area_detail),branch$area_detail[2]),
                    uiOutput("merchant_selector"),uiOutput("hr_selector"),checkboxInput("include_com","Include comparison merchant"),
             plotOutput('merchant_com_plot_sales'),
             plotOutput('merchant_com_plot_sales_amount')
             ),
    tabPanel("Push",
            dateRangeInput('dates_push', 'Date range',start="2016-09-05", end=substr(Sys.time(),1,10)),
            selectInput("dataset_push_type", "Customer behavior:", 
                        choices = c("view")),
            numericInput("view_click", "clicks:", 5, min = 1),
            selectInput("dataset_push_category", "Choose a category:", 
                        choices = c("晚鳥過夜","台中晚鳥過夜","來場電影","泡湯x休憩","美甲x美睫",   
                                    "按摩紓壓","主題酒吧","特色咖啡","中信兄弟","台中按摩紓壓",
                                    "台中中信兄弟","台中享樂摩鐵")),
            radioButtons("filetype", "file type:",choices = c("email", "uid")),
            downloadButton('downloadData_push', 'Download')),
    tabPanel("Sales item edit", 
             dateRangeInput('dates_sie', 'Date range',start="2015-11-02", end=substr(Sys.time(),1,10)),
             HTML("Top 20 sales item edited merchant"),
             tableOutput('sie_table'),
             downloadButton('downloadData_sie', 'Download')

             
    )
)))