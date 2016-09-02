library(markdown)

shinyUI(navbarPage(title=div(img(src="https://pbs.twimg.com/profile_images/665252061873700864/uVzWPFZq.jpg",width="55px")),
  tabPanel("Regional Business",
    sidebarLayout(
      sidebarPanel( h3("Welcome to Superstore"),
                    selectInput("Priority", "Order Priority:",
                                c("Low" = "Low","Medium" = "Medium","Critical" = "Critical","High" = "High","Not Specified"="Not Specified","All"="all"),
                                selected = "all"),
                    radioButtons("RegionType", label = h4("Choose your Region"),
                                 choices = list("West" = "West", "East" = "East", "Central" = "Central", "South" = "South","All"="all"), 
                                 selected = "all"),
                    hr()),
      mainPanel(textOutput("title"),uiOutput("ggvis1_ui"),ggvisOutput("ggvis1"),plotOutput("plot"))
      )),
  tabPanel("State Business",
           sidebarLayout(
             sidebarPanel( h3("Sales Orders per Business type"),
                           selectInput("states", "Select State:", choices = superstore.states,
                                       selected = "New York"),
                           hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),
                           hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr()),
             mainPanel(uiOutput("ggvis2_ui"),ggvisOutput("ggvis2"),
                        h3("Profit/Loss per State"),
                        plotlyOutput("trendPlot"))
           )),
  tabPanel("Order History",
           sidebarLayout(
             sidebarPanel(dateRangeInput("superstoreDates", label = h3("Sales Orders between Dates"),start = "2015-01-01",end = "2015-06-30"),
                           hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr()),
             mainPanel(uiOutput("ggvis3_ui"),ggvisOutput("ggvis3"))
           )),
  tabPanel("Business Summary",
    verbatimTextOutput("summary")),
  navbarMenu("More",
    tabPanel("Table",dataTableOutput("table")),
    tabPanel("About",
      fluidRow(column(6,includeMarkdown("about.md")),
               column(3,img(class="img-polaroid",
                            src=paste0("https://www.nbc.com/sites/nbcunbc/files/files/styles/1280x720/",
                                       "public/images/2015/11/24/",
                                       "mdot-Superstore.jpg?itok=Ryu4dLv8")),
                      tags$small("Source: nbc "))
      )
    )
  )
))
