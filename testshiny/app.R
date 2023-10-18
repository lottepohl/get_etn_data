install.packages(c('shinymanager', 'shinyjs', 'shinydashboard'))
library(shinymanager)
library(shinyjs)
library(shiny)
library(shinydashboard)

credentials <- data.frame(
    user = c("shiny", "shiny2"), # mandatory
    password = c("111", "111"), # mandatory
    start = c("2015-04-15"), # optinal (all others)
    expire = c(NA, "2032-12-31"),
    admin = c(FALSE, TRUE),
    comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
    stringsAsFactors = FALSE,
    moreInfo = c("someData1", "someData2"),
    level = c(2, 0)
)

header <- dashboardHeader()

sidebar <- dashboardSidebar(
    shinyjs::useShinyjs(),
    sidebarUserPanel("User Name",
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                     # Image file should be in www/ subdir
                     image = "userimage.png"
    ),
    sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
    sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItemOutput("widgetsOutput"),
        menuItem("Charts", icon = icon("bar-chart-o"),
                 menuSubItem("Sub-item 1", tabName = "subitem1"),
                 menuSubItem("Sub-item 2", tabName = "subitem2")
        )
    )
)

body <- dashboardBody(
    tabItems(
        tabItem("dashboard",
                div(p("Dashboard tab content"))
        ),
        tabItem("widgets",
                "Widgets tab content"
        ),
        tabItem("subitem1",
                "Sub-item 1 tab content"
        ),
        tabItem("subitem2",
                "Sub-item 2 tab content"
        )
    )
)

shinyApp(
    ui = secure_app(dashboardPage(header, sidebar, body)),
    server = function(input, output, session) { 
        
        res_auth <- secure_server(
            check_credentials = check_credentials(credentials)
        )
        # Create reactive values including all credentials
        creds_reactive <- reactive({
            reactiveValuesToList(res_auth)
        })
        
        output$widgetsOutput <- renderMenu({
            if(creds_reactive()$user == "shiny"){
                menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new", badgeColor = "green") 
            }
        })
        
    }
)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 
# library(shiny)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }

# Run the application 
shinyApp(ui = ui, server = server)
