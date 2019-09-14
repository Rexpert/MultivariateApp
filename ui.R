library(shiny)
library(shinydashboard)
library(rgl)
library(lubridate)

dashboardPage(
  header <- dashboardHeader(
    title = textOutput("title") 
  ), 
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Rectangular Coordinate", tabName = "rec", icon = icon("th")),
      menuItem("Cylinder Coordinate", tabName = "cyl", icon = icon("database")),
      menuItem("Spherical Coordinate", tabName = "sph", icon = icon("globe"))
    )
  ), 
  dashboardBody(
    tabItems(
      tabItem("rec", 
              tags$style(".col-sm-3 {height: 68vh} .well {height: 100%}"),
              fluidRow(
                valueBox(
                  tagList(tags$h3(style = "font-style: italic;font-size: 50px;text-align: center;font-family: \"Computer Modern\"", 
                                  HTML("y = x<sup>2</sup> + 2xj - j<sup>2</sup> + 1"))),
                  "", width = 12, color = "green")
              ),
              
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  titlePanel("Sample View Point"),
                  br(),br(),br(),
                  actionButton("viewPointA", "A", width = "100%"),
                  br(),br(),br(),
                  actionButton("viewPointB", "B", width = "100%"),
                  br(),br(),br(),
                  actionButton("reset", "Reset", width = "100%")
                ),
                mainPanel(
                  width = 9,
                  rglwidgetOutput("surface", height = "68vh", width = "100%")
                )
              )), 
      tabItem("cyl",
              fluidRow(
                valueBoxOutput("htmlColour", width = 12)
              ),
              fluidRow(
                box(
                  width = 4,
                  height = "65vh",
                  sliderInput("hue", "Hue:",
                              min = 0, max = 360,
                              value = 0),
                  sliderInput("sat", "Saturation:",
                              min = 0, max = 100,
                              value = 0),
                  sliderInput("bright", "Brightness:",
                              min = 0, max = 100,
                              value = 0)
                ),
                box(width = 8,
                    rglwidgetOutput("plot", width = "100%", height = "62vh")
                )
              )
      ),
      tabItem("sph", 
              fluidRow(
                valueBoxOutput("coor", width = 12)
              ),
              sidebarLayout(
                sidebarPanel(
                  tags$style(".col-sm-4 {height: 68vh} .well {height: 100%}"),
                  tags$head(tags$style(HTML("
                                            .shiny-split-layout > div {overflow: visible;}"))),
                  tags$style('.mySpan { font-weight: bold; font-size: 20px; }'),
                  tags$span("Latitude", class = "mySpan"),
                  splitLayout(
                    cellWidths = c("65%", "35%"),
                    textInput("lat", "", placeholder = "Range: 0 - 90"),
                    selectInput("dirLat", "", choices = list("N" = "N", "S" = "S"))
                  ),
                  tags$span("Longitude", class = "mySpan"),
                  splitLayout(
                    cellWidths = c("65%", "35%"),
                    textInput("long", "", placeholder = "Range: 0 - 180"),
                    selectInput("dirLong", "", choices = list("W" = "W", "E" = "E"))
                  ),
                  br(),br(),
                  splitLayout(
                    cellWidths = c("65%", "35%"),"",
                    actionButton("submit", "GO!", width = "100%")
                  )
                  ),
                mainPanel(
                  rglwidgetOutput("globe", width = "100%", height = "68vh")
                )
              )
      )
    )
  )
)