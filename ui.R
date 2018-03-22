# NEWS, CONFIRMATION BIAS, AND BELIEF POLZRIZATION
# << UI >>
# by Aydin Mohseni


# Load the shiny GUI library
library(shiny)
library(ggplot2)
library(ggthemes)

# Set encoding for special characters
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(
  # CSS for visual
  includeCSS("www/style.css"),
  
  # Title
  titlePanel("News, Confirmation Bias, and Belief Polarization"),
  
  # Load MathJax
  withMathJax(),
  
  fluidRow(style = "background-color:#F2F2F2; margin-top: 30px; margin-bottom: 30px; padding: 10px",
           column(
             width = 4,
             # Introduction text:
             p(
               tags$b("Description:"),
               "This model explore the way that individual confirmation bias along with hyperbole and cherry-picking of events in the news media can lead to belief polarization."
             )
           )),
  
  # Sidebar for Parameter Input
  sidebarLayout(
    sidebarPanel(
      
      # True distribution of events in the world
      fluidRow(
        sliderInput(
          "trueStateMean",
          "Mean \\(\\mu\\):",
          min = -5,
          max = 5,
          value = 0
        ),
        sliderInput(
          "trueStateSD",
          "Standard Deviation \\(\\sigma\\):",
          min = 0,
          max = 5,
          value = 1
        )
      ), 
      
      # Hyperbole by the news
      sliderInput(
        "hyperbole",
        "Hyperbole by the news:",
        min = 1,
        max = 5,
        value = 1.5
      ),
      
      # Cherry-picking of extreme events by news
      sliderInput(
        "cherryPicking",
        "Cherry-picking of extreme events by the news:",
        min = 0,
        max = 1,
        value = 0.2
      ),
      
      # Individual confirmation bias
      sliderInput(
        "individualBias",
        "Individual confirmation bias \\(\\beta)\\:",
        min = -5,
        max = 5,
        value = 0
      )
      
    ),
    
    # Main Panel with Stationary Distribution + Simulation & Stats Panels
    mainPanel(
      plotOutput("trueStatePlotOutput", height = "350px"),
      plotOutput("newsAppearancePlotOutput", height = "350px"),
      plotOutput("IndividualBeliefPlotOutput", height = "350px")
    )
  )
))