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
             width = 12,
             # Introduction text:
             p(
               tags$b("Description:"),
               "This model explore the way that individual confirmation bias along with selective representation of events via the news media can lead to belief polarization. Individuals may begin with initial beliefs and a tendency to accept evidence congenial their beliefs and reject evidence uncongenial to their beliefs. The news media, on the other hand, tends to introduce several forms of distortion: (1) hyperbole, where the news presents events as more extreme than they truly are; (2) cherry-picking, where the news omit events are not sufficiently extreme; or forced balance, where the news presents both positive and negative sides of an issue with equal frequency."
             )
           )),
  
  # Sidebar for Parameter Input
  sidebarLayout(
    sidebarPanel(
      
      # True distribution of events in the world
      fluidRow(
        sliderInput(
          "trueStateMean",
          "True Mean \\(\\mu\\):",
          min = -5,
          max = 5,
          value = 0,
          step = 0.5
        ),
        sliderInput(
          "trueStateSD",
          "True Standard Deviation \\(\\sigma\\):",
          min = 1,
          max = 5,
          value = 1,
          step = 0.5
        )
      ), 
      
      # Hyperbole by the news
      sliderInput(
        "hyperbole",
        "Hyperbole by the news:",
        min = 1,
        max = 5,
        value = 1.5,
        step = 0.5
      ),
      
      # Cherry-picking of extreme events by news
      sliderInput(
        "cherryPicking",
        "Cherry-picking of extreme events by the news:",
        min = 0,
        max = 5,
        value = 0.5,
        step = 0.5
      ),
      
      # Individual confirmation bias
      sliderInput(
        "individualBias",
        "Individual confirmation bias \\(\\beta\\):",
        min = -5,
        max = 5,
        value = 0
      )
      
    ),
    
    # Main Panel with Stationary Distribution + Simulation & Stats Panels
    mainPanel(
      plotOutput("trueStatePlotOutput", height = "250px"),
      plotOutput("newsAppearancePlotOutput", height = "250px"),
      plotOutput("IndividualBeliefPlotOutput", height = "250px")
    )
  )
))