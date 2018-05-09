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
             width = 6,
             # Introduction text:
             div(HTML(
               "<strong>Description:</strong> This model explore the way that individual confirmation bias along with selective or distorted representations of events by the news media can lead to individual belief polarization. There is a true distribution of events which occur and are then selected and/or distorted by the news. Individuals begin with prior beliefs and a tendency to accept news congenial those beliefs and reject news uncongenial to those beliefs. Upon hearing the news, individuals select which new items to trust, given their biases, and update their beliefs accordingly."
             ))
           ),
           column(
             width = 6,
             div(HTML(
               "We consider several ways in which the news media may select or distort events: (1) <strong>hyperbole</strong>, where the news presents events as more extreme than they truly are; (2) <strong>cherry-picking</strong>, where the news omits events are not sufficiently extreme; or (3) <strong>'fair-and-balanced'</strong> reporting, where the news presents both positive and negative sides of an issue with equal frequency regardless of the true underlying distribution. Each form of distortion has its own characteristics effects."
             ))
           )), 
  
  # Sidebar for Parameter Input
  sidebarLayout(
    sidebarPanel(
      # True distribution of events in the world
      sliderInput(
        "trueStateMean",
        "True mean \\(\\mu\\):",
        min = -5,
        max = 5,
        value = 0,
        step = 0.5
      ),
      sliderInput(
        "trueStateSD",
        "True standard deviation \\(\\sigma\\):",
        min = 0.5,
        max = 3.5,
        value = 1,
        step = 0.5
      ),
      
      
      # Hyperbole by the news
      sliderInput(
        "hyperbole",
        "Hyperbole or exaggeration of events by the news \\(h\\):",
        min = 1,
        max = 3,
        value = 1.5,
        step = 0.1
      ),
      
      # Cherry-picking of extreme events by news
      sliderInput(
        "cherryPicking",
        "Cherry-picking of extreme events by the news \\(p\\):",
        min = 0,
        max = 5,
        value = 0,
        step = 0.5
      ),
      
      # "Fair-and-balanced" reporting of events by the news
      radioButtons("fairAndBalanced", 
                   "'Fair-and-balanced' reporting of events by the news:",
                   choices = list("TRUE" = 1, "FALSE" = 2), 
                   selected = 2),
      
      # Individual confirmation bias
      sliderInput(
        "individualBias",
        "Individual confirmation bias \\(\\beta\\):",
        min = -5,
        max = 5,
        value = 0,
        step = 0.5
      )
      
    ),
    
    # Main Panel with Stationary Distribution + Simulation & Stats Panels
    mainPanel(
      fluidRow(
        style = "padding-left: 20px; text-align: center;",
        plotOutput("trueStatePlotOutput", height = "250px"),
        uiOutput("ui1params"),
        br(),
        plotOutput("newsAppearancePlotOutput", height = "250px"),
        uiOutput("ui2params"),
        br(),
        plotOutput("IndividualBeliefPlotOutput", height = "250px"),
        uiOutput("ui3params"),
        br()
      )
    )
  )
))

### EID ###