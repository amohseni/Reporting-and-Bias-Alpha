# NEWS, CONFIRMATION BIAS, AND BELIEF POLZRIZATION
# << SERVER >>
# by Aydin Mohseni


# Load packages
library(ggplot2)
library(ggthemes)
library(reshape2)

# Define server logic
shinyServer(function(input, output, session) {
  computeDynamics <- reactive({
    # Create the distribution of events for true state of the world
    x <- seq(from = -10,
             to = 10,
             by = 0.1)
    TrueStateMean <- as.numeric(input$trueStateMean)
    TrueStateSD <- as.numeric(input$trueStateSD)
    WorldDistribution <-
      sapply(x, dnorm, mean = TrueStateMean, sd = TrueStateSD)
    NormalizaingFactor <- sum(WorldDistribution)
    
    # Create the distribution of events for the appearance portrayed by news media
    Hyperbole <- as.numeric(input$hyperbole)
    NewsMean <- TrueStateMean
    NewsSD <- Hyperbole * TrueStateSD
    NewsDistribution <- sapply(x, dnorm, mean = NewsMean, sd = NewsSD)
    CherryPicking <- as.numeric(input$cherryPicking)
    NewsDistribution[which(-CherryPicking < x & x < CherryPicking)] <- 0
    NewsDistribution <- NewsDistribution * (NormalizaingFactor / sum(NewsDistribution)) # renormalize
    
    # Create the distribution of events for the beliefs of individuals
    Bias <- as.numeric(input$individualBias)
    IndividualDismissal <- sapply(x, dnorm, mean = Bias)
    IndividualBeliefDistribution <- IndividualDismissal * NewsDistribution
    IndividualBeliefDistribution <- IndividualBeliefDistribution * (NormalizaingFactor / sum(IndividualBeliefDistribution)) # renormalize
    
    # OUTPUT the data for the plots
    h <-
      list(WorldDistribution,
           NewsDistribution,
           IndividualBeliefDistribution)
    return(h)
    
  })
  
  # PLOT 1: State of the world distribution
  output$trueStatePlotOutput <- renderPlot({
    # Import computed distribution
    World <- computeDynamics()[[1]]
    # Format and label the imported data
    WorldPlot <- melt(data.frame(x, World), id.vars = 'x')
    colnames(WorldPlot) <-
      c("Evidence",  "Distribution", "Probability")
    # Create the ggplot
    X <- ggplot(WorldPlot) +
      geom_area(
        data = WorldPlot,
        size = 1,
        aes(
          x = Evidence,
          y = Probability,
          fill = Distribution,
          color = Distribution
        ),
        alpha = 0.5
      ) +
      theme_minimal() +
      ggtitle("True State of the World") +
      labs(x = "Events", y = "Objective Probability") +
      scale_x_continuous(limits = c(-10, 10)) +
      scale_y_continuous(limits = c(0, 0.75)) +
      scale_fill_manual(values = c("orange2")) +
      scale_color_manual(values = c("orange2")) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(X)
  })
  
  # PLOT 2: News media distribution
  output$newsAppearancePlotOutput <- renderPlot({
    # Import computed distribution
    News <- computeDynamics()[[2]]
    # Format and label the imported data
    NewsPlot <- melt(data.frame(x, News), id.vars = 'x')
    colnames(NewsPlot) <-
      c("Evidence",  "Distribution", "Probability")
    # Create the ggplot
    Y <- ggplot(NewsPlot) +
      geom_area(
        data = NewsPlot,
        size = 1,
        aes(
          x = Evidence,
          y = Probability,
          fill = Distribution,
          color = Distribution
        ),
        alpha = 0.5
      ) +
      theme_minimal() +
      ggtitle("Appearance of the World Through the News Media") +
      labs(x = "Events", y = "Broadcast Probability") +
      scale_x_continuous(limits = c(-10, 10)) +
      scale_y_continuous(limits = c(0, 0.75)) +
      scale_fill_manual(values = c("darkred")) +
      scale_color_manual(values = c("darkred")) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(Y)
  })
  
  # PLOT 1: State of the world distribution
  output$IndividualBeliefPlotOutput <- renderPlot({
    # Import computed distribution
    IndividualBelief <- computeDynamics()[[3]]
    # Format and label the imported data
    IndividualBeliefPlot <-
      melt(data.frame(x, IndividualBelief), id.vars = 'x')
    colnames(IndividualBeliefPlot) <-
      c("Evidence",  "Distribution", "Probability")
    # Create the ggplot
    Z <- ggplot(IndividualBeliefPlot) +
      geom_area(
        data = IndividualBeliefPlot,
        size = 1,
        aes(
          x = Evidence,
          y = Probability,
          fill = Distribution,
          color = Distribution
        ),
        alpha = 0.5
      ) +
      theme_minimal() +
      ggtitle("Individual Subjective Belief About the World") +
      labs(x = "Events", y = "Perceived Probability") +
      scale_x_continuous(limits = c(-10, 10)) +
      scale_y_continuous(limits = c(0, 0.75)) +
      scale_fill_manual(values = c("darkblue")) +
      scale_color_manual(values = c("darkblue")) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(Z)
  })
  
  
})

### EOD ###