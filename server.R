# NEWS, CONFIRMATION BIAS, AND BELIEF POLARIZATION
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
    NormalizingFactor <- sum(WorldDistribution)
    
    # Create the distribution of events for the appearance portrayed by news media
    Hyperbole <- as.numeric(input$hyperbole)
    NewsMean <- TrueStateMean
    NewsSD <- TrueStateSD
    # Apply hyperbole distortion
    NewsDistribution <-
      sapply(x, dnorm, mean = Hyperbole * NewsMean, sd = Hyperbole ^ 2 * NewsSD)
    # Apply cherry picking distortion
    CherryPicking <- as.numeric(input$cherryPicking)
    NewsDistribution[which(-CherryPicking < x &
                             x < CherryPicking)] <- 0
    # Apply fair-and-balanced distortion
    if (as.numeric(input$fairAndBalanced) == 1) {
      NewsDistribution[which(x < 0)] <-
        NewsDistribution[which(x < 0)] * (sum(NewsDistribution[which(x > 0)]) / (sum(NewsDistribution[which(x < 0)])))
    }
    # Re-normalize distribution
    NewsDistribution <-
      NewsDistribution * (NormalizingFactor / sum(NewsDistribution)) # renormalize
    data <-
      sample(x,
             size = 10000,
             prob = NewsDistribution,
             replace = TRUE)
    # Output summary statistics
    ReportedMean <- mean(data)
    ReportedSD <- sd(data)
    
    # Create the prior & posterior distributions of beliefs of an individual
    Bias <- as.numeric(input$individualBias)
    IndividualBias <- sapply(x, dnorm, mean = Bias, sd = TrueStateSD)
    # Update the number of reports observed by individuals
    weightOfEvidence <- as.numeric(input$weightOfEvidence)
    meanPerception <- (weightOfEvidence * ReportedMean) + ((1 - weightOfEvidence) * Bias)
    sdPerception <- (weightOfEvidence * ReportedSD) + ((1 - weightOfEvidence) * TrueStateSD)
    IndividualPerception <- sapply(x, dnorm, mean = meanPerception, sd = sdPerception)
    IndividualPerceptionParam <- IndividualPerception
    
    # OUTPUT the data for the plots
    h <-
      list(
        WorldDistribution,
        NewsDistribution,
        IndividualBias,
        IndividualPerception,
        IndividualPerceptionParam,
        c(ReportedMean, ReportedSD),
        c(meanPerception, sdPerception)
      )
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
      ggtitle("Objective Distribution of Evidence") +
      labs(x = "Evidence", y = "Objective Frequency") +
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
      ggtitle("Reported Distribution of Evidence") +
      labs(x = "Evidence", y = "Reported Frequency") +
      scale_x_continuous(limits = c(-10, 10)) +
      scale_y_continuous(limits = c(0, 0.75)) +
      scale_fill_manual(values = c("darkorange3")) +
      scale_color_manual(values = c("darkorange3")) +
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
    Bias <- computeDynamics()[[3]]
    Perception <- computeDynamics()[[5]]
    # Format and label the imported data
    IndividualBeliefPlot <-
      melt(data.frame(x, Bias, Perception), id.vars = 'x')
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
        alpha = 0.5,
        position = "identity"
      ) +
      coord_cartesian(ylim = c(0, 0.75)) +
      theme_minimal() +
      ggtitle("Individual Perception of Evidence") +
      labs(x = "Evidence", y = "Subjective Probability") +
      scale_x_continuous(limits = c(-10, 10)) +
      scale_fill_manual(values = c("pink", "firebrick2")) +
      scale_color_manual(values = c("pink", "firebrick2")) +
      guides(fill = guide_legend(
        keywidth = 0.4,
        keyheight = 0.4,
        default.unit = "inch"
      )) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.6),
        legend.background = element_rect(
          colour = 'white',
          fill = 'white',
          size = 3
        ),
        legend.text = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(Z)
  })
  
  # Print to UI: statistics of objective distribution
  output$ui1params <- renderUI({
    withMathJax(HTML(
      paste(
        "<h5>\\(\\mu_{Objective}=\\) ",
        as.numeric(input$trueStateMean),
        ", \\(\\quad \\sigma_{Objective}=\\) ",
        as.numeric(input$trueStateSD),
        "</h5>",
        sep = ""
      )
    ))
  })
  
  # Print to UI: statistics of reported distribution
  output$ui2params <- renderUI({
    meanNews <- round(computeDynamics()[[6]][1], digits = 1)
    sdNews <- round(computeDynamics()[[6]][2], digit = 1)
    withMathJax(HTML(
      paste(
        "<h5>\\(\\mu_{Reported}=\\) ",
        meanNews,
        ", \\(\\quad \\sigma_{Reported}=\\) ",
        sdNews,
        "</h5>",
        sep = ""
      )
    ))
  })
  
  # Print to UI: statistics of belief distribution
  output$ui3params <- renderUI({
    meanPerception <- round(computeDynamics()[[7]][1], digits = 1)
    sdPerception <- round(computeDynamics()[[7]][2], digit = 1)
    withMathJax(HTML(
      paste(
        "<h5>\\(\\beta_{Bias}=\\) ",
        as.numeric(input$individualBias),
        ", \\(\\quad \\mu_{Perceived}=\\) ",
        meanPerception,
        ", \\(\\quad \\sigma_{Perceived}=\\) ",
        sdPerception,
        "</h5>",
        sep = ""
      )
    ))
  })
  
  
})

### EOD ###