# NEWS, CONFIRMATION BIAS, AND BELIEF POLZRIZATION
# by Aydin Mohseni

# Load packages
library(ggplot2)
library(ggthemes)
library(reshape2)

# Determin parameter sweep
HyperboleVEC <- seq(from = 1, to = 3, by = 0.2)
CherryPickingVEC <- seq(from = 0, to = 3, by = 0.2)
FairAndBalancedVEC <- c(0, 1)
BiasVEC <- seq(from = 0, to = 1, by = 0.2)
TotalDataPoints <-
  length(HyperboleVEC) * length(CherryPickingVEC) * length(FairAndBalancedVEC) * length(BiasVEC)

# Produce the data structure in which to save the data
Df <- data.frame(matrix(
  data = NA,
  ncol = 5,
  nrow = TotalDataPoints,
  byrow = TRUE
))
colnames(Df) <-
  c("Hyperbole", "Cherrypicking", "FaB",  "Bias", "Belief")
Df[, 1] <- rep(
  HyperboleVEC,
  times = 1,
  each = length(CherryPickingVEC) * length(FairAndBalancedVEC) * length(BiasVEC)
)
Df[, 2] <- rep(
  CherryPickingVEC,
  times = TotalDataPoints / (
    length(CherryPickingVEC) * length(FairAndBalancedVEC) * length(BiasVEC)
  ),
  each = length(FairAndBalancedVEC) * length(BiasVEC)
)
Df[, 3] <- rep(FairAndBalancedVEC,
               times = TotalDataPoints / (length(FairAndBalancedVEC) * length(BiasVEC)),
               each = length(BiasVEC))
Df[, 4] <- rep(BiasVEC,
               times = TotalDataPoints / length(BiasVEC),
               each = 1)

# Set up the for loops for the parameter sweep
for (i in 1:TotalDataPoints) {
  # Fix key model parameters
  Hyperbole <- Df[i, 1]
  CherryPicking <- Df[i, 2]
  FairAndBalanced <- Df[i, 3]
  Bias <- Df[i, 4]
  
  # Set the parameters of the world
  # Create the distribution of events for OBJECTIVE STATE of the world
  TrueStateMean <- 0
  TrueStateSD <- 1
  x <- seq(from = -10,
           to = 10,
           by = 0.1)
  WorldDistribution <-
    sapply(x, dnorm, mean = TrueStateMean, sd = TrueStateSD)
  NormalizingFactor <- sum(WorldDistribution)
  
  ### Create the distribution of events for REPORTING by news media
  NewsMean <- TrueStateMean
  NewsSD <- Hyperbole * TrueStateSD
  NewsDistribution <-
    sapply(x, dnorm, mean = NewsMean, sd = NewsSD)
  NewsDistribution[which(-CherryPicking < x &
                           x < CherryPicking)] <- 0
  if (as.numeric(FairAndBalanced) == 1) {
    NewsDistribution[which(x < 0)] <-
      NewsDistribution[which(x < 0)] * (sum(NewsDistribution[which(x > 0)]) / (sum(NewsDistribution[which(x < 0)])))
  }
  NewsDistribution <-
    NewsDistribution * (NormalizingFactor / sum(NewsDistribution)) # renormalize
  data <-
    sample(x,
           size = 10000,
           prob = NewsDistribution,
           replace = TRUE)
  meanNews <- mean(data)
  sdNews <- sd(data)
  
  # Create the pior & posterior distributions of INDIVIDUAL BELIEFS
  IndividualBias <-
    sapply(x, dnorm, mean = Bias, sd = TrueStateSD)
  IndividualPerception <- IndividualBias * NewsDistribution
  IndividualPerception <-
    IndividualPerception * (NormalizingFactor / sum(IndividualPerception)) # renormalize
  data <-
    sample(x,
           size = 10000,
           prob = IndividualPerception,
           replace = TRUE)
  meanPerception <- mean(data)
  # sdPerception <- sd(data)
  # IndividualPerceptionParam <-
  #   sapply(x, dnorm, mean = meanPerception, sd = sdPerception)
  
  # Save computation to data frame
  Df[i, 5] <- meanPerception
  
  # # Plot TRUE STATE OF THE WORLD distribution
  # WorldPlot <- melt(data.frame(x, WorldDistribution), id.vars = 'x')
  # colnames(WorldPlot) <-
  #   c("Evidence",  "Distribution", "Probability")
  # # Create the ggplot
  # ggplot(WorldPlot) +
  #   geom_area(
  #     data = WorldPlot,
  #     size = 1,
  #     aes(
  #       x = Evidence,
  #       y = Probability,
  #       fill = Distribution,
  #       color = Distribution
  #     ),
  #     alpha = 0.5
  #   ) +
  #   theme_minimal() +
  #   ggtitle("True Distribution of Evidence") +
  #   labs(x = "Events", y = "Objective Frequency") +
  #   scale_x_continuous(limits = c(-10, 10)) +
  #   scale_y_continuous(limits = c(0, 0.75)) +
  #   scale_fill_manual(values = c("orange2")) +
  #   scale_color_manual(values = c("orange2")) +
  #   theme(
  #     plot.title = element_text(
  #       hjust = 0.5,
  #       margin = margin(b = 10, unit = "pt"),
  #       lineheight = 1.15
  #     ),
  #     legend.position = "none",
  #     axis.text.x = element_blank(),
  #     axis.text.y = element_blank(),
  #     axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
  #     axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
  #     text = element_text(size = 16)
  #   )
  #
  # # Plot APPEARANCE GIVEN BY NEWS distribution
  # NewsPlot <- melt(data.frame(x, NewsDistribution), id.vars = 'x')
  # colnames(NewsPlot) <-
  #   c("Evidence",  "Distribution", "Probability")
  # # Create the ggplot
  # ggplot(NewsPlot) +
  #   geom_area(
  #     data = NewsPlot,
  #     size = 1,
  #     aes(
  #       x = Evidence,
  #       y = Probability,
  #       fill = Distribution,
  #       color = Distribution
  #     ),
  #     alpha = 0.5
  #   ) +
  #   theme_minimal() +
  #   ggtitle("Appearance of Evidence Through the News Media") +
  #   labs(x = "Events", y = "Reported Frequency") +
  #   scale_x_continuous(limits = c(-10, 10)) +
  #   scale_y_continuous(limits = c(0, 0.75)) +
  #   scale_fill_manual(values = c("darkorange3")) +
  #   scale_color_manual(values = c("darkorange3")) +
  #   theme(
  #     plot.title = element_text(
  #       hjust = 0.5,
  #       margin = margin(b = 10, unit = "pt"),
  #       lineheight = 1.15
  #     ),
  #     legend.position = "none",
  #     axis.text.x = element_blank(),
  #     axis.text.y = element_blank(),
  #     axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
  #     axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
  #     text = element_text(size = 16)
  #   )
  #
  # # Plot INDIVIDUAL BELIEF distribution
  # IndividualBeliefPlot <-
  #   melt(data.frame(x, IndividualBias, IndividualPerception), id.vars = 'x')
  # colnames(IndividualBeliefPlot) <-
  #   c("Evidence",  "Distribution", "Probability")
  # # Create the ggplot
  # ggplot(IndividualBeliefPlot) +
  #   geom_area(
  #     data = IndividualBeliefPlot,
  #     size = 1,
  #     aes(
  #       x = Evidence,
  #       y = Probability,
  #       fill = Distribution,
  #       color = Distribution
  #     ),
  #     alpha = 0.5,
  #     position = "identity"
  #   ) +
  #   coord_cartesian(ylim = c(0, 0.75)) +
  #   theme_minimal() +
  #   ggtitle("Individual Perception of Evidence") +
  #   labs(x = "Events", y = "Subjective Probability") +
  #   scale_x_continuous(limits = c(-10, 10)) +
  #   scale_fill_manual(values = c("pink", "firebrick2")) +
  #   scale_color_manual(values = c("pink", "firebrick2")) +
  #   guides(fill = guide_legend(
  #     keywidth = 0.4,
  #     keyheight = 0.4,
  #     default.unit = "inch"
  #   )) +
  #   theme(
  #     plot.title = element_text(
  #       hjust = 0.5,
  #       margin = margin(b = 10, unit = "pt"),
  #       lineheight = 1.15
  #     ),
  #     legend.title = element_blank(),
  #     legend.position = c(0.85, 0.6),
  #     legend.background = element_rect(
  #       colour = 'white',
  #       fill = 'white',
  #       size = 3
  #     ),
  #     legend.text = element_text(size = 16),
  #     axis.text.x = element_blank(),
  #     axis.text.y = element_blank(),
  #     axis.title.x =  element_text(margin = margin(t = 5, unit = "pt")),
  #     axis.title.y =  element_text(margin = margin(r = 5, unit = "pt")),
  #     text = element_text(size = 16)
  #   )
}
###
