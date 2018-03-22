# NEWS, CONFIRMATION BIAS, AND BELIEF POLZRIZATION
# by Aydin Mohseni

# Load packages
library(ggplot2)
library(ggthemes)
library(reshape2)

# Set global variables
x <- seq(from = -10, to = 10, by = 0.1)
World <- sapply(x, dnorm)
Hyperbole <- 1
News <- sapply(x, dnorm, mean = 0, sd = Hyperbole)
CherryPicking <- 0
News[which(-CherryPicking < x & x < CherryPicking)] <- 0
News <- News * (10 / sum(News)) # renormalize

# Fixed bias individuals
Bias <- 0
IndividualDismissal <- sapply(x, dnorm, mean = Bias)
IndividualBelief <- IndividualDismissal * News
IndividualBelief <- IndividualBelief * (10 / sum(IndividualBelief)) # renormalize

# Plot TRUE STATE OF THE WORLD distribution
WorldPlot <- melt(data.frame(x, World), id.vars = 'x')
colnames(WorldPlot) <- c("Evidence",  "Distribution", "Probability")
ggplot(WorldPlot) +
  geom_area(
    data = WorldPlot,
    size = 1,
    aes(x = Evidence, y = Probability, fill = Distribution, color = Distribution),
    alpha = 0.5
  ) +
  theme_minimal() +
  ggtitle("True State of the World") +
  labs(x = "Events", y = "Objective Probability") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(0, 1)) +
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

# Plot APPEARANCE GIVEN BY NEWS distribution
NewsPlot <- melt(data.frame(x, News), id.vars = 'x')
colnames(NewsPlot) <- c("Evidence",  "Distribution", "Probability")
ggplot(NewsPlot) +
  geom_area(
    data = NewsPlot,
    size = 1,
    aes(x = Evidence, y = Probability, fill = Distribution, color = Distribution),
    alpha = 0.5
  ) +
  theme_minimal() +
  ggtitle("Appearance of the World") +
  labs(x = "Events", y = "Broadcast Probability") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(0, 1)) +
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

# Plot INDIVIDUAL BELIEF distribution
IndividualBeliefPlot <- melt(data.frame(x, IndividualBelief), id.vars = 'x')
colnames(IndividualBeliefPlot) <- c("Evidence",  "Distribution", "Probability")
ggplot(IndividualBeliefPlot) +
  geom_area(
    data = IndividualBeliefPlot,
    size = 1,
    aes(x = Evidence, y = Probability, fill = Distribution, color = Distribution),
    alpha = 0.5
  ) +
  theme_minimal() +
  ggtitle("Individual Belief") +
  labs(x = "Events", y = "Perceived Probability") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(0, 1)) +
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

