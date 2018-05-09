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
IndividualPrior <- sapply(x, dnorm, mean = Bias)
IndividualPosterior <- IndividualPrior * News
IndividualPosterior <- IndividualPosterior * (sum(IndividualPrior) / sum(IndividualPosterior)) # renormalize

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
IndividualPosteriorPlot <- melt(data.frame(x, IndividualPrior, IndividualPosterior), id.vars = 'x')
colnames(IndividualPosteriorPlot) <- c("Evidence",  "Distribution", "Probability")
ggplot(IndividualPosteriorPlot) +
  geom_area(
    data = IndividualPosteriorPlot,
    size = 1,
    aes(x = Evidence, y = Probability, fill = Distribution, color = Distribution),
    alpha = 0.5,
    position = "identity"
  ) +
  theme_minimal() +
  ggtitle("Individual Belief") +
  labs(x = "Events", y = "Perceived Probability") +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c("lightblue", "darkblue")) +
  scale_color_manual(values = c("lightblue", "darkblue")) +
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

length(IndividualPrior)
length(IndividualPosterior)
max(IndividualPrior)
max(IndividualPosterior)
mean(IndividualPrior)
mean(IndividualPosterior)
print(sum(IndividualPrior))
print(sum(IndividualPosterior))
plot(x, IndividualPrior, type="l", col="blue", pch="o", lty=1, ylim=c(0,1), ylab="y" )
lines(x, IndividualPosterior, col = "red")
legend(1, 0.5, legend=c("Line 1", "Line 2"), col=c("red", "blue"), lty=1:2, cex=0.8)


