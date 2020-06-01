

library(Bolstad)

# Create the distribution of events for true state of the world
x <- seq(from = -10,
         to = 10,
         by = 0.1)
NewsDistribution <- sapply(x, dnorm, mean = 0, sd = 1)

# Alternative form of updating
# Sample 10^w data points from the News Distribution
SampleOfNewsReports <-
  sample(x,
         10 ^ (10 * weightOfEvidence),
         prob = NewsDistribution,
         replace = T)

# The strength of bias reflects the quantity of reports that are collected
StrengthOfBias <- .1

# Set the initial agent beliefts
priorMean <- -3
priorSD <- 1

# The Bayesian update function which describes how agents learn from news reports
# It take reports and prior beliefs about the mean and SD as inputs,
# and has posterior beliefs aobut the population mean and SD as outputs.
update <- function(priorMean, priorSD, report) {
  posteriorMean <-
    normnp(
      report,
      m.x = priorMean,
      s.x = priorSD,
      sigma.x = 1,
      mu = NULL,
      n.mu = 100,
      plot = FALSE
    )$mean
  posteriorSD <-
    normnp(
      report,
      m.x = priorMean,
      s.x = priorSD,
      sigma.x = 1,
      mu = NULL,
      n.mu = 100,
      plot = FALSE
    )$sd
  return(c(posteriorMean, posteriorSD))
}

# First, create a vector in which to store the means and SDs of the belief state
priorMeanVector <- rep(0, length(SampleOfNewsReports))
priorSDVector <- rep(0, length(SampleOfNewsReports))
acceptRejectVector <- rep(0, length(SampleOfNewsReports))

# Invdividiual learning now proceeds as follows.
# For each news report, the agent (1) decides whether to accept or reject it
# which is determined by how close it is to her current belief.
# (2) If she rejects the report, then her view remains unchanged.
# If she accepts the report, then she updates her beliefs via Bayes rule.
# (3) The process begins again with a new report and her new beliefs.
for (i in 1:length(SampleOfNewsReports)) {
  # Consider the news report
  report <- SampleOfNewsReports[i]
  # And (1) Decide whether confirmation bias will allow you to update
  # If confirmation bias makes it so that the report is rejected,
  # then simply leave the belief state as is
  if (abs(priorMean - report) > abs(priorMean - rnorm(1, mean = priorMean, StrengthOfBias ^ -1))) {
    # Record that the report was rejected
    acceptRejectVector[i] <- 0
    # Leave belief state as is
  } else {
    # If conformation bias does not make it so that the report is rejected,
    # record that the report was rejected,
    acceptRejectVector[i] <- 1
    # then update the belief state via Bayes' rule.
    updatedParams <- update(priorMean, priorSD, report)
  }
  # Update the belief means and SD for the next round of learning
  priorMean <- updatedParams[1]
  priorSD <- updatedParams[2]
  # Store thes in their respective vectors
  priorMeanVector[i] <- priorMean
  priorSDVector[i] <- priorSD
}
# Determin which reports were accepted with whcih to calculate the Bayesian estimator of the SD of the accpeted reports 
whichReportsAccepted <- which(acceptRejectVector %in% c(1))

meanPerception <- priorMeanVector[length(SampleOfNewsReports)]
sdPerception <- sd(SampleOfNewsReports[whichReportsAccepted])

print(acceptRejectVector)
plot(priorSDVector)
plot(priorMeanVector)