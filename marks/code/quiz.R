# Some exercises with R using data from MAM 2017-18 OOP quiz 4 results.
# The hypothesis is that there is a relation between the time taken on the quiz
# and the resulting mark.

setwd("code")  # this should be the source code folder within your project folder
markz <- read.csv("../data/MAM-Quiz_4.csv")

str(markz)  # have a look


#### Basic displays ####

# Display histograms.
statz.marks <- function() {
  par(mfcol=c(1,2))
  # apply the function to the marks and their x-axis labels
  hist(markz$Grade, xlim=c(0,20), main="", xlab="Marks ( /20)")
  hist(markz$Time, main="", xlab="Time (secs)")
}


# Prints and plots statistics (mean, std dev) for grade
# and time taken.
statz.mean_sd <- function(category) {
  # calculates and displays stats
  print("mean (grade, time):", quote=FALSE)
  print(sapply(list(markz$Grade, markz$Time), mean))
  print("sd (grade, time):", quote=FALSE)
  print(sapply(list(markz$Grade, markz$Time), sd))
  par(mfcol=c(2,2))
  hist(markz$Grade, xlim=c(0,20), main="", xlab="Grade")
  hist(markz$Time, xlim=c(0,800), main="", xlab="Time (secs)")
  boxplot(markz$Grade, xlab="Median grade")
  boxplot(markz$Time, xlab="Median time")
}



#### Bring on the stats! ####

### t- and F-tests ###
# Well, thesemake no sense here as there is no categorical data.
# Moving right along...


# Here's what we want to look at - is there any sort of relation between
# the time taken for the quiz and the mark obtained.

# Compare correlations between quiz mark and time taken.
cor.marks <- function() {
  # test correlation between quiz mark and time
  # this uses a t test to give some confidence (or not) in the
  #  null hypothesis H0 that the correlation = 0
  # if the t-value falls inside the confidence interval
  #  then, assuming H0 to be true, there is a > 5% probability that
  #  the observed results came about by chance. This is sufficiently
  #  big that H0 cannot be rejected.
  print(cor.test(markz$Grade, markz$Time))
  # plots correlation time vs mark
  plot(markz$Grade ~ markz$Time, xlab="Time (secs)", ylab="Mark")
  abline(lm(markz$Grade ~ markz$Time), col="red")
  return()
}


model.marks <- function() {
  print(summary(lm(Grade ~ Time, data=markz)))
}


model.diagnostics <- function() {
  par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
  plot(lm(Grade ~ Time, data=markz))
}



# Runs stuff.
statz.marks()
# The marks look pretty Gaussian. The time taken looks quite
# clustered around the limit at which the quiz timed out (13mins
# or 780secs).


statz.mean_sd()
# No surprises for the medians, means, std devs for the marks and time.


cor.marks()
# Determines whether the marks and times are correlated. Calculates
# the -1 <= correlation value <= 1 where -1, 1 indicate perfect negative and
# positive correlation respectively, and 0 indicates no correlation. For
# marks vs time the null hypothesis H0 says correlation = 0 so there
# is no correlation. Given H0, we get
# t = 2.26, df = 43, p-value = 0.029 with 95% confidence interval (0.03 0.57).
# The p-value is sufficiently small that we can say (with >95% confidence)
# that, since is well outside the interval, there is a very small
# probability of obtaining the given result. We reject H0 which gives support
# for saying the marks are correlated.


model.marks()
# Now we're looking at different models of the data, in particular to see
# how well a relation, eg, mark vs time taken, can be modeled by a
# straight line given by its slope - intercept parameters.
# Under the covers, we calculate the deviations
# between the data points (time i, mark i) and two models:
# the mean (essentially a constant line with no slope) and a best-fit line
# with slope. The sloped line model should be an improvement over the
# constant line model and should reduce the deviations. The F-test looks at
# the F-value which is the ratio:
# improvement / residual variance (ie deviation from the improved model).
# A large F-value says that the improved model fits the data better.
# For course mark vs final mark:
# F-statistic: 5.118 on 1 and 43 DF,  p-value: 0.029
# which is pretty big. The null hypothesis H0 for the associated F-test
# would have been that there is no differnce between the models. However,
# the result says that, given H0, there is a very very small probability of
# obtaining that result. So H0 can be rejected and we have reason to believe
# (> 95% confidence) that the linear model fits better than the constant model.


# Plot some diagnostics of the linear model fit to various marks
model.diagnostics()
# All reasonable. I think.


##### Conclusion #####

# Take your time, get a better mark.

