# Some exercises with R using data from SI 2017-18 OOP course results.

setwd("code")  # this should be the source code folder within your project folder
markz <- read.csv("../data/SI_2017-18.csv")

str(markz)  # have a look
# Oops! Need to restructure Group from numerical vector into a factor.
markz$Group <- factor(as.character(markz$Group), as.character(1:4))


# Display histograms for various marks.
statz.marks <- function() {
  par(mfcol=c(2,2))
  par(mfg=c(1,1))
  hist(markz$Course.mark, xlim=c(0,20), xlab="Overall course mark", main="OOP marks")
  par(mfg=c(1,2))
  hist(markz$Quiz.mark, xlim=c(0,20), xlab="Quiz mark", main="")
  par(mfg=c(2,1))
  hist(markz$Midterm.mark, xlim=c(0,20), xlab="Midterm test mark", main="")
  par(mfg=c(2,2))
  hist(markz$Final.mark, xlim=c(0,20), xlab="Final test mark", main="")
}


# Calculates and displays stats (mean and std dev) for the overall course marks by
#  various categories.
statz.mean_sd <-function() {
  # calculates and displays stats
  # overall course mark
  print(paste("course mark mean:", mean(markz$Course.mark)))
  print(paste("course mark std dev:", sd(markz$Course.mark)))
  # course mark by cursus
  print("course mark mean by cursus:")
  print(tapply(markz$Course.mark, markz$Cursus, mean))
  print("course mark std dev by cursus:")
  print(tapply(markz$Course.mark, markz$Cursus, sd))
  # course mark by group
  print("course mark mean by group:")
  print(tapply(markz$Course.mark, markz$Group, mean))
  print("course mark std dev by group:")
  print(tapply(markz$Course.mark, markz$Group, sd))
  # course mark by gender
  print("course mark mean by gender:")
  print(tapply(markz$Course.mark, markz$Gender, mean))
  print("course mark std dev by gender:")
  print(tapply(markz$Course.mark, markz$Gender, sd))
  # plots course mark stats
  par(mfcol=c(2,2))
  par(mfg=c(1,2))
  # by cursus
  plot(Course.mark ~ Cursus, data=markz)
  par(mfg=c(2,1))
  # by gender
  plot(Course.mark ~ Gender, data=markz)
  par(mfg=c(2,2))
  # by group
  plot(Course.mark ~ Group, data=markz)
}


# Compare means between overall marks and various partial marks.
ttest.marks <- function() {
  # overall vs quiz
  print(t.test(markz$Course.mark, markz$Quiz.mark))
  # overall vs midterm test
  print(t.test(markz$Course.mark, markz$Midterm.mark))
  # overall vs final test
  print(t.test(markz$Course.mark, markz$Final.mark))
}


# Compare variances between overall marks and various partial marks.
vartest.marks <- function() {
  # overall vs quiz
  print(var.test(markz$Course.mark, markz$Quiz.mark))
  # overall vs midterm test
  print(var.test(markz$Course.mark, markz$Midterm.mark))
  # overall vs final test
  print(var.test(markz$Course.mark, markz$Final.mark))
}


# Scatterplots between all sets of marks
scatter.marks <- function() {
  marks.cols = c(7,8,11,12)  # indices of various marks
  # plot pairwise scatterplots between marks
  pairs(markz[, marks.cols])
}


# Compare correlations between overall marks and various partial marks.
cor.marks <- function() {
  marks.cols = c(7,8,11,12)  # indices of various marks
  # display correlations between marks
  # this just displays the correlations without any indication whether
  #  the number is significant
  cor(markz[, marks.cols], use="pairwise")
  # overall vs quiz
  # this uses a t test to give some confidence (or not) in the
  #  null hypothesis that the correlation = 0
  # if the correlation falls inside the confidence interval
  #  then the null hypothesis cannot be rejected and the
  #  variances are sufficiently similiar
  # or something like that...
  print(cor.test(markz$Course.mark, markz$Quiz.mark))
  # overall vs midterm test
  print(cor.test(markz$Course.mark, markz$Midterm.mark))
  # overall vs final test
  print(cor.test(markz$Course.mark, markz$Final.mark))
  # plots correlation overall vs...
  par(mfcol=c(2,2))
  par(mfg=c(1,1))
  # vs quiz
  plot(markz$Course.mark ~ markz$Quiz.mark)
  abline(lm(Course.mark ~ Quiz.mark, data=markz), col="red")
  par(mfg=c(2,1))
  # vs midterm
  plot(markz$Course.mark ~ markz$Midterm.mark)
  abline(lm(Course.mark ~ Midterm.mark, data=markz), col="green")
  par(mfg=c(2,2))
  plot(markz$Course.mark ~ markz$Final.mark)
  abline(lm(Course.mark ~ Final.mark, data=markz), col="blue")
}


model.marks <- function() {
  print(summary(lm(Course.mark ~ Quiz.mark, data=markz)))
  print(summary(lm(Course.mark ~ Midterm.mark, data=markz)))
  print(summary(lm(Course.mark ~ Final.mark, data=markz)))
}


model.diagnostics <- function(what) {
  par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
  plot(lm(what))
}


# Anovas
model.anova <- function() {
  lm.gender <- lm(Course.mark ~ Gender, data=markz)  # course marks by gender
  print(anova(lm.gender))
  lm.group <- lm(Course.mark ~ Group, data=markz)
  print(anova(lm.group))
  lm.cursus <- lm(Course.mark ~ Cursus, data=markz)
  print(anova(lm.cursus))
}



# Runs stuff.
statz.marks()
# Looks like there might be some differences in the distribution of the marks
# for the different eveluations: quiz, midterm, and final exam.
# Looks like the overall mark is less than the final test, which is in turn
# less than the midterm and quiz.


statz.mean_sd()
# Seems to be less difference by gender and group than by cursus.


ttest.marks()
# Lessee if we can interpret the above results. We'll just compare the
# overall course marks with the three partial marks.
# For overall course marks vs final test.
# The null hypothesis H0 would be that there is no significant difference
# in the means, ie, any differences probably arise from individual differences
# between students.
# The t-test gives: t = 3.9179, df = 157.44, p-value = 0.0001328
# Thus means that if H0 were true, then the probability of getting the
# observed results would be p = 0.0001 which seems kinda small.
# Also, t = 3.9 falls outside the 95% confidence interval (0.71, 2.15)
# so we could reject H0 and say with 95% confidence that the means for
# the two evaluations are different.

# Same for course marks vs midterm marks and vs quiz marks. On both cases
# the calculated t falls outside the 95% confidence interval and H0 can be
# rejected.

# However, for quiz vs midterm, not included in ttest.marks, t = 0.78 is
# in (-0.57, 1.32) and so H0 shouldn't be rejected.
# There is no reason to say that the means are not the same.


vartest.marks()
# For overall course marks vs final test.
# We're comparing variances between the course marks and the final test
# marks, and we look at the ratio between them. The null hypothesis H0
# says that ratio = 1, so the variances are essentially equal.
# The test says: F = 0.75732, num df = 81, denom df = 80, p-value = 0.2143
# So, given H0, the probability of getting this F-value is p = 0.21; and this
# value falls within the 95% confidence interval (0.49, 1.18). There is no
# good reason to reject H0, so the variances are essentially the same.

# The variances seem to be pretty much the same for all the tests.


scatter.marks()
cor.marks()
# Determines whether the different sets of marks are correlated. Calculates
# the -1 <= correlation value <= 1 where -1, 1 indicate perfect negative and
# positive correlation respectively, and 0 indicates no correlation. For
# course marks vs quiz marks the null hypothesis H0 says correlation = 0 so there
# is no correlation. Given H0, we get
# t = 8.7406, df = 80, p-value = 2.842e-13 with 95% confidence interval (0.57, 0.80).
# t is well outside the interval so we can say that given H0 there is a very small
# probability of obtaining the given result. We reject H0 which gives support
# for saying the marks are correlated.

# For the other comparisons as well we can reject H0.


model.marks()
# Now we're looking at different models of the data, in particular to see
# how well a relation, eg, course mark vs final mark, can be modeled by a
# straight line given by its slope - intercept parameters.
# Under the covers, we calculate the deviations
# between the data points (course mark i, final test mark i) and two models:
# the mean (essentially a constant line with no slope) and a best-fit line
# with slope. The sloped line model should be an improvement over the
# constant line model and should reduce the deviations. The F-test looks at
# the F-value which is the ratio:
# improvement / residual variance (ie deviation from the improved model).
# A large F-value says that the improved model fits the data better.
# For course mark vs final mark:
# F-statistic: 193.5 on 1 and 79 DF,  p-value: < 2.2e-16
# which is pretty big. The null hypothesis H0 for the associated F-test
# would have been that there is no differnce between the models. However,
# the result says that, given H0, there is a very very small probability of
# obtaining that result. So H0 can be rejected and we have reason to believe
# that the linear model fits better than the constant model.


# Plot some diagnostics of the linear model fit to various marks
model.diagnostics(markz$Course.mark ~ markz$Quiz.mark)
model.diagnostics(markz$Course.mark ~ markz$Midterm.mark)
model.diagnostics(markz$Course.mark ~ markz$Final.mark)



# There's a problem to resolve before anova-ing the data. Some rows have NAs,
# which means that the different marks colums have different lengths (where the NAs
# are excluded). And that makes anova unhappy.
# So let's filter out those rows.
markz.noNA = markz[complete.cases(markz), ]
# Now we can get the linear models for course marks vs the various marks.
lm.quiz_courseMarks <- lm(Course.mark ~ Quiz.mark, data=markz.noNA)
lm.midterm_courseMarks <- lm(Course.mark ~ Midterm.mark, data=markz.noNA)
lm.final_courseMarks <- lm(Course.mark ~ Final.mark, data=markz.noNA)


model.anova()
# Comparison of the means of the overall course marks by different
# categories. So, looking at the course marks by gender, anova will
# try to determine whether the difference between the means of males
# and females can be explained away the the variances between individuals,
# or whether the difference between the categories really is significant.
# Anova determines: Gender     1   4.19  4.1864  0.8982 0.3461
# As usual, the null hypothesis H0 would say that there is no significant
# difference in the means. Given H0, there would be a probability p = 0.35
# of getting an F-value = 0.9. That seems pretty big, so it seems that
# H0 can't be rejected. Any differences are just random.
# Or something.

# Differences in cursus seem more significant: larger F, smaller p.

