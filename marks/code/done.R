# Testing the hypothesis that there is some relation between student 
# marks on an exam and the order in which they handed in their papers
# (which we'll call "done").

setwd("code")  # this should be the source code folder within your project folder
markz <- read.csv("../data/SI_2017-18.csv")

str(markz)  # have a look
# Oops! Need to restructure Group from numerical vector into a factor.
markz$Group <- factor(as.character(markz$Group), as.character(1:4))

# There are three different data catgories: gender, group, and cursus.
# Meaning that every student has a gender, was in a different lab group,
# and following a particular cursus before landing in this course.
categories <- list(markz$Gender, markz$Group, markz$Cursus)  # list not c because inhomogeneous data

# While we're at it, collect the marks
marks <- list(markz$Course.mark, markz$Quiz.mark, markz$Midterm.mark, markz$Final.mark)
marks.labels <- c("Overall course mark", "Quiz mark", "Midterm test mark", "Final test mark")

# Scatterplot between done order  and final test marks.
done_vs_mark.scatter <- function() {
  plot(Final.mark ~ Final.done, data=markz)
}


# Correlation between done order  and final test marks.
done_vs_mark.cor <- function() {
  # print correlations between done and mark
  # this just prints the correlation without any indication whether
  #  the number is significant
  cor(markz$Final.done, markz$Final.mark, use="pairwise")
  # test correlation between done order and mark
  # this uses a t test to give some confidence (or not) in the
  #  null hypothesis H0 that the correlation = 0
  # if the t-value falls inside the confidence interval
  #  then, assuming H0 to be true, there is a > 5% probability that
  #  the observed results came about by chance. This is sufficiently
  #  big that H0 cannot be rejected.
  print(cor.test(markz$Final.mark, markz$Final.done))
  # plots correlation done vs mark
  plot(Final.mark ~ Final.done, data=markz)
  abline(lm(Final.mark ~ Final.done, data=markz), col="red")
  return()
}


#### Bring on the stats! ####

### t- and F-tests ###

# Compare means between done order and a given category.
ttest.done <- function(category, samples=1) {  # default arg value = 1
  mn = mean(markz$Final.done, na.rm=TRUE)  # grand mean over all samples
  print(tapply(markz$Final.done, category, function(dun) {
    if (length(dun) > samples) {
      t.test(dun, mu=mn, na.rm=TRUE)
    }
  }))
  return()
}


# Compare variances between done order and a given category.
vartest.done <- function(category, samples=1) {  # default arg value = 1
  print(tapply(markz$Final.done, category, function(dun) {
    if (length(dun) > samples) {
      var.test(dun, markz$Final.done, na.rm=TRUE)  # compare to "grand variance"
    }
  }))
  return()
}


# Scatterplots between all pairs of marks
scatter.marks <- function() {
  marks.cols = c(7,8,11,12)  # indices of various marks
  # plot pairwise scatterplots between different marks
  pairs(markz[, marks.cols])
}


# Compare correlations between overall marks and different partial marks.
cor.marks <- function() {
  marks.cols = c(7,8,11,12)  # indices of various marks
  # print correlations between marks
  # this just prints the correlations without any indication whether
  #  the number is significant
  cor(markz[, marks.cols], use="pairwise")
  # test correlation between overall and quiz
  # this uses a t test to give some confidence (or not) in the
  #  null hypothesis H0 that the correlation = 0
  # if the t-value falls inside the confidence interval
  #  then, assuming H0 to be true, there is a > 5% probability that
  #  the observed results came about by chance. This is sufficiently
  #  big that H0 cannot be rejected.
  par(mfcol=c(2,2))
  mapply(function(mrk, lbl) {
    print(cor.test(markz$Course.mark, mrk))
    # plots correlation overall vs...
    plot(markz$Course.mark ~ mrk, xlab=lbl)
    abline(lm(Course.mark ~ mrk, data=markz), col="red")
  }, marks[2:4], marks.labels[2:4])
  return()
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


# Anovas between different categories
# Calculates anova for a done order category passed as argument. If the above
# list is used for a single category, then the call is, eg,
# model.anova(categories[[2]])
# to get the group dones. To run the function on all the categories, use
# sapply(categories, model.anova)
model.anova <- function(dada, category) {
  lm.category <- lm(dada ~ category)  # course marks by category
  print(anova(lm.category))
  print(summary(lm.category))
}


# Some category levels have too few samples to be statistically relevant, eg,
# in the given data set there is only one student with the cursus Mundus.
# This function drops from the data set students who are in a category level
# with too few samples.
# @param min The minimun number of samples in a category level.
# @param cat The category being filtered.
# @return A new data frame with only students from a category level
#   with  sufficient samples
# Note: This was not totally evident to figure out; there might well be a
# more elegant way to do this.
drop.small_samples <- function(cat=markz$Cursus, min=1) {
  # determines which category levels have sufficient samples
  enough.samples = as.integer(summary(cat)) >= min
  names(enough.samples) <- levels(cat)  # makes debugging easier
  # determines which students are in sufficiently-sampled categories
  got.sufficient.samples <- sapply(cat, function(student) {
    enough.samples[student]
  })
  names(got.sufficient.samples) <- markz$Last.name
  return(markz[got.sufficient.samples,])
}



########## Runs stuff ##########

done_vs_mark.scatter()
# Well, can't really say whether there's some effect or not


done_vs_mark.cor()
# This gives the result:
# data:  markz$Final.mark and markz$Final.done
# t = -1.7366, df = 78, p-value = 0.08641
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.39586809  0.02797034
# Soooo...given the null hypothesis H0 that the correlation = 0,
# there is a p = 0.09 (9%) probability of getting the t-value
# actually observed purely by chance. Since this is over the
# commonly-accepted p = 0.05 we have no reason to reject H0.
# So we can conclude that there is no correlation.


# However, the above was for the class as a whole. In the spirit
# of never throwing away a data set without a publication,
# Let's torture the data some more and see if we can wring
# something, anything, out of it.

# Prints and plots statistics (mean, std dev) for done order
# for a given category.
statz.mean_sd.category <- function(category) {
  # calculates and displays stats for the given category
  print(tapply(markz$Final.done, category, mean, na.rm=TRUE))
  print(tapply(markz$Final.done, category, sd, na.rm=TRUE))
  plot(markz$Final.done ~ category, na.rm=TRUE, xlab="Category")
  return()
}


# Prints and plots statistics (mean, std dev) for done order
# for all categories.
statz.mean_sd.categories <- function() {
  # displays in 2x2 grid
  par(mfcol=c(2,2))
  # applies the above function to all categories
  sapply(categories, statz.mean_sd.category)
  return()
  # note: did i say that sapply is your friend?
}
# It looks like not much difference in the mean done order by gender
# or by group. But all is not lost as the cursus category seems to
# show significant differences. Let's see...

# Compare category means to the grand mean
ttest.done()
# Meh! For all categories p > 0.05, so there's a significant
# probability of getting the observation by chance. We can't
# draw any conclusions.
# Curiously, the mean for some categories is quite different from
# the grand mean, ie, for "Autre Bac +2" the means are 73 and 40.5
# respectively. However, for these categories df < 3 which seems
# pretty feeble. Running the tests again on only those categories
# with a larger sample size
ttest.done(8)
# gives the following best result for "DUT Info":t = -1.8682, df = 18, p-value = 0.07809
# alternative hypothesis: true mean is not equal to 40.5
# 95 percent confidence interval:
#  21.21138 41.63073
# sample estimates:
# mean of x 
#  31.42105
# Unfortunately p = 7.8% is too big for a publishable result.


# Ok then. Comparing category variances to the variance of all samples
vartest.done(markz$Cursus, 8)
# None of the p-values (all big) give any reason to reject the null 
# hypothesis that the variances by category levels are the same as the variance
# over all the samples. Another meh.


# Let's anova the done order by cursus
model.anova(markz$Final.done, markz$Cursus)
# Looks like the p-values are small only where the levels have a very
# small number of samples.
# So, let's try again after having eliminated those marginal levels. 
markz.enough_samples <- drop.small_samples(markz$Cursus, 8)
model.anova(markz.enough_samples$Final.done, markz.enough_samples$Cursus)
# This gives:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        40.320      4.602   8.762 1.15e-12 ***
# categoryDUT Info   -8.899      7.003  -1.271    0.208    
# categoryPEIP        2.960      6.508   0.455    0.651 
# where the p-value again seems big enough to rule out eliminating
# the null hypothsis.

