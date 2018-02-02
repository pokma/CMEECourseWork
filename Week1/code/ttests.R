## Script: running one- and two-sample t tests, F tests ##
# (for sets of continuous data)
# t test relies of the test statistic t, defined as the difference between the means of the two data sets (or a mean and a ref value) divided by the pooled standard error of the mean(s).
# t tests should be used with data that are 1) relatively normally distributed and 2) have similar variances (use F test)
# F test used to compare the variances of two sample populations.

######## One-sample t tests ########
# Compare a sample mean to a hypothesized or target value

# Load data
genome <- read.csv("../data/GenomeSize.csv")
str(genome) #Check data has loaded correctly

# Calculate the three values (mean, variance, number of data points) from the data
mean.gs <- mean(genome$GenomeSize)
print(mean.gs)

var.gs <- var(genome$GenomeSize)
print(var.gs)

n.gs <- length(genome$GenomeSize)
print(n.gs)

# Get the difference
diff <- mean.gs - 1.25
print(diff)

# Get the standard error
se.gs <- sqrt(var.gs/n.gs)
print(se.gs)

# Get the t value
t.gs <- diff/se.gs
print(t.gs)


# Use the function t.test to do this more easily and get more information (p value, degrees of freedom, confidence interval). The null hypothesis can be set using the option mu.
t.test(genome$GenomeSize, mu=1.25)



######## Two-sample t tests ########
# Compare the means of two groups

# Calculate the mean, variance and number of data points (sample size) from the data
mean.gs <- tapply(X = genome$GenomeSize, INDEX = genome$Suborder, FUN = mean)
print(mean.gs)

var.gs <- tapply(X = genome$GenomeSize, INDEX = genome$Suborder, FUN = var)
print(var.gs)

n.gs <- tapply(X = genome$GenomeSize, INDEX = genome$Suborder, FUN = length)
print(n.gs)

# Get the difference
diff <- mean.gs[1]-mean.gs[2]
print(diff)

# Get the standard error of the difference
se.gs <- sqrt((var.gs[1]/n.gs[1])+(var.gs[2]/n.gs[2]))
print(se.gs)

# Get the t value
t.gs <- diff/se.gs
print(t.gs)


# Automate this by using the t.test function, using a formula to get a test between the two suborders. Output gives two estimated means (rather than 1 in the one sample test) and reports the p value for the calculated t value.
t.test(GenomeSize ~ Suborder, data = genome)



######## F tests ########
## If var(a)=var(b), then F=1
## If var(a)>var(b), then F>1
## If var(a)<var(b), then F<1

# Plot the data
# Initialize multi-paneled plot
par(mfrow=c(1,2))
# Specifiy which subplot to use first
par(mfg=c(1,1))
boxplot(log(genome$GenomeSize) ~ genome$Suborder, xlab="Suborder", ylab="Genome Size")
# Second subplot
par(mfg=c(1,2))
boxplot(log(genome$BodyWeight) ~ genome$Suborder, xlab="Suborder", ylab="Body Weight")

# Calculate F
var.gs[1]/var.gs[2] # already calculated the varuance for the t test above

# Use the var.test function to do the calculations
var.test(GenomeSize ~ Suborder, data = genome)


