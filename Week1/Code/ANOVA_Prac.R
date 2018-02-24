### ANOVA (Analysis of variance) ###

# Load data into data frame and examine data
mammals <- read.csv("../Data/MammalData.csv")
str(mammals)
summary(mammals)

######## PLOT DATA ########
# Before fitting any models, plot the data to see if the means withing the groupings of interest look different, check if varience looks similar (constant normal variance). Use box and whisker plots.
plot(meanCvalue ~ TrophicLevel, data = mammals) #differences in genome size between trophic levels
mammals$logCvalue <- log(mammals$meanCvalue)
boxplot(mammals$logCvalue ~ TrophicLevel, data = mammals)

plot(meanCvalue ~ GroundDwelling, data = mammals) #differences between grounddwelling and other species
boxplot(meanCvalue ~ GroundDwelling, data = mammals)
boxplot(mammals$logCvalue ~ GroundDwelling, data = mammals)

#### Differences in means with barplots ####
# Get standard error of the mean from a set of values (x)
seMean <- function(x) {  # corrected
    x <- na.omit(x) # get rid of missing values
    se <- sqrt(var(x)/length(x)) # calculate the standard error
    return(se) # tell the function to return the standard error
}

##### Plot for TrophicLevel #####
# Now use function tapply: it splits a variable up into groups from a factor and calculates stats on each group using a function
trophMeans <- tapply(mammals$logCvalue, mammals$TrophicLevel, FUN=mean, na.rm=TRUE)
print(trophMeans)
trophSE <- tapply(mammals$logCvalue, mammals$TrophicLevel, FUN=seMean)
print(trophSE)

## Put values together on the plot ##
# Get the upper and lower limits of the error bars
upperSE <- trophMeans + trophSE
lowerSE <- trophMeans - trophSE
# Get a barplot: this function can report where the middle of the bars are on the x axis.
# Set the y axis limits to contain the error bars
barMids <- barplot(trophMeans, ylim=c(0, max(upperSE)), ylab="log C value (pg)")
# Add error bars using function: draws arrows between the points (x0,y0) and (x1,y1).
# Arrow heads at each end (code=3) and at 90 degree angles
arrows(barMids, upperSE, barMids, lowerSE, ang=90, code=3)



##### Plot for GroundDwelling (same process, different values) #####
groundMeans <- tapply(mammals$logCvalue, mammals$GroundDwelling, FUN = mean, na.rm=TRUE)
print(trophMeans)
groundSE <- tapply(mammals$logCvalue, mammals$GroundDwelling, FUN=seMean)
print(groundSE)

upperSE <- groundMeans + groundSE
lowerSE <- groundMeans - groundSE
barMids <- barplot(groundMeans, ylim=c(0, max(upperSE)),ylab="log C value (pg)")
arrows(barMids, upperSE, barMids, lowerSE, ang=90, code=3)



####### Alternative to barplots #######
# Use the gplots package to create plots of group means and confidence intervals.
# Load the gplots package
install.packages("gplots") #(install package)
library(gplots)

# Get plots of group means and standard errors
par(mfrow=c(1,2))
plotmeans(logCvalue ~ TrophicLevel, data = mammals, p=0.95, connect=FALSE)
plotmeans(logCvalue ~ GroundDwelling, data = mammals, p=0.95, connect=FALSE)



######### ANALYSIS OF VARIANCE ##########
# Use linear model to test whether trophic differences are significant
trophicLM <- lm(logCvalue ~ TrophicLevel, data = mammals)
summary(trophicLM) #coefficients
anova(trophicLM) #terms

# Use linear model to test whether ground-dwelling differences are significant
groundDwellingLM <- lm(logCvalue ~ GroundDwelling, data = mammals)
summary(groundDwellingLM) #coefficients
anova(groundDwellingLM) #terms
