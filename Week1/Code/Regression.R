### Linear Models: Regression

# Exploring the data: use indices to access data frame content
dat <- data.frame(A = c ("a", "b", "c", "d", "e"), B= c(1,2,3,4,5))
# select row 1 (all columns selected)
dat[1,]
# select column 2 (all rows selected)
dat[,2]
# row 2, column 1
dat[2,1]

# Load data
genome <- read.csv("../Data/GenomeSize.csv")

# Record columns of key variables of interest
morpho <- c(4,7,8,12,14)
pairs(genome[, morpho], col=genome$Suborder)

### CORRELATION ###
cor(genome[, morpho], use = "pairwise") # the use="pairwise" tells R to omit observations with missing data
cor.test(genome$GenomeSize, genome$TotalLength, use = "pairwise")

# Create logged version of the variables of interest
genome$logGS <- log(genome$GenomeSize)
genome$logBW <- log(genome$BodyWeight)
genome$logTL <- log(genome$TotalLength)
genome$logFL <- log(genome$ForewingLength)
genome$logFA <- log(genome$ForewingArea)
# Create new variable containing columns of these logged variales
str(genome)
logmorpho <- c(22,23,24,25,26)
# pairs and cor test
pairs(genome[, logmorpho], col=genome$Suborder)
cor(genome[, logmorpho], use = "pairwise")
cor.test(genome$logGS, genome$logTL, use = "pairwise")

### REGRESSION ###
nullModelDragon <- lm(logBW ~ 1, data = genome, subset = Suborder == "Anisoptera")
genomeSizeModelDragon <- lm(logBW ~ logGS, data = genome, subset = Suborder == "Anisoptera")
# Look at coefficients of model
summary(genomeSizeModelDragon)
# Look at terms of model
anova(genomeSizeModelDragon)
# Residuals: sums of their squares from the two models
sum(resid(nullModelDragon) ^ 2)
sum(resid(genomeSizeModelDragon) ^ 2)


## For the damselflies instead of dragonflies now
nullModelDamsel <- lm(logBW ~ 1, data = genome, subset = Suborder == "Zygoptera")
genomeSizeModelDamsel <- lm(logBW ~ logGS, data = genome, subset = Suborder == "Zygoptera")
# Look at coefficients of model
summary(genomeSizeModelDamsel)
# Look at terms of model
anova(genomeSizeModelDamsel)
# Residuals: sums of their squares from the two models
sum(resid(nullModelDamsel) ^ 2)
sum(resid(genomeSizeModelDamsel) ^ 2)


### PLOTTING THE MODEL ###
# This took a bit of cobbling to hack together #
logGS.Anis <- log(Anisoptera$GenomeSize)
logBW.Anis <- log(Anisoptera$BodyWeight)
logGS.Zyg <- log(Zygoptera$GenomeSize)
logBW.Zyg <- log(Zygoptera$BodyWeight)
plot(logBW.Zyg ~ logGS.Zyg, col="blue", xlim=c(-0.9, 0.85), ylim=c(-6.5,-1),
     ylab="log Body Weight (g)", xlab="log Genome Size (pg)")
points(logBW.Anis ~ logGS.Anis, col="red")
abline(genomeSizeModelDragon, col="red")
abline(genomeSizeModelDamsel, col="blue")
