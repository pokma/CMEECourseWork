### F tests + t tests revisited + non-parametric tests ###

genome <- read.csv("../Data/GenomeSize.csv")

# F test
var.test(BodyWeight ~ Suborder, data = genome)


# t test revisited
genome$logBodyWeight <- log(genome$BodyWeight)
boxplot(genome$logBodyWeight ~ genome$Suborder, xlab= "Suborder", ylab="log Body Weight")
var.test(logBodyWeight ~ Suborder, data = genome)
t.test(logBodyWeight ~ Suborder, data = genome)


# Non-parametric tests: if assumptions of parameters (mean and variance) don't seem sound, these tests provide a way of of using the ranks of the data ti test for diffrences
