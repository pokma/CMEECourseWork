### Plotting ###

# Set working directory
getwd()
setwd("/home/kimanh/CMEECourseWork/Week1/Code")

MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv")
dim(MyDF) # check size of data frame loaded
MyDF$Record.number #check what the data contain. Hit tab after $

# scatter plots
plot(MyDF$Predator.mass,MyDF$Prey.mass)
plot(log(MyDF$Predator.mass),log(MyDF$Prey.mass),pch=20) # change marker (pch is plot characters)
plot(log(MyDF$Predator.mass),log(MyDF$Prey.mass),pch=20,xlab="Predator Mass (kg)", ylab="Prey Mass (kg)")

# histograms
hist(MyDF$Predator.mass)
hist(log(MyDF$Predator.mass), xlab="Predator Mass (kg)", ylab = "Count")
hist(log(MyDF$Predator.mass), xlab="Predator Mass (kg)", ylab = "Count", col="lightblue",border="pink")
hist(log(MyDF$Prey.mass), xlab="Prey Mass (kg)", ylab = "Count", col="lightblue",border="pink")
hist(log(MyDF$Prey.mass), xlab="Prey Mass (kg)", ylab = "Count", col="yellow",border="purple", font.lab=2, cex.lab=1.5, breaks=30)

# add labels
### xlab is x axis, ylab is y axis, col is colour, border is border colour, font.lab is font type (2 is bold), cex.lab is size of font, breaks is the bin width, main is the title ###

# subplots
graphics.off()
par(mfcol=c(2,1)) # initialize multi-paneled plot
par(mfg=c(1,1)) # specify which subplot to use first
hist(log(MyDF$Predator.mass), xlab= "Predator Mass (kg)", ylab="Count", col="lightblue", border="pink", main= 'Predator')
par(mfg=c(2,1)) # second subplot
hist(log(MyDF$Prey.mass), xlab= "Prey mass (kg", ylab= "Count", col="lightgreen", border="pink", main='Prey')

#overlaying plots
graphics.off()
hist(log(MyDF$Predator.mass), xlab="Body Mass (kg)", ylab="Count", col=rgb(1,0,0,0.5),main="Predatory-prey size overlap", breaks = 30)
hist(log(MyDF$Prey.mass), col=rgb(0,0,1,0.5), add=T, breaks = 30)
legend('topleft', c('Predators','Prey'), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

# boxplots
graphics.off()
boxplot(log(MyDF$Predator.mass), xlab="Location", ylab="Predator Mass", main="Predator mass")
boxplot(log(MyDF$Predator.mass) ~ MyDF$Location, xlab="Location", ylab="Predator Mass", main="Predator mass by location")
boxplot(log(MyDF$Predator.mass) ~ MyDF$Type.of.feeding.interaction, xlap="Location", ylab="Predator Mass", main="Predator mass by feeding interaction type")

# combining plot types
graphics.off()
par(fig=c(0,0.8,0,0.8)) # specify figure size as proportion
plot(log(MyDF$Predator.mass), log(MyDF$Prey.mass), xlab="Predator Mass (kg)", ylab="Prey Mass (kg")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(log(MyDF$Predator.mass), horizontal = TRUE, axes=FALSE)
par(fig=c(0.65,1,0,0.8), new=TRUE)
boxplot(log(MyDF$Prey.mass), axes=FALSE)
mtext("Fancy Predator-prey scatterplot", side=3, outer=TRUE, line=-3)

# lattice plots"
library(lattice) # load library
densityplot(~log(Predator.mass) | Type.of.feeding.interaction, data=MyDF)

# saving graphs as a PDF
pdf("../Results/Pred_Prey_Overlay.pdf", 11.7, 8.3) #open blank pdf page, the numbers are page dimensions in inches
hist(log(MyDF$Predator.mass), xlab="Body Mass (kg)", ylab="Count", col=rgb(1,0,0,0.5), main="Predator-Prey Size Overlap") #plot predator histogram (note 'rgb')
hist(log(MyDF$Prey.mass), col=rgb(0,0,1,0.5), add=T) # plot prey weights. # add to same plot = TRUE
legend('topleft', c('Predators', 'Prey'), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
dev.off()

### workflow should be: store and retrieve data from data directory, keep code and work in a Code directory, and save outputs to a results directory

