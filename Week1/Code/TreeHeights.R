# Calculates heights of trees given distance of each tree
# from its base and angle to its top, using the trigonometric formula
#
# height = distance * tan(radians)
#
# Tree species, distance, and angle are read from a hard-wired data file.
# Needs prerequisite TreeHeight function, so you'll need
# to run this as:
# R
# > source('TreeHeight.R')
# > source('TreeHeights.R')
# > TreeHeights()
#
# OUTPUT:
# A data frame with the following columns:
# Species, Distance.m, Angle.degrees, Tree.Height.m
# <pedantic>
# The last column name has redundant info and should be just Height.m
# </pedantic>

# loads the TreeHeight function
source('TreeHeight.R')


TreeHeights <- function() {
  treeFrame <- read.csv("../Data/trees.csv", header=TRUE)
  Height.m <- TreeHeight(treeFrame$Distance.m, treeFrame$Angle.degrees)
  treeFrame$Tree.Height.m <- Height.m
  # don't want row numbers in the output
  write.csv(treeFrame, '../Results/TreeHts.csv', row.names=FALSE)
  return(treeFrame)
}


# runs it, but don't want voluminous amount of stuff printed
th <- TreeHeights()
