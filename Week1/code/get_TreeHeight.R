# Calculates heights of trees given distance of each tree
# from its base and angle to its top, using the trigonometric formula
#
# height = distance * tan(radians)
#
# Tree species, distance, and angle are read from a data file
#  specified on the command line. So to run this you'd launch, eg,
# R ../Data/trees.csv
# Needs prerequisite TreeHeight function, so you'll need
# to run this as:
#
# R ../Data/trees.csv
# > source('TreeHeight.R')
# > source('get_TreeHeight.R')
# > getTreeHeights()
#
# OUTPUT:
# A .csv data file with the following:
# Species, Distance.m, Angle.degrees, Tree.Height.m

getTreeHeight <- function() {
  # the input file name is the second command-line argument
  inFile <- commandArgs()[2]
  # calculates tree heights
  treeFrame <- TreeHeights(inFile)
  # gets output file
  outFile = getOutFile(inFile)
  # writes output file
  write.csv(treeFrame, outFile, row.names=FALSE)
}

# Converts the input file name into the output file name.
# Eg, ../Data/arbres.csv  -->  ../Results/arbres_treeheights.csv
# ARGUMENT:
# inFile - Input file name.
# OUTPUT
# Returns output file name.
getOutFile <- function(inFile) {
  # print(inFile)
  # print()
  # strip off trailing suffix from input file name
  inFile <- substr(inFile, 1, nchar(inFile) - nchar('.csv'))
  # strip off leading relative path, if any
  # this is ugly
  inFile.split <- strsplit(inFile, '/')
  inFile <- inFile.split[[1]][length(inFile.split[[1]])]
  # append suffixy bit to make output file name
  outFile <- paste(inFile, '_treeheights.csv', sep='')
  outFile <- paste('../Results/', outFile, sep='')
  # print(paste('output file:', outFile))
  return(outFile)
}

# Calculates heights of trees given distance of each tree
# from its base and angle to its top, using the trigonometric formula
#
# height = distance * tan(radians)
#
# ARGUMENT:
# inFile - Input file name
# OUTPUT:
# A data frame with the following columns:
# Species, Distance.m, Angle.degrees, Tree.Height.m
TreeHeights <- function(inFile) {
  treeFrame <- read.csv(inFile, header=TRUE)
  Height.m <- TreeHeight(treeFrame$Distance.m, treeFrame$Angle.degrees)
  treeFrame$Tree.Height.m <- Height.m
  return(treeFrame)
}