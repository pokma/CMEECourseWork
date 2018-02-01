# Calculates heights of trees given distance of each tree
# from its base and angle to its top, using the trigonometric formula
#
# height = distance * tan(radians)
#
# Tree species, distance, and angle are read from a data file
#  specified on the command line. The easiest option is to
#  run this from the command-line from the code directory as
#
# Rscript get_TreeHeight.R ../Data/trees.csv
#
# OUTPUT:
# A .csv data file with the following:
# Species, Distance.m, Angle.degrees, Tree.Height.m

# loads the TreeHeight function
source('TreeHeight.R')


getTreeHeight <- function() {
  # the input file name is the first command-line argument
  #  with the option trailingOnly=TRUE
  inFile <- commandArgs(trailingOnly=TRUE)[1]
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
  outFile <- paste('../results/', outFile, sep='')
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
  # treeFrame <- read.csv("../Data/trees.csv", header=TRUE)
  # not reading from a wired-in file any more so the previous line
  #  is replaced by...
  treeFrame <- read.csv(inFile, header=TRUE)
  Height.m <- TreeHeight(treeFrame$Distance.m, treeFrame$Angle.degrees)
  treeFrame$Tree.Height.m <- Height.m
  return(treeFrame)
}


# let's do it
getTreeHeight()
