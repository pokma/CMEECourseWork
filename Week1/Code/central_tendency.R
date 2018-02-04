# the more you sample, the more the sample's distribution will look like the population distribution

# This script displays a number of graphs showing the number of samples vs the value of the samples for
#  normally-distributed values with mean = 0 and standard deviation = 1.


# Demonstration.
# This version uses loops, which is kind of anti-R.
# @param sample_sizes A vector where each element represents the number of values to generate.
# @param num_cols The number of grid columns to display.
demoo <- function(sample_sizes, num_cols) {
  num_graphs <- length(sample_sizes)
  num_rows <- num_graphs / num_cols
  graphics.off()  # erases existing plots, if any
  par(mfcol=c(num_rows, num_cols))  # sets up the display grid
  for (r in 1:num_rows) {
    for (c in 1:num_cols) {
      sample <- rnorm(sample_sizes[c + (r - 1) * num_cols], m=0, sd=1)
      par(mfg=c(r, c))
      hist(sample, col=rgb(1, 1, 0), main=paste('n =', length(sample)))
    }
  }
}

# Runs the demonstration.
# demoo(c(5, 10, 20, 40, 80, 160, 360, 720), 4)


# Another demonstration.
# This version uses vector processing rather than loops, which is much more R-style. Not to
#  mention a whole lot faster apparently.
# @param sample_sizes A vector where each element represents the number of values to generate.
# @param num_cols The number of grid columns to display.

demtwo <- function(sample_sizes, num_cols) {
  num_graphs <- length(sample_sizes)
  num_rows <- num_graphs / num_cols
  graphics.off()  # erases existing plots, if any
  par(mfcol=c(num_rows, num_cols))  # sets up the display grid
  # generates an appropriate number of random numbers for each sample size
  samples <- sapply(sample_sizes, function(size) {
    rnorm(size, m=0, sd=1)
  })
  # displays each set of samples on the grid
  sapply(samples, function(sample) {
    hist(sample, col=rgb(1, 1, 0), main=paste('n =', length(sample)))
  })
}

# Runs the demonstration.
# demtwo(c(5, 10, 20, 40, 80, 160, 360, 720), 4)


# Yet another demonstration. Here we generate samples of a number of 5 uniformly-distributed values, eg,
#  the first sample consists of 5 sets of 5 numbers, the second consists of 10 sets of 5 numbers, etc.
# Then we calculate the mean of each set of 5 numbers, eg, the first set consists of 10 means, the second set
#  consists of 10 means, etc.
# Finally, calculate the mean of each set of means. The means should increasingly approach 0.
demMore <- function(sample_sizes) {
  # for each sample_size, generates that many sets of 5 uniformly-distributed random values
  samples <- sapply(sample_sizes, function(size) {
    return(sapply(1:size, function(num) {
      rnorm(5, m=0, sd=1)
    }))
  })
  # for each set of 5 random values, determines the mean
  means <- sapply(samples, mean)
  # determines the mean of the means
  print(sapply(means, mean))
  # now do the same for the standard deviations...
}

demMore(c(5, 10, 20, 40, 80, 160, 360, 1000, 10000, 100000))