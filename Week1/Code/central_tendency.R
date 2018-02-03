# the more you sample, the more the sample's distribution will look like the population distribution

# This script displays a number of graphs showing the number of samples vs the value of the samples for
#  normally-distributed values with mean = 0 and standard deviation = 1.


# Generates a number of uniformly-distributed random values.
# @param size The size of the sample.
# @return The vector of generated numbers.
# sampling <- function(size) {
#   return(rnorm(size, m=0, sd=1))
# }


# Displays the histogram of generated values at a given position on a grid of graphs.
# @param sample The vector of sample values.
# @param rw The grid position row-number.
# @param label A label for the graph.
# @param cl The grid position column-numver.
# visualize <- function(sample, rw=1, cl=1, label="Sample this!") {
#   par(mfg=c(rw, cl))
#   hist(sample, col=rgb(1, 1, 0), main=label)
# }


# Demonstration.
# @param sample_sizes A vector where each element represents the number of values to generate.
# @param num_cols The number of grid columns to display.
demoo <- function(sample_sizes, num_cols) {
  num_graphs <- length(sample_sizes)
  num_rows <- num_graphs / num_cols
  dev.off()  # erases existing plots
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
demoo(c(5, 10, 20, 40, 80, 160, 360, 720), 4)
