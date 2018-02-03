samples = c(5, 10, 20, 40, 80, 160)

display_maker <- function(num_cols) {
  return(function(samples) {
    num_items = length(samples) 
    num_rows = num_items / num_cols
    print(paste(num_items, 'items in', num_rows, 'X', num_cols))
  })
}

displayer <- display_maker(2)
displayer(samples)
