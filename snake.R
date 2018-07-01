# Define function for generating positions
new_pos <- function(rows, columns){
  pos <- c(sample(0:(rows-1),1), sample(0:(columns-1),1))
}

# Define function for generating new dot position 
pick_new_dot_pos <- function(pos, rows, columns) {
  new_spot <- new_pos(rows, columns)
  while (any(new_spot[1] == pos[1,] & new_spot[2] == pos[2,])){
    new_spot <- new_pos(rows, columns)
  }
  return(new_spot)
}

# Define function for moving
move <- function(pos, dot_pos, direction, rows, columns) {
  # Parse for direction movement
  if (direction == 'd') mov_dir <- c(1, 0)
  if (direction == 'a') mov_dir <- c(-1, 0)
  if (direction == 's') mov_dir <- c(0, -1)
  if (direction == 'w') mov_dir <- c(0, 1)
  
  # Initialize new dot and intersect flags
  new_dot <- FALSE
  intersect <- FALSE
  
  # Move
  if (all(pos[,1] + mov_dir == dot_pos)) {
    new_pos <- cbind(pos[,1] + as.matrix(mov_dir), pos)
    new_dot <- TRUE
  } else{
    if (ncol(pos) == 1){
      new_pos <- pos[,1] + as.matrix(mov_dir)
    } else {
      new_pos <- cbind(pos[,1] + as.matrix(mov_dir), pos[,-ncol(pos)])
    }
  }
  
  # Deal with rolling borders
  new_pos[1,] <- new_pos[1,]%%rows
  new_pos[2,] <- new_pos[2,]%%columns
  if (any((new_pos[1,1] == pos[1,]) & (new_pos[2,1] == pos[2,]))) intersect <- TRUE
  to_return <- list(new_pos, new_dot, intersect)
  return(to_return)
}

play_snake <- function(rows = 10, columns = 10) {
  # Initialize first position
  pos <- as.matrix(new_pos(rows, columns))
  
  # Initialize dot position
  dot_pos <- pick_new_dot_pos(pos, rows, columns)
  
  # Initialize self-intersection flag
  self_intersect <- FALSE
  
  # Initialize dot-intesection flag
  dot_intersect <- FALSE
  
  while(!self_intersect) {
    # Print position
    print(pos)
    print(dot_pos)
    
    # Get input command
    input <- readline(prompt = "Enter direction: ")
    
    # Move
    output <- move(pos, dot_pos, input, rows, columns)
    pos <- output[[1]]
    dot_intersect <- output[[2]]#as.logical(output[2])
    self_intersect <- output[[3]]#as.logical(output[3])
    if (dot_intersect) {
      dot_pos <- pick_new_dot_pos(pos, rows, columns)
      dot_intersect <- FALSE
    }
  }
  return(0)
}

# NN input
nn_input <- function(pos
                     #, dot_pos
                     #, rows
                     #, columns
                     ) {
  # calculate whether position is open (w,a,s,d)
  is_open <- vector(mode = "logical", length = 4)
  if (length(is_open) < 2) {
    is_open <-  !is_open
  } else {
    chopped_pos <- pos[,c(-1,-ncol(pos))]
    is_open[1] <- !any((pos[1] == chopped_pos[1,]) && (pos[2] == chopped_pos[2,] + 1)) # check w
    is_open[2] <- !any((pos[1] == chopped_pos[1,] - 1) && (pos[2] == chopped_pos[2,])) # check a
    is_open[3] <- !any((pos[1] == chopped_pos[1,]) && (pos[2] == chopped_pos[2,] - 1)) # check s
    is_open[4] <- !any((pos[1] == chopped_pos[1,] + 1) && (pos[2] == chopped_pos[2,])) # check d
  }

  return(is_open)
}

# plot game
plot_snake <- function(pos
                       # , dot_pos
) {
  ggplot() +
    geom_point(data = pos, aes(x = x, y = y))
}