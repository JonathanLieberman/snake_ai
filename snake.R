# Define function for generating positions
new_pos <- function(rows, columns){
  pos <- data.frame(cbind(sample(0:(rows-1),1), sample(0:(columns-1),1)))
  colnames(pos) <-  c("x", "y")
  return(pos)
}



# Define function for generating new dot position 
pick_new_dot_pos <- function(pos, rows, columns) {
  new_spot <- new_pos(rows, columns)
  while (any(new_spot$x == pos$x & new_spot$y == pos$y)){
    new_spot <- new_pos(rows, columns)
  }
  return(new_spot)
}



# Define function for moving
move <- function(pos, dot_pos, direction, rows, columns) {
  # Parse for direction movement
  if (direction == 'd') mov_dir <- cbind(1, 0)
  if (direction == 'a') mov_dir <- cbind(-1, 0)
  if (direction == 's') mov_dir <- cbind(0, -1)
  if (direction == 'w') mov_dir <- cbind(0, 1)
  mov_dir <- as.data.frame(mov_dir)
  
  # Initialize new dot and intersect flags
  new_dot <- FALSE
  intersect <- FALSE
  
  # Move
  if (all(pos[1,] + mov_dir == dot_pos)) {
    new_pos <- rbind(pos[1,] + mov_dir, pos)
    new_dot <- TRUE
  } else{
    if (nrow(pos) == 1){
      new_pos <- pos + mov_dir
    } else {
      new_pos <- rbind(pos[1,] + mov_dir, pos[-nrow(pos),])
    }
  }
  
  # Deal with rolling borders
  new_pos[,1] <- new_pos[,1]%%rows
  new_pos[,2] <- new_pos[,2]%%columns
  if (nrow(new_pos) > 1) {
    if (any((new_pos[1,1] == new_pos[-1,1]) & (new_pos[1,2] == new_pos[-1,2]))) intersect <- TRUE
  }
  to_return <- list(new_pos, new_dot, intersect)
  return(to_return)
}



# NN input
nn_input <- function(pos
                     #, dot_pos
                     , rows
                     , columns
                     ) {
  # calculate whether position is open (w,a,s,d)
  is_open <- vector(mode = "logical", length = 4)
  if (nrow(pos) < 2) {
    is_open <- !is_open
  } else {
    chopped_pos <- pos[c(-nrow(pos),-1),]
    is_open[1] <- !any((pos[1,1] == chopped_pos[,1]) & ((pos[1,2] + 1) %% columns == chopped_pos[,2])) # check w
    is_open[2] <- !any(((pos[1,1] - 1) %% rows == chopped_pos[,1]) & (pos[1,2] == chopped_pos[,2])) # check a
    is_open[3] <- !any((pos[1,1] == chopped_pos[,1]) & ((pos[1,2] - 1) %% columns == chopped_pos[,2])) # check s
    is_open[4] <- !any(((pos[1,1] + 1) %% rows == chopped_pos[,1]) & (pos[1,2] == chopped_pos[,2])) # check d
  }
  
  # Shortest distance (up/down and left/right)
  
  return(is_open)
}



# plot game
plot_snake <- function(pos
                       , dot_pos
                       , rows
                       , columns
) {
  frame <- ggplot(#xmin = 0
         #, xmax = rows - 1
         #, ymin = 0
         #, ymax = columns - 1
         ) +
    geom_point(data = pos, aes(x = x, y = y)) +
    geom_point(data = dot_pos, aes(x = x, y = y))
  return(frame)
  # plot(rbind(pos,dot_pos))  
}