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



# can_move checks the position data and returns whether moves are possible 
can_move <- function(pos
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
  # Convert to 1s and 0s for neural net
  is_open <- as.numeric(is_open)
  is_open <- as.data.frame(t(is_open))
  colnames(is_open) <- c('can_w'
                         , 'can_a'
                         , 'can_s'
                         , 'can_d'
                         )
  return(is_open)
}


get_distances <- function(pos
                          , dot_pos
                          , rows
                          , columns
                          ) {
  # Shortest distance (up/down and left/right)
  location <- pos[1,]
  differences <- abs(location - dot_pos)
  
  
  if (differences[1] <= rows/2){
    x_distance <- dot_pos[1] - location[1]
  } else {
    x_temp <- dot_pos[1] - location[1]
    x_distance <-  (rows - abs(x_temp)) * sign(-x_temp) 
  }
  
  if (differences[2] <= columns/2){
    y_distance <- dot_pos[2] - location[2]
  } else {
    y_temp <- dot_pos[2] - location[2]
    y_distance <-  (columns - abs(y_temp)) * sign(-y_temp)
  }
  
  distances <- data.frame(cbind(x_distance, y_distance))
  colnames(distances) <- colnames(dot_pos)
  
  min_steps_away <- sum(abs(distances))
  
  to_return <- list(distances, min_steps_away)
  return(to_return)
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
    geom_point(data = dot_pos, aes(x = x, y = y, colour = "Red"))
  return(frame)
  # plot(rbind(pos,dot_pos))  
}


prep_training_data <-  function(move_data
                       , rows
                       , columns
) {
  # Determine success of move 
  successful_move <-  as.numeric(move_data$steps_change < 0)# * .75 +.25
  
  # Overwrite success if necessary
  if (move_data$died) successful_move <- -1
  if (move_data$got_dot) successful_move <- 1
  successful_move <- as.data.frame(successful_move)
  
  # Get possible movement directions
  move_directions <- can_move(pos = move_data$snake_position
                              , rows = rows
                              , columns = columns
                              )
  
  # Grab command of the move
  # Convert direction to normalized angle, branch cut = negative real x-axis
  command <- as.data.frame(move_data$command)
  if (command == "a") command_num <- 1
  if (command == "w") command_num <- .5
  if (command == "d") command_num <- 0
  if (command == "s") command_num <- -.5
  command_num <- data.frame(command_num)
  colnames(command_num) <- "command"
  
  # Get angle
  angle <- atan2(move_data$distance_away[1,2], move_data$distance_away[1,1])/pi
  angle <- as.data.frame(angle)
  colnames(angle) <- "angle"
  
  output <- cbind.data.frame(move_directions
                             , command_num
                             , angle
                             , successful_move
                             )
  return(output)
}