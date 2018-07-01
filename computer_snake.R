computer_snake <- function(rows = 10, columns = 10) {
  # Initialize first position
  #pos <- as.matrix(new_pos(rows, columns))
  half_row <-  floor(rows/2)
  half_col <-  floor(columns/2)
  pos <- matrix(c(half_col, half_row, half_col, half_row-1), nrow = 2)
  rownames(pos) <- c("x", "y")
  
  # Initialize dot position
  dot_pos <- pick_new_dot_pos(pos, rows, columns)
  # rownames(dot_pos) <- c("x", "y")
  
  # Initialize self-intersection flag
  self_intersect <- FALSE
  
  # Initialize dot-intesection flag
  dot_intersect <- FALSE
  
  while(!self_intersect) {
    # Print position
    print(pos)
    print(dot_pos)
    
    # Get input command
    direction <- choose_direction(pos, dot_pos)
    
    # Move
    output <- move(pos, dot_pos, direction, rows, columns)
    
    # Add data to data_list for function return
    data_to_add <- list(pos, dot_pos, direction)
    names(data_to_add) <- c('snake_position', 'dot_position', 'command')
    if (exists('data_list')) {
      data_list <- c(data_list, list(data_to_add))
    } else {
      data_list <- list(data_to_add)
    }
    
    # Parse output of move() and update variables
    pos <- output[[1]]
    dot_intersect <- output[[2]]
    self_intersect <- output[[3]]
    
    # If snake intersected, set flag to break loop
    if (dot_intersect) {
      dot_pos <- pick_new_dot_pos(pos, rows, columns)
      dot_intersect <- FALSE
    }
  }
  return(data_list)
}