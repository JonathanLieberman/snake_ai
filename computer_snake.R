computer_snake <- function(rows = 10, columns = 10) {
  # Initialize first position
  pos <- new_pos(rows, columns)
  
  # Initialize dot position
  dot_pos <- pick_new_dot_pos(pos, rows, columns)
  
  # Initialize self-intersection flag
  self_intersect <- FALSE
  
  # Initialize dot-intesection flag
  dot_intersect <- FALSE
  
  # Print start location
  print(pos)
  print(dot_pos)
  
  while(!self_intersect) {
    # Print position
    
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
    
    # Print information
    print(pos)
    print(dot_pos)
    print(self_intersect)
    print(direction)
    
    # If snake intersected, set flag to break loop
    if (dot_intersect) {
      dot_pos <- pick_new_dot_pos(pos, rows, columns)
      dot_intersect <- FALSE
    }
    #frame <- plot_snake(pos
    #                    , dot_pos
    #                    , rows
    #                    , columns
    #                    )
    #frame
    plot(rbind(pos, dot_pos), xlim = c(0,rows-1), ylim = c(0,columns-1))
    Sys.sleep(.3)
  }
  return(data_list)
}