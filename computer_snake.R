computer_snake <- function(rows = 10, columns = 10) {
  # Initialize first position
  pos <- new_pos(rows, columns)
  
  # Initialize dot position
  dot_pos <- pick_new_dot_pos(pos, rows, columns)
  
  # Initialize self-intersection flag
  self_intersect <- FALSE
  
  # Initialize dot-intesection flag
  dot_intersect <- FALSE
  
  distances <- get_distances(pos = pos
                                 , dot_pos = dot_pos
                                 , rows = rows
                                 , columns = columns
                                 )
  old_distance_away <- distances[[1]]
  old_steps_away <-  distances[[2]]
  
  # Print start location
  print(pos)
  print(dot_pos)
  
  while(!self_intersect) {
    # Print position
    
    # Get input command
    direction <- choose_direction(pos, dot_pos)
    
    # Move and unpack
    output <- move(pos, dot_pos, direction, rows, columns)
    
    # Parse output of move() and update variables
    pos <- output[[1]]
    dot_intersect <- output[[2]]
    self_intersect <- output[[3]]
    
    # Calculate Distances
    distances <- get_distances(pos = pos
                               , dot_pos = dot_pos
                               , rows = rows
                               , columns = columns
                               )
    distance_away <-  distances[[1]]
    steps_away <-  distances[[2]]
    
    steps_change <- steps_away - old_steps_away
    
    # Add data to data_list for function return
    data_to_add <- list(pos
                        , dot_pos
                        , direction
                        , dot_intersect
                        , self_intersect
                        , steps_change
                        , distance_away
                        )
    names(data_to_add) <- c('snake_position'
                            , 'dot_position'
                            , 'command'
                            , 'got_dot'
                            , 'died'
                            , 'steps_change'
                            , 'distance_away'
                            )
    if (exists('data_list')) {
      data_list <- c(data_list, list(data_to_add))
    } else {
      data_list <- list(data_to_add)
    }
    
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
    
    # Move distances and steps to old
    old_steps_away <- steps_away
    old_distance_away <- distance_away
    
    #frame <- plot_snake(pos
    #                    , dot_pos
    #                    , rows
    #                    , columns
    #                    )
    # frame
    plot(rbind(pos, dot_pos), xlim = c(0,rows-1), ylim = c(0,columns-1))
    Sys.sleep(.3)
  }
  return(data_list)
}