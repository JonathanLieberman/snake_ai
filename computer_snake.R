computer_snake <- function(how_played = "simple"
                           , rows = 10
                           , columns = 10
                           , neural_net_model = NULL
                           , do_plot = FALSE
                           , do_print = FALSE
                           ) {
  # Initialize first position
  pos <- new_pos(rows, columns)
  
  # Initialize dot position
  dot_pos <- pick_new_dot_pos(pos, rows, columns)
  
  # Initialize self-intersection flag
  self_intersect <- FALSE
  
  # Initialize dot-intesection flags
  dot_intersect <- FALSE
  recently_got_dot <- FALSE
  
  distances <- get_distances(pos = pos
                                 , dot_pos = dot_pos
                                 , rows = rows
                                 , columns = columns
                                 )
  old_distance_away <- distances[[1]]
  old_steps_away <-  distances[[2]]
  if (do_print) {
    # Print start location
    print(pos)
    print(dot_pos)
  }
  
  while(!self_intersect) {
    # Save old position
    pos_old <- pos
    
    # Get input command
    direction <- choose_direction(pos, dot_pos)
    
    # Move and unpack
    output <- move(pos, dot_pos, direction, rows, columns)
    
    # Parse output of move() and update variables
    pos <- output[[1]]
    dot_intersect <- output[[2]]
    self_intersect <- output[[3]]
    
    # Calculate distances
    distances <- get_distances(pos = pos
                               , dot_pos = dot_pos
                               , rows = rows
                               , columns = columns
                               )
    distance_away <-  distances[[1]]
    steps_away <-  distances[[2]]
    
    steps_change <- steps_away - old_steps_away
    
    # Add data to data_list for function return
    data_to_add <- list(pos_old
                        , dot_pos
                        , direction
                        , dot_intersect
                        , recently_got_dot
                        , self_intersect
                        , steps_change
                        , old_distance_away
                        )
    names(data_to_add) <- c('snake_position'
                            , 'dot_position'
                            , 'command'
                            , 'got_dot'
                            , 'recently_got_dot'
                            , 'died'
                            , 'steps_change'
                            , 'distance_away'
                            )
    if (exists('data_list')) {
      data_list <- c(data_list, list(data_to_add))
    } else {
      data_list <- list(data_to_add)
    }
    
    if (do_print){
      # Print information
      print(pos)
      print(dot_pos)
      print(self_intersect)
      print(direction)
    }
    
    # Save recently_got_dot
    recently_got_dot <- dot_intersect
    
    # If snake intersected, set flag to break loop
    if (dot_intersect) {
      dot_pos <- pick_new_dot_pos(pos, rows, columns)
      dot_intersect <- FALSE
    }
    
    if (recently_got_dot) {
      distances <- get_distances(pos = pos
                                 , dot_pos = dot_pos
                                 , rows = rows
                                 , columns = columns
                                 )
      old_distance_away <-  distances[[1]]
      old_steps_away <-  distances[[2]]
    } else {
      # Move distances and steps to old
      old_steps_away <- steps_away
      old_distance_away <- distance_away
    }
    
    #frame <- plot_snake(pos
    #                    , dot_pos
    #                    , rows
    #                    , columns
    #                    )
    # frame
    if (do_plot) {
      plot(rbind(pos, dot_pos), xlim = c(0,rows-1), ylim = c(0,columns-1))
      Sys.sleep(.3)
    }
  }
  return(data_list)
}


# change direction to a number
# normalize the nn_input data to between 0 and 1