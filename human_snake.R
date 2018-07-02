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