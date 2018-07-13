choose_direction <- function(pos
                             , dot_pos
                             , how_played = "simple"
                             , model = NULL
                             , neural_net_formula = NULL
                             , rows = 10
                             , columns = 10
                             ) {
  if (how_played == "simple") {
    
    #Get current direction
    direction <- as.matrix(pos[1,]-pos[2,])
    
    #Get distances to dot
    distances <- dot_pos - pos[1,]
    
    #Get angle to dot
    angle <- get_angle(distances, direction)
    
    if (angle > pi/4) return("r")
    if (angle < -pi/4) return("l")
    return("s")
  }
  
  if (how_played == "xgboost") {
    model_input <- can_move(pos = pos
                         , dot_pos = dot_pos
                         , rows = rows
                         , columns = columns
    )
    
    #Get current direction
    direction <- as.matrix(pos[1,]-pos[2,])
    
    #Get distances to dot
    distances <- dot_pos - pos[1,]
    
    #Save spot for command
    model_input$command <- NA
    
    #Get angle to dot
    angle <- get_angle(distances, direction)/pi
    names(angle) <-  "angle"
    
    #Add angle to list of inputs
    model_input$angle <- angle
    
    model_outputs <- data.frame(matrix(NA, nrow = 1, ncol = 3))
    colnames(model_outputs) <- c("l_score"
                                 , "s_score"
                                 , "r_score"
    )
    
    model_input = as.matrix(model_input)
    
    #Test l
    model_input[1,colnames(model_input)=="command"] = 1
    model_outputs$l_score <- predict(model, model_input)
    
    #Test s
    model_input[1,colnames(model_input)=="command"] = 0
    model_outputs$s_score <- predict(model, model_input)
    
    #Test r
    model_input[1,colnames(model_input)=="command"] = -1
    model_outputs$r_score <- predict(model, model_input)
    
    best_score <- max(model_outputs)
    print(model_outputs)
    print(best_score)
    if (model_outputs$l_score == best_score) return("l")
    if (model_outputs$s_score == best_score) return("s")
    if (model_outputs$r_score == best_score) return("r")
  }
  
  if (how_played == "neural_net") {
    #Generate nueral_net input
    nn_input <- can_move(pos = pos
                         , dot_pos = dot_pos
                         , rows = rows
                         , columns = columns
                         )
    print(nn_input)
    
    #Get distances away
    distances <- get_distances(pos = pos
                               , dot_pos = dot_pos
                               , rows = rows
                               , columns = columns
                               )
    
    #Drop steps_away
    distances <- distances[[1]]
    
    #CHANGE TO APPROPRIATE ANGLE
    
    #Calculate best angle
    nn_input$angle <- atan2(distances[1,2], distances[1,1])/pi
    
    model_outputs <- data.frame(matrix(NA, nrow = 1, ncol = 3))
    colnames(model_outputs) <- c("l_score"
                                 , "s_score"
                                 , "r_score"
                                 )
    
    #Test l
    nn_input$command_num = 1
    model_outputs$l_score <- compute(model, nn_input)$net.result
    
    #Test s
    nn_input$command_num = 0
    model_outputs$s_score <- compute(model, nn_input)$net.result
    
    #Test r
    nn_input$command_num = -1
    model_outputs$r_score <- compute(model, nn_input)$net.result
    
    best_score <- max(model_outputs)
    print(model_outputs)
    print(best_score)
    if (model_outputs$l_score == best_score) return("l")
    if (model_outputs$s_score == best_score) return("s")
    if (model_outputs$r_score == best_score) return("r")
  }
}