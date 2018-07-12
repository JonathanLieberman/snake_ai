choose_direction <- function(pos
                             , dot_pos
                             , how_played = "simple"
                             , neural_net_model = NULL
                             , neural_net_formula = NULL
                             , rows = 10
                             , columns = 10
                             ) {
  if (how_played == "simple") {
    if (pos[1,1] < dot_pos[1]) return('d')
    if (pos[1,1] > dot_pos[1]) return('a')
    if (pos[1,2] < dot_pos[2]) return('w')
    if (pos[1,2] > dot_pos[2]) return('s')
  }
  
  if (how_played == "neural_net") {
    #Generate nueral_net input
    nn_input <- can_move(pos = pos
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
    
    #Calculate best angle
    nn_input$angle <- atan2(distances[1,2], distances[1,1])/pi
    
    model_outputs <- data.frame(matrix(NA, nrow = 1, ncol = 4))
    colnames(model_outputs) <- c("w_score"
                                 , "a_score"
                                 , "s_score"
                                 , "d_score"
                                 )
    
    #Test w
    nn_input$command_num = .5
    model_outputs$w_score <- compute(neural_net_model, nn_input)$net.result
    
    #Test a
    nn_input$command_num = 1
    model_outputs$a_score <- compute(neural_net_model, nn_input)$net.result
    
    #Test s
    nn_input$command_num = -.5
    model_outputs$s_score <- compute(neural_net_model, nn_input)$net.result
    
    #Test d
    nn_input$command_num = 0
    model_outputs$d_score <- compute(neural_net_model, nn_input)$net.result
    
    best_score <- max(model_outputs)
    print(model_outputs)
    print(best_score)
    if (model_outputs$w_score == best_score) return("w")
    if (model_outputs$a_score == best_score) return("a")
    if (model_outputs$s_score == best_score) return("s")
    if (model_outputs$d_score == best_score) return("d")
  }
}