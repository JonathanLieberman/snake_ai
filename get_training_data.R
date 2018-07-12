get_training_data <- function(nruns = 10) {
  data_list <- computer_snake()
  for (ii in 2:nruns) {
    data_list <- c(data_list, computer_snake())
  }
  return(data_list)
}