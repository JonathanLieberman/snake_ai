library(tidyverse)

#xgboost
snake_data <- get_training_data(nruns = 100)
xgb_data <- as.matrix(bind_rows(lapply(snake_data, prep_training_data, rows = 10, columns = 10)))
bst <- xgboost(data = xgb_data[,-6], label = xgb_data[,6], max_depth = 2, eta = 1, nrounds = 3)
predict(bst, xgb_data[,-6])

#neuralnet
library(neuralnet)
snake_data <- get_training_data(nruns = 100)
nn_data <- bind_rows(lapply(snake_data, prep_training_data, rows = 10, columns = 10))
nn_formula <- "successful_move ~ can_l + can_s + can_r + command + angle"
nn <- neuralnet(nn_formula, nn_data, hidden = c(4,2))
nn_out <- compute(nn,nn_data[,-7])
