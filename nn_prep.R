library(tidyverse)
library(neuralnet)
nn_data <- bind_rows(lapply(snake_data, prep_training_data, rows = 10, columns = 10))
nn_formula <- "successful_move ~ can_w + can_a + can_s + can_d + command + angle"
nn <- neuralnet(nn_formula, nn_data, hidden = c(4,2))
nn_out <- compute(nn,nn_data[,-7])
