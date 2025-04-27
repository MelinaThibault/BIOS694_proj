# --- Helper Functions ---
library(keras)

df_train <- data_nn %>%
  dplyr:: select(Hour, Month, Weekday, first_resource)

# One-hot or dummy encode your two features:
x_train <- model.matrix(~ Hour + Month + Weekday, df_train)[, -1]
y_train <- as.integer(factor(df_train$first_resource)) - 1

num_classes <- length(unique(y_train))
set.seed(2025)
train_idx <- sample(seq_len(nrow(x_train)), size = 0.8 * nrow(x_train))
x_tr <- x_train[train_idx, ]
y_tr <- y_train[train_idx]
x_val <- x_train[-train_idx, ]
y_val <- y_train[-train_idx]

model_nn <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = ncol(x_tr)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = num_classes, activation = "softmax")

model_nn %>% compile(
  optimizer = "adam",
  loss      = "sparse_categorical_crossentropy",
  metrics   = "accuracy"
)

history <- model_nn %>% fit(
  x        = x_tr,
  y        = y_tr,
  epochs   = 30,
  batch_size = 128,
  validation_data = list(x_val, y_val),
  verbose  = 1
)

# Predict candidates for every incident
# Create the same dummy columns for each incident, then predict in batch.
x_all <- model.matrix(~ Hour + Month + Weekday, data_nn)[, -1]

probs_all <- model_nn %>% predict(x_all)  
# probs_all is an N × num_classes matrix

# For each incident, grab the top-5 responders & their probs:
library(purrr)

amb_candidates <- map_dfr(
  seq_len(nrow(probs_all)),
  function(i) {
    p <- probs_all[i, ]
    ord <- order(p, decreasing = TRUE)[1:min(5, length(p))]
    tibble(
      pat_ID = data_nn$pat_ID[i],
      resource_id_potential   = levels(factor(df_train$first_resource))[ord],
      prob        = p[ord],
      rank        = seq_along(ord)
    )
  }
)
