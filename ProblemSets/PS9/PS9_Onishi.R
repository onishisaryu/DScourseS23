library(tidyverse)
library(tidymodels)
library(glmnet)

set.seed(123456)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# dimension of training data
dim(housing_train)

housing_recipe <- recipe(medv~., data = housing ) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~crim:zn:indus:rm:age:rad:tax:
                     ptratio :b: lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly(crim ,zn ,indus ,rm ,age ,rad ,tax ,ptratio ,b,
             lstat ,dis ,nox , degree =6)
# Run the recipe
housing_prep <- hoing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)
# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select( medv)
housing_test_y <- housing_test_prepped %>% select( medv)

# lasso
tune_spec_l <- linear_reg(penalty = tune(),mixture = 1) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)
# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)
# Workflow
rec_wf_l <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec_l) 
# Tuning results
rec_res_l <- rec_wf_l %>% tune_grid(resamples = rec_folds,grid = lambda_grid)

top_rmse_l  <- show_best(rec_res_l, metric = "rmse")
best_rmse_l <- select_sample_rmse, "\n")
# Fit the model to the training data
final_lasso_fit <- final_lasso %>% fit(data = housing_train_prepped)

# Make predictions on the test set
predictions <- predict(final_lasso_fit, new_data = housing_test_prepped) %>%
  bind_cols(housing_test)

# Calculate out-of-sample RMSE
out_of_sample_rmse <- predictions %>%
  rmse(truth = medv, estimate = .pred) %>%
  filter(.metric == "rmse") %>% 
  slice(1) %>% 
  pull(.estimate) %>%
  as.numeric()

# Print the out-of-sample RMSE
cat("Out-of-sample RMSE:", out_of_sample_rmse, "\n")

# Ridge
tune_spec_r <- linear_reg(penalty = tune(),mixture = 0) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)
# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)
# Workflow
rec_wf_r <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec_r) # %>%
# add_recipe(housing_recipe)
# Tuning results
rec_res_r <- rec_wf_r %>% tune_grid(resamples = rec_folds,grid = lambda_grid)

top_rmse_r  <- show_best(rec_res_r, metric = "rmse")
best_rmse_r <- select_best(rec_res_r, metric = "rmse")
# Now train with tuned lambda
final_ridge <- finalize_workflow(rec_wf_r, best_rmse_r)
# show best RMSE
top_rmse_r %>% print(n = 1)
# Print out results in test set
in_sample_rmse_ridge <- last_fit(final_ridge, split = housing_split) %>%
  collect_metrics() %>% 
  filter(.metric == "rmse") %>%
  select(.estimate) %>%
  as.numeric()
#print
cat("In-sample RMSE:", in_sample_rmse_ridge, "\n")

# Fit the model to the training data
final_ridge_fit <- final_ridge %>% fit(data = housing_train_prepped)

# Make predictions on the test set
predictions_ridge <- predict(final_ridge_fit, new_data = housing_test_prepped) %>%
  bind_cols(housing_test)

# Calculate out-of-sample RMSE
out_of_sample_rmse_ridge <- predictions_ridge %>%
  rmse(truth = medv, estimate = .pred) %>%
  filter(.metric == "rmse") %>% 
  slice(1) %>% 
  pull(.estimate) %>%
  as.numeric()

# Print the out-of-sample RMSE
cat("Out-of-sample RMSE:", out_of_sample_rmse, "\n")

