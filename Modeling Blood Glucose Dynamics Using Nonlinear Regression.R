packages <- c("tidyverse", "corrplot", "GGally", "zoo", "caret", "Metrics")

missing_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(missing_pkgs)) install.packages(missing_pkgs)

library(tidyverse)
library(corrplot)
library(GGally)
library(zoo)
library(caret)
library(Metrics)


# Loading a Dataset 
data_csv <- read.csv("/Users/satishadhikari/Downloads/assignment.csv") 

# Dataset didnt show up so checking if the file exists
file.exists("/Users/satishadhikari/Downloads/assignment.csv") 

#Exploratory Data Analysis # Viewing the structure of the dataset 
summary(data_csv)
str(data_csv)
class(data_csv)

# First few rows of the dataset 
head(data_csv) 

# Checking how many column contains NA values and the exact number of them 
colSums(is.na(data_csv)) 


# The column hr_mean contains 7872 number of NA values,
#first we fix it properly by replacing NA with the mean of that row's HR window
# Replacing ALL NA values in hr_mean with the GLOBAL 
# mean of the hr_mean column (not row-wise)

data_csv$hr_mean <- ifelse(is.na(data_csv$hr_mean),
                           mean(data_csv$hr_mean, na.rm = TRUE), 
                           data_csv$hr_mean)
str(data_csv$hr_mean)  # Should be numeric
data_csv$hr_mean <- as.numeric(as.character(data_csv$hr_mean))

data_csv 

# Checking the columns containing only zeros 
cols_zero <- names(data_csv)[sapply(data_csv, function(x) all (x==0))]
cols_zero 

# This means there are no columns in the dataset where all values are 0. 
data_csv$time <- 1:nrow(data_csv)

# scale() returns a matrix, not a dataframe. Matrices do not support $,
# so we convert the scaled matrix back to data frame 
num_cols <- sapply(data_csv, is.numeric)

# scale() returns a matrix, so it is converted back to a data frame
# to allow use of `$` and ggplot compatibility

scaled_data_csv <- as.data.frame(scale(data_csv[, num_cols]))
scaled_data_csv$time <- 1:nrow(data_csv)  # Add time separately



# TASK-1 : Preliminary Data Analysis

# Task 1.1 -> Time series plots of input and output signal 
ggplot(scaled_data_csv, aes(x = time, y = bg.1.00)) + 
  geom_line(alpha = 0.3, color = 'blue') + 
  geom_smooth(color = "red", linewidth = 1.2)+ 
  labs(title = " Time series of Future Glucose (bg+1.00)", 
       x= "Time", y = "Glucose (mmol/L)") 

plot(scaled_data_csv$bg_mean, type = "l", col = "darkred", 
     main = "Time-Series: Mean Past Glucose", 
     ylab = "bg_mean", xlab = "Time Index") 

plot(scaled_data_csv$hr_mean, type = "l", col = "darkgreen", 
     main = "Time-Series: Mean Heart Rate", 
     ylab = "hr_mean", xlab = "Time Index") 

plot(scaled_data_csv$insulin_sum, type = "l", col = "purple", 
     main = "Time-Series: Insulin Sum", 
     ylab = "insulin_sum", xlab = "Time Index") 

plot(scaled_data_csv$carbs_sum, type = "l", col = "orange", 
     main = "Time-Series: Carbs Sum", 
     ylab = "carbs_sum", xlab = "Time Index") 

plot(scaled_data_csv$steps_sum, type = "l", col = "brown", 
     main = "Time-Series: Steps Sum", 
     ylab = "steps_sum", xlab = "Time Index") 

plot(scaled_data_csv$cals_sum, type = "l", col = "darkcyan", 
     main = "Time-Series: Calories Burned", 
     ylab = "cals_sum", xlab = "Time Index") 

timeseries_vars <- c( 
  "bg_mean", 
  "hr_mean", 
  "insulin_sum", 
  "carbs_sum", 
  "steps_sum", 
  "cals_sum" 
) 

rolling_200 <- data.frame(
  lapply( scaled_data_csv[timeseries_vars], 
          function(x) rollmean(x, k = 200, fill = NA, align = "right") 
  )
) 

# Base R: Time series plots 
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1)) 

for (var in timeseries_vars) { 
  plot( scaled_data_csv[[var]],
        type = "l",
        col = adjustcolor("#1f77b4", alpha.f = 0.4),
        lwd = 1,
        xlab = "Time Index",
        ylab = paste("Scaled", var),
        main = paste(var, "with 200-Point Rolling Average")
  )
  lines( rolling_200[[var]],
         col = "#d62728",
         lwd = 2
  ) 
  legend( 
    "topright",
    legend = c("Original Series", "200-Point Rolling Average"),
    col = c("#1f77b4", "#d62728"),
    lwd = c(1, 2),
    bty = "n",
    cex = 0.8 
  ) 
} 

# Distribution of each variable 
dist_plot <- scaled_data_csv |>
  select(bg.1.00, bg_mean, hr_mean,insulin_sum,
         carbs_sum, steps_sum, cals_sum) |> 
  pivot_longer(cols= everything(),
               names_to ="variable",
               values_to = "value")

ggplot(dist_plot, aes(x = value)) + 
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40,
                 fill = "skyblue",
                 color = "white",
                 alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~ variable, scales = "free") +
  labs( title = "Distribution of Scaled Variables",
        x = "Scaled Value",
        y = "Density" ) + 
  theme_minimal() 

#Correlation and scatter plots (between different combinations of input 
# and output variables) to examine their dependencies 
input_vars <- c("insulin_sum", 
                "carbs_sum",
                "hr_mean",
                "steps_sum",
                "bg_mean",
                "cals_sum")


# At this stage, `y` implicitly refers to future glucose (bg.1.00)
# The explicit renaming to `y` occurs later in Task-2

par(mfrow = c(2, 3))  # 2 rows, 3 columns
for (var in input_vars) {
  plot(scaled_data_csv[[var]], scaled_data_csv$y,
       xlab = var,
       ylab = "y",
       main = paste(var, "vs y"),
       pch = 16, col = rgb(0.2,0.4,0.6,0.5))
}
par(mfrow = c(1,1))

#Selecting input and output variables 

#output variable 
out_var <- "bg.1.00" 

#input variables 
inp_var <- c( "bg_mean", 
              "hr_mean",
              "insulin_sum",
              "carbs_sum",
              "steps_sum",
              "cals_sum" )

# Creating a new dataset that contains only the o/p and i/p 
# variables for analyzing
corr_data <- scaled_data_csv[,c(out_var, inp_var)] 

#correlation matrix 
corr_matrix <- cor(corr_data, use = "complete.obs") 
round(corr_matrix, 2)      #values ranges from -1 to +1 

# Visualizing with heatmap 
corrplot( corr_matrix,
          method = "color",
          type = "upper",
          addCoef.col = "black",
          tl.col = "black",
          tl.srt = 45,
          number.cex = 0.8 ) 
# Red and blue indicates positive and negative correlation respectively
# Darker the color stronger the dependency 

# Scatter plots for dependency examination 
corr_data <- na.omit(corr_data)

#ggpairs() shows pairwise scatter plots, correlations,and marginal 
# densities for exploratory analysis
ggpairs( corr_data,
         lower = list(continuous = wrap("points", 
                                        alpha = 0.4, 
                                        col = "steelblue")), 
         upper = list(continuous = wrap("cor", size = 4)),
         diag = list(continuous = "densityDiag") ) 
inp_var <- c("insulin_sum",
             "carbs_sum",
             "hr_mean",
             "steps_sum")

out_var <- "bg_mean" # future glucose (bg + 1.0) 

important_pairs <- list( c("bg_mean", "insulin_sum"),
                         c("bg_mean", "carbs_sum"),
                         c("bg_mean", "hr_mean"),
                         c("bg_mean", "steps_sum"),
                         c("insulin_sum", "carbs_sum"),
                         c("insulin_sum", "hr_mean"),
                         c("insulin_sum", "steps_sum"),
                         c("carbs_sum", "hr_mean"),
                         c("carbs_sum", "steps_sum"),
                         c("hr_mean", "steps_sum") ) 

# Combine input-output pairs with important pairs to avoid repetition 
all_pairs <- c( lapply(inp_var, function(x) c(x, out_var)), # input vs output 
                important_pairs
) 

# Loop through all pairs 
for(pair in all_pairs) { 
  xvar <- pair[1] 
  yvar <- pair[2] 
}

#TASK-2 : Regression - modeling the relationship between gene expressions 

# Renaming column names i.e. input variables and output variable 
# to x1,x2,x3,x4,x5,x6 and y respectively 
scaled_data_csv <- scaled_data_csv %>%
  rename( x1 = insulin_sum,
          x2 = carbs_sum,
          x3 = hr_mean,
          x4 = steps_sum,
          x5 = bg_mean,
          x6 = cals_sum,
          y = bg.1.00 ) 


# Task- 2.1: Estimate model parameters(Least Sqaures) 


# These models explore nonlinear effects using polynomial terms 
# and joint effects using interaction terms
# Model comparison is based on RSS, AIC, and BIC

#Fitting models 
model_1 <- lm(y ~ I(x1^3) + I(x2^2) + I(x3^2) + x4 + x5 + x6, 
              data = scaled_data_csv)
model_2 <- lm(y ~ I(x1^2) + I(x2^2) + I(x3^3) + x4 + x5 + x6, 
              data = scaled_data_csv)
model_3 <- lm(y ~ x1 + x2 + x3 + I(x4^2) + x5 + I(x6^2), 
              data = scaled_data_csv)
model_4 <- lm(y ~ I(x1^2) + I(x2^2) + I(x3^2) + I(x4^2) + I(x5^2) + I(x6^2), 
              data = scaled_data_csv)
model_5 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + I(x1*x2) + I(x3*x4) + I(x2*x6), 
              data = scaled_data_csv)

models <- list(model_1=model_1, model_2=model_2, model_3=model_3, 
               model_4=model_4, model_5=model_5) 

#viewing estimated coefficients 
coef_list <- lapply(models, coef) 

#Converting to df for easy viewing 
# coef_list contains coefficients for each model 
# Convert to long-format data frame 

coef_df <- do.call(rbind, lapply(names(coef_list), function(name) { 
  data.frame( Model = name,
              Coefficient = names(coef_list[[name]]),
              Value = coef_list[[name]],
              row.names = NULL 
  ) 
})) 

# View the data frame 
coef_df 

#Computing θ using Least Squares
theta <-function(model){
  coef(model)
}
theta_all <- lapply(models,theta)
theta_all_df <- do.call(rbind, lapply(names(theta_all), function(name){
  data.frame(
    Model = name,
    Coefficient = names(theta_all[[name]]),
    Value = theta_all[[name]],
    row.names = NULL
  )
}))

theta_all_df


# Compute residual standard deviation
sigma_model <- function(model){
  n <- length(resid(model))      # number of observations
  rss <- sum(resid(model)^2)     # residual sum of squares
  sqrt(rss / n)                  # sigma = sqrt(RSS / n)
}

sigma_all <- sapply(models, sigma_model)
sigma_all  # view sigma for all models


# Task-2.2 : Compute RSS 

#Computing RSS(Residual Sum of Squared) 
fun_rss <- function(model){
  sum(resid(model)^2)
}
rss_value <- sapply(models, fun_rss)
rss_value 


# Task 2.3: Compute Log-Likelihood 

# Log-likelihood is computed assuming Gaussian residuals 
# with variance estimated as RSS / n
# This is consistent with AIC/BIC assumptions

log_lik <- function(model){
  n <- length(resid(model))
  rss <- sum(resid(model)^2)
  sigma2 <- rss / n
  loglik <- - (n/2) * log(2 * pi * sigma2) - (rss / (2 * sigma2))
  return(loglik)
}

loglik_val <- sapply(models, log_lik)
loglik_val 

#Task-2.4 : Compute AIC and BIC for each model 
aic_val <- sapply(models, AIC) 
bic_val <- sapply(models, BIC) 

#Combining RSS, AIC, BIC into a summary table 
summary_table <- data.frame(
  RSS = sapply(models, function(m) sum(resid(m)^2)),
  Sigma = sapply(models, sigma_model),
  LogLik = sapply(models, log_lik),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC)
)

summary_table

library(tidyverse)

aic_bic_df <- data.frame(
  Model = factor(names(models), levels = names(models)),
  AIC = as.numeric(aic_val),
  BIC = as.numeric(bic_val)
) |>
  pivot_longer(
    cols = c(AIC, BIC),
    names_to = "Criterion",
    values_to = "Value"
  )

# The best model under each criterion is the one with the minimum 
# AIC or BIC value
# AIC favors prediction; BIC penalizes complexity more strongly

best_models <- aic_bic_df |>
  group_by(Criterion) |>
  slice_min(Value)

ggplot(aic_bic_df, aes(x = Model, y = Value, fill = Criterion)) +
  
  geom_col(width = 0.65, alpha = 0.85) +
  
  geom_text(
    aes(label = round(Value, 1)),
    vjust = -0.4,
    size = 3.8,
    fontface = "bold"
  ) +
  
  geom_col(
    data = best_models,
    aes(x = Model, y = Value),
    width = 0.65,
    fill = "gold",
    alpha = 0.9
  ) +
  
  facet_wrap(~ Criterion, nrow = 2, scales = "free_y") +
  
  labs(
    title = "AIC and BIC Comparison Across Candidate Models",
    subtitle = "Lower values indicate better model fit with complexity penalty",
    y = "Information Criterion Value",
    x = NULL
  ) +
  
  scale_fill_manual(values = c("AIC" = "#D55E00", "BIC" = "#0072B2")) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "none",
    axis.text.x = element_text(face = "bold")
  )



# Task-2.5: Residual Analysis 

# Residual diagnostics assess normality (Q-Q plot) and 
# distributional symmetry (histogram)
# Deviations indicate possible model misspecification

par(mfrow = c(3,2)) 
for(i in 1: length(models)){
  model <-models[[i]] 
  res <- resid(model) 
  
  # Histogram 
  hist(res, main=paste("Residuals of", names(models)[i]),
       xlab = "Residuals", col= "skyblue", breaks = 40)
  
  #Q-Q plot 
  qqnorm(res, main= paste("Q-Q Plot of", names(models)[i]))
  qqline(res, col = "red", lwd = 2)
} 
par(mfrow = c(1,1))



# TASK-2.7 : Train/Test Split, Prediction and Confidence Intervals

#Splitting the data into training and testing sets of 70/30
set.seed(123)
n <- nrow(scaled_data_csv)
train_index <- sample(1:n, size = 0.7 * n)
train_data  <- scaled_data_csv[train_index, ]
test_data   <- scaled_data_csv[-train_index, ]

# fitting best model on training data
best_model <- lm(
  y ~ x1 + x2 + x3 + x4 + x5 + x6 + I(x1*x2) + I(x3*x4) + I(x2*x6),
  data = train_data
)
summary(best_model)


#Predicting output on testing data
pred_test <- predict(best_model, newdata = test_data)

# Computing 95% prediction confidence intervals

pred_intervals <- predict(
  best_model,
  newdata = test_data,
  # Prediction intervals include both model uncertainty and observation 
  # noise, so they are wider than confidence intervals for the mean
  interval = "prediction",
  level = 0.95
)
plot_data <- data.frame(
  actual = test_data$y,
  predicted = pred_intervals[, "fit"],
  lower = pred_intervals[, "lwr"],
  upper = pred_intervals[, "upr"]
)
#PLotting predictions, prediction intervals & testing data
ggplot(plot_data, aes(x = actual, y = predicted)) +
  
  # Prediction interval ribbon (much cleaner than error bars)
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = "lightblue",
    alpha = 0.3
  ) +
  
  # Predicted points
  geom_point(
    color = "blue",
    alpha = 0.4,
    size = 1.3
  ) +
  
  # Ideal 45-degree line
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  
  labs(
    title = "Model Predictions with 95% Prediction Intervals",
    subtitle = "Test dataset (70/30 split)",
    x = "Observed Values (Test Data)",
    y = "Predicted Values"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

error_df <- data.frame(
  Model = factor(names(models), levels = names(models)),
  Sigma = sapply(models, sigma_model)
)

ggplot(error_df, aes(x = Model, y = Sigma)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(
    aes(ymin = Sigma - 0.1 * Sigma,
        ymax = Sigma + 0.1 * Sigma),
    width = 0.2,
    alpha = 0.6
  ) +
  labs(
    title = "Model Error Comparison Using Residual Standard Deviation",
    x = "Model",
    y = "Residual Standard Deviation (σ)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )


#TASK-3 : Approximate Bayesian Computation(Rejection ABC)
# TASK-3 : Approximate Bayesian Computation (Rejection ABC)

y_obs <- scaled_data_csv$y

# Define RSS threshold (5% of total variance)

# The rejection threshold is set to 5% of total variance in y
# If this value is too small, very few or no samples may be accepted
# by the ABC algorithm

threshold <- 0.05 * sum((y_obs - mean(y_obs))^2)

# Identify top 2 parameters by absolute LS estimates
coef_ls <- coef(best_model)
abs_coef <- abs(coef_ls[names(coef_ls) != "(Intercept)"])

# Only the two most influential parameters (by absolute 
# least-squares estimate) are inferred using ABC
# All other parameters are fixed

top2_names <- names(sort(abs_coef, decreasing = TRUE))[1:2]
theta_hat <- coef_ls[top2_names]

# Define prior ranges (±30%)
prior_range <- 0.3
p1_vals <- theta_hat[1] * c(1 - prior_range, 1 + prior_range)
p2_vals <- theta_hat[2] * c(1 - prior_range, 1 + prior_range)

# Fixed parameters
fixed_params <- coef_ls
fixed_params[top2_names] <- NA

# Simulation function
simulate_abc <- function(t1, t2, data, model, names_top) {
  beta <- fixed_params
  beta[names_top[1]] <- t1
  beta[names_top[2]] <- t2
  X <- model.matrix(model, data)
  as.vector(X %*% beta)
}

# Rejection ABC
set.seed(123)
N <- 5000
accepted_list <- vector("list", N)
count <- 0

for (i in 1:N) {
  t1_trial <- runif(1, min(p1_vals), max(p1_vals))
  t2_trial <- runif(1, min(p2_vals), max(p2_vals))
  
  y_sim <- simulate_abc(t1_trial, t2_trial, scaled_data_csv, 
                        best_model, top2_names)
  rss_val <- sum((y_obs - y_sim)^2)
  
  if (!is.na(rss_val) && rss_val < threshold) {
    count <- count + 1
    accepted_list[[count]] <- c(theta1 = t1_trial, theta2 = t2_trial)
  }
}

accepted <- do.call(rbind, accepted_list[1:count])
accepted <- as.data.frame(accepted)


cat("Number of accepted points:", nrow(accepted), "\n")

# Optional: sample max 200 points for plotting
if(nrow(accepted) > 0){
  set.seed(123)
  accepted_sample <- accepted[sample(1:nrow(accepted), 
                                     min(200, nrow(accepted))), ]
  
  # Joint posterior plot with optional density
  
  # This plot approximates the joint posterior distribution 
  # obtained via rejection ABC
  # Density contours indicate regions of high posterior mass
  ggplot(accepted_sample, aes(x = theta1, y = theta2)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_density_2d(color = "darkred", alpha = 0.5) +
    labs(
      title = "Joint Posterior Distribution (Rejection ABC)",
      subtitle = paste("Threshold RSS =", round(threshold, 2),
                       "| Accepted points =", nrow(accepted)),
      x = top2_names[1],
      y = top2_names[2]
    ) +
    theme_minimal(base_size = 13)
}
