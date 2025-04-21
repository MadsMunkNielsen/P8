source("Files2Overleaf/GGSAVE.R")
library(minpack.lm)

set.seed(123) 

# Definein the function
One_Compartment_EV <- function(A_0, K, K_a, F, t) {
    A_ORAL = (K_a * F * A_0) / (K_a - K) * (exp(- K * t) - exp(-K_a * t))
    return(A_ORAL)
}
# Input values
A_0 <- 5
K <- 0.28
K_a <- 1.81
F <- 0.9
V <- 40
t <- seq(0, 14, 0.01)

# Predicted values
A_ORAL_pred <- One_Compartment_EV(A_0, K, K_a, F, t)

# Additive error model
A_ORAL_add <- A_ORAL_pred + rnorm(length(t), mean = 0, sd = 0.2)

# Proportional error model
A_ORAL_prop <- A_ORAL_pred * (1 + rnorm(length(t), mean = 0, sd = 0.1))

# Exponential error model
A_ORAL_exp <- A_ORAL_pred * exp(rnorm(length(t), mean = 0, sd = 0.1))

# Combination of additive and proportional error model
A_ORAL_add_prop <- A_ORAL_pred * (1 + rnorm(length(t), mean = 0, sd = 0.1)) + rnorm(length(t), mean = 0, sd = 0.1)

# Store as data frames
df_add <- data.frame(t, A_ORAL_add, Model = A_ORAL_pred)
df_prop <- data.frame(t, A_ORAL_prop, Model = A_ORAL_pred)
df_exp <- data.frame(t,A_ORAL_exp, Model = A_ORAL_pred)
df_add_prop <- data.frame(t, A_ORAL_add_prop, Model = A_ORAL_pred)

# Fit the non linear regression models
fit_add <- nls(A_ORAL_add ~ (K_a * F * A_0) / (K_a - K) * (exp(- K * t) - exp(-K_a * t)), data = df_add, start = c(K = 0.1, A_0 = 1, K_a = 1.2), algorithm = "port")
fit_prop <- nls(A_ORAL_add ~ (K_a * F * A_0) / (K_a - K) * (exp(- K * t) - exp(-K_a * t)), data = df_prop, start = c(K = 0.1, A_0 = 1, K_a = 1.2), algorithm = "port")
fit_exp <- nls(A_ORAL_add ~ (K_a * F * A_0) / (K_a - K) * (exp(- K * t) - exp(-K_a * t)), data = df_exp, start = c(K = 0.1, A_0 = 1, K_a = 1.2), algorithm = "port")
fit_add_prop <- nls(A_ORAL_add ~ (K_a * F * A_0) / (K_a - K) * (exp(- K * t) - exp(-K_a * t)), data = df_add_prop, start = c(K = 0.1, A_0 = 1, K_a = 1.2), algorithm = "port")

# Store the values in a dataframe
predicted_df_add <- data.frame(t = df_add$t, predicted = predict(fit_add), A_ORAL_add)
predicted_df_prop <- data.frame(t = df_prop$t, predicted = predict(fit_prop), A_ORAL_prop)
predicted_df_exp <- data.frame(t = df_exp$t, predicted = predict(fit_exp), A_ORAL_exp)
predicted_df_add_prop <- data.frame(t = df_add_prop$t, predicted = predict(fit_add_prop), A_ORAL_add_prop)



# Plot 1: Add
OralPlot_add <- ggplot() + 
  geom_line(data = predicted_df_add, aes(x = t, y = predicted), color = "#001965", size = 2) + 
  geom_point(data = predicted_df_add, aes(x = t, y = A_ORAL_add), color = "#3B97DE") + 
  ylab("Concentration (mg / L)") + 
  xlab("Time (h)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_blank()) +
  geom_segment(aes(x = 0, xend = max(predicted_df_add$t), y = 0, yend = 0), color = "black", size = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(predicted_df_add$A_ORAL_add)), color = "black", size = 1) +
  coord_cartesian(xlim = c(0.4, max(predicted_df_add$t) - 1))
SavePlotRes("Concentration W. Oral Add.pdf", OralPlot_add)

# Plot 2: Prop
OralPlot_prop <- ggplot() + 
  geom_line(data = predicted_df_prop, aes(x = t, y = predicted), color = "#001965", size = 2) + 
  geom_point(data = predicted_df_prop, aes(x = t, y = A_ORAL_prop), color = "#3B97DE") + 
  ylab("Concentration (mg / L)") + 
  xlab("Time (h)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_blank()) +
  geom_segment(aes(x = 0, xend = max(predicted_df_prop$t), y = 0, yend = 0), color = "black", size = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(predicted_df_prop$A_ORAL_prop)), color = "black", size = 1) +
  coord_cartesian(xlim = c(0.4, max(predicted_df_prop$t) - 1))
SavePlotRes("Concentration W. Oral Prop.pdf", OralPlot_prop)

# Plot 3: Exp
OralPlot_exp <- ggplot() + 
  geom_line(data = predicted_df_exp, aes(x = t, y = predicted), color = "#001965", size = 2) + 
  geom_point(data = predicted_df_exp, aes(x = t, y = A_ORAL_exp), color = "#3B97DE") + 
  ylab("Concentration (mg / L)") + 
  xlab("Time (h)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_blank()) +
  geom_segment(aes(x = 0, xend = max(predicted_df_exp$t), y = 0, yend = 0), color = "black", size = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(predicted_df_exp$A_ORAL_exp)), color = "black", size = 1) +
  coord_cartesian(xlim = c(0.4, max(predicted_df_exp$t) - 1))
SavePlotRes("Concentration W. Oral Exp.pdf", OralPlot_exp)

# Plot 4: Add Prop
OralPlot_add_prop <- ggplot() + 
  geom_line(data = predicted_df_add_prop, aes(x = t, y = predicted), color = "#001965", size = 2) + 
  geom_point(data = predicted_df_add_prop, aes(x = t, y = A_ORAL_add_prop), color = "#3B97DE") + 
  ylab("Concentration (mg / L)") + 
  xlab("Time (h)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_blank()) +
  geom_segment(aes(x = 0, xend = max(predicted_df_add_prop$t), y = 0, yend = 0), color = "black", size = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(predicted_df_add_prop$A_ORAL_add_prop)), color = "black", size = 1) +
  coord_cartesian(xlim = c(0.4, max(predicted_df_add_prop$t) - 1))
  SavePlotRes("Concentration W. Oral Add Prop.pdf", OralPlot_add_prop)
