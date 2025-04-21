# Define the models
Sigmoid_Emax <- function(E_0, E_max, C, C50, n) {
    E <- E_0 + (E_max * C^n) / (C^n + C50^n)
    return(E)
}
Simple_model <- function(E_0, S, C) {
    E <- E_0 + S * C
    return(E)
}
Log_linear_model <- function(m, C, C_0) {
    E <- m * log(C + C_0)
    return(E)
}

# Loss function to minimize the differences at C=1 and a target C
loss_function <- function(params, E_0, C_values, C50, n) {
    E_max <- params[1]
    S <- params[2]
    C_0 <- params[3]
    m <- params[4]
    
    # Outputs at C = 1
    output_sigmoid_1 <- Sigmoid_Emax(E_0, E_max, 1, C50, n)
    output_simple_1 <- Simple_model(E_0, S, 1)
    output_log_linear_1 <- Log_linear_model(m, 1, C_0)
    
    # Calculate the differences at C = 1
    loss_1 <- (output_sigmoid_1 - output_simple_1)^2 + (output_sigmoid_1 - output_log_linear_1)^2
    
    # Choose a C in the range [6, 15]
    target_C <- C_values
    output_sigmoid_target <- Sigmoid_Emax(E_0, E_max, target_C, C50, n)
    output_simple_target <- Simple_model(E_0, S, target_C)
    output_log_linear_target <- Log_linear_model(m, target_C, C_0)
    
    # Calculate the differences at the target C
    loss_target <- (output_sigmoid_target - output_simple_target)^2 + 
                   (output_sigmoid_target - output_log_linear_target)^2
    
    # Return the total loss
    return(loss_1 + loss_target)
}

# Initial parameters
E_0 <- 1.01
C50 <- 100
n <- 1

C_values <- 100 

# Starting values for E_max, S, C_0, and m
initial_params <- c(E_max = 6, S = 0.1473901, C_0 = 4.0247337, m = 1.6941318)

# Set lower bounds for parameters
lower_bounds <- c(6, -Inf, -Inf, -Inf)  # E_max >= 6, no lower limits for S, C_0, and m

# Run optimization with lower bounds
optimized_results <- optim(
    par = initial_params,
    fn = loss_function,
    E_0 = E_0,
    C_values = C_values,
    C50 = C50,
    n = n,
    method = "L-BFGS-B",
    lower = lower_bounds
)

# Extract optimized parameters
optimized_E_max <- optimized_results$par[1]
optimized_S <- optimized_results$par[2]
optimized_C_0 <- optimized_results$par[3]
optimized_m <- optimized_results$par[4]

# Print results
cat(sprintf("Optimized E_max: %f\n", optimized_E_max))
cat(sprintf("Optimized S: %f\n", optimized_S))
cat(sprintf("Optimized C_0: %f\n", optimized_C_0))
cat(sprintf("Optimized m: %f\n", optimized_m))

# Verify if models return the same value at C=1 and at the chosen value in [6, 15] with optimized parameters
C1 <- 1
E_sigmoid_C1 <- Sigmoid_Emax(E_0, optimized_E_max, C1, C50, n)
E_simple_C1 <- Simple_model(E_0, optimized_S, C1)
E_log_linear_C1 <- Log_linear_model(optimized_m, C1, optimized_C_0)

E_sigmoid_target <- Sigmoid_Emax(E_0, optimized_E_max, C_values, C50, n)
E_simple_target <- Simple_model(E_0, optimized_S, C_values)
E_log_linear_target <- Log_linear_model(optimized_m, C_values, optimized_C_0)

cat(sprintf("E at C=1 for Optimized Sigmoid Emax: %f\n", E_sigmoid_C1))
cat(sprintf("E at C=1 for Optimized Simple Model: %f\n", E_simple_C1))
cat(sprintf("E at C=1 for Optimized Log-linear Model: %f\n", E_log_linear_C1))

cat(sprintf("E at C=%f for Optimized Sigmoid Emax: %f\n", C_values, E_sigmoid_target))
cat(sprintf("E at C=%f for Optimized Simple Model: %f\n", C_values, E_simple_target))
cat(sprintf("E at C=%f for Optimized Log-linear Model: %f\n", C_values, E_log_linear_target))