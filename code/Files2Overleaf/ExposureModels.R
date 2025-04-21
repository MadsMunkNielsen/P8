source("Files2Overleaf/GGSAVE.R")

# One compartment model IV
One_Compartment <- function(A_0, K, t) {
    A_IV = A_0 * exp(-K * t) 
    return(A_IV)
}

C_ORAL_One <- function(A_0, K, t, V) {
  C_ORAL1 = One_Compartment(A_0, K, t) / V
  return(C_ORAL1)
}

# One compartment model EV
One_Compartment_EV <- function(A_0, K, K_a, F, t) {
    A_ORAL = (K_a * F * A_0) / (K_a - K) * (exp(- K * t) - exp(-K_a * t))
    return(A_ORAL)
}

C_One_Compartment_EV <- function(A_0, K, K_a, F, t, V) {
  C_ORAL = One_Compartment_EV(A_0, K, K_a, F, t) / V
  return(C_ORAL)
}

# Store as dataframe
A_0 <- 5
K <- 0.28
K_a <- 1.81
F <- 0.9
V <- 40
t <- seq(0, 14, 0.1)

my.df <- data.frame(A_0, K, K_a, F, V, t, One_Compartment(A_0, K, t), One_Compartment_EV(A_0, K, K_a, F, t), C_ORAL_One(A_0, K, t, V), C_One_Compartment_EV(A_0, K, K_a, F, t, V))
colnames(my.df) <- c("A_0", "K", "K_a", "F", "V", "t", "One", "Two", "C_ORAL_One", "C_One_Compartment_EV")


# Plot of one compartment IV 
IVplot <- ggplot(data = my.df, aes(x = t, y = C_ORAL_One)) + 
  geom_line(color = "#001965", size = 2) + 
  ylab("Concentration (mg / L)") + 
  xlab("Time (h)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_blank()) +
  geom_segment(aes(x = 0, xend = max(t), y = 0, yend = 0), color = "black", size = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(my.df$C_ORAL_One)), color = "black", size = 1) +
  coord_cartesian(xlim = c(0.4, max(my.df$t) - 1))
SavePlot("Concentration W. IV.pdf", IVplot)

# Plot of one compartment EV
OralPlot <- ggplot(data = my.df, aes(x = t)) + 
  geom_line(aes(y = C_One_Compartment_EV), color = "#001965", size = 2) + 
  ylab("Concentration (mg / L)") + 
  xlab("Time (h)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_blank()) +
  geom_segment(aes(x = 0, xend = max(t), y = 0, yend = 0), color = "black", size = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(my.df$C_One_Compartment_EV)), color = "black", size = 1) +
  coord_cartesian(xlim = c(0.4, max(my.df$t) - 1))
SavePlot("Concentration W. Oral.pdf", OralPlot)



#Multiple dosing
dose_times <- c(0, 3, 6, 9, 12)  # Dosing every 3 hours
t <- seq(0, 20, length.out = 500)
ka <- 1.8
k <- 0.28
f <- 0.89
V <- 42
Anull <- 1000
CL <- k * V
R <- (f * Anull) / (dose_times[2] - dose_times[1])


# Steady state
Steady_State_IN <- function(R, CL, V, t) {
  C_IN = (R / CL) * (1- exp(((-CL) / V) * t))
  return(C_IN)
}

MD_single <- function(t, t_dose) {
 (ka*f*Anull) / (V * (ka - k)) * (exp(-k * (t - t_dose)) - exp(-ka * (t - t_dose)))
}

# Compute total MD by summing over all doses
MD <- sapply(t, function(t) {
  sum(sapply(dose_times, function(t_dose) {
    if (t >= t_dose) {
      MD_single(t, t_dose)
    } else {
      0
    }
  }))
})

# Create dataframe
df <- data.frame(t = t, MD = MD, Steady_State_IN(R, CL, V, t))
colnames(df) <- c("t", "MD", "IN")


CIplot <- ggplot(df, aes(x = t)) +
  geom_line(aes(y = IN, color = "IN"), size = 1.1) +
  geom_line(aes(y = MD, color = "MD"), size = 1.1) +
  labs(title = "",
       x = "Time (h)",
       y = "Concentration (mg / L)") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.text = element_blank(),
        axis.title = element_text(size = 15), 
        legend.text = element_text(size = 15)) +
  scale_color_manual(values = c("IN" = "#001965", "MD" = "#3B97DE")) +
  geom_segment(aes(x = 0, xend = max(t), y = 0, yend = 0), color = "black", size = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(c(IN, MD)) + 1.25), color = "black", size = 1) +
  coord_cartesian(xlim = c(0.8, max(t) -1)) 
SavePlot("3hoursDosing.pdf", CIplot)
