source("Files2Overleaf/GGSAVE.R")
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


# Using different values
E_0 <- 1.01
E_max <- 6.000000
C <- seq(1, 1000, 1)
C50 <- 30
S <- 0.030003
C_0 <- 2.436826
n <- 1
m <- 0.866232

Sig <- Sigmoid_Emax(E_0, E_max, C, C50, n) # Sig står for Sigmoid
Simp <- Simple_model(E_0, S, C) # Simp står for simple
LogLin <- Log_linear_model(m, C, C_0) # LogLin står for log linear. 

df <- data.frame(C, Sig, Simp, LogLin)


ModelPlot <- ggplot(data = df, aes(x = log10(C))) + 
  geom_line(aes(y = Sig, color = "Sigmoid Emax"), size = 1) + 
  ylab("Effect") + 
  xlab("ln(Concentration (mg/L))") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text.y = element_blank(), # Remove y-axis text
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    axis.text.x = element_text(size = 15), # Keep the x-axis text
    legend.title = element_blank(), # Optionally removes the legend title
    legend.text = element_text(size = 15), # Adjusts legend text size
    legend.position = "bottom" # Places legend at the bottom
  ) + 
  scale_color_manual(values = c("Sigmoid Emax" = "#001965")) +
  geom_segment(aes(x = log10(1), xend = log10(max(C)), y = 0, yend = 0), color = "black", size = 0.5) +
  geom_segment(aes(x = log10(1), xend = log10(1), y = 0, yend = max(df$Sig) + 1), color = "black", size = 0.5) +
  coord_cartesian(xlim = c(-0.1, log10(max(df$C) - 1)), ylim = c(-0.5, max(df$Sig) + 1)) +
  scale_x_continuous(
    breaks = log10(c(1, 10, 100, 1000)),
    labels = c("", "", "", "")
  ) +
  geom_text(aes(x = -0.1, y = E_0 -0.15, label = "E[0]"), parse = TRUE, size = 7, vjust = 0) + 
  geom_text(aes(x = log10(C50), y = -0.7, label = "C[50]"), parse = TRUE, size = 7, vjust = 0) + 
  geom_text(aes(x = -0.14, y = E_max + E_0 -0.15, label = "E[max]"), parse = TRUE, size = 7, vjust = 0) +
  geom_segment(aes(x = log10(1), xend = log10(1000), y = E_0, yend = E_0), linetype = "dashed", color = "black", size = 0.5) +
  geom_segment(aes(x = log10(1), xend = log10(1000), y = E_max + E_0, yend = E_max + E_0), linetype = "dashed", color = "black", size = 0.5)
SavePlotPD("ModelPlots.pdf", ModelPlot)




# Standard effekt. 
effect <- function(E_max, M, K_d) {
  E <- (E_max * M) / (M + K_d)
  return(E)
}

E_max <- 5
M <- seq(1, 100, 1)
K_d <- 10

e <- effect(E_max, M, K_d)
df2 <- data.frame(M, e)

ModelPlotLogCon <- ggplot(data = df2, aes(x = log10(M), y = e)) +
  geom_line(color = "#001965", size = 1) +
  ylab("Effect") + 
  xlab("ln(Concentration (mg/L))") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 15)
  ) + 
  geom_segment(aes(x = log10(1), xend = log10(max(M)), y = 0, yend = 0), color = "black", size = 0.5) +
  geom_segment(aes(x = log10(1), xend = log10(1), y = 0, yend = max(df2$e) + 1), color = "black", size = 0.5) +
  coord_cartesian(xlim = c(-0.15, log10(max(df2$M) - 1)), ylim = c(-0.3, max(df2$e) + 1)) +
  geom_text(aes(x = -0.1, y = E_max - 0.15, label = "E_max"), size = 7, vjust = 0) +
  geom_segment(aes(x = log10(1), xend = log10(100), y = E_max, yend = E_max), linetype = "dashed", color = "black", size = 0.5) +
  geom_segment(aes(x = log10(10), xend = log10(10), y = 0, yend = df2$e[10]), linetype = "solid", color = "black", size = 0.5) +
  geom_segment(aes(x = 0, xend = log10(10), y = df2$e[10], yend = df2$e[10]), linetype = "solid", color = "black", size = 0.5) +
  geom_text(aes(x = -0.13, y = (1 / 2) * E_max - 0.15, label = "1/2 E_max"), size = 7, vjust = 0) +
  geom_text(aes(x = log10(10), y = -0.5, label = "C_50"), size = 7, vjust = 0) +
  scale_x_continuous(
    breaks = log10(c(1, 10, 100)),
    labels = c("", "", "")
  )
SavePlotPD("ModelPlotLogCon.pdf", ModelPlotLogCon)



# ModelPlotCon <- ggplot(data = df2, aes(x = M, y = e)) +
#   geom_line(color = "#001965", size = 1) +
#   ylab("Effect") + 
#   xlab("Concentration (mg/L)") +
#   theme_minimal() +
#   theme(
#     axis.title = element_text(size = 25),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.text.x = element_blank()
#   ) + 
#   geom_segment(aes(x = 0, xend = max(M), y = 0, yend = 0), color = "black", size = 0.5) +
#   geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(df2$e) + 1), color = "black", size = 0.5) +
#   coord_cartesian(xlim = c(-40, max(df2$M) - 1), ylim = c(-0.15, max(df2$e) + 1)) +
#   geom_text(aes(x = -50, y = E_max -0.15 , label = "E_max"), size = 7, vjust = 0, fontface = "plain") +
#   geom_segment(aes(x = 0, xend = 1000, y = E_max, yend = E_max), linetype = "dashed", color = "black", size = 0.5) +
#   geom_segment(aes(x = 100, xend = 100, y = 0, yend = df2$e[100]), linetype = "solid", color = "black", size = 0.5) +
#   geom_segment(aes(x = 0, xend = 100, y = df2$e[100], yend = df2$e[100]), linetype = "solid", color = "black", size = 0.5) +
#   geom_text(aes(x = -37, y = (1 / 2) * E_max -0.15, label = "C_50"), size = 7, vjust = 0, fontface = "plain") +
#   geom_text(aes(x = 100, y = -0.5, label = "k_d"), size = 7, vjust = 0, fontface = "plain")
# SavePlotPD("ModelPlotCon.pdf", ModelPlotCon)
