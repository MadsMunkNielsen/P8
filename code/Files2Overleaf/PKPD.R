source("Files2Overleaf/GGSAVE.R")
source("Files2Overleaf/ExposureModels.R")


One_Compartment_EV_PK_PD <- function(C, K_e0, K_e1, t) {
    PD = (K_e1 * C) / (K_e1 - K_e0) * (exp(- K_e0 * t) - exp(-K_e1 * t))
    return(PD)
}

C <- my.df$C_One_Compartment_EV
K_e0 <- 0.05
K_e1 <- 0.1
t <- my.df$t

Concentration_Effect_Comp <- One_Compartment_EV_PK_PD(C, K_e0, K_e1, t)


effect <- function(E_max, M, K_d) {
  E <- (E_max * M) / (M + K_d)
  return(E)
}

E_max <- 20
K_d <- 5

Effect <- effect(E_max, Concentration_Effect_Comp, K_d)


EffectDF <- data.frame(my.df$t, my.df$C_One_Compartment_EV, Concentration_Effect_Comp, Effect)
colnames(EffectDF) <- c("t", "Concentration", "Concentration Effect Compartment", "Effect")



PKPDModel <- ggplot(data = EffectDF, aes(x = t)) + 
  geom_line(aes(y = Concentration, color = "Concentration"), size = 1) + 
  geom_line(aes(y = Effect, color = "Effect"), size = 1) + 
  ylab("Concentration (mg / L)") + 
  xlab("Time (h)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text.y = element_blank(), # Remove y-axis text
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(), # Remove y-axis ticks
    axis.text.x = element_blank(), # Keep the x-axis text
    legend.title = element_blank(), # Optionally removes the legend title
    legend.text = element_text(size = 15), # Adjusts legend text size
    legend.position = "bottom" # Places legend at the bottom
  ) +
  scale_color_manual(values = c("Concentration" = "#001965", "Effect" = "#3B97DE")) +
  geom_segment(aes(x = 0, xend = max(EffectDF$t), y = 0, yend = 0), color = "black", size = 0.5) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(EffectDF$Concentration) + 1), color = "black", size = 0.5) +
  coord_cartesian(xlim = c(-0.1, (max(EffectDF$t) - 1)), ylim = c(0, max(EffectDF$Concentration)))
SavePlotPKPD("PK-PDmodel.pdf", PKPDModel)


PKPDModel <- ggplot(data = EffectDF, aes(x = t)) + 
  geom_line(aes(y = Concentration, color = "Concentration"), size = 1) + 
  geom_line(aes(y = Effect, color = "Effect"), size = 1) + # Optional: use dashed lines for distinction
  ylab("Concentration (mg / L)") + 
  xlab("Time (h)") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 25),
    axis.text.y = element_blank(), # Remove y-axis text for Concentration
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(), # Remove y-axis ticks
    axis.text.x = element_blank(), # Keep the x-axis text
    legend.title = element_blank(), # Optionally removes the legend title
    legend.text = element_text(size = 15), # Adjusts legend text size
    legend.position = "bottom", # Places legend at the bottom
    legend.key.size = unit(1.5, "cm")
  ) +
  scale_color_manual(values = c("Concentration" = "#001965", "Effect" = "#3B97DE")) +
  geom_segment(aes(x = 0, xend = max(EffectDF$t), y = 0, yend = 0), color = "black", size = 0.5) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(EffectDF$Concentration) + 1), color = "black", size = 0.5) +
  geom_segment(aes(x = max(EffectDF$t) - 0.357, xend = max(EffectDF$t) - 0.357, y = 0, yend = max(EffectDF$Concentration) + 1), color = "black", size = 0.5) +
  coord_cartesian(xlim = c(-0.1, (max(EffectDF$t) - 1)), ylim = c(0, max(EffectDF$Concentration))) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., name = "Effect", breaks = seq(0, max(EffectDF$Effect), by = 1)) # Set 'some_value' based on the range of Effect
  )

SavePlotPKPD("PK-PDmodel.pdf", PKPDModel)
