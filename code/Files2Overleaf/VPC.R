source("Files2Overleaf/GGSAVE.R")
library(nlmixr2)
library(tidyverse)

one_compartment_depot <- function() {
  ini({
    tka <- log(1)
    tcl <- log(3)
    tv <- log(32)
    eta.ka ~ log(2)
    eta.cl ~ log(1)
    eta.v ~ log(1)
    add.sd <- log(2)
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + log(WT) + eta.cl)
    v <- exp(tv + log(WT) + eta.v)
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - cl / v * central
    cp = central / v
    cp ~ add(add.sd)
  })
}


fit <- nlmixr(one_compartment_depot, theo_sd, "focei")
print(fit)
coef(fit)[1]
VPCplot <- vpcPlot(fit)

SavePlotPD("VPCplot2.pdf", VPCplot)
