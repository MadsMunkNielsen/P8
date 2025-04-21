# Source the data
source("Data/Data.R")

# Two compartment model without ETA
two.cmt.depot.no.BSV <- function() {
  ini({
    tka <- log(1.3)       # Absorption
    tq <- log(2.7)        # Inter-compartmental clearance
    tcl <- log(2.2)       # Clearance
    tvc <- log(30)      # Volume, central
    tvp <- log(60)      # Volume, peripheral
    tf <- log(1.8)      # Bioavailability
    # Residual error
    add.sd <- 0.1
    })
  model({
    # Model for theta
    ka <- exp(tka)
    cl <- exp(tcl)
    vp <- exp(tvp)
    vc <- exp(tvc)
    q <- exp(tq)
    F <- exp(tf)
    # Specify system of ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = F * ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ add(add.sd)
    })
}


# Fit model on 10, 25, 50, and 100 profiles
# CP
for (i in c(10, 25, 50, 100)) {
  variable_name <- paste0("NoBSV_CP_", i)
  assign(variable_name, nlmixr(two.cmt.depot.no.BSV, subsetTrue[subsetTrue$ID >= 1 & subsetTrue$ID <= i, ], est = "focei"))
}


# DV
for (i in c(10, 25, 50, 100)) {
  variable_name <- paste0("NoBSV_DV_", i)
  assign(variable_name, nlmixr(two.cmt.depot.no.BSV, subsetNoise[subsetNoise$ID >= 1 & subsetNoise$ID <= i, ], est = "focei"))
}