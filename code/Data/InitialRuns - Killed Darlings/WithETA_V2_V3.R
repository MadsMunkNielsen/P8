source("Data/Data.R")

# Two compartment with ETA CL and Q
two.cmt.depot.w.V2.V3 <- function() {
  ini({
    tka <- log(1.3)       # Absorption
    tq <- log(2.2)        # Inter-compartmental clearance
    tcl <- log(2.2)       # Clearance
    tvc <- log(30)      # Volume, central
    tvp <- log(60)      # Volume, peripheral
    tf <- log(2)      # Bioavailability
    # Random effects
    eta.vc ~ 1
    eta.vp ~ 1
    # Residual error
    prop.sd <- 0.1
  })
  model({
    # Model for theta
    ka <- exp(tka)
    cl <- exp(tcl)
    vp <- exp(tvp + eta.vp)
    vc <- exp(tvc + eta.vc)
    q <- exp(tq)
    F <- exp(tf)
    # Specify system of ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = F * ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    # Model concentration in plasma
    conc <- central / vc
    # with proportional error model
    conc ~ prop(prop.sd)
  })
}

# Fit model on 10, 25, 50, and 100 profiles
# CP
for (i in c(10, 25, 50, 100)) {
  variable_name <- paste0("V2V3_CP_", i)
  assign(variable_name, nlmixr(two.cmt.depot.w.V2.V3, subsetTrue[subsetTrue$ID >= 1 & subsetTrue$ID <= i, ], est = "focei"))
}


# DV
for (i in c(10, 25, 50, 100)) {
  variable_name <- paste0("V2V3_DV_", i)
  assign(variable_name, nlmixr(two.cmt.depot.w.V2.V3, subsetNoise[subsetNoise$ID >= 1 & subsetNoise$ID <= i, ], est = "focei"))
}