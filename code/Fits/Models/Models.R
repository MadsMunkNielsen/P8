source("Fits/Models/Data.R")


one.cmt.depot.All.BW <- function(tka_init, tcl_init, tvc_init, tf_init, eta.q_init, eta.vp_init, prop.sd_init, cov) {
  ini({
    tka <- log(tka_init)
    tcl <- log(tcl_init)
    tvc <- log(tvc_init)
    tf <- log(tf_init)
    pow1 <- 1
    pow2 <- 1
    eta.cl + eta.v ~ c(eta.q_init,
                        cov, eta.vp_init)
    prop.sd <- prop.sd_init
  })
  model({
    F <- exp(tf)
    ka <- exp(tka)
    cl <- exp(tcl) * (BWBASE/85)^(pow1) * exp(eta.cl)
    vc <- exp(tvc) * (BWBASE/85)^(pow2) * exp(eta.v)
    d/dt(depot) = -ka * depot
    d/dt(central) = F * ka * depot - cl / vc * central
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}

two.cmt.depot.All <- function(tka_init, tq_init, tcl_init, tvc_init, tvp_init, tf_init, eta.q_init, eta.vp_init, prop.sd_init, cov) {
  ini({
    tka <- log(tka_init)       # Absorption
    tq <- log(tq_init)        # Inter-compartmental clearance
    tcl <- log(tcl_init)       # Clearance
    tvc <- log(tvc_init)      # Volume, central
    tvp <- log(tvp_init)      # Volume, peripheral
    tf <- log(tf_init)        # Bioavailability
    eta.q + eta.vp ~ c(eta.q_init,
                        cov, eta.vp_init)
    prop.sd <- prop.sd_init   # Residual error
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl + eta.q)
    vp <- exp(tvp + eta.vp)
    vc <- exp(tvc + eta.vp)
    q <- exp(tq + eta.q)
    F <- exp(tf)
    # ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = F * ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}

two.cmt.depot.All.BW <- function(tka_init, tq_init, tcl_init, tvc_init, tvp_init, tf_init, eta.q_init, eta.vp_init, prop.sd_init, cov) {
  ini({
    tka <- log(tka_init)       # Absorption
    tq <- log(tq_init)        # Inter-compartmental clearance
    tcl <- log(tcl_init)       # Clearance
    tvc <- log(tvc_init)      # Volume, central
    tvp <- log(tvp_init)      # Volume, peripheral
    tf <- log(tf_init)        # Bioavailability
    pow1 <- 1
    pow2 <- 1
    eta.q + eta.vp ~ c(eta.q_init,
                        cov, eta.vp_init)
    prop.sd <- prop.sd_init    # Residual error
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl) * (BWBASE/85)^(pow1) * exp(eta.q)
    vp <- exp(tvp) * (BWBASE/85)^(pow2) * exp(eta.vp)
    vc <- exp(tvc) * (BWBASE/85)^(pow2) * exp(eta.vp)
    q <- exp(tq) * (BWBASE/85)^(pow1) * exp(eta.q)
    F <- exp(tf)
    # ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = F * ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}