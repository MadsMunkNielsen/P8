source("Data/Models/Data.R")


two.cmt.depot.no.BSV <- function(tka_init, tq_init, tcl_init, tvc_init, tvp_init, prop.sd_init) {
  ini({
    tka <- log(tka_init)       # Absorption
    tq <- log(tq_init)        # Inter-compartmental clearance
    tcl <- log(tcl_init)       # Clearance
    tvc <- log(tvc_init)      # Volume, central
    tvp <- log(tvp_init)      # Volume, peripheral
    prop.sd <- prop.sd_init      # Residual error
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl)
    vp <- exp(tvp)
    vc <- exp(tvc)
    q <- exp(tq)
    # # ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}

two.cmt.depot.CL.Q <- function(tka_init, tq_init, tcl_init, tvc_init, tvp_init, eta.q_init, prop.sd_init) {
  ini({
    tka <- log(tka_init)       # Absorption
    tq <- log(tq_init)        # Inter-compartmental clearance
    tcl <- log(tcl_init)       # Clearance
    tvc <- log(tvc_init)      # Volume, central
    tvp <- log(tvp_init)      # Volume, peripheral
    eta.q ~ eta.q_init
    prop.sd <- prop.sd_init   # Residual error
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl + eta.q)
    vp <- exp(tvp)
    vc <- exp(tvc)
    q <- exp(tq + eta.q)
    # ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}

two.cmt.depot.V2.V3 <- function(tka_init, tq_init, tcl_init, tvc_init, tvp_init, eta.vp_init, prop.sd_init) {
  ini({
    tka <- log(tka_init)       # Absorption
    tq <- log(tq_init)        # Inter-compartmental clearance
    tcl <- log(tcl_init)       # Clearance
    tvc <- log(tvc_init)      # Volume, central
    tvp <- log(tvp_init)      # Volume, peripheral
    eta.vp ~ eta.vp_init
    prop.sd <- prop.sd_init            # Residual error
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl)
    vp <- exp(tvp + eta.vp)
    vc <- exp(tvc + eta.vp)
    q <- exp(tq)
    # ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}

two.cmt.depot.All <- function(tka_init, tq_init, tcl_init, tvc_init, tvp_init, eta.q_init, eta.vp_init, prop.sd_init) {
  ini({
    tka <- log(tka_init)       # Absorption
    tq <- log(tq_init)        # Inter-compartmental clearance
    tcl <- log(tcl_init)       # Clearance
    tvc <- log(tvc_init)      # Volume, central
    tvp <- log(tvp_init)      # Volume, peripheral
    eta.q ~ eta.q_init
    eta.vp ~ eta.vp_init
    prop.sd <- prop.sd_init   # Residual error
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl + eta.q)
    vp <- exp(tvp + eta.vp)
    vc <- exp(tvc + eta.vp)
    q <- exp(tq + eta.q)
    # ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}

two.cmt.depot.CL.Q.BW <- function(tka_init, tq_init, tcl_init, tvc_init, tvp_init, eta.q_init, prop.sd_init) {
  ini({
    tka <- log(tka_init)       # Absorption
    tq <- log(tq_init)        # Inter-compartmental clearance
    tcl <- log(tcl_init)       # Clearance
    tvc <- log(tvc_init)      # Volume, central
    tvp <- log(tvp_init)      # Volume, peripheral
    pow1 <- 1
    eta.q ~ eta.q_init
    prop.sd <- prop.sd_init   # Residual error
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl) * (BWBASE/85)^(pow1) * exp(eta.q)
    vp <- exp(tvp)
    vc <- exp(tvc)
    q <- exp(tq) * (BWBASE/85)^(pow1) * exp(eta.q)
    # ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}

two.cmt.depot.V2.V3.BW <- function(tka_init, tq_init, tcl_init, tvc_init, tvp_init, eta.vp_init, prop.sd_init) {
  ini({
    tka <- log(tka_init)       # Absorption
    tq <- log(tq_init)        # Inter-compartmental clearance
    tcl <- log(tcl_init)       # Clearance
    tvc <- log(tvc_init)      # Volume, central
    tvp <- log(tvp_init)      # Volume, peripheral
    pow1 <- 1
    eta.vp ~ eta.vp_init
    prop.sd <- prop.sd_init    # Residual error
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl)
    vp <- exp(tvp) * (BWBASE/85)^(pow1) * exp(eta.vp)
    vc <- exp(tvc) * (BWBASE/85)^(pow1) * exp(eta.vp)
    q <- exp(tq)
    # ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}

two.cmt.depot.All.BW <- function(tka_init, tq_init, tcl_init, tvc_init, tvp_init, eta.q_init, eta.vp_init, prop.sd_init) {
  ini({
    tka <- log(tka_init)       # Absorption
    tq <- log(tq_init)        # Inter-compartmental clearance
    tcl <- log(tcl_init)       # Clearance
    tvc <- log(tvc_init)      # Volume, central
    tvp <- log(tvp_init)      # Volume, peripheral
    pow1 <- 1
    pow2 <- 1
    eta.q ~ eta.q_init
    eta.vp ~ eta.vp_init
    prop.sd <- prop.sd_init    # Residual error
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl) * (BWBASE/85)^(pow1) * exp(eta.q)
    vp <- exp(tvp) * (BWBASE/85)^(pow2) * exp(eta.vp)
    vc <- exp(tvc) * (BWBASE/85)^(pow2) * exp(eta.vp)
    q <- exp(tq) * (BWBASE/85)^(pow1) * exp(eta.q)
    # ODEs
    d/dt(depot) = -ka * depot
    d/dt(central) = ka * depot - cl/vc * central + q/vp * peripheral - q/vc * central
    d/dt(peripheral) = q/vc * central - q/vp * peripheral
    conc <- central / vc
    conc ~ prop(prop.sd)
  })
}
