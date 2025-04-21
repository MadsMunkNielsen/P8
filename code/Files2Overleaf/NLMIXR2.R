# Including the package
library(nlmixr2)
library(broom)
library(ggplot2)

# Viewing the data
str(theo_sd)
View(theo_sd)

# The PK-model
one.cmt <- function() {
  ini({
    ## You may label each parameter with a comment
    tka <- 0.45 # Log Ka
    tcl <- log(c(0, 2.7, 100)) # Log Cl
    ## This works with interactive models
    ## You may also label the preceding line with label("label text")
    tv <- 3.45; label("log V")
    ## the label("Label name") works with all models
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd)
  })
}

fit <- nlmixr(one.cmt, theo_sd, est="saem",
              control=list(print=0))

print(fit)
tidy(fit)
summary(fit)

df <- fit[1:11, ]
# Plotting the result
ggplot(df, aes(x = TIME)) +
  geom_line(aes(y = PRED, color = "PRED")) +
  geom_line(aes(y = DV, color = "DV")) +
  labs(y = "Values", color = "Legend") +
  theme_minimal()



# Fitting a model where the ODEs are specified
one.compartment <- function() {
  ini({
    tka <- 0.45 # Log Ka
    tcl <- 1 # Log Cl
    tv <- 3.45    # Log V
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl / v * center
    cp = center / v # Concentration in the central compartment
    cp ~ add(add.sd)
  })
}

fit_one <- nlmixr(one.compartment, theo_sd, est = "focei")
print(fit_one)
tidy(fit_one)




# Fitting a PKPD model
model <- function() {
  ini({
    tcl <- log(0.1) # log CL (L/hr)
    tv <- log(8) # log Vc (L)
    eta.cl ~ 0.1
    eps.pkprop <- 0.1
    tc50 <- log(1) #log ec50 (mg/L)
    tkout <- log(0.05) #log tkout (/h)
    e0 <- 100 #e0
    eta.c50 ~ .5
    eps.pdadd <- 100})
  model({
    cl <- exp(tcl + eta.cl)
    v <- exp(tv)
    c50 = exp(tc50 + eta.c50)
    kout = exp(tkout)
    cp = center / v
    d/dt(center) = - cl * cp
    effect(0) = e0
    kin = e0*kout
    PD = 1-cp/(c50+cp)
    d/dt(effect) = kin*PD -kout*effect
#specify dvid ("center","effect") in data
    cp ~ prop(eps.pkprop) | center
    effect ~ add(eps.pdadd) | effect })}

model_fit <- nlmixr(model, theo_sd, est = "focei")
print(model_fit)

# Goodness of fit 
library(xpose.nlmixr2)
install.packages("xpose.nlmixr2")
install.packages("polyclip") # Bjane skal installere plyclip