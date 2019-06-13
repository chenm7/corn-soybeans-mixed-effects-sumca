rm(list = ls())
library(car)
library(lme4)

############################################################ 
# HELPER FUNCTIONS 
############################################################ 

# Take a dataframe and return IC estimates for all 32 models for corn
subsets.corn.ICs <- function(mydata){
  # Models for corn
  fit.c.none <- lmer(hectares.corn ~ (1|county), data = mydata)
  
  fit.c.c <- lmer(hectares.corn ~ pixels.corn + (1|county), data = mydata)
  fit.c.s <- lmer(hectares.corn ~ pixels.soy + (1|county), data = mydata)
  fit.c.i <- lmer(hectares.corn ~ pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.c.c2 <- lmer(hectares.corn ~ I(pixels.corn^2) + (1|county), data = mydata)
  fit.c.s2 <- lmer(hectares.corn ~ I(pixels.soy^2) + (1|county), data = mydata)
  
  fit.c.cs <- lmer(hectares.corn ~ pixels.corn + pixels.soy + (1|county), data = mydata)
  fit.c.ci <- lmer(hectares.corn ~ pixels.corn + pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.c.si <- lmer(hectares.corn ~ pixels.soy + pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.c.cc2 <- lmer(hectares.corn ~ pixels.corn + I(pixels.corn^2) + (1|county), data = mydata)
  fit.c.cs2 <- lmer(hectares.corn ~ pixels.corn +I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.ss2 <- lmer(hectares.corn ~ pixels.soy +I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.sc2 <- lmer(hectares.corn ~ pixels.soy +I(pixels.corn^2) + (1|county), data = mydata)
  fit.c.ic2 <- lmer(hectares.corn ~ I(pixels.corn^2) + pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.c.is2 <- lmer(hectares.corn ~ I(pixels.soy^2) + pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.c.c2s2 <- lmer(hectares.corn ~ I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  
  fit.c.csi <- lmer(hectares.corn ~ pixels.corn*pixels.soy + (1|county), data = mydata)
  fit.c.csc2 <- lmer(hectares.corn ~ pixels.corn + pixels.soy + I(pixels.corn^2) + (1|county), data = mydata)
  fit.c.css2 <- lmer(hectares.corn ~ pixels.corn +pixels.soy + I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.cic2 <- lmer(hectares.corn ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.corn^2) + (1|county), data = mydata)
  fit.c.cis2 <- lmer(hectares.corn ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.cc2s2 <- lmer(hectares.corn ~ pixels.corn + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.sic2 <- lmer(hectares.corn ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.corn^2) + (1|county), data = mydata)
  fit.c.sis2 <- lmer(hectares.corn ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.sc2s2 <- lmer(hectares.corn ~ pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.ic2s2 <- lmer(hectares.corn ~ pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  
  fit.c.csc2s2 <- lmer(hectares.corn ~ pixels.corn + pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.csic2 <- lmer(hectares.corn ~ pixels.corn*pixels.soy + I(pixels.corn^2) + (1|county), data = mydata)
  fit.c.csis2 <- lmer(hectares.corn ~ pixels.corn*pixels.soy + I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.cic2s2 <- lmer(hectares.corn ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  fit.c.sic2s2 <- lmer(hectares.corn ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  
  fit.c.full <- lmer(hectares.corn ~ pixels.corn*pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  
  # get AIC, BICs
  fits.corn.names <- c("fit.c.none", "fit.c.c", "fit.c.s", "fit.c.i", "fit.c.c2", "fit.c.s2", "fit.c.cs",
                       "fit.c.ci", "fit.c.si", "fit.c.cc2", "fit.c.cs2", "fit.c.ss2", "fit.c.sc2",
                       "fit.c.ic2", "fit.c.is2", "fit.c.c2s2", "fit.c.csi", "fit.c.csc2", "fit.c.css2",
                       "fit.c.cic2", "fit.c.cis2", "fit.c.cc2s2", "fit.c.sic2", "fit.c.sis2",
                       "fit.c.sc2s2", "fit.c.ic2s2", "fit.c.csc2s2", "fit.c.csic2", "fit.c.csis2",
                       "fit.c.cic2s2", "fit.c.sic2s2", "fit.c.full")
  
  fits.IC <- data.frame(fitnames = substring(fits.corn.names, first = 7),
                        AIC.corn = rep(NA, 32),
                        BIC.corn = rep(NA, 32))
  for(i in 1:32){
    fits.IC$AIC.corn[i] <- AIC(get(fits.corn.names[i]))
    fits.IC$BIC.corn[i] <- BIC(get(fits.corn.names[i]))
  }
  
  return(fits.IC)
}

# Take a dataframe and return IC estimates for all 32 models for soy
subsets.soy.ICs <- function(mydata){
  # Models for soy
  fit.s.none <- lmer(hectares.soy ~ (1|county), data = mydata)
  
  fit.s.c <- lmer(hectares.soy ~ pixels.corn + (1|county), data = mydata)
  fit.s.s <- lmer(hectares.soy ~ pixels.soy + (1|county), data = mydata)
  fit.s.i <- lmer(hectares.soy ~ pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.s.c2 <- lmer(hectares.soy ~ I(pixels.corn^2) + (1|county), data = mydata)
  fit.s.s2 <- lmer(hectares.soy ~ I(pixels.soy^2) + (1|county), data = mydata)
  
  fit.s.cs <- lmer(hectares.soy ~ pixels.corn + pixels.soy + (1|county), data = mydata)
  fit.s.ci <- lmer(hectares.soy ~ pixels.corn + pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.s.si <- lmer(hectares.soy ~ pixels.soy + pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.s.cc2 <- lmer(hectares.soy ~ pixels.corn + I(pixels.corn^2) + (1|county), data = mydata)
  fit.s.cs2 <- lmer(hectares.soy ~ pixels.corn +I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.ss2 <- lmer(hectares.soy ~ pixels.soy +I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.sc2 <- lmer(hectares.soy ~ pixels.soy +I(pixels.corn^2) + (1|county), data = mydata)
  fit.s.ic2 <- lmer(hectares.soy ~ I(pixels.corn^2) + pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.s.is2 <- lmer(hectares.soy ~ I(pixels.soy^2) + pixels.corn:pixels.soy + (1|county), data = mydata)
  fit.s.c2s2 <- lmer(hectares.soy ~ I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  
  fit.s.csi <- lmer(hectares.soy ~ pixels.corn*pixels.soy + (1|county), data = mydata)
  fit.s.csc2 <- lmer(hectares.soy ~ pixels.corn + pixels.soy + I(pixels.corn^2) + (1|county), data = mydata)
  fit.s.css2 <- lmer(hectares.soy ~ pixels.corn +pixels.soy + I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.cic2 <- lmer(hectares.soy ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.corn^2) + (1|county), data = mydata)
  fit.s.cis2 <- lmer(hectares.soy ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.cc2s2 <- lmer(hectares.soy ~ pixels.corn + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.sic2 <- lmer(hectares.soy ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.corn^2) + (1|county), data = mydata)
  fit.s.sis2 <- lmer(hectares.soy ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.sc2s2 <- lmer(hectares.soy ~ pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.ic2s2 <- lmer(hectares.soy ~ pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  
  fit.s.csc2s2 <- lmer(hectares.soy ~ pixels.corn + pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.csic2 <- lmer(hectares.soy ~ pixels.corn*pixels.soy + I(pixels.corn^2) + (1|county), data = mydata)
  fit.s.csis2 <- lmer(hectares.soy ~ pixels.corn*pixels.soy + I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.cic2s2 <- lmer(hectares.soy ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  fit.s.sic2s2 <- lmer(hectares.soy ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  
  fit.s.full <- lmer(hectares.soy ~ pixels.corn*pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = mydata)
  
  
  fits.soy.names <- c("fit.s.none", "fit.s.c", "fit.s.s", "fit.s.i", "fit.s.c2", "fit.s.s2", "fit.s.cs",
                      "fit.s.ci", "fit.s.si", "fit.s.cc2", "fit.s.cs2", "fit.s.ss2", "fit.s.sc2",
                      "fit.s.ic2", "fit.s.is2", "fit.s.c2s2", "fit.s.csi", "fit.s.csc2", "fit.s.css2",
                      "fit.s.cic2", "fit.s.cis2", "fit.s.cc2s2", "fit.s.sic2", "fit.s.sis2",
                      "fit.s.sc2s2", "fit.s.ic2s2", "fit.s.csc2s2", "fit.s.csic2", "fit.s.csis2",
                      "fit.s.cic2s2", "fit.s.sic2s2", "fit.s.full")
  
  fits.IC <- data.frame(fitnames = substring(fits.soy.names, first = 5),
                        AIC.soy = rep(NA, 32),
                        BIC.soy = rep(NA, 32))
  for(i in 1:32){
    fits.IC$AIC.corn[i] <- AIC(get(fits.corn.names[i]))
    fits.IC$BIC.corn[i] <- BIC(get(fits.corn.names[i]))
    fits.IC$AIC.soy[i] <- AIC(get(fits.soy.names[i]))
    fits.IC$BIC.soy[i] <- BIC(get(fits.soy.names[i]))
  }
  
  return(fits.IC)
}

# Takes a dataframe and returns IC estimates for all 32 models for both corn and soy
subsets.ICs <- function(mydata){
  fits.corn.IC <- subsets.corn.ICs(mydata)
  fits.soy.IC <- subsets.soy.ICs(mydata)
  
  fits.IC <- data.frame(fitnames = fits.corn.IC$fitnames,
                        AIC.corn = rep(NA, 32),
                        BIC.corn = rep(NA, 32),
                        AIC.soy = rep(NA, 32),
                        BIC.soy = rep(NA, 32))
  for(i in 1:32){
    fits.IC$AIC.corn <- fits.corn.IC$AIC.corn
    fits.IC$BIC.corn <- fits.corn.IC$BIC.corn
    fits.IC$AIC.soy <- fits.soy.IC$AIC.soy
    fits.IC$BIC.soy <- fits.soy.IC$BIC.soy
  }
  
  return(fits.IC)
}

### Nonparametric bootstrap
# Takes dataframe, nonparametric bootstrap, fits each of the 32 models
# Returns model number of lowest AIC/BIC for corn and soy
oneBoot <- function(d){
  boot.ind <- sample(1:dim(d)[1], replace = TRUE)
  bootsamp <- d[boot.ind,]
  boot.ICs <- subsets.ICs(bootsamp)
  return(apply(boot.ICs[,-1], 2, which.min))
}

### Return ds for SUMCA for corn, use in estimating MSPE
# Requires cropdata.edit, the 32 main models, and the names vector fits.soy.names in global environment
sumca_corn = function(y.k, modelnum, Z)
{
  
  ##First use the selected model
  modelfit = get(fits.corn.names[modelnum])
  n <- dim(cropdata.edit)[1]
  X <- getME(modelfit, "X")
  m <- dim(ranef(model.temp)$`county`)[1]
  var.e = sigma(modelfit)**2
  var.s = unlist(VarCorr(modelfit))
  v.hat = var.e*diag(n)+var.s*Z%*%t(Z)
  G.hat = var.s*diag(m)
  a2.hat =  diag(G.hat - G.hat%*%t(Z)%*%solve(v.hat)%*%Z%*%G.hat)
  
  ##Select the model based on y.k
  
  mydata <- cropdata.edit
  mydata$hectares.corn <- y.k
  sim.corn.IC <- subsets.corn.ICs(mydata)
  sim.corn.IC <- data.frame(simnames = sim.corn.names, AIC = rep(NA, length(sim.corn.names)), BIC = rep(NA, length(sim.corn.names)))
  
  modelsim.AIC = get(fits.corn.names[which(sim.corn.IC$AIC == min(sim.corn.IC$AIC))])
  modelsim.BIC = get(fits.corn.names[which(sim.corn.IC$BIC == min(sim.corn.IC$BIC))])
  
  var.e.k.AIC = sigma(modelsim.AIC)**2
  var.s.k.AIC = unlist(VarCorr(modelsim.AIC))
  v.hat.k.AIC = var.e.k.AIC*diag(n)+var.s.k.AIC*Z%*%t(Z)
  G.hat.k.AIC = var.s.k.AIC*diag(m)
  a2.sim.AIC =  diag(G.hat.k.AIC - G.hat.k.AIC%*%t(Z)%*%solve(v.hat.k.AIC)%*%Z%*%G.hat.k.AIC)
  
  d.AIC = a2.hat - a2.sim.AIC
  
  var.e.k.BIC = sigma(modelsim.BIC)**2
  var.s.k.BIC = unlist(VarCorr(modelsim.BIC))
  v.hat.k.BIC = var.e.k.BIC*diag(n)+var.s.k.BIC*Z%*%t(Z)
  G.hat.k.BIC = var.s.k.BIC*diag(m)
  a2.sim.BIC =  diag(G.hat.k.BIC - G.hat.k.BIC%*%t(Z)%*%solve(v.hat.k.BIC)%*%Z%*%G.hat.k.BIC)
  
  d.BIC = a2.hat - a2.sim.BIC
  
  final <- c(d.AIC, d.BIC)
  names(final) <- c(rep("AIC", 12), rep("BIC", 12))
  return(final)
  
}


### Return ds for SUMCA for soy, use in estimating MSPE
# Requires cropdata.edit, the 32 main models, and the names vector fits.soy.names in global environment
sumca_soy = function(y.k, modelnum, Z){
  ##First use the selected model
  modelfit = get(fits.soy.names[modelnum])
  n <- dim(cropdata.edit)[1]
  X <- getME(modelfit, "X")
  m <- dim(ranef(model.temp)$`county`)[1]
  var.e = sigma(modelfit)**2
  var.s = unlist(VarCorr(modelfit))
  v.hat = var.e*diag(n)+var.s*Z%*%t(Z)
  G.hat = var.s*diag(m)
  a2.hat =  diag(G.hat - G.hat%*%t(Z)%*%solve(v.hat)%*%Z%*%G.hat)
  
  ##Select the model based on y.k
  mydata <- cropdata.edit
  mydata$hectares.soy <- y.k
  sim.soy.IC <- subsets.soy.ICs(mydata)
  
  modelsim.AIC = get(fits.soy.names[which(fits.soy.IC$AIC == min(fits.soy.IC$AIC))])
  modelsim.BIC = get(fits.soy.names[which(fits.soy.IC$BIC == min(fits.soy.IC$BIC))])
  
  var.e.k.AIC = sigma(modelsim.AIC)**2
  var.s.k.AIC = unlist(VarCorr(modelsim.AIC))
  v.hat.k.AIC = var.e.k.AIC*diag(n)+var.s.k.AIC*Z%*%t(Z)
  G.hat.k.AIC = var.s.k.AIC*diag(m)
  a2.sim.AIC =  diag(G.hat.k.AIC - G.hat.k.AIC%*%t(Z)%*%solve(v.hat.k.AIC)%*%Z%*%G.hat.k.AIC)
  
  d.AIC = a2.hat - a2.sim.AIC
  
  var.e.k.BIC = sigma(modelsim.BIC)**2
  var.s.k.BIC = unlist(VarCorr(modelsim.BIC))
  v.hat.k.BIC = var.e.k.BIC*diag(n)+var.s.k.BIC*Z%*%t(Z)
  G.hat.k.BIC = var.s.k.BIC*diag(m)
  a2.sim.BIC =  diag(G.hat.k.BIC - G.hat.k.BIC%*%t(Z)%*%solve(v.hat.k.BIC)%*%Z%*%G.hat.k.BIC)
  
  d.BIC = a2.hat - a2.sim.BIC
  
  final <- c(d.AIC, d.BIC)
  names(final) <- c(rep("AIC", 12), rep("BIC", 12))
  return(final)
  
}

############################################################################

# Data prep
cropdata <- read.csv("cropareas.csv", head = T)
cropdata$county <- factor(cropdata$county, levels=c("Cerro Gordo","Hamilton","Worth","Humboldt","Franklin","Pocahontas","Winnebago","Wright","Webster","Hancock","Kossuth","Hardin"))
cropdata.edit <- subset(cropdata, !(county == "Hardin" & samp.segs == 2))

# Hectares vs. Pixels in segments plots
plot(hectares.corn ~ pixels.corn, data = cropdata, col = cropdata$county, main = "Hectares vs. Pixels of Corn, by Segment",ylab="hectares in segment",xlab="pixels in segment",pch=16)
legend("topleft", legend = unique(cropdata$county), pch = 16, col = unique(cropdata$county))
points(340,88.59,pch="x")

plot(hectares.soy ~ pixels.soy, data = cropdata, col = cropdata$county, xlab = "pixels in segment", ylab = "hectares in segment", main = "Hectares vs. Pixels of Soy, by Segment",pch=16)
legend("topleft", legend = unique(cropdata$county), pch = 16, col = unique(cropdata$county))
points(87,29.46,pch="x")


############## IC SELECTION ##############

### Models and IC selection for corn ###
fit.corn.none <- lmer(hectares.corn ~ (1|county), data = cropdata.edit)

fit.corn.c <- lmer(hectares.corn ~ pixels.corn + (1|county), data = cropdata.edit)
fit.corn.s <- lmer(hectares.corn ~ pixels.soy + (1|county), data = cropdata.edit)
fit.corn.i <- lmer(hectares.corn ~ pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.corn.c2 <- lmer(hectares.corn ~ I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.corn.s2 <- lmer(hectares.corn ~ I(pixels.soy^2) + (1|county), data = cropdata.edit)

fit.corn.cs <- lmer(hectares.corn ~ pixels.corn + pixels.soy + (1|county), data = cropdata.edit)
fit.corn.ci <- lmer(hectares.corn ~ pixels.corn + pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.corn.si <- lmer(hectares.corn ~ pixels.soy + pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.corn.cc2 <- lmer(hectares.corn ~ pixels.corn + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.corn.cs2 <- lmer(hectares.corn ~ pixels.corn +I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.ss2 <- lmer(hectares.corn ~ pixels.soy +I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.sc2 <- lmer(hectares.corn ~ pixels.soy +I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.corn.ic2 <- lmer(hectares.corn ~ I(pixels.corn^2) + pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.corn.is2 <- lmer(hectares.corn ~ I(pixels.soy^2) + pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.corn.c2s2 <- lmer(hectares.corn ~ I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)

fit.corn.csi <- lmer(hectares.corn ~ pixels.corn*pixels.soy + (1|county), data = cropdata.edit)
fit.corn.csc2 <- lmer(hectares.corn ~ pixels.corn + pixels.soy + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.corn.css2 <- lmer(hectares.corn ~ pixels.corn +pixels.soy + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.cic2 <- lmer(hectares.corn ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.corn.cis2 <- lmer(hectares.corn ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.cc2s2 <- lmer(hectares.corn ~ pixels.corn + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.sic2 <- lmer(hectares.corn ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.corn.sis2 <- lmer(hectares.corn ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.sc2s2 <- lmer(hectares.corn ~ pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.ic2s2 <- lmer(hectares.corn ~ pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)

fit.corn.csc2s2 <- lmer(hectares.corn ~ pixels.corn + pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.csic2 <- lmer(hectares.corn ~ pixels.corn*pixels.soy + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.corn.csis2 <- lmer(hectares.corn ~ pixels.corn*pixels.soy + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.cic2s2 <- lmer(hectares.corn ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.corn.sic2s2 <- lmer(hectares.corn ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)

fit.corn.full <- lmer(hectares.corn ~ pixels.corn*pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)

fits.corn.names <- c("fit.corn.none", "fit.corn.c", "fit.corn.s", "fit.corn.i", "fit.corn.c2", "fit.corn.s2", "fit.corn.cs",
                     "fit.corn.ci", "fit.corn.si", "fit.corn.cc2", "fit.corn.cs2", "fit.corn.ss2", "fit.corn.sc2",
                     "fit.corn.ic2", "fit.corn.is2", "fit.corn.c2s2", "fit.corn.csi", "fit.corn.csc2", "fit.corn.css2",
                     "fit.corn.cic2", "fit.corn.cis2", "fit.corn.cc2s2", "fit.corn.sic2", "fit.corn.sis2",
                     "fit.corn.sc2s2", "fit.corn.ic2s2", "fit.corn.csc2s2", "fit.corn.csic2", "fit.corn.csis2",
                     "fit.corn.cic2s2", "fit.corn.sic2s2", "fit.corn.full")

fits.corn.IC <- data.frame(fitnames = fits.corn.names, AIC = rep(NA, length(fits.corn.names)), BIC = rep(NA, length(fits.corn.names)))
for(i in 1:length(fits.corn.names)){
  fits.corn.IC$AIC[i] <- AIC(get(fits.corn.names[i]))
  fits.corn.IC$BIC[i] <- BIC(get(fits.corn.names[i]))
  fits.corn.IC$BICmin[i] <- AIC(get(fits.corn.names[i]),k=log(12))
}

fits.corn.IC
which(fits.corn.IC$BICmin==min(fits.corn.IC$BICmin))

### Models and IC selection for soy ###
fit.soy.none <- lmer(hectares.soy ~ (1|county), data = cropdata.edit)

fit.soy.c <- lmer(hectares.soy ~ pixels.corn + (1|county), data = cropdata.edit)
fit.soy.s <- lmer(hectares.soy ~ pixels.soy + (1|county), data = cropdata.edit)
fit.soy.i <- lmer(hectares.soy ~ pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.soy.c2 <- lmer(hectares.soy ~ I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.soy.s2 <- lmer(hectares.soy ~ I(pixels.soy^2) + (1|county), data = cropdata.edit)

fit.soy.cs <- lmer(hectares.soy ~ pixels.corn + pixels.soy + (1|county), data = cropdata.edit)
fit.soy.ci <- lmer(hectares.soy ~ pixels.corn + pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.soy.si <- lmer(hectares.soy ~ pixels.soy + pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.soy.cc2 <- lmer(hectares.soy ~ pixels.corn + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.soy.cs2 <- lmer(hectares.soy ~ pixels.corn +I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.ss2 <- lmer(hectares.soy ~ pixels.soy +I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.sc2 <- lmer(hectares.soy ~ pixels.soy +I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.soy.ic2 <- lmer(hectares.soy ~ I(pixels.corn^2) + pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.soy.is2 <- lmer(hectares.soy ~ I(pixels.soy^2) + pixels.corn:pixels.soy + (1|county), data = cropdata.edit)
fit.soy.c2s2 <- lmer(hectares.soy ~ I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)

fit.soy.csi <- lmer(hectares.soy ~ pixels.corn*pixels.soy + (1|county), data = cropdata.edit)
fit.soy.csc2 <- lmer(hectares.soy ~ pixels.corn + pixels.soy + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.soy.css2 <- lmer(hectares.soy ~ pixels.corn +pixels.soy + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.cic2 <- lmer(hectares.soy ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.soy.cis2 <- lmer(hectares.soy ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.cc2s2 <- lmer(hectares.soy ~ pixels.corn + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.sic2 <- lmer(hectares.soy ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.soy.sis2 <- lmer(hectares.soy ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.sc2s2 <- lmer(hectares.soy ~ pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.ic2s2 <- lmer(hectares.soy ~ pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)

fit.soy.csc2s2 <- lmer(hectares.soy ~ pixels.corn + pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.csic2 <- lmer(hectares.soy ~ pixels.corn*pixels.soy + I(pixels.corn^2) + (1|county), data = cropdata.edit)
fit.soy.csis2 <- lmer(hectares.soy ~ pixels.corn*pixels.soy + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.cic2s2 <- lmer(hectares.soy ~ pixels.corn + pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)
fit.soy.sic2s2 <- lmer(hectares.soy ~ pixels.soy + pixels.corn:pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)

fit.soy.full <- lmer(hectares.soy ~ pixels.corn*pixels.soy + I(pixels.corn^2) + I(pixels.soy^2) + (1|county), data = cropdata.edit)

fits.soy.names <- c("fit.soy.none", "fit.soy.c", "fit.soy.s", "fit.soy.i", "fit.soy.c2", "fit.soy.s2", "fit.soy.cs",
                    "fit.soy.ci", "fit.soy.si", "fit.soy.cc2", "fit.soy.cs2", "fit.soy.ss2", "fit.soy.sc2",
                    "fit.soy.ic2", "fit.soy.is2", "fit.soy.c2s2", "fit.soy.csi", "fit.soy.csc2", "fit.soy.css2",
                    "fit.soy.cic2", "fit.soy.cis2", "fit.soy.cc2s2", "fit.soy.sic2", "fit.soy.sis2",
                    "fit.soy.sc2s2", "fit.soy.ic2s2", "fit.soy.csc2s2", "fit.soy.csic2", "fit.soy.csis2",
                    "fit.soy.cic2s2", "fit.soy.sic2s2", "fit.soy.full")

fits.soy.IC <- data.frame(fitnames = fits.soy.names, AIC = rep(NA, length(fits.soy.names)), BIC = rep(NA, length(fits.soy.names)))
for(i in 1:length(fits.soy.names)){
  fits.soy.IC$AIC[i] <- AIC(get(fits.soy.names[i]))
  fits.soy.IC$BIC[i] <- BIC(get(fits.soy.names[i]))
  fits.soy.IC$BICmin[i] <- AIC(get(fits.soy.names[i]),k=log(12))
}

fits.soy.IC
which(fits.soy.IC$BICmin==min(fits.soy.IC$BICmin))

######################################################################

############## calculating and plotting EBLUPs ##############
# calculating corn EBLUPs
eblups.c=data.frame(matrix(ncol=2,nrow=12))
colnames(eblups.c)=c("County","EBLUP corn")
count=1
for(eachCounty in unique(cropdata.edit$county)){
  eblups.c[count,1]=eachCounty
  eblups.c[count,2]=ranef(fit.corn.c)$county[eachCounty,]+fixef(fit.corn.c)%*%c(1,cropdata.edit[min(which(cropdata.edit$county==eachCounty)),"meanpixels.corn"])
  count=count+1
}

# calculating soy EBLUPs
eblups.s=data.frame(matrix(ncol=2,nrow=12))
colnames(eblups.s)=c("County","EBLUP soy")
count=1
for(eachCounty in unique(cropdata.edit$county)){
  eblups.s[count,1]=eachCounty
  eblups.s[count,2]=ranef(fit.soy.s)$county[eachCounty,]+fixef(fit.soy.s)%*%c(1,cropdata.edit[min(which(cropdata.edit$county==eachCounty)),"meanpixels.soy"])
  count=count+1
}

eblups=cbind(eblups.c,eblups.s$`EBLUP soy`)
colnames(eblups)=c("County","EBLUP corn","EBLUP soy")

####### Calculate MSPEs #######

# Determine model selection probabilities
set.seed(77)
boots.ICs.1000 <- replicate(1000, oneBoot(cropdata.edit))

# Weight the models by how often they appear, obtain probabilities
AIC.corn.probs <- table(boots.ICs.1000[1,])/1000
BIC.corn.probs <- table(boots.ICs.1000[2,])/1000
AIC.soy.probs <- table(boots.ICs.1000[3,])/1000
BIC.soy.probs <- table(boots.ICs.1000[4,])/1000

## Parametric bootstrap to acquire yks
set.seed(88)
K <- 3000
M.K.corn.AIC <- sample(names(AIC.corn.probs), K, prob = AIC.corn.probs, replace = T)
M.K.corn.BIC <- sample(names(BIC.corn.probs), K, prob = BIC.corn.probs, replace = T)
M.K.soy.AIC <- sample(names(AIC.soy.probs), K, prob = AIC.soy.probs, replace = T)
M.K.soy.BIC <- sample(names(BIC.soy.probs), K, prob = BIC.soy.probs, replace = T)

M.K.corn.AIC <- as.numeric(M.K.corn.AIC)
M.K.corn.BIC <- as.numeric(M.K.corn.BIC)
M.K.soy.AIC <- as.numeric(M.K.soy.AIC)
M.K.soy.BIC <- as.numeric(M.K.soy.BIC)

##### Generate y.K for corn #####
y.corn.K.AIC <- matrix(rep(NA, K*dim(cropdata.edit)[1]), ncol = K)
y.corn.K.BIC <- matrix(rep(NA, K*dim(cropdata.edit)[1]), ncol = K)

colnames(y.corn.K.AIC) <- M.K.corn.AIC
colnames(y.corn.K.BIC) <- M.K.corn.BIC

## Generate y_k for corn, AIC

for(k in 1:K){
  model.temp <- get(fits.corn.names[M.K.corn.AIC[k]])
  beta.temp <- fixef(model.temp)
  X.temp = getME(model.temp, "X")
  Z.temp = getME(model.temp, "Z")
  var.e.temp = sigma(model.temp)**2
  var.s.temp = unlist(VarCorr(model.temp))
  m <- dim(ranef(model.temp)$`county`)[1]
  
  e.temp = rnorm(dim(cropdata.edit)[1], 0, sqrt(var.e.temp))
  v.temp = rnorm(m, 0, sqrt(var.s.temp))
  y.corn.K.AIC[,k] = as.vector(X.temp%*%beta.temp + Z.temp%*% v.temp + e.temp)
}

## Generate y_k for corn, BIC
for(k in 1:K){
  model.temp <- get(fits.corn.names[M.K.corn.BIC[k]])
  beta.temp <- fixef(model.temp)
  X.temp = getME(model.temp, "X")
  Z.temp = getME(model.temp, "Z")
  var.e.temp = sigma(model.temp)**2
  var.s.temp = unlist(VarCorr(model.temp))
  m <- dim(ranef(model.temp)$`county`)[1]
  
  e.temp = rnorm(dim(cropdata.edit)[1], 0, sqrt(var.e.temp))
  v.temp = rnorm(m, 0, sqrt(var.s.temp))
  y.corn.K.BIC[,k] = as.vector(X.temp%*%beta.temp + Z.temp%*% v.temp + e.temp)
}


##### Generate y.K for soybeans #####
y.soy.K.AIC <- matrix(rep(NA, K*dim(cropdata.edit)[1]), ncol = K)
y.soy.K.BIC <- matrix(rep(NA, K*dim(cropdata.edit)[1]), ncol = K)

colnames(y.soy.K.AIC) <- M.K.soy.AIC
colnames(y.soy.K.BIC) <- M.K.soy.BIC

## Generate y.K for soybeans, AIC
for(k in 1:K){
  model.temp <- get(fits.soy.names[M.K.soy.AIC[k]])
  beta.temp <- fixef(model.temp)
  X.temp = getME(model.temp, "X")
  Z.temp = getME(model.temp, "Z")
  var.e.temp = sigma(model.temp)**2
  var.s.temp = unlist(VarCorr(model.temp))
  m <- dim(ranef(model.temp)$`county`)[1]
  
  e.temp = rnorm(dim(cropdata.edit)[1], 0, sqrt(var.e.temp))
  v.temp = rnorm(m, 0, sqrt(var.s.temp))
  y.soy.K.AIC[,k] = as.vector(X.temp%*%beta.temp + Z.temp%*% v.temp + e.temp)
}

## Generate y.K for soybeans, BIC
for(k in 1:K){
  model.temp <- get(fits.soy.names[M.K.soy.BIC[k]])
  beta.temp <- fixef(model.temp)
  X.temp = getME(model.temp, "X")
  Z.temp = getME(model.temp, "Z")
  var.e.temp = sigma(model.temp)**2
  var.s.temp = unlist(VarCorr(model.temp))
  m <- dim(ranef(model.temp)$`county`)[1]
  
  e.temp = rnorm(dim(cropdata.edit)[1], 0, sqrt(var.e.temp))
  v.temp = rnorm(m, 0, sqrt(var.s.temp))
  y.soy.K.BIC[,k] = as.vector(X.temp%*%beta.temp + Z.temp%*% v.temp + e.temp)
}


##### Find ds #####

# Wrapper functions for sapply
sumca_corn_wrapper <- function(k, y.K, Z){
  sumca_corn(y.K[,k],modelnum = as.numeric(colnames(y.K)[k]), Z)
}
sumca_soy_wrapper <- function(k, y.K, Z){
  sumca_soy(y.K[,k],modelnum = as.numeric(colnames(y.K)[k]), Z)
}

Z = getME(fit.corn.c, "Z")  # any will work for Z
ds.corn.AIC <- sapply(1:K, sumca_corn_wrapper, y.K = y.corn.K.AIC, Z = Z)
ds.corn.BIC <- sapply(1:K, sumca_corn_wrapper, y.K = y.corn.K.BIC, Z = Z)
ds.soy.AIC <- sapply(1:K, sumca_soy_wrapper, y.K = y.soy.K.AIC, Z = Z)
ds.soy.BIC <- sapply(1:K, sumca_soy_wrapper, y.K = y.soy.K.BIC, Z = Z)

ds.mean.corn.AIC <- rowMeans(ds.corn.AIC)
ds.mean.corn.BIC <- rowMeans(ds.corn.BIC)
ds.mean.soy.AIC <- rowMeans(ds.soy.AIC)
ds.mean.soy.BIC <- rowMeans(ds.soy.BIC)




### Find a.star for AIC,BIC,soy,corn combinations

n = 36
m = 12
### MSPEs for corn
a2.mat <- matrix(NA, nrow=m,ncol=length(as.integer(names(AIC.corn.probs))))
for(l in as.integer(names(AIC.corn.probs))){
  mod <- get(fits.corn.names[l])
  X = getME(mod, "X")
  Z = getME(mod, "Z")
  var.e = sigma(mod)**2
  var.s = unlist(VarCorr(mod))
  v.hat = var.e*diag(n)+var.s*Z%*%t(Z)
  G.hat = var.s*diag(m)
  a2.hat =  diag(G.hat - G.hat%*%t(Z)%*%solve(v.hat)%*%Z%*%G.hat)
  a2.mat[,which(as.integer(names(AIC.corn.probs))==l)] <- a2.hat*AIC.corn.probs[which(as.integer(names(AIC.corn.probs))==l)]
}

a.star.AIC.corn <- rowSums(a2.mat)

a2.mat <- matrix(NA, nrow=m,ncol=length(as.integer(names(BIC.corn.probs))))
for(l in as.integer(names(BIC.corn.probs))){
  mod <- get(fits.corn.names[l])
  X = getME(mod, "X")
  Z = getME(mod, "Z")
  var.e = sigma(mod)**2
  var.s = unlist(VarCorr(mod))
  v.hat = var.e*diag(n)+var.s*Z%*%t(Z)
  G.hat = var.s*diag(m)
  a2.hat =  diag(G.hat - G.hat%*%t(Z)%*%solve(v.hat)%*%Z%*%G.hat)
  a2.mat[,which(as.integer(names(BIC.corn.probs))==l)] <- a2.hat*BIC.corn.probs[which(as.integer(names(BIC.corn.probs))==l)]
}

a.star.BIC.corn <- rowSums(a2.mat)

MSPE.corn.AIC = a.star.AIC.corn + ds.mean.corn.AIC
MSPE.corn.BIC = a.star.BIC.corn + ds.mean.corn.BIC

write.csv(cbind(MSPE.corn.AIC, MSPE.corn.BIC), file = "corn MSPE.csv")

### MSPEs for soy
a2.mat <- matrix(NA, nrow=m,ncol=length(as.integer(names(AIC.soy.probs))))
for(l in as.integer(names(AIC.soy.probs))){
  mod <- get(fits.soy.names[l])
  X = getME(mod, "X")
  Z = getME(mod, "Z")
  var.e = sigma(mod)**2
  var.s = unlist(VarCorr(mod))
  v.hat = var.e*diag(n)+var.s*Z%*%t(Z)
  G.hat = var.s*diag(m)
  a2.hat =  diag(G.hat - G.hat%*%t(Z)%*%solve(v.hat)%*%Z%*%G.hat)
  a2.mat[,which(as.integer(names(AIC.soy.probs))==l)] <- a2.hat*AIC.soy.probs[which(as.integer(names(AIC.soy.probs))==l)]
}

a.star.AIC.soy <- rowSums(a2.mat)

a2.mat <- matrix(NA, nrow=m,ncol=length(as.integer(names(BIC.soy.probs))))
for(l in as.integer(names(BIC.soy.probs))){
  mod <- get(fits.soy.names[l])
  X = getME(mod, "X")
  Z = getME(mod, "Z")
  var.e = sigma(mod)**2
  var.s = unlist(VarCorr(mod))
  v.hat = var.e*diag(n)+var.s*Z%*%t(Z)
  G.hat = var.s*diag(m)
  a2.hat =  diag(G.hat - G.hat%*%t(Z)%*%solve(v.hat)%*%Z%*%G.hat)
  a2.mat[,which(as.integer(names(BIC.soy.probs))==l)] <- a2.hat*BIC.soy.probs[which(as.integer(names(BIC.soy.probs))==l)]
}

a.star.BIC.soy <- rowSums(a2.mat)

MSPE.soy.AIC = a.star.AIC.soy + ds.mean.soy.AIC
MSPE.soy.BIC = a.star.BIC.soy + ds.mean.soy.BIC

write.csv(cbind(MSPE.soy.AIC, MSPE.soy.BIC), file = "soy MSPE.csv")


# Read in MSPEs
cornMSPEs=read.csv("corn MSPE.csv",header=T)
soyMSPEs=read.csv("soy MSPE.csv",header=T)

# plotting corn EBLUPs
plot(1:12,eblups.c$`EBLUP corn`,pch=19,xlab="County",ylab="EBLUP",main="Corn EBLUPs",xaxt='n',ylim=c(-50,360))
axis(1,at=1:12,labels=unique(cropdata.edit$county))
count=1
for(eachCounty in unique(cropdata.edit$county)){
  indices=which(cropdata.edit$county==eachCounty)
  for(i in 1:length(indices)){
    points(count,cropdata.edit$hectares.corn[indices[i]],col=which(levels(cropdata.edit$county)==eachCounty),pch=4)
  }
  count=count+1
}
arrows(1:12-0.15,eblups.c$`EBLUP corn`-2*cornMSPEs$MSPE.corn.AIC,1:12-0.15,eblups.c$`EBLUP corn`+2*cornMSPEs$MSPE.corn.AIC,angle=90,length=0.05,code=3,col="blue")
arrows(1:12+0.15,eblups.c$`EBLUP corn`-2*cornMSPEs$MSPE.corn.BIC,1:12+0.15,eblups.c$`EBLUP corn`+2*cornMSPEs$MSPE.corn.BIC,angle=90,length=0.05,code=3,col="red")
legend("topright",c("EBLUPs","original data","AIC interval","BIC interval"),pch=c(19,4,NA,NA),lty=c(0,0,1,1),col=c("black","black","blue","red"))

# plotting soy EBLUPs
plot(1:12,eblups.s$`EBLUP soy`,pch=19,xlab="County",ylab="EBLUP",main="Soy EBLUPs",xaxt='n',ylim=c(-160,360))
axis(1,at=1:12,labels=unique(cropdata.edit$county))
count=1
for(eachCounty in unique(cropdata.edit$county)){
  indices=which(cropdata.edit$county==eachCounty)
  for(i in 1:length(indices)){
    points(count,cropdata.edit$hectares.soy[indices[i]],col=which(levels(cropdata.edit$county)==eachCounty),pch=4)
  }
  count=count+1
}
arrows(1:12-0.15,eblups.s$`EBLUP soy`-2*soyMSPEs$MSPE.soy.AIC,1:12-0.15,eblups.s$`EBLUP soy`+2*soyMSPEs$MSPE.soy.AIC,angle=90,length=0.05,code=3,col="blue")
arrows(1:12+0.15,eblups.s$`EBLUP soy`-2*soyMSPEs$MSPE.soy.BIC,1:12+0.15,eblups.s$`EBLUP soy`+2*soyMSPEs$MSPE.soy.BIC,angle=90,length=0.05,code=3,col="red")
legend("topright",c("EBLUPs","original data","AIC interval","BIC interval"),pch=c(19,4,NA,NA),lty=c(0,0,1,1),col=c("black","black","blue","red"))


##### Check that MSPE values have converged at our K

n = 36
m = 12

### MSPEs for corn

kends <- seq(100, 3000, by = 100)
MSPEs.corn <- data.frame(MSPE.AIC = rep(NA, length(kends)), MSPE.BIC = rep(NA, length(kends)))
i <- 1
for(kend in kends){
  ds.mean.corn.AIC <- rowMeans(ds.corn.AIC[,1:kend])
  ds.mean.corn.BIC <- rowMeans(ds.corn.BIC[,1:kend])
  
  a2.mat <- matrix(NA, nrow=m,ncol=length(as.integer(names(AIC.corn.probs))))
  for(l in as.integer(names(AIC.corn.probs))){
    mod <- get(fits.corn.names[l])
    X = getME(mod, "X")
    Z = getME(mod, "Z")
    var.e = sigma(mod)**2
    var.s = unlist(VarCorr(mod))
    v.hat = var.e*diag(n)+var.s*Z%*%t(Z)
    G.hat = var.s*diag(m)
    a2.hat =  diag(G.hat - G.hat%*%t(Z)%*%solve(v.hat)%*%Z%*%G.hat)
    a2.mat[,which(as.integer(names(AIC.corn.probs))==l)] <- a2.hat*AIC.corn.probs[which(as.integer(names(AIC.corn.probs))==l)]
  }
  
  a.star.AIC.corn <- rowSums(a2.mat)
  
  a2.mat <- matrix(NA, nrow=m,ncol=length(as.integer(names(BIC.corn.probs))))
  for(l in as.integer(names(BIC.corn.probs))){
    mod <- get(fits.corn.names[l])
    X = getME(mod, "X")
    Z = getME(mod, "Z")
    var.e = sigma(mod)**2
    var.s = unlist(VarCorr(mod))
    v.hat = var.e*diag(n)+var.s*Z%*%t(Z)
    G.hat = var.s*diag(m)
    a2.hat =  diag(G.hat - G.hat%*%t(Z)%*%solve(v.hat)%*%Z%*%G.hat)
    a2.mat[,which(as.integer(names(BIC.corn.probs))==l)] <- a2.hat*BIC.corn.probs[which(as.integer(names(BIC.corn.probs))==l)]
  }
  
  a.star.BIC.corn <- rowSums(a2.mat)
  
  MSPEs.corn$MSPE.AIC[i] = a.star.AIC.corn + ds.mean.corn.AIC
  MSPEs.corn$MSPE.BIC[i] = a.star.BIC.corn + ds.mean.corn.BIC
  i <- i+1
}

plot(kends, MSPEs.corn$MSPE.AIC, type = "l", ylim = c(75, 85), xlab = "K", ylab = "MSPE",
     main = "MSPE of EBLUP for corn, using AIC")
lines(kends, MSPEs.corn$MSPE.BIC, col = 2, ylim = c(75, 85), xlab = "K", ylab = "MSPE",
      main = "MSPE of EBLUP for corn, using BIC")
legend("bottomright", legend = c("AIC", "BIC"), col = 1:2, lty = 1)


