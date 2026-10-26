# Load packages (install them first if needed) -----------------------------------------------------------

load.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[ , "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = T)
  }
  sapply(pkg, require, character.only = T)
}

packages <- c("lme4", "lattice", "raster", "boot", "pbkrtest", "parallel",
              "psych", "tidyverse", "nloptr", "multcomp", "optimx", "extrafont")

load.packages(packages)


# Load data ---------------------------------------------------------------

curr_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curr_dir)
DATA <- read_csv("DATA_GABAiu_68.csv")

# Change certain variables to factors for linear mixed-effects modeling
DATA$SubjectID <- as.factor(DATA$SubjectID)
DATA$Vendor    <- as.factor(DATA$Vendor)
DATA$Site      <- as.factor(DATA$Site)


# In-line functions ----------------------------------------------------------

nloptFun <- function(fn, par, lower, upper, control=list(), ...) {
  for (n in names(defaultControl)) 
    if (is.null(control[[n]])) control[[n]] <- defaultControl[[n]]
    res <- nloptr(x0=par, eval_f=fn, lb=lower, ub=upper, opts=control, ...)
    with(res, list(par=solution,
                   fval=objective,
                   feval=iterations,
                   conv=if (status>0) 0 else status,
                   message=message))
}

cs. <- function(x) scale(x, center=T, scale=T) # for standardizing (z-transforming) outcome and predictor variables; aids with model convergence and interpretability of parameter estimates

r2.global <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.local <- function(small_model, large_model) {
  vc_small_model <- as.data.frame(VarCorr(small_model))[4]
  vc_large_model <- as.data.frame(VarCorr(large_model))[4]
  out <- as.numeric((tail(vc_small_model,n=1) - tail(vc_large_model,n=1)) / tail(vc_small_model,n=1))
  return(out)
}


# Descriptive stats -------------------------------------------------------

GABA_iu.mean.vendor <- aggregate(DATA$GABA_ConcIU_TissCorr_Gasp, list(DATA$Vendor), mean)
GABA_iu.sd.vendor   <- aggregate(DATA$GABA_ConcIU_TissCorr_Gasp, list(DATA$Vendor), sd)
GABA_iu.mean.site   <- aggregate(DATA$GABA_ConcIU_TissCorr_Gasp, list(DATA$Site), mean)
GABA_iu.sd.site     <- aggregate(DATA$GABA_ConcIU_TissCorr_Gasp, list(DATA$Site), sd)
GABA_iu.cv.vendor   <- aggregate(DATA$GABA_ConcIU_TissCorr_Gasp, list(DATA$Vendor), cv)
GABA_iu.cv.site     <- aggregate(DATA$GABA_ConcIU_TissCorr_Gasp, list(DATA$Site), cv)
GABA_iu.cv.ppt      <- cv(DATA$GABA_ConcIU_TissCorr_Gasp)

GABA_Cr.mean.vendor <- aggregate(DATA$GABA_Cr, list(DATA$Vendor), mean)
GABA_Cr.sd.vendor   <- aggregate(DATA$GABA_Cr, list(DATA$Vendor), sd)
GABA_Cr.mean.site   <- aggregate(DATA$GABA_Cr, list(DATA$Site), mean)
GABA_Cr.sd.site     <- aggregate(DATA$GABA_Cr, list(DATA$Site), sd)
GABA_Cr.cv.vendor   <- aggregate(DATA$GABA_Cr, list(DATA$Vendor), cv)
GABA_Cr.cv.site     <- aggregate(DATA$GABA_Cr, list(DATA$Site), cv)
GABA_Cr.cv.ppt      <- cv(DATA$GABA_Cr)

# Site CVs
site_CVs <- cbind(GABA_Cr.cv.site, GABA_iu.cv.site[, 2])
names(site_CVs) <- c("Site","GABA_Cr","GABA_iu")

# Vendor CVs
vendor_CVs <- cbind(GABA_Cr.cv.vendor, GABA_iu.cv.vendor[, 2])
names(vendor_CVs) <- c("Vendor","GABA_Cr","GABA_iu")


# Multilevel analysis -----------------------------------------------------

# Selection of optimizers to use
defaultControl <- list(algorithm="NLOPT_LN_BOBYQA", xtol_rel=1e-6, maxeval=1e5)
optWrap.nloptwrap.NLOPT_LN_BOBYQA <- lmerControl(optimizer="nloptFun")
optWrap.optimx.nlminb             <- lmerControl(optimizer="optimx", optCtrl=list(method="nlminb", eval.max=1e5))
optWrap.minqa.bobyqa              <- lmerControl(optimizer="bobyqa")

optimToUse <- optWrap.minqa.bobyqa

Y.M0.1  <- lm(cs.(GABA_ConcIU_TissCorr_Gasp) ~ 1, data=DATA)
Y.M0.2a <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ (1 | Site), data=DATA, REML=F, control=optimToUse)
Y.M0.2b <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ (1 | Vendor), data=DATA, REML=F, control=optimToUse)
Y.M0.3a <- lmer(cs.(GABA_ConcIU_TissCorr) ~ (1 | Site) + (1 | Vendor), data=DATA, REML=F, control=optimToUse)
Y.M0.3b <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ (1 | Site) + (1 | Vendor), data=DATA, REML=F, control=optimToUse)

Y_uncorr <- lmer(cs.(GABA_ConcIU) ~ (1 | Site) + (1 | Vendor), data=DATA, REML=F, control=optimToUse)

Y.M1.a  <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ cs.(WaterFitError) + (cs.(WaterFitError) | Site) + (cs.(WaterFitError) | Vendor), data=DATA, REML=F, control=optimToUse)
Y.M1.b  <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ cs.(NAALinewidth) + (cs.(NAALinewidth) | Site) + (cs.(NAALinewidth) | Vendor), data=DATA, REML=F, control=optimToUse)

Y.M1.c1 <- lmer(cs.(GABA_ConcIU_TissCorr) ~ cs.(fGM) + (cs.(fGM) | Site) + (cs.(fGM) | Vendor), data=DATA, REML=F, control=optimToUse)
Y.M1.c2 <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ cs.(fGM) + (cs.(fGM) | Site) + (cs.(fGM) | Vendor), data=DATA, REML=F, control=optimToUse)

Y.M1.d <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ cs.(CNR) + (cs.(CNR) | Site) + (cs.(CNR) | Vendor), data=DATA, REML=F, control=optimToUse)

Y.M1.e <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ Sex + (Sex | Site) + (Sex | Vendor), data=DATA, REML=F, control=optimToUse)
Y.M1.f <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ cs.(Age) + (cs.(Age) | Site) + (cs.(Age) | Vendor), data=DATA, REML=F, control=optimToUse)

Y <- lmer(cs.(GABA_ConcIU_TissCorr) ~ cs.(fCSF) + (cs.(fCSF) | Site) + (cs.(fCSF) | Vendor), data=DATA, REML=F, control=optimToUse)


# Variance components -----------------------------------------------------

# Save variance components of three-level null model
print(VarCorr(Y.M0.3b), comp = c("Variance", "Std.Dev."))
vc <- as.data.frame(VarCorr(Y.M0.3b))[4]
vpc.M0.3b <- round(vc/sum(vc)*100,1)
dimnames(vpc.M0.3b)[[1]] <- c("Site", "Vendor", "Participant")


# Inferential tests -------------------------------------------------------

# Likelihood ratio tests
(LRT.1 <- anova(Y.M0.2a, Y.M0.3b))
(LRT.2 <- anova(Y.M0.2b, Y.M0.3b))

(LRT.3  <- anova(Y.M0.3b, Y.M1.a))
(LRT.4  <- anova(Y.M0.3b, Y.M1.b))
(LRT.5a <- anova(Y.M0.3a, Y.M1.c1))
(LRT.5b <- anova(Y.M0.3b, Y.M1.c2))

(LRT.6 <- anova(Y.M0.3b, Y.M1.e))
(LRT.7 <- anova(Y.M0.3b, Y.M1.f))

(LRT.8 <- anova(Y.M0.3b, Y.M1.d))

(L <- anova(Y.M0.3b,Y))

# Effect sizes
(r2.1 <- r2.local(Y.M0.3b, Y.M1.c1))
(r2.2 <- r2.local(Y.M0.3b, Y.M1.c2))

# Pairwise comparisons
M.Vendor <- lmer(cs.(GABA_ConcIU_TissCorr_Gasp) ~ Vendor + (1 | Site), data=DATA, REML=F, control=optimToUse)
summary(glht(M.Vendor, linfct=mcp(Vendor="Tukey")), test=adjusted("holm"))


# # Bootstrapping -----------------------------------------------------------
# 
# # Set up parallel computation
# nc <- detectCores()
# clus <- makeCluster(rep("localhost", nc))
# clusterEvalQ(clus, library(pbkrtest))
# clusterExport(clus, c("nloptr", "defaultControl", "nloptFun"))
# 
# # Bootstrap likelihood ratio tests
# PB_LRT.1 <- PBmodcomp(Y.M0.3b, Y.M0.2a, nsim=2e3, cl=clus)
# PB_LRT.2 <- PBmodcomp(Y.M0.3b, Y.M0.2b, nsim=2e3, cl=clus)
# 
# # PB_LRT.3 <- PBmodcomp(Y.M1.a, Y.M0.3b, nsim=2e3, cl=clus)
# PB_LRT.4 <- PBmodcomp(Y.M1.b, Y.M0.3b, nsim=2e3, cl=clus)
# # PB_LRT.5a <- PBmodcomp(Y.M1.c1, Y.M0.3a, nsim=2e3, cl=clus)
# PB_LRT.5b <- PBmodcomp(Y.M1.c2, Y.M0.3b, nsim=2e3, cl=clus)
# 
# PB_LRT.6 <- PBmodcomp(Y.M1.e, Y.M0.3b, nsim=2e3, cl=clus)
# PB_LRT.7 <- PBmodcomp(Y.M1.f, Y.M0.3b, nsim=2e3, cl=clus)
# 
# stopCluster(clus)


# Check assumptions -------------------------------------------------------

df <- as_tibble(cbind(residuals=resid(Y.M0.3b), fitted=fitted(Y.M0.3b), pred=cs.(DATA$fGM)))
names(df)[3] <- "pred"

p1 <- ggplot(data=df, aes(sample=residuals)) +
  geom_point(stat="qq", shape=1) +
  geom_abline(slope=1, intercept=0) +
  theme_bw() +
  theme(aspect.ratio=1, axis.ticks.length=unit(2, "mm"), panel.grid=element_blank())
p1

p2 <- ggplot(data=df, aes(pred, residuals)) +
  geom_point(shape=1) +
  theme_bw() +
  theme(aspect.ratio=1, axis.ticks.length=unit(2, "mm"), panel.grid=element_blank())
p2



