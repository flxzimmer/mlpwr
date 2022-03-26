

# Calculating sample size from analytical power analysis

install.packages("simpackage_0.0.0.9000.tar.gz",repos=NULL)
library(simpackage)
load.libs()
folder = paste0(getwd(),"/results data/") # File Location of Results

# Function 3 --------------------------------------------------------------

runfun = runfun.skewed2(delta = .4,alpha =10)
design = list(n = c(50,200))
a = ceiling(power.t.test(delta = .4, sig.level = .05, power = .8,type = "two.sample", alternative = "two.sided")$n)

an3 = list(analytical= a)


# Function 4 --------------------------------------------------------------

#install package from Zimmer&Debelak,2022 (https://doi.org/10.31234/osf.io/yqa2e)
# for Power analysis for Marginal-Maximum-Likelihood-estimated IRT models
install_github("flxzimmer/pwrml",quiet = TRUE)
library(pwrml)

itempars = runfun.irt.itempars(delta=.3,n.items = 20,seed=1)
hyp = setup_hypothesis(type = "1PLvs2PL", altpars = itempars)
ncps = calculate_ncps(hyp=hyp,sampling=TRUE,sampling.npers = 10^5,approx.npers=10^5)
analytical = ssize(hyp=hyp,ncp=ncps["LR"],alpha=.05,power=.8)
a = ceiling(analytical)

an4 = list(analytical= a)


# Save all in one file ----------------------------------------------------

an = list(NA,NA,an3,an4,NA,NA)

save(an,file= paste0(folder,"an.Rdata"))


