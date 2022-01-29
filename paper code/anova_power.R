
# Bisherige Runfun durch eine mit konstanter effect size ersetzen

# We can use the effect size from the pwr package implementation

# We use as effect size the effect size of the solution of the previous function. This way, the solution should be identical.

# as a new effect size, we can use f = .16298

library(pwr)


folder = "C:/Users/admin/switchdrive/4 irt/paper 2/"
# folder = "C:/Users/admin/switchdrive/4 irt/paper 2/"
load(file= paste0(folder,"at.Rdata"))
at[[2]]$actually_true

# The solution is 42,18

# Other parameters were
k = 18
n = 42
within.var = 10
delta = delta = 10^(-1)
groupmeans = seq(0,(k-1)*delta,by=delta)
between.var = var(groupmeans)

# Check effect size in new function
pw = pwr.anova.test(k, n , sig.level = .05, power =.8)
pw$f

f = .16298
pw = pwr.anova.test(k, n , f , sig.level = .05)
pw$power


# test new implementation

devtools::load_all(".")
fun.true = runfun.anova.true()
fun = runfun.anova()

k = 18
n = 42
x = c(42,18)
fun.true(x)
fun(x)



# stuff -------------------------------------------------------------------



# Vergleich zwei Implementierungen
k = 5
n = 100
within.var=10




# base package
pw = power.anova.test(groups=k,n=n,between.var = between.var, within.var = 10)$power
pw

# pwr package
library(pwr)
f = sqrt(between.var / within.var)
pwr.anova.test(k, n , f = , sig.level = .05, power =pw)
f


# alte Implementierung
delta = delta=10^(-1)
groupmeans= seq(0,(k-1)*delta,by=delta)
pow = power.anova.test(groups=k,n=n,between.var = var(groupmeans), within.var = within.var)$power




# alter code
runfun.anova = function(delta=10^(-1),within.var=10) {

  runfun = function(x) {

    n <- x[1] # Number of Persons
    k <- x[2] # Number of Clusters
    groupmeans= seq(0,(k-1)*delta,by=delta)
    pow = power.anova.test(groups=k,n=n,between.var = var(groupmeans), within.var = within.var)$power
    return(runif(1)<pow)
  }
  return(runfun)
}

