

# some runs land on the edge of the parameter Space in Task C for SVRs, across all budget conditions. why?

# Problem ist, dass alle tatsächlich guten Werte in den Startdaten kacke aussehen, und der Algorithmus nie wieder dort hingeht. Wie soll er auch wissen dass er in einem lokalen Maximum ist?

# Das Problem ist sicher nicht in get.pred (der beste Kandidat WIRD gefunden.). also muss der Learner gefixt werden.

# Leicht lösbares Problem könnte die Range der Tuning Parameter sein.


# Loss Function bei der Tuning CV angepasst von MSE zu logloss -> macht viel mehr Sinn so
# Range der starting tuning pars angepasst -> funktioniert viel besser für Problemseed.

# Testsimulation Funktion 6-> funzt es jetzt besser??
# Falls ja, nochmal alles komplett laufen lassen



#Load Preprocessed -------------------------------------------------------

devtools::load_all(".")
load.libs()

load(file = paste0(folder,"tempres.Rdata")) #resx
load(file = paste0(folder,"at.Rdata")) # at

ind = as.numeric(resx$fun_nr)==6 & resx$learner=="SVR"& resx$budget=="high" & resx$X1==17
which(ind)

exam = which(ind)[4]

resx[exam,]

a = resx[ind,]

atx = at[[6]]

a1 = a[1,c("X1","X2")]
a1

atx$true_power.fun(c(17,30))


seed = 76623998 # Outlier in Plot 4B -> War gerade am explorieren, war die erste Prediction dort
seed = 49059585 # Outlier in Plot 4C -> nicht aus dem 17,30 Loch rausgekommen..



# closer ------------------------------------------------------------------


restag = c("52","53","54","55")

folder = "C:/Users/felix/switchdrive/4 irt/paper 2/"
# folder = "C:/Users/admin/switchdrive/4 irt/paper 2/"

rest = list()
for (i in restag) {
  load(file= paste0(folder,"res_",i,".Rdata"))
  rest = c(rest,res)
}
res = rest


load(file= paste0(folder,"at.Rdata"))


seeds.used = sapply(res,function(x)x[[1]]$seed)
ind = which(seeds.used ==seed)
as.numeric(ind)

x = res[[ind]]


# run ---------------------------------------------------------------------

load(file= "problemcondition.Rdata")



fun_nr = x$fun_nr
task = x$task
budget = x$budget
goal.ci = x$goal.ci
seed = x$seed
x1 = load.cond(fun_nr,task,budget,goal.ci)

runfun=x1$runfun
design=x1$design
cost=x1$cost
budget=x1$budget
goal.ci=x1$goal.ci
analytical=x1$analytical
goal=x1$goal
fixed_cost=x1$fixed_cost

# run ---------------------------------------------------------------------

re0 = list(value=analytical,value.sd = NA,data = NA,budget=NA,fun=NA,fun.sd =NA)

startdat= ss.find(reg.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget,seed=seed,cost=cost,dat.only=T)

# save(startdat,file= "problemstartdat.Rdata")

load(file= "problemstartdat.Rdata")



re1=re2=NULL
if (!fun_nr%in%c(2,6)) {
  re1 =  ss.find(reg.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget,seed=seed,cost=cost,dat=startdat)
  re2 =  ss.find(logi.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget, seed=seed,cost=cost,dat=startdat)
}
re3 =  ss.find(svm.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget, seed=seed,cost=cost,dat=startdat,fixed_cost=fixed_cost)
re4 =  ss.find(gauss.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget, seed=seed,cost=cost,dat=startdat,fixed_cost=fixed_cost)

