
install.packages("simpackage_0.0.0.9000.tar.gz",repos=NULL)
library(simpackage)
load.libs()
folder = paste0(getwd(),"/results data/") # File Location of Results

# Initialization / Termination  ---------------------------

load.libs(all=TRUE)
set.seed(1)

runfun = runfun.ttest(delta = .4)
true_power.fun = runfun.ttest.true(delta=.4)

design = list(n = c(50,200))
goal=.8



#plot with uncertainty ribbon

dat= ss.find(reg.pred,runfun=runfun,design=design,goal=goal,dat.only=T,budget=4000,seed=1)
dat.obs = todataframe(dat)
dat.obs$type=NA

re2 = gauss.pred(dat,goal=goal)
fn2 = re2$fun
fn2.sd = re2$fun.sd

ns = seq(50,200)

# True Power
true.power = sapply(ns,true_power.fun)
dat.true = data.frame(n =ns,y=true.power,type="True Power")
#Prediction
p2 = data.frame(n =ns,y=sapply(ns,fn2),type="Prediction")
dat1 = rbind(dat.true,p2)
#SD
dat.sd = data.frame(n =ns,pred=p2$y,sd=sapply(ns,fn2.sd))

pl1 = ggplot(dat.true, aes(n,y)) +theme_bw() +
  geom_ribbon(data=dat.sd,aes(ymin = pred - sd, ymax = pred + sd,y=1), fill = "grey70") +
  geom_line(data = dat1,aes(color=type)) +
  geom_point(data=dat.obs,aes(x =V1,y = y))+scale_color_brewer(palette="Set1") + theme(legend.position = "none") + xlab("Sample Size") + ylab("Power") +theme(legend.position="bottom")+ theme(legend.title = element_blank()) + ggtitle("After Initialization")  + theme(plot.title = element_text(hjust = 0.5))
pl1


#Final plot with uncertainty ribbon

re4= ss.find(gauss.pred,runfun=runfun,design=design,goal=goal,budget=4000,dat=dat,seed = 1)
dat.obs = todataframe(re4$dat)
dat.obs$type=NA

fn2 = re4$fun
fn2.sd = re4$fun.sd

ns = seq(50,200)

# True Power
true.power = sapply(ns,true_power.fun)
dat.true = data.frame(n =ns,y=true.power,type="True Power")
#Prediction
p2 = data.frame(n =ns,y=sapply(ns,fn2),type="Prediction")
dat1 = rbind(dat.true,p2)
#SD
dat.sd = data.frame(n =ns,pred=p2$y,sd=sapply(ns,fn2.sd))


pl2 = ggplot(dat.true, aes(n,y)) +theme_bw() +
  geom_ribbon(data=dat.sd,aes(ymin = pred - sd, ymax = pred + sd,y=1), fill = "grey70") +
  geom_line(data = dat1,aes(color=type)) +
  geom_point(data=dat.obs,aes(x =V1,y = y))+scale_color_brewer(palette="Set1") + theme(legend.title = element_blank())+ xlab("Sample Size") + ylab("Power") +theme(legend.position="bottom") + ggtitle("After Termination") + theme(plot.title = element_text(hjust = 0.5))
pl2

g1 = grid.arrange(pl1,pl2,ncol=2,widths=list(100,100))
pdf(paste0(folder,"uncertainty.pdf"),height=3,width=6);grid.draw(g1);dev.off()


# 3D plot -----------------------------------------------------------------


library(ploty)

#Setup Framework

runfun = runfun.anova()
design = list(n = c(20,90),k=c(5,25))
cost = function(x) x[1]*1.9+x[2]*6
goal=.8


#Run

re3 = ss.find(gauss.pred,runfun=runfun,design=design,goal=goal,budget=4000,cost=cost,seed=1)



#Plot data

dat= re3$data
datx = todataframe(dat)
fig = plot_ly(x=datx$V1, y=datx$V2, z=datx$y,type='scatter3d')
fig

fig = fig %>% layout(scene = list(
  xaxis = list(title ='Participants per Group'),
  yaxis = list(title ='Groups'),
  zaxis = list(title ='Power')
))


#Plot Function on Top

fun = re3$fun

ns = min(datx$V1):max(datx$V1)
ks = min(datx$V2):max(datx$V2)
x = expand.grid(ns,ks)
preddat = matrix(apply(x,1,fun),ncol=length(ns),byrow=TRUE)

fig <- add_surface(fig,x=ns, y=ks, z=preddat,opacity=.8)
fig





# Learner Comparison ------------------------------------------------------



runfun = runfun.ttest(delta = .4)
true_power.fun = runfun.ttest.true(delta=.4)

design = list(n = c(50,200))
goal=.8

dat= ss.find(reg.pred,runfun=runfun,design=design,goal=goal,dat.only=T,budget=10000,seed=1,n.startsets=10)

# Observed data
dat.obs = todataframe(dat)
dat.obs$type=NA

# n Values
ns = seq(min(dat.obs[,1]),max(dat.obs[,1]))

# True Power
true.power = sapply(ns,true_power.fun)
dat.true = data.frame(n =ns,y=true.power,type="True Power")

fn1 = reg.pred(dat,goal=goal)$fun
fn2 = logi.pred(dat,goal=goal)$fun
fn3 = svm.pred(dat,goal=goal)$fun
fn4 = gauss.pred(dat,goal=goal)$fun

# Predictions
p1 = data.frame(n =ns,y=sapply(ns,fn1),type="Linear")
p2 = data.frame(n =ns,y=sapply(ns,fn2),type="Logistic")
p3 = data.frame(n =ns,y=sapply(ns,fn3),type="SVR")
p4 = data.frame(n =ns,y=sapply(ns,fn4),type="GPR")

dat1 = rbind(dat.true,p1,p2,p3,p4)
dat1$type = as.factor(dat1$type)
levels(dat1$type)
dat1$type = factor(dat1$type,levels=c("True Power","Linear","Logistic","SVR","GPR"),ordered=T)


pl3 = ggplot(dat1, aes(n,y)) +theme_bw() +
  geom_line(aes(color=type)) +
  geom_point(data=dat.obs,aes(x =V1,y = y)) +scale_color_brewer(palette="Set1") + ylab("Power") + xlab("Sample Size")+ guides(color=guide_legend(title=""))+theme(legend.position="bottom")
pl3

pdf(paste0(folder,"learner_plot.pdf"),height=4,width=5);pl3;dev.off()


