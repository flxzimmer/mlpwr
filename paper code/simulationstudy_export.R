
devtools::load_all(".")
load.libs()

# Load Results ------------------------------------------------------------

restag = "31"

folder = "C:/Users/admin/switchdrive/4 irt/paper 2/"
# folder = "C:/Users/admin/switchdrive/4 irt/paper 2/"

load(file= paste0(folder,"res_",restag,".Rdata"))

load(file= paste0(folder,"at.Rdata"))


# Get results for funs 2 + 6 from another file

fns = sapply(res,function(x) x[[1]]$fun_nr) #delete fns 2 and 6
ind = !fns %in% c(2,6)
res = res[ind]
res1 = res

load(file= paste0(folder,"res_",37,".Rdata"))
# attach
res = c(res,res1)



#Replace results for function 2 with new results

fns = sapply(res,function(x) x[[1]]$fun_nr) #delete fns 2 and 6
ind = !fns %in% c(2)
res = res[ind]
res1 = res

load(file= paste0(folder,"res_",40,".Rdata"))
# attach
res = c(res,res1)



# Manual input ------------------------------------------------------------

learner.names = c("Analytical","Linreg","Logreg","SVR","GP")
learner = factor(learner.names,levels=learner.names,ordered=T)

funlabels = c("1 t-test","2 ANOVA","3 Skewed t-test","4 IRT","5 Mixed Model","6 2D Mixed Model")
# funlabels = c("4 IRT","5 Mixed Model","6 2D Mixed Model")
# funlabels = c("2 2D-t-test","6 2D Mixed Model")
# funlabels = c("4 IRT")

# resx$cost[resx$learner=="Analytical"]=150


# Long Result Table -------------------------------------------------------

resx = lapply(res, function(x) {

    cond = x[[1]]
    task = cond$task
    seed = cond$seed
    budget=cond$budget
    goal.ci = cond$goal.ci
    fun_nr = cond$fun_nr
    x1 = load.cond(fun_nr,task,budget=budget,goal.ci=goal.ci)
    costfun=x1$cost
    if (is.null(goal.ci))goal.ci ="NULL"
    if (is.null(budget))budget ="NULL"

    atx = at[[fun_nr]]
    true_power.fun = atx$true_power.fun

    if (task != "C") {
        actual_power = atx$actual_power
        actually_true = atx$actually_true
        actual_cost = atx$actual_cost
    }
    if (task == "C") {
        actual_power = atx$actual_powerC
        actually_true = atx$actually_trueC
        actual_cost = atx$actual_costC
    }

    value = sapply(2:length(x),function(i) x[[i]]$value)
    value = lapply(value,function(x) if(is.null(x))NA else x)
    t1 = do.call(rbind,value)
    X1 = t1[,1]
    X2 = tryCatch(t1[,2],error=function(e)NA)
    value = list()
    for (i in 1:nrow(t1)) {
        value[[i]] = t1[i,]
    }
    cost = sapply(value,costfun) |> as.numeric()
    # if(task=="C"&fun_nr==6&&cost[4]!=320) browser()

    value.sd  = sapply(2:length(x),function(i) x[[i]]$value.sd)
    value.sd = lapply(value.sd,function(x) if(is.null(x))NA else x) |> as.numeric()
    budget_used = sapply(2:length(x),function(i) x[[i]]$budget)
    budget_used = lapply(budget_used,function(x) if(is.null(x))NA else x) |> as.numeric()

    time_used   = sapply(2:length(x),function(i) x[[i]]$time_used[1])
    time_used = lapply(time_used,function(x) if(is.null(x))NA else x) |> as.numeric()

    if (!is.null(true_power.fun)) {
    true_power = sapply(value,function(x){
        if(is.na(x[1])) return(NA)
        else return(true_power.fun(x))
        })
    } else {true_power= NA}


    # if(seed == 300)browser()

    re = data.frame(cost,value.sd,budget_used,learner,actual_cost,actual_power,true_power,time_used,budget,goal.ci,fun_nr,task,seed,I(value),X1,X2)
    return(re)
})

# save(resx,file= paste0(folder,"resx_temp.Rdata"))
# load(file= paste0(folder,"resx_temp.Rdata"))

resx = do.call(rbind,resx)

resx$budget = as.factor(resx$budget)
resx$goal.ci = as.factor(resx$goal.ci)
resx$fun_nr = factor(resx$fun_nr, labels=funlabels)

resx = resx[!is.na(resx$fun_nr),]

resx$value.sd[resx$value.sd ==10] = NA


# Task C -----------------------------------------------------------------

usetask = "C"
resX = resx[resx$task==usetask,]
resX = resX[!is.na(resX$cost),]

# resX2 = resx[resx$task==usetask&resx$fun_nr=="6 2D Mixed Model",]
# table(resX2$cost)
# resX = resX[!is.na(resX$value.sd),]

# Boxplots
p1 = ggplot(resX, aes(x= learner,y=cost,fill=budget)) + geom_violin(draw_quantiles = 0.5) + geom_hline(data= resX, aes(yintercept=actual_cost), linetype="dashed", color = "red") + facet_wrap(~ fun_nr,  scales = "free")

p2 = ggplot(resX, aes(x= learner,y=budget_used,fill=budget)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")

# resX$value.sd_valid =
p3 = ggplot(resX, aes(x= learner,y=value.sd,fill=budget)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")

# percent "no estimate" für value.sd
no_sd = aggregate(resX$value.sd,list(resX$learner,resX$budget,resX$fun_nr),function(x) mean(is.na(x)))
names(no_sd)=c("learner","budget","fun_nr","no_sd")
p3.1 = ggplot(no_sd, aes(x= learner,y=no_sd,fill=budget)) +  geom_bar(position="dodge",stat="identity") + facet_wrap(~ fun_nr, scales = "free_x")

resy = resX[!is.na(resX$true_power),]
# p4 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  geom_boxplot() + geom_hline(data= resy, aes(yintercept=actual_power), linetype="dashed", color = "red") + facet_wrap(~ fun_nr,scales = "free_x")

p4 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  geom_boxplot() + geom_hline(data= resX, aes(yintercept=actual_power, linetype="Optimal power"), color = "red") + facet_wrap(~ fun_nr,scales = "free_x") + scale_linetype_manual(name = "", values = c(2),guide = guide_legend(override.aes = list(color = c( "red")))) + ylab("Power") + xlab("Surrogate Model")


# Höhenlinien Plot Function 2
fn_nr = 2
# resX1 = resX[resX$budget=="low"& resX$fun_nr=="2 2D-t-test",]
resX1 = resX[resX$fun_nr=="2 ANOVA",]

atx = at[[fn_nr]]
true_power.fun = atx$true_power.fun
actual_power = atx$actual_powerC
actual_cost = atx$actual_costC
x1 = load.cond(fn_nr,usetask,budget=NA,goal.ci=NA)
costfun=x1$cost
design=x1$design

xvals = seq(min(resX1$X1,na.rm=T),max(resX1$X1,na.rm=T),.5)
eqpower.y = sapply(xvals,function(x) {
    fn = function(y) abs(true_power.fun(c(x,y))-actual_power)
    a = optim(20,fn,method="L-BFGS-B",lower=design[[2]][[1]],upper=design[[2]][[2]])
    legit = a$value<.001
    if(legit) return(a$par) else return(NA)
})
eqpower = data.frame(X1=xvals,X2=eqpower.y)

eqcost.y = sapply(xvals,function(x) {
    fn = function(y) abs(costfun(c(x,y))-actual_cost)
    optim(20,fn,method="L-BFGS-B",lower=design[[2]][[1]],upper=design[[2]][[2]])$par})
eqcost = data.frame(X1=xvals,X2=eqcost.y)

# p5 = ggplot(resX1, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=eqpower,aes(x=X1, y=X2,col="power=.8"),size=1)+ geom_line(data=eqcost,aes(x=X1, y=X2,col="optimal cost"),size=1)
# # +  scale_fill_brewer(palette="Set1",aesthetics = "col")

p5 = ggplot(resX1, aes(x=X1, y=X2))  + geom_line(data=eqpower,aes(x=X1, y=X2,col="Optimal power"),size=1)+ geom_line(data=eqcost,aes(x=X1, y=X2,col="Maximal cost"),size=1)+ geom_point(aes(col = learner))+
    scale_colour_manual(values = c("orange", "cyan", "red", "black"), guide = guide_legend(title="",override.aes = list(linetype = c("blank", "solid", "solid","blank"),shape = c(16,NA, NA,16))))+ xlab("Participants per group") + ylab("Number of groups")


# Höhenlinien Plot Function 6
fn_nr = 6
resX1 = resX[resX$fun_nr=="6 2D Mixed Model",]

atx = at[[fn_nr]]
true_power.fun = atx$true_power.fun
actual_power = atx$actual_powerC
actual_cost = atx$actual_costC
x1 = load.cond(fn_nr,usetask,budget=NA,goal.ci=NA)
costfun=x1$cost
design=x1$design

xvals = seq(min(resX1$X1,na.rm=T),max(resX1$X1,na.rm=T),.5)
eqpower.y = sapply(xvals,function(x) {
    fn = function(y) abs(true_power.fun(c(x,y))-actual_power)
    a = optim(20,fn,method="L-BFGS-B",lower=design[[2]][[1]],upper=design[[2]][[2]])
    legit = a$value<.001
    if(legit) return(a$par) else return(NA)
})
eqpower = data.frame(X1=xvals,X2=eqpower.y)

eqcost.y = sapply(xvals,function(x) {
    fn = function(y) abs(costfun(c(x,y))-actual_cost)
    optim(20,fn,method="L-BFGS-B",lower=design[[2]][[1]],upper=design[[2]][[2]])$par})
eqcost = data.frame(X1=xvals,X2=eqcost.y)

# p6 = ggplot(resX1, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=eqpower,aes(x=X1, y=X2,col="power=.8"),size=1)+ geom_line(data=eqcost,aes(x=X1, y=X2,col="optimal cost"),size=1)

p6 = ggplot(resX1, aes(x=X1, y=X2))  + geom_line(data=eqpower,aes(x=X1, y=X2,col="Optimal power"),size=1)+ geom_line(data=eqcost,aes(x=X1, y=X2,col="Maximal cost"),size=1)+ geom_point(aes(col = learner)) +
    scale_colour_manual(values = c("orange", "cyan", "red", "black"), guide = guide_legend(title="",override.aes = list(linetype = c("blank", "solid", "solid","blank"),shape = c(16,NA, NA,16))))+ xlab("Participants per Cluster") + ylab("Number of Clusters")

p_time = ggplot(resX, aes(x= learner,y=time_used,fill=budget)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")

# Export
pdf(paste0(folder,restag,"_plot1_",usetask,".pdf"),height=6 ,width=11);p1;dev.off()
pdf(paste0(folder,restag,"_plot2_",usetask,".pdf"),height=6 ,width=11);p2;dev.off()
pdf(paste0(folder,restag,"_plot3_",usetask,".pdf"),height=6 ,width=11);p3;dev.off()
pdf(paste0(folder,restag,"_plot31_",usetask,".pdf"),height=6 ,width=11);p3.1;dev.off()
pdf(paste0(folder,restag,"_plot4_",usetask,".pdf"),height=6, width=11);p4;dev.off()
pdf(paste0(folder,restag,"_plot5_",usetask,".pdf"),height=4, width=7);p5;dev.off()
pdf(paste0(folder,restag,"_plot6_",usetask,".pdf"),height=4, width=7);p6;dev.off()
pdf(paste0(folder,restag,"_plot_time_",usetask,".pdf"),height=4, width=7);p_time;dev.off()


# Task B ------------------------------------------------------------

usetask = "B"
resX = resx[resx$task==usetask,]
# resX$budget = resX$budget |> as.numeric()
resX = resX[!is.na(resX$cost),]

# convert analytical to column
# table(resx$cost[resx$learner=="Analytical"])
# table(resx$true_power[resx$learner=="Analytical"])
# View(resx[resx$learner=="Analytical",])
resX$analytical = NA
resX$analytical[as.numeric(resX$fun_nr)==3] = 99.0805650087345
resX$analytical[as.numeric(resX$fun_nr)==4] = 150
resX$analytical_power = NA
resX$analytical_power[as.numeric(resX$fun_nr)==3] = 0.786687772969949
resX$analytical_power[as.numeric(resX$fun_nr)==4] = 0.787123625772207

resX = resX[resX$learner!="Analytical",]

# Boxplots
p1 = ggplot(resX, aes(x= learner,y=cost,fill=budget)) + geom_violin(draw_quantiles = 0.5) + geom_hline(data= resX, aes(yintercept=actual_cost, linetype="Analytical cost"), color = "red") + geom_hline(data= resX, aes(yintercept=analytical, linetype="Optimal cost"), color = "black") + facet_wrap(~ fun_nr,  scales = "free") + scale_linetype_manual(name = "", values = c(2, 2),guide = guide_legend(override.aes = list(color = c("black", "red")))) + ylab("Cost")+ xlab("Surrogate Model")


p2 = ggplot(resX, aes(x= learner,y=budget_used,fill=budget)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")


resX$CI = 1.96*resX$value.sd * 2
p3 = ggplot(resX, aes(x= learner,y=CI,fill=budget)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")+ ylab("95%CI width")

# percent "no estimate" für value.sd
no_sd = aggregate(resX$value.sd,list(resX$learner,resX$budget,resX$fun_nr),function(x) mean(is.na(x)))
names(no_sd)=c("learner","budget","fun_nr","no_sd")
p3.1 = ggplot(no_sd, aes(x= learner,y=no_sd,fill=budget)) +  geom_bar(position="dodge",stat="identity") + facet_wrap(~ fun_nr, scales = "free_x")

resy = resX[!is.na(resX$true_power),]
p4 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  geom_boxplot() + geom_hline(data= resX, aes(yintercept=actual_power, linetype="Analytical power"), color = "red") + geom_hline(data= resX, aes(yintercept=analytical_power, linetype="Optimal power"), color = "black") + facet_wrap(~ fun_nr,scales = "free_x") + scale_linetype_manual(name = "", values = c(2, 2),guide = guide_legend(override.aes = list(color = c("black", "red")))) + ylab("Power")+ xlab("Surrogate Model")

# delete outlier
resy = resy[resy$true_power>.7,]
p41 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  geom_boxplot() + geom_hline(data= resX, aes(yintercept=actual_power, linetype="Analytical power"), color = "red") + geom_hline(data= resX, aes(yintercept=analytical_power, linetype="Optimal power"), color = "black") + facet_wrap(~ fun_nr,scales = "free_x") + scale_linetype_manual(name = "", values = c(2, 2),guide = guide_legend(override.aes = list(color = c("black", "red")))) + ylab("Power")+ xlab("Surrogate Model")


# p4 = ggplot(resy, aes(x= learner,y=true_power,fill=budget)) +  geom_boxplot() + geom_hline(data= resX, aes(yintercept=actual_cost, linetype="Analytical cost"), color = "red") + geom_hline(data= resX, aes(yintercept=analytical, linetype="Optimal cost"), color = "black") + facet_wrap(~ fun_nr,scales = "free_x") + scale_linetype_manual(name = "", values = c(2, 2),guide = guide_legend(override.aes = list(color = c("black", "red")))) + ylab("Power")
#
#
# ggplot(resX, aes(x= learner,y=cost,fill=budget)) + geom_violin(draw_quantiles = 0.5) + geom_hline(data= resX, aes(yintercept=actual_cost, linetype="Analytical cost"), color = "red") + geom_hline(data= resX, aes(yintercept=analytical, linetype="Optimal cost"), color = "black") + facet_wrap(~ fun_nr,  scales = "free") + scale_linetype_manual(name = "", values = c(2, 2),guide = guide_legend(override.aes = list(color = c("black", "red")))) + ylab("Cost")

# Höhenlinien Plot Function 2
fn_nr = 2
# resX1 = resX[resX$budget=="low"& resX$fun_nr=="2 2D-t-test",]
resX1 = resX[resX$fun_nr=="2 ANOVA",]

atx = at[[fn_nr]]
true_power.fun = atx$true_power.fun
actual_power = atx$actual_power
actual_cost = atx$actual_cost
x1 = load.cond(fn_nr,usetask,budget=NA,goal.ci=NA)
costfun=x1$cost
design=x1$design

xvals = seq(min(resX1$X1,na.rm=T),max(resX1$X1,na.rm=T),.5)
eqpower.y = sapply(xvals,function(x) {
    fn = function(y) abs(true_power.fun(c(x,y))-actual_power)
    a = optim(20,fn,method="L-BFGS-B",lower=design[[2]][[1]],upper=design[[2]][[2]])
    legit = a$value<.001
    if(legit) return(a$par) else return(NA)
})
eqpower = data.frame(X1=xvals,X2=eqpower.y)

eqcost.y = sapply(xvals,function(x) {
    fn = function(y) abs(costfun(c(x,y))-actual_cost)
    optim(20,fn,method="L-BFGS-B",lower=design[[2]][[1]],upper=design[[2]][[2]])$par})
eqcost = data.frame(X1=xvals,X2=eqcost.y)
# ignore some wrong values
correct = sapply(1:nrow(eqcost),function(i) {
    costfun(eqcost[i,])==actual_cost
})
eqcost = eqcost[correct,]

# p5 = ggplot(resX1, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=eqpower,aes(x=X1, y=X2,col="Optimal power"),size=1)+ geom_line(data=eqcost,aes(x=X1, y=X2,col="Optimal cost"),size=1)
# # +  scale_fill_brewer(palette="Set1",aesthetics = "col")

p5 = ggplot(resX1, aes(x=X1, y=X2))  + geom_line(data=eqpower,aes(x=X1, y=X2,col="Minimal power"),size=1)+ geom_line(data=eqcost,aes(x=X1, y=X2,col="Optimal cost"),size=1)+ geom_point(aes(col = learner))+
scale_colour_manual(values = c("orange", "red","cyan",  "black"), guide = guide_legend(title="",override.aes = list(linetype = c("blank", "solid", "solid","blank"),shape = c(16,NA, NA,16)))) + xlab("Participants per group") + ylab("Number of groups")

# Höhenlinien Plot Function 6
fn_nr = 6
resX1 = resX[resX$fun_nr=="6 2D Mixed Model",]

atx = at[[fn_nr]]
true_power.fun = atx$true_power.fun
actual_power = atx$actual_power
actual_cost = atx$actual_cost
x1 = load.cond(fn_nr,usetask,budget=NA,goal.ci=NA)
costfun=x1$cost
design=x1$design

xvals = seq(min(resX1$X1,na.rm=T),max(resX1$X1,na.rm=T),.5)
eqpower.y = sapply(xvals,function(x) {
    fn = function(y) abs(true_power.fun(c(x,y))-actual_power)
    a = optim(20,fn,method="L-BFGS-B",lower=design[[2]][[1]],upper=design[[2]][[2]])
    legit = a$value<.001
    if(legit) return(a$par) else return(NA)
    })
eqpower = data.frame(X1=xvals,X2=eqpower.y)

eqcost.y = sapply(xvals,function(x) {
    fn = function(y) abs(costfun(c(x,y))-actual_cost)
    optim(10,fn,method="Brent",lower=design[[2]][[1]],upper=design[[2]][[2]])$par})
eqcost = data.frame(X1=xvals,X2=eqcost.y)

# p6 = ggplot(resX1, aes(x=X1, y=X2,col = learner)) + geom_point() + geom_line(data=eqpower,aes(x=X1, y=X2,col="power=.8"),size=1)+ geom_line(data=eqcost,aes(x=X1, y=X2,col="optimal cost"),size=1)

p6 = ggplot(resX1, aes(x=X1, y=X2))  + geom_line(data=eqpower,aes(x=X1, y=X2,col="Minimal power"),size=1)+ geom_line(data=eqcost,aes(x=X1, y=X2,col="Optimal cost"),size=1)+ geom_point(aes(col = learner))+
    scale_colour_manual(values = c("orange", "red","cyan", "black"), guide = guide_legend(title="",override.aes = list(linetype = c("blank", "solid", "solid","blank"),shape = c(16,NA, NA,16))))+ xlab("Participants per Cluster") + ylab("Number of Clusters")


p_time = ggplot(resX, aes(x= learner,y=time_used,fill=budget)) +  geom_boxplot() + facet_wrap(~ fun_nr, scales = "free_x")

# Export
#old format: 9:16 and 6:11
pdf(paste0(folder,restag,"_plot1_",usetask,".pdf"),height=6,width=11);p1;dev.off()
pdf(paste0(folder,restag,"_plot2_",usetask,".pdf"),height=6 ,width=11);p2;dev.off()
pdf(paste0(folder,restag,"_plot3_",usetask,".pdf"),height=6 ,width=11);p3;dev.off()
pdf(paste0(folder,restag,"_plot31_",usetask,".pdf"),height=6 ,width=11);p3.1;dev.off()
pdf(paste0(folder,restag,"_plot4_",usetask,".pdf"),height=6, width=11);p4;dev.off()
pdf(paste0(folder,restag,"_plot41_",usetask,".pdf"),height=6, width=11);p41;dev.off()
pdf(paste0(folder,restag,"_plot5_",usetask,".pdf"),height=4,width=7);p5;dev.off()
pdf(paste0(folder,restag,"_plot6_",usetask,".pdf"),height=4, width=7);p6;dev.off()
pdf(paste0(folder,restag,"_plot_time_",usetask,".pdf"),height=4, width=7);p_time;dev.off()


# Höhenlinienplot for introduction----------------------------------

dat.at = data.frame(t(atx$actually_true))

# p7 = ggplot(resX1, aes(x=X1, y=X2))  +
#     geom_line(data=eqpower,aes(x=X1, y=X2,col="Minimal power"),size=1)+
#     geom_line(data=eqcost,aes(x=X1, y=X2,col="Maximal cost"),size=1)+
#     scale_colour_manual(values = c("cyan", "red"), guide = guide_legend(title="",override.aes = list(linetype = c("solid", "solid"),shape = c(NA, NA)))) + xlab("Participants per Cluster") + ylab("Number of Clusters")

p7 = ggplot(resX1, aes(x=X1, y=X2))  +
    geom_line(data=eqpower,aes(x=X1, y=X2,col="Minimal power"),size=1)+
    geom_line(data=eqcost,aes(x=X1, y=X2,col="Maximal cost"),size=1)+
    scale_colour_manual(values = c("cyan", "red","black"), guide = guide_legend(title="",override.aes = list(linetype = c("solid", "solid","blank"),shape = c(NA, NA,3)))) + xlab("Participants per Cluster") + ylab("Number of Clusters") +  geom_point(data=dat.at, aes(x=X1, y=X2,col="Optimal set"),shape=3,size=3)

pdf(paste0(folder,"multi_true_2D.pdf"),height=6,width=11);p7;dev.off()


# Version for Presentation

p7 = ggplot(resX1, aes(x=X1, y=X2))  +
    geom_line(data=eqcost,aes(x=X1, y=X2,col="Optimal cost"),size=1)+
    geom_line(data=eqpower,aes(x=X1, y=X2,col="Power = .8"),size=1)+
    scale_colour_manual(values = c("cyan", "black","red"), guide = guide_legend(title="",override.aes = list(linetype = c("solid", "blank","solid"),shape = c(NA, 3,NA)))) + xlab("Participants per Cluster") + ylab("Number of Clusters") +  geom_point(data=dat.at, aes(x=X1, y=X2,col="Optimal set"),shape=3,size=3)

pdf(paste0(folder,"multi_true_2Dx.pdf"),height=4,width=7);p7;dev.off()





# how many NAs ------------------------------------------------------------

sapply(unique(resx$learner), function(i)  mean(is.na(resx[resx$learner==i,1])))

# a = resx[is.na(resx$cost),]

#remove NAs
resx = resx[!is.na(resx$cost),]



# Trace something ---------------------------------------------------------

# missing seeds
seeds = sapply(res,function(x) x[[1]]$seed)
max(seeds)
is.missing = function(x) return(! x %in% seeds)
missing = which(sapply(1:max(seeds),is.missing))
missing
save(missing,file="missing.Rdata")


n = which(resx$budget_used>10000&resx$task=="B"&resx$fun_nr=="5 Mixed Model")

n = which(resx$task=="B"&as.numeric(resx$fun_nr)==6)


View(resx[n,])

n = which(resx$value.sd*1.96>.03&resx$learner=="GP"&resx$task=="B"&resx$fun_nr==5)
resx$seed[n]

n = which(resx$value.sd<.001&resx$learner=="Linreg"&resx$task=="B"&resx$fun_nr==4)
resx$seed[n]

n = which(resx$budget_used==200&resx$goal.ci==0.015&learner=="Linreg")
resx$seed[n]

n = which(resx$cost>440&as.numeric(resx$fun_nr)==6)
n = which(resx$budget_used<700&as.numeric(resx$fun_nr)==6)



n = which(resx$cost>19&as.numeric(resx$fun_nr)==2)
seeds = resx[n,]$seed

for (i in seeds) {
    print(length(res[[i]][[5]]$data))
}


View(resx[n,])


View(res[[395]][[5]])
res[[395]][[5]]$data[[4]]$y |> mean()
startdat = res[[395]][[5]]$data
design = list(n = c(10,20))
t1 = svm.pred(startdat,goal=.8,design=design)
re3 =  ss.find(svm.pred,runfun=runfun,design=design,goal=goal,goal.ci=goal.ci,budget=budget, seed=seed,cost=cost,dat=startdat,fixed_cost=fixed_cost)

fn = t1$fun
a = sapply(10:40,fn)
names(a) = 10:40
a
todataframe(startdat)

