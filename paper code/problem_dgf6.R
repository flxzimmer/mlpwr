# Problem DGF6: "Konvergiert gegen falschen Wert"

# Beobachtung: Der tatsächlich beste Wert für DGF6 ist:
load(file= paste0(folder,"at.Rdata"))
at = at[[6]]
at$actually_true
at$actual_power
at$actual_cost


# Aber die meisten Ergebnisse bei High Budget sind:

# (aus simulationstudy_export_new)

a = resx
a = a[!is.na(a$cost),]
a1 = a[a$budget=="high",]
re = sapply(a1$value,function(x) paste(x[1],x[2]))
table(re)


# analyse:
x1 = load.cond(6,"B",1234124,NA)
costfun=x1$cost

val = c(25,15)
at$true_power.fun(val)
costfun(val)

val = c(26,13)
at$true_power.fun(val)
costfun(val)

val = c(24,16)
at$true_power.fun(val)
costfun(val)



# specifications of old simulation study ----------------------------------

# was runseach  = 10^4, damit kann man es nicht gut abschätzen!
#jetzt:

runseach= 10^5

# variance at p=.8 is:
v11=.8*(1-.8)/runseach
oldval = .8023547 # power of old optimal value
oldval - sqrt(v11)*2

#versuche mit mehr iterationen das Problem zu lösen, siehe fun6_sim_2.R
