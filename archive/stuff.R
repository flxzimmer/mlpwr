

res = list()

for (i in 1:10^4) {
  a = rnorm(10^2,.56)
  res[[i]] = t.test(a)$p.value
}

hist(as.numeric(res))
