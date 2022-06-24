data {
int<lower=1> num_person;
int<lower=1> num_item;
int<lower=0, upper=1> U[num_person, num_item];
}
parameters {
vector[num_person] theta;
vector[num_item] beta;
real mu_beta;
real<lower=0> sigma2_theta;
real<lower=0> sigma2_beta;
}
transformed parameters {
real<lower=0, upper=1> prob_solve[num_person, num_item];
for (p in 1:num_person)
for (i in 1:num_item)
prob_solve[p, i] = inv_logit(beta[i] - theta[p]);
}
model {
for (p in 1:num_person)
for (i in 1:num_item)
U[p, i] ~ bernoulli(prob_solve[p, i]);
theta ~ normal(0, sqrt(sigma2_theta));
beta ~ normal(mu_beta, sqrt(sigma2_beta));
sigma2_theta ~ inv_chi_square(0.5);
sigma2_beta ~ inv_chi_square(0.5);
}
