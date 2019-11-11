data {
  int<lower=0> N;
  vector[N] y1;
  vector[N] y2;
  vector[N] y3;
  vector[N] y4;
  vector[N] y5;
  vector[N] y6;
}

parameters {
  real mu0;
  real mu1;
  real mu2;
  real mu3;
  real mu4;
  real mu5;
  real mu6;
  real tao;
  real<lower=0> sigma;
}

model {
  y1 ~ normal(mu1, sigma);
  y2 ~ normal(mu2, sigma);
  y3 ~ normal(mu3, sigma);
  y4 ~ normal(mu4, sigma);
  y5 ~ normal(mu5, sigma);
  y6 ~ normal(mu6, sigma);
  mu1 ~ normal(mu0, tao);
  mu2 ~ normal(mu0, tao);
  mu3 ~ normal(mu0, tao);
  mu4 ~ normal(mu0, tao);
  mu5 ~ normal(mu0, tao);
  mu6 ~ normal(mu0, tao);
  tao ~ normal(14,5);
  mu0 ~ normal(93.5,15);
}

