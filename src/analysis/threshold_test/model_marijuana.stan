# this is the same as: threshold_test/model_flat_informative_priors_tight.stan

functions {
  real beta_ccdf(real x, real a, real b) {
    return 1 - beta_cdf(x, a, b);
  }
  
  // E[X | X > x] for X ~ beta(a,b)
  real beta_conditional_mean(real x, real a, real b) {
    return beta_ccdf(x, a+1, b) / beta_ccdf(x, a, b) * a / (a+b);
  }
}

data {
  int<lower=1> N; // number of observations
  int<lower=1> R; // number of suspect races
  int<lower=1> D; // number of counties
  
  int<lower=1,upper=R> r[N]; // race of suspect
  int<lower=1,upper=D> d[N]; // county where stop occurred
  int<lower=0,upper=1> legal[N]; // is marijuana legal?
  
  int<lower=1> n[N]; // # of stops
    int<lower=0> s[N]; // # of searches
    int<lower=0> h[N]; // # of successful searches (hits)
}

parameters {	
  // hyperparameters
  vector<lower=0>[R] sigma_t;

  // search thresholds
  vector[R] t_r;
  vector[N] t_i_raw;
  
  vector[R] t_legal_r;
  vector[R] phi_legal_r;
  vector[R] lambda_legal_r;
  
  // parameters for signal distribution
  vector[R] phi_r;
  vector[D-1] phi_d_raw;
  
  vector[R] lambda_r; 
  vector[D-1] lambda_d_raw;
}

transformed parameters {
  vector[D] phi_d;
  vector[D] lambda_d;
  vector[N] phi;
  vector[N] lambda;
  vector<lower=0, upper=1>[N] search_rate;
  vector<lower=0, upper=1>[N] hit_rate;
  vector<lower=0, upper=1>[N] t_i;
  
  phi_d[1]      = 0;
  phi_d[2:D]    = phi_d_raw;
  lambda_d[1]   = 0;
  lambda_d[2:D] = lambda_d_raw;
  
  for (i in 1:N) {	
    real a;
    real b;
    
    // implies t_i[i] ~ logit-normal(t_r[r[i]] + legal[i] * t_legal_r[r[i]], sigma_t[r[i]])
    t_i[i] = inv_logit(t_r[r[i]] + t_i_raw[i] * sigma_t[r[i]] + legal[i] * t_legal_r[r[i]]);
    
    // signal distribution parameters	
    phi[i]    = inv_logit(phi_r[r[i]] + phi_d[d[i]] + legal[i] * phi_legal_r[r[i]]);
    lambda[i] = exp(lambda_r[r[i]] + lambda_d[d[i]] + legal[i] * lambda_legal_r[r[i]]);
    
    // transformed signal distribution parameters
    a = lambda[i] * phi[i];
    b = lambda[i] * (1 - phi[i]);
    
    // implied search and hit rates
    search_rate[i] = beta_ccdf(t_i[i], a, b);
    hit_rate[i]    = beta_conditional_mean(t_i[i], a, b);
  }
}

model {  
  // Draw threshold hyperparameters
  sigma_t ~ normal(0, 0.25);
  t_r     ~ normal(-2, 1);
  
  t_legal_r ~ normal(0, 1);
  phi_legal_r ~ normal(0, 1);
  lambda_legal_r ~ normal(0, 1);
  
  // Draw race and department specific thresholds
  t_i_raw ~ normal(0, 1);           // implies t_i[i] ~ logit-normal(t_r[r[i]] + legal[i] * t_legal_r[r[i]], sigma_t[r[i]])
  
  // Draw race parameters
  phi_r    ~ normal(-3.5, 0.25);
  lambda_r ~ normal(2, 0.5);
  
  // Draw department parameters (for un-pinned departments)
  phi_d_raw    ~ normal(0, 0.25);    
  lambda_d_raw ~ normal(0, 0.5);     
  
  // Draw search and hit observations
  s ~ binomial(n, search_rate);
  h ~ binomial(s, hit_rate);
}



generated quantities {
  // Stop-weighted per-race parameters
  vector[R] thresholds;
  
  {
    vector[R] counts;
    vector[D] dep_stops;
    
    thresholds = rep_vector(0, R);
    counts     = rep_vector(0, R);
    dep_stops  = rep_vector(0, D);
    
    
    // calculate total stops per department
    for (i in 1:N) {
      dep_stops[d[i]] = dep_stops[d[i]] + n[i];
    }
    
    for (i in 1:N) {
      thresholds[r[i]] = thresholds[r[i]] + t_i[i]*dep_stops[d[i]];
      counts[r[i]]     = counts[r[i]] + dep_stops[d[i]];
    }
    thresholds = thresholds ./ counts;
  }
}
