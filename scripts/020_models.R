###########################################################################
# Compartmental Models in POMP
###########################################################################

# Models used for flu A


# ____________________________
#
## - Deterministic Model - Multiple betas ----
# ____________________________

## note that epoch terminology here is called interval in the article

model_Flu_deter_multi <- function(data, df_covar, ...){
  # note make beta as beta per epoch when testing periods via covar mechanism
  
  seir.ode <- Csnippet("
    
    double beta = (epoch == 1)*beta1 + 
      (epoch == 2)*beta2 + 
      (epoch == 3)*beta3 +
      (epoch == 4)*beta4 + 
      (epoch == 5)*beta5;
    
    double lambda = beta*S*(I + H0)/N;
    
    DS = -lambda;
    DE = lambda - epsilon*E;
    DI = epsilon*E - delta*I;
    DH0 = delta*I*(1-h) - gamma0*H0;
    DH1 = delta*I*h - gamma1*H1;
    DR = gamma0*H0 + gamma1*H1;
    DC = delta*I*h;

  ")
  
  seir_dmeas <- Csnippet("
  
    double ll = dnbinom_mu(reports, k, rf*C , give_log);

    if(epoch != epoch_test) {
      lik = 0;
    } else {
      lik = ll;
    }
    
    if(epoch_test == 0)
      lik = ll;
    
  ")
  
  model <- 
    data %>% 
    pomp(times="day",t0=0,
         skeleton=vectorfield(seir.ode),
         dmeasure=seir_dmeas,
         accumvars="C",
         covar=covariate_table(df_covar, order="constant", times="day"),
         statenames=c("S","E","I","H0","H1","R","C"),
         paramnames=c("epsilon","delta","gamma0","gamma1",
                      "N","h",
                      "rf","k",
                      "beta1",
                      "beta2",
                      "beta3",
                      "beta4",
                      "beta5",
                      "epoch_test")
    ) 
  
  # return the pomp model
  model
}
