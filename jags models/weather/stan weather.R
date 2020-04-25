data {
    dim_d <- dim(dead)

    # extract dimensions for loops
    nc <- dim_d[2]
    nt <- dim_d[1]

    dim_D <- dim(D)
    nD <- dim_D[2]


    # Define number of days after infection in which
    # transmission and death is still considered in model
    SId_T <- 50

    # t --> 1 ... Tinit ...       ... Ti ...       ... T0 ...      ... T
    #                      init i           start i          start d



}

model
{
    # Distribution of deaths over time --------------------------------------------------
    for(k in 1:SId_T){
        #note: Safed in reverse from SId_T to 1
       SId[SId_T+1-k] <- dnorm(k,mean_SId, 1/sd_SId^2)
    }

    # likelihood of transmission over time ------------------------------------
    for(k in 1:transmission_T){
        #note: Safed in reverse from transmission_T to 1
       transmission[(transmission_T+1)-k] <- dgamma(k,mean_transmission, shape_transmission)
    }



    for(c in 1:nc){

        # Before modeling [1,Tinit] ---------------------------------------------------------
        for (t in 1:(Tinit[c]-1)){
            i[t,c] <- 0
        }


        # Initialize i [Tinit,Ti] ---------------------------------------------------------
        for (t in Tinit[c]:(Ti[c]-1)){
            i[t,c] ~ dexp(1/(tau[c]))
        }

        #  Model period for i [Ti,T] ----------------------------------------------
        for (t in Ti[c]:T[c]){

            iload[t,c] <- sum(
                inprod(
                    #note: transmission stored with reverse indeces
                    i[max(1,t-transmission_T):(t-1),c],
                    transmission[max(1,transmission_T-t+2):transmission_T])
            )

            # current reproduction rate
            R[t,c] <- R0[c]*Rtime[t]*prod(effect[t,,c])


            # expected infected by sum_k i[t-k]*transmission[k]
            Ei[t,c] <-
                R[t,c]*iload[t,c]


            # infected is a draw from Expected infected
            i[t,c] ~ dpois(Ei[t,c])

        }



        #  Model period for d [T0,T] ----------------------------------------------

        for (t in T0[c]:T[c]){


            dload[t,c] <- sum(
                inprod(
                    #note: SId stored with reverse indezes (k = SId_T-k)
                    i[max(1,t-SId_T):(t-1),c],
                    SId[max(1,SId_T-t+2):SId_T]
                ))

            # expcted dead by sum_k i[t-k]*SId[k]
            Ed[t,c] <- frate[t,c]*dload[t,c]

            dead[t,c] ~ dpois(Ed[t,c]+0.01)

            # Include measurement error
            #real_dead[t,c] ~ dpois(Ed[t,c]+0.01)
            #sd_measurement[t,c] <- measurement_error_factor * max(real_dead[t,c],1)
            #dead[t,c] ~ dnorm(real_dead[t,c],1/sd_measurement[t,c]^2)
        }
    }




    # PRIORS -----------------------------------------------------------


    # Basic reproduction rate -------------------------------------------------

    for(c in 1:nc)
    {
        R0[c] ~ dnorm(3, 1/3^2)
    }

    for(t in min(Ti):max(T))
    {
        Rtime[t] ~ dunif(0,1)
    }

    # R noise -----------------------------------------------------------------
#
#     for (c in 1:nc){
#         for (t in Ti[c]:T[c]){
#             Rnoise[t,c]~dunif(0,1)
#         }
#     }

    # treatment effect --------------------------------------------------------

    for(d in 1:nD)
    {
        #uncomment for Lasso
        beta[d] ~ dnorm(0,1/psi[d])
        psi[d] ~ dexp(2)

        ### uncomment for Ishara
        # # theta sim dnorm(0,taustar/Ik)
        # # same as
        # # theta sim dnorm(0,1/(Ik*tau^2))
        # beta[d] ~ dnorm(0,tauly[d]/lambdaly[d])
        #
        # # taustar sim gamma
        # # same as
        # # tau^-2 sim dgamma
        # tauly[d] ~ dgamma(9,4)
        #
        # # wk sim uni(0,1)
        # gammaly[d] ~ dbeta(1,1)
        # # Ik sim (1-wk)*d0 + wk*d1 with d0=.005 as in Ishwaran p. 7
        # lambdaly[d] <- (1-gammaly[d])*0.005 + gammaly[d]
        #


        for(c in 1:nc){

            for (t in Ti[c]:T[c]){
                effect[t,d,c] <- ifelse(D[t,d,c],1-beta[d],1)
                            }
        }
    }

    # frate -------------------------------------------------------------------

    # sd of daily change
    sd_frate ~ dunif(frate_sd_min,frate_sd_max)

    for(c in 1:nc){
        frate[T0[c],c] ~ dunif(frate_min,frate_max)
        for(t in (T0[c]+1):T[c]){
            # frate truncated between frate_min and frate_max
            frate_change[t,c] ~ dnorm(0,1/sd_frate^2) T(frate_min-frate[t-1,c],frate_max-frate[t-1,c])
            frate[t,c]<-frate[t-1,c]+frate_change[t,c]
        }
    }

    # Intervals ---------------------------------------------------------------

    # SId: Serial interval from transmission to death
    sd_SId ~ dunif(2,5)
    mean_SId ~ dunif(mean_SId_min,mean_SId_max)

    # transmission: distribution of transmissions over time
    mean_transmission ~ dunif(1, 7)
    shape_transmission <- 1


    # initial infections ------------------------------------------------------
    for(c in 1:nc){
        tau[c] ~ dunif(0,tau_max)
    }

    # output ------------------------------------------------------------------
    transmission_dist ~ dgamma(mean_transmission, shape_transmission)
    SId_dist ~ dnorm(mean_SId, 1/sd_SId^2)
}


# Literature for priors ---------------------------------------------------
#Estimates of the severity of coronavirus disease 2019 Lancet
#mean duration from onset of symptoms to death to be 17·8 days (95% credible interval [CrI] 16·9–19·2
#
#https://github.com/HopkinsIDD/ncov_incubation
#JHU-IDD	log-normal		1.62	0.42
#JHU-IDD	gamma        	5.81	0.95
#ar1 and par2 are log-mean and log-sd of the log-normal distribution, while they are the shape and scale parameters for the gamma,
