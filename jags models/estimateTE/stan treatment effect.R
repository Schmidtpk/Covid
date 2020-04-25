data {
    dim_d <- dim(dead)

    # extract dimensions for loops
    nc <- dim_d[2]
    nt <- dim_d[1]

    dim_D <- dim(D)
    #D array with time*treatment*country
    nd <- dim_D[2]

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
       transmission[(transmission_T+1)-k] <- dgamma(k,shape_transmission, rate_transmission)
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
            R[t,c] <-
                R0[c]*prod(effect[t,,c])*time_effect[t]

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

            real_dead[t,c] ~ dpois(Ed[t,c]+0.01)
            sd_measurement[t,c] <- measurement_error_factor * max(real_dead[t,c],1)
            dead[t,c] ~ dnorm(real_dead[t,c],1/sd_measurement[t,c]^2)
        }
    }




    # PRIORS -----------------------------------------------------------

    # Basic reproduction rate -------------------------------------------------

    for(c in 1:nc)
    {
        R0[c] ~ dnorm(3, 1/3^2)
    }


    # Time FEs on reproduction rate ---------------------------------

    # loops over all t that model infections
    for (t in min(Ti):max(T)){
        time_effect[t] ~ dunif(0,2)
    }


    # treatment effect --------------------------------------------------------

    for(d in 1:nd)
    {
        #uncomment for constant TE
        #beta[d] ~ dunif(0,1)

        for(c in 1:nc){
            # country specific TE
            beta[d,c] ~ dunif(0,1)
            for (t in 1:nt){
                effect[t,d,c] <- ifelse(D[t,d,c],1-beta[d,c],1)
                #effect[t,d,c] <- ifelse(D[t,d,c],1-beta[c],1)
            }
        }
    }


    # frate -------------------------------------------------------------------

    # sd of daily change
    sd_frate ~ dunif(10^(-4),100*10^(-4))

    for(c in 1:nc){
        frate[T0[c],c] ~ dunif(0,.05)
        for(t in (T0[c]+1):T[c]){
            # frate truncated between 0.001 and 0.05
            frate_change[t,c] ~ dnorm(0,1/sd_frate^2) T(0.001-frate[t-1,c],0.05-frate[t-1,c])
            frate[t,c]<-frate[t-1,c]+frate_change[t,c]
        }
    }


    # Intervals ---------------------------------------------------------------

    # SId: Serial interval from transmission to death
    sd_SId ~ dunif(2,5)
    mean_SId ~ dunif(20,25)

    # transmission: distribution of transmissions over time
    mean_transmission ~ dunif(4, 7)
    sd_transmission ~ dunif(1,3)

    #mean = shape/rate
    #sd2 = shape / rate2 -> sd2 = mean/rate -> rate=mean/sd2

    shape_transmission <- mean_transmission*rate_transmission
    rate_transmission <- mean_transmission/sd_transmission^2


    # initial infections ------------------------------------------------------
    for(c in 1:nc){
        tau[c] ~ dunif(0,tau_max)
    }

    # output ------------------------------------------------------------------
    transmission_dist ~ dgamma(shape_transmission, rate_transmission)
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
