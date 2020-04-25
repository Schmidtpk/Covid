data {
    dim_d <- dim(dead)

    # extract dimensions for loops
    nc <- dim_d[2]
    nt <- dim_d[1]


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
            Ed[t,c] <- frate[c]*dload[t,c]


            dead[t,c] ~ dpois(Ed[t,c]+0.01)
        }
    }




    # PRIORS -----------------------------------------------------------


    # rate R -----------------------------------------------------------------------

    # +declining prior for R---------------------------------------------------------
    # for (c in 1:nc){
    #     Tall[c]<-T[c]-Ti[c]
    #     for(t in Ti[c]:T[c]){
    #         Rmax[t,c] <- 2+2*(Tall[c]-(t-Ti[c]))/Tall[c]
    #         R[t,c] ~ dunif(0,Rmax[t,c])
    #     }
    # }
    # +fixed prior for R---------------------------------------------------------
    for (c in 1:nc){
        for(t in Ti[c]:T[c]){
            R[t,c] ~ dunif(0,4)
        }
    }


    # Intervals ---------------------------------------------------------------

    # SId: Serial interval from transmission to death
    sd_SId ~ dunif(2,5)
    mean_SId ~ dunif(20,25)

    # # fatality rate
    for(c in 1:nc){
        frate[c] ~ dunif(0,.05)
    }


    # transmission: distribution of transmissions over time
    mean_transmission ~ dunif(4, 7)
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
