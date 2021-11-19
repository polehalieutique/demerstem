#'  mixdist_polymod
#'
#' \code{mixdist_polymod} returns the polymodal decomposition of length frequencies in age class
#'
#' @param   data_freq       input dataset table
#' @param   title           fraction of the title in the plots
#' @param   K               value for K
#' @param   L_inf           value for L_inf
#' @param   t0              value for t0
#' @param   fix_mu          Fix values for mean
#' @param   fix_sigma       Fix values for standard deviation
#' @param   lmsd            just for this one
#' @param   ngroup          number of age class
#' @param   step_class      length step between two length class
#'
#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

mixdist_polymod <- function(data_freq, K, L_inf, t0, fix_mu, fix_sigma, lmsd, ngroup, step_class, step_time, month_recrue, get_lmsd = FALSE, plot = FALSE, sigma_adjust = 0){
  print("Polymodal decomposition of length frequencies")

  data_mix <- as.list(list(NULL))
  data_count <- as.list(list(NULL))
  length_min <- 0
  length_max <- max(data_freq$lclass)
  sd_lm <- data.frame(NULL)
  Matrice_Capture <- as.data.frame(NULL)
  mix_constraint_musd <- as.list(list(NULL))
  mix <- as.list(list(NULL))
  mean_constraint_musd <- as.list(list(NULL))
  mean <- as.list(list(NULL))
  prop_constraint_musd <- as.list(list(NULL))
  prop <- as.list(list(NULL))
  sd_constraint_musd <- as.list(list(NULL))
  sd <- as.list(list(NULL))

  # indice croissance pour VB dynamique
  data_freq <- data_freq %>% mutate(indice_croissance = case_when( month - month_recrue >= 0 ~ month - month_recrue,
                                                                   month - month_recrue < 0 ~ 12 - (month_recrue - month)))
  if (step_time != 13) {
    data_freq <- data_freq %>% mutate(quarter =  ceiling((data_freq$indice_croissance+1)/(12/step_time)))
    data_freq$step_time <- data_freq$quarter
  }
  else {data_freq$step_time <- data_freq$month}

  #data_freq <- data_freq %>%  mutate(total = total * ctot)

  # mise en forme data pour décomposition polymodale avec mixdist

  for (i in sort(unique(data_freq$step_time))){
    data_count[[i]] <- rep(data_freq$lclass[data_freq$step_time==i],data_freq$total[data_freq$step_time==i])
    data_mix[[i]] <- mixgroup(data_count[[i]],breaks=c(length_min, seq(length_min + step_class, length_max - step_class, step_class), length_max), xname="lclass")
  }
  if (step_time != 13) {
    ALK <- cbind(data.frame(matrix(0, nrow = length(data_mix[[1]]$lclass), ncol = 3,
                                   dimnames = list(NULL, c("lclass", "count", "prop_obs")))),
                 data.frame(matrix(0, nrow = length(data_mix[[1]]$lclass), ncol = (ngroup*step_time),
                                   dimnames = list(NULL, paste0("Age_", seq(1, ngroup + 1 - 1/step_time, by = 1/step_time))))))
    Matrice_Capture <- ALK
  }

  for (i in sort(unique(data_freq$step_time))){
    print (i)
    #if by quaters for example

    if(is.null(data_mix[[i]])==T) {
      mix_constraint_musd[[i]] <- NULL
      mean_constraint_musd[[i]] <- NULL
      prop_constraint_musd[[i]] <- NULL
      sd_constraint_musd[[i]] <- NULL

      mix[[i]] <- NULL
      mean[[i]] <- NULL
      prop[[i]] <- NULL
      sd[[i]] <- NULL
    }
    ### Computation of mean lengths Lt
    if (step_time!=1 & step_time !=12) {
      if (i != max(data_freq$step_time)) {
        vb <- (seq(1 + (unique(data_freq$step_time[data_freq$step_time == i])-1)/max(data_freq$step_time), ngroup + (unique(data_freq$step_time[data_freq$step_time == i])-1)/max(data_freq$step_time), by=1) +
                 seq(1 + (unique(data_freq$step_time[data_freq$step_time == i+1])-1)/max(data_freq$step_time), ngroup + (unique(data_freq$step_time[data_freq$step_time == i+1])-1)/max(data_freq$step_time), by=1))/2
      }
      else {
        vb <- (seq(1 + (unique(data_freq$step_time[data_freq$step_time == i])-1)/max(data_freq$step_time), ngroup + (unique(data_freq$step_time[data_freq$step_time == i])-1)/max(data_freq$step_time), by=1) +
                 seq(1 + (unique(data_freq$step_time[data_freq$step_time == i-1])-1)/max(data_freq$step_time), ngroup + (unique(data_freq$step_time[data_freq$step_time == i-1])-1)/max(data_freq$step_time), by=1))/2 + ((unique(data_freq$step_time[data_freq$step_time == i])-1)/max(data_freq$step_time) - (unique(data_freq$step_time[data_freq$step_time == i-1])-1)/max(data_freq$step_time))
      }
    }
    if (step_time ==1)  {
      vb <- seq(1 + (unique(data_freq$step_time[data_freq$step_time == i])-1)/max(data_freq$step_time), ngroup + (unique(data_freq$step_time[data_freq$step_time == i])-1)/max(data_freq$step_time), by=1) + 0.5
    }
    if (step_time==12) {
      vb <- seq(1 + (unique(data_freq$step_time[data_freq$step_time == i])-1)/max(data_freq$step_time), ngroup + (unique(data_freq$step_time[data_freq$step_time == i])-1)/max(data_freq$step_time), by=1)
    }
    L <- L_inf*(1-exp(-K*(vb - t0)))
    print(vb)
    print(L)
    if (get_lmsd == F){
      sigma <- predict(lmsd, data.frame(age = vb)) + sigma_adjust # for sigma
    }
    else {
      sigma <- rep(5, ngroup)
    }
    param <- mixparam(L,sigma)

    mix[[i]] <- mix(data_mix[[i]], param, dist="norm", emsteps=100, mixconstr(conmu="MFX", fixmu= fix_mu,
                                                                              consigma="SFX", fixsigma=fix_sigma))#fitting function
    #plot(mixpagrus4)
    mean[[i]] <- mix[[i]]$parameters$mu
    prop[[i]] <- mix[[i]]$parameters$pi
    sd[[i]] <- mix[[i]]$parameters$sigma

    if (get_lmsd==T) {
      sd_lm <- rbind(sd_lm, data.frame(sd = mix[[i]]$parameters$sigma, age = vb))
    }
    if(step_time!=12) {
      ALK[,1:3] <- data_mix[[i]] %>%  mutate(prop_obs = count/sum(count, na.rm = T)) # might be removed
      vect <- NULL
      for (k in 1:ngroup) {
        for (l in 1:length(ALK$lclass)) {
          if (l ==1) {
            ALK[l, 3+i+(step_time*(k-1))] <- prop[[i]][k] * pnorm(ALK$lclass[l], mean = mean[[i]][k], sd = sd[[i]][k])
          }
          else if (l == max(length(ALK$lclass))) {
            ALK[l, 3+i+(step_time*(k-1))] <-  prop[[i]][k] * (1-pnorm(ALK$lclass[l-1], mean = mean[[i]][k], sd = sd[[i]][k]))
          }
          else {
            ALK[l, 3+i+(step_time*(k-1))] <- prop[[i]][k]  * (pnorm(ALK$lclass[l], mean = mean[[i]][k], sd = sd[[i]][k]) - pnorm(ALK$lclass[l-1], mean = mean[[i]][k], sd = sd[[i]][k]) )
          }
        }
        vect <- c(vect, 3+i+(step_time*(k-1)))
      }

      ALK[,vect] <- t(apply(ALK[,vect], 1, function(i) i/sum(i)))
      ALK_trim <- apply(ALK[,4:(3+ngroup)], 1, sum)

      Matrice_Capture[,vect] <- data_mix[[i]]$count * ALK[,vect]

      assign("Matrice_Capture", Matrice_Capture, envir=globalenv())
      assign("ALK", ALK, envir=globalenv())
    }

    #Matrice_Capture <- rbind(Matrice_Capture, t(as.matrix(ALK$count)) %*% as.matrix(ALK[,4:(4 + ngroup -1)]))

  }




  if (step_time == 12) {
    Matrice_Capture <- as.data.frame(NULL)
    for (month in sort(unique(data_freq$step_time))) {

      ALK <- data_mix[[month]] %>%  mutate(prop_obs = count/sum(count, na.rm = T))



      for (u in 1:ngroup) {
        ALK <- cbind(ALK, data.frame(age = rep(0, length(ALK$lclass))))
      }
      names(ALK)[4:ncol(ALK)]<- paste0(rep("Age_"), 1:ngroup)

      for (k in 1:ngroup) {
        for (l in 1:length(ALK$lclass)) {
          if (l ==1) {
            ALK[l,3+k] <- prop[[month]][k] * pnorm(ALK$lclass[l], mean = mean[[month]][k], sd = sd[[month]][k])
          }
          else if (l == max(length(ALK$lclass))) {
            ALK[l,3+k] <-   prop[[month]][k] * (1 -pnorm(ALK$lclass[l-1], mean = mean[[month]][k], sd = sd[[month]][k]))
          }
          else {
            ALK[l,3+k] <- prop[[month]][k]  * ( pnorm(ALK$lclass[l], mean = mean[[month]][k], sd = sd[[month]][k]) - pnorm(ALK$lclass[l-1], mean = mean[[month]][k], sd = sd[[month]][k]) )
          }
        }
      }

      ALK[,4:(3+ngroup)] <- t(apply(ALK[,4:(3+ngroup)], 1, function(i) i/sum(i)))
      Matrice_Capture <- rbind(Matrice_Capture, cbind(ALK[,1:3], data_mix[[month]]$count * ALK[,4:(4+ngroup-1)]))

      assign("Matrice_Capture", Matrice_Capture, envir=globalenv())
      #assign("ALK", ALK, envir=globalenv())

    }# We'll see next time if I put this function inside, because of the apply function which might nessitate some change to get the catch structure inside
  }
  # D  <- c(S_final, K_final, L_final)
  # names(D) <- c("S_final", "K_final", "L_final")
  # # list2env(D, envir = .GlobalEnv)
  # return(D)
  if (plot == TRUE) {
    for (i in sort(unique(data_freq$step_time))) {
      print(i)
      if(step_time==12){
        hist(data_count[[i]], breaks=c(length_min,seq(length_min + step_class, length_max - step_class, step_class),length_max),xlab = paste0(espece, " length (cm)"), ylab = "Relative frequency", main = paste0("Polymodal decomposition : Month ",i), col = "lightgrey", freq = FALSE, cex.axis = 1.3, cex.lab = 1.3, ylim = c(0,0.4))
      }
      else{
        hist(data_count[[i]], breaks=c(length_min,seq(length_min + step_class, length_max - step_class, step_class),length_max),xlab = paste0(espece, " length (cm)"), ylab = "Relative frequency", main = paste0("Polymodal decomposition : Period ",i), col = "lightgrey", freq = FALSE, cex.axis = 1.3, cex.lab = 1.3, ylim = c(0,0.4))
      }
      lgen <- seq(0,length_max, by = 0.01)
      for (k in (1:ngroup)){
        # dens1 <- dnorm(lgen,mean = mean[[i]][k],sd = sd[[i]][k])
        # dens.rel1 <- dens1/sum(dens1) * prop[[i]][k] * 100
        # lines(lgen,dens.rel1,col = "black", type = "l", lwd = 2)
        dens <- dnorm(lgen, mean = mean[[i]][k], sd = sd[[i]][k])
        dens.rel <- dens/sum(dens) * prop[[i]][k] * 100
        lines(lgen,dens.rel,col = "black", type = "l", lty=1, lwd = 2)
      }
    }    #legend(x = 15, y = 0.3, legend = c("No constraints", "Constraints on means"), lty = c(1,2), lwd = c(2,2), bty = "n", cex = 1.3)
  }
  assign("sd_lm", sd_lm, envir=globalenv())
  assign("data_mix", data_mix, envir=globalenv())
  assign("sd", sd, envir=globalenv())
  assign("mean", mean, envir=globalenv())
  assign("prop", prop, envir=globalenv())

}
