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
#' @param   age             ## May be removed
#'
#' @examples
#'  data(freq_landings)
#'  data_freq <- freq_landings
#'  espece <- 'Pseudotolithus senegalensis'
#'  ngroup <- 3
#'
#' S_landings <- 28.76
#' K_landings <- 0.36
#' L_landings <- 50.4
#' t0_landings <- -0.22
#' a <- 0.005
#' b <- 3.16
#'
#'
#' data_freq <- data_freq %>%  mutate(weight_sampling = total_sampling * a*lclass^b/1000)
#' #data_catch_month <- data.frame(Wtot = rep(c(1000,2000,3000,5000,1000,3000),4), year = rep(rep(c(2000,2001),3),4), month = rep(seq(1:12),2))
#' data_catch_month <- data.frame(Wtot = rep(180000,12), month = rep(seq(1:12)))
#' data_catch_month <- data_catch_month %>% group_by(month) %>% dplyr::summarise(Wtot= mean(Wtot)) #mean over the years
#' data_freq <- merge(data_freq, data_catch_month, by = "month")
#' data_freq <- data_freq %>% group_by(month) %>%  dplyr::mutate(W_sampling = sum(weight_sampling),
#'                                                               N_sampling = sum(total_sampling),
#'                                                               Ntot= floor(Wtot * N_sampling/W_sampling),
#'                                                               total = floor(total_sampling*Ntot/N_sampling))
#' ngroup <- 3
#' fixmu <- rep(T, ngroup)
#' fixsigma <- rep(F, ngroup)
#' step_class <- 1
#' step_time <- 4
#' month_recrue <- 3
#' mixdist_polymod(data_freq, K = K_landings, L_inf = L_landings, t0 = t0_landings,
#'                 step_class = 1, step_time = 4, ngroup = 3, month_recrue = 3,
#'                 fix_mu = fixmu, fix_sigma = fixsigma, sigma_adjust = 0,
#'                 lmsd = lmsd, get_lmsd = T, plot = T)
#'
#' @export
mixdist_polymod <- function (data_freq, K, L_inf, t0, fix_mu, fix_sigma, lmsd,
                             ngroup, step_class, step_time, month_recrue = 1, get_lmsd = FALSE,
                             plot = FALSE, sigma_adjust = 1, age = 0)
{

  print("Polymodal decomposition of length frequencies")
  data_mix <- as.list(list(NULL))
  data_count <- as.list(list(NULL))
  data_count_gear <- as.list(list(NULL))
  length_min <- 0
  length_max <- max(data_freq$lclass)
  sd_lm <- data.frame(NULL)
  Matrice_Capture <- as.list(list())
  mix_constraint_musd <- as.list(list(NULL))
  mix <- as.list(list(NULL))
  mean_constraint_musd <- as.list(list(NULL))
  mean <- as.list(list(NULL))
  prop_constraint_musd <- as.list(list(NULL))
  prop <- as.list(list(NULL))
  sd_constraint_musd <- as.list(list(NULL))
  sd <- as.list(list(NULL))


  if (!is.null(data_freq$gear)){

    #if (step_time == 1) {
    data_freq <- data_freq %>% ungroup() %>% group_by(step_time,lclass) %>%
      mutate(freq_gear = total_gear/sum(total_gear))
    #}
    # else {
    #   data_freq <- data_freq %>% ungroup() %>% group_by(step_time, gear,lclass) %>%
    #     mutate(tot_freq_gear = sum(total_gear)) %>% ungroup() %>% group_by(step_time, lclass) %>%  mutate(freq_gear = tot_freq_gear/sum(total_gear))
    # }
  }
  print(unique(data_freq$step_time))
  for (i in sort(unique(data_freq$step_time))) {
    data_count[[i]] <- rep(data_freq$lclass[data_freq$step_time ==
                                              i], data_freq$total_gear[data_freq$step_time == i])
    data_mix[[i]] <- mixgroup(data_count[[i]], breaks = c(length_min,
                                                          seq(length_min + step_class, length_max - step_class,
                                                              step_class), length_max), xname = "lclass")
  }
  ALK <- cbind(data.frame(matrix(0, nrow = length(data_mix[[1]]$lclass),
                                 ncol = 3, dimnames = list(NULL, c("lclass", "count",
                                                                   "prop_obs")))), data.frame(matrix(0, nrow = length(data_mix[[1]]$lclass),
                                                                                                     ncol = (ngroup * step_time), dimnames = list(NULL, paste0("Age_",
                                                                                                                                                               seq(age, ngroup + age - 1/step_time, by = 1/step_time))))))
  if(!is.null(data_freq$gear)){
    for (w in 1:length(unique(data_freq$gear))){
      Matrice_Capture[[w]] <- ALK
    }
  }
  else {
    Matrice_Capture[[1]] <- ALK
  }
  for (i in sort(unique(data_freq$step_time))) {
    print(i)
    if (is.null(data_mix[[i]]) == T) {
      mix_constraint_musd[[i]] <- NULL
      mean_constraint_musd[[i]] <- NULL
      prop_constraint_musd[[i]] <- NULL
      sd_constraint_musd[[i]] <- NULL
      mix[[i]] <- NULL
      mean[[i]] <- NULL
      prop[[i]] <- NULL
      sd[[i]] <- NULL
    }
    if (step_time != 1 & step_time != 12) {
      if (i != max(data_freq$step_time)) {
        vb <- (seq(age + (unique(data_freq$step_time[data_freq$step_time ==
                                                       i]) - 1)/max(data_freq$step_time), ngroup -
                     1 + age + (unique(data_freq$step_time[data_freq$step_time ==
                                                             i]) - 1)/max(data_freq$step_time), by = 1) +
                 seq(age + (unique(data_freq$step_time[data_freq$step_time ==
                                                         i + 1]) - 1)/max(data_freq$step_time), ngroup -
                       1 + age + (unique(data_freq$step_time[data_freq$step_time ==
                                                               i + 1]) - 1)/max(data_freq$step_time), by = 1))/2
      }
      else {
        vb <- (seq(age + (unique(data_freq$step_time[data_freq$step_time ==
                                                       i]) - 1)/max(data_freq$step_time), ngroup -
                     1 + age + (unique(data_freq$step_time[data_freq$step_time ==
                                                             i]) - 1)/max(data_freq$step_time), by = 1) +
                 seq(age + (unique(data_freq$step_time[data_freq$step_time ==
                                                         i - 1]) - 1)/max(data_freq$step_time), ngroup -
                       1 + age + (unique(data_freq$step_time[data_freq$step_time ==
                                                               i - 1]) - 1)/max(data_freq$step_time), by = 1))/2 +
          ((unique(data_freq$step_time[data_freq$step_time ==
                                         i]) - 1)/max(data_freq$step_time) - (unique(data_freq$step_time[data_freq$step_time ==
                                                                                                           i - 1]) - 1)/max(data_freq$step_time))
      }
    }
    if (step_time == 1) {
      vb <- seq(age + (unique(data_freq$step_time[data_freq$step_time ==
                                                    i]) - 1)/max(data_freq$step_time), ngroup -
                  1 + age + (unique(data_freq$step_time[data_freq$step_time ==
                                                          i]) - 1)/max(data_freq$step_time), by = 1) +
        0.5
    }
    if (step_time == 12) {
      vb <- seq(age + (unique(data_freq$step_time[data_freq$step_time ==
                                                    i]) - 1)/12, ngroup - 1 + age + (unique(data_freq$step_time[data_freq$step_time ==
                                                                                                                  i]) - 1)/12, by = 1)
    }
    L <- L_inf * (1 - exp(-K * (vb - t0)))
    print(vb)
    print(L)
    if (get_lmsd == F) {
      sigma <- predict(lmsd, data.frame(age = vb))
      param <- mixparam(L, sigma)
      mix[[i]] <- mix(data_mix[[i]], param, dist = "norm",
                      emsteps = 100, mixconstr(conmu = "MFX", fixmu = fix_mu,
                                               consigma = "SFX", fixsigma = fix_sigma))
    }
    else {
      param <- mixparam(mu = L, sigma = rep(sigma_adjust,
                                            length(L)))
      mix[[i]] <- mix(data_mix[[i]], param, dist = "norm",
                      emsteps = 100, mixconstr(conmu = "MFX", fixmu = fix_mu,
                                               consigma = "NONE"))
    }
    mean[[i]] <- mix[[i]]$parameters$mu
    prop[[i]] <- mix[[i]]$parameters$pi
    sd[[i]] <- mix[[i]]$parameters$sigma
    sd_lm <- rbind(sd_lm, data.frame(sd = mix[[i]]$parameters$sigma,
                                     age = vb, prop = prop[[i]], L))
    if (step_time != 12) {
      ALK[, 1:3] <- data_mix[[i]] %>% mutate(prop_obs = count/sum(count,
                                                                  na.rm = T))
      vect <- NULL
      for (k in 1:ngroup) {
        for (l in 1:length(ALK$lclass)) {
          if (l == 1) {
            ALK[l, 3 + i + (step_time * (k - 1))] <- prop[[i]][k] *
              pnorm(ALK$lclass[l], mean = mean[[i]][k],
                    sd = sd[[i]][k])
          }
          else if (l == max(length(ALK$lclass))) {
            ALK[l, 3 + i + (step_time * (k - 1))] <- prop[[i]][k] *
              (1 - pnorm(ALK$lclass[l - 1], mean = mean[[i]][k],
                         sd = sd[[i]][k]))
          }
          else {
            ALK[l, 3 + i + (step_time * (k - 1))] <- prop[[i]][k] *
              (pnorm(ALK$lclass[l], mean = mean[[i]][k],
                     sd = sd[[i]][k]) - pnorm(ALK$lclass[l -
                                                           1], mean = mean[[i]][k], sd = sd[[i]][k]))
          }
        }
        vect <- c(vect, 3 + i + (step_time * (k - 1)))
      }
      if (!is.null(data_freq$gear)) {
        ALK_gear <- as.list(list())
        for (k in 1:length(unique(data_freq$gear))) {
          ALK_gear[[k]] <- t(apply(ALK[, vect], 1, function(i) i/sum(i)))
          df_gear <- data.frame(data_mix[[i]]$count * ALK_gear[[k]]) %>% mutate(lclass = c(1: length_max)) %>% left_join(data_freq %>% group_by(lclass, gear, step_time) %>% filter(step_time==i, gear == unique(data_freq$gear)[k]) %>%   dplyr::select(lclass, freq_gear) %>%  distinct())
          Matrice_Capture[[k]][, vect] <- df_gear[,c(1:3)] * df_gear$freq_gear # multpiply by prop for lclass of fleet k at time i
        }
      }
      else {
        ALK[, vect] <- t(apply(ALK[, vect], 1, function(i) i/sum(i)))
        Matrice_Capture[[1]][, vect] <- data_mix[[i]]$count *
          ALK[, vect]
        assign("Matrice_Capture", Matrice_Capture, envir = globalenv())
        assign("ALK", ALK, envir = globalenv())
      }
    }
  }
  print("")
  print(sd_lm)
  if (step_time == 12) {
    Matrice_Capture <- as.data.frame(NULL)
    for (month in sort(unique(data_freq$step_time))) {
      ALK <- data_mix[[month]] %>% mutate(prop_obs = count/sum(count,
                                                               na.rm = T))
      for (u in 1:ngroup) {
        ALK <- cbind(ALK, data.frame(age = rep(0, length(ALK$lclass))))
      }
      names(ALK)[4:ncol(ALK)] <- paste0(rep("Age_"), age:ngroup)
      for (k in 1:ngroup) {
        for (l in 1:length(ALK$lclass)) {
          if (l == 1) {
            ALK[l, 3 + k] <- prop[[month]][k] * pnorm(ALK$lclass[l],
                                                      mean = mean[[month]][k], sd = sd[[month]][k])
          }
          else if (l == max(length(ALK$lclass))) {
            ALK[l, 3 + k] <- prop[[month]][k] * (1 -
                                                   pnorm(ALK$lclass[l - 1], mean = mean[[month]][k],
                                                         sd = sd[[month]][k]))
          }
          else {
            ALK[l, 3 + k] <- prop[[month]][k] * (pnorm(ALK$lclass[l],
                                                       mean = mean[[month]][k], sd = sd[[month]][k]) -
                                                   pnorm(ALK$lclass[l - 1], mean = mean[[month]][k],
                                                         sd = sd[[month]][k]))
          }
        }
      }
      ALK[, 4:(3 + ngroup)] <- t(apply(ALK[, 4:(3 + ngroup)],
                                       1, function(i) i/sum(i)))
      Matrice_Capture <- rbind(Matrice_Capture, cbind(ALK[,
                                                          1:3], data_mix[[month]]$count * ALK[, 4:(4 +
                                                                                                     ngroup - 1)]))
      assign("Matrice_Capture", Matrice_Capture, envir = globalenv())
    }
  }
  if (plot == TRUE) {
    for (i in sort(unique(data_freq$step_time))) {
      print(i)
      if (step_time == 12) {
        A <- data.frame(count = data_count[[i]]) %>%
          count(count) %>% mutate(freq = n/sum(n))
        hist(data_count[[i]], breaks = c(length_min,
                                         seq(length_min + step_class, length_max -
                                               step_class, step_class), length_max), xlab = paste0(espece,
                                                                                                   " length (cm)"), ylab = "Relative frequency",
             main = paste0("Polymodal decomposition : Month ",
                           i), col = "lightgrey", freq = FALSE, cex.axis = 1.3,
             cex.lab = 1.3, ylim = c(0, ceiling(max(A$freq) *
                                                  10)/10))
      }
      else {
        A <- data.frame(count = data_count[[i]]) %>%
          count(count) %>% mutate(freq = n/sum(n))
        hist(data_count[[i]], breaks = c(length_min,
                                         seq(length_min + step_class, length_max -
                                               step_class, step_class), length_max), xlab = paste0(espece,
                                                                                                   " length (cm)"), ylab = "Relative frequency",
             main = paste0("Polymodal decomposition : Period ",
                           i), col = "lightgrey", freq = FALSE, cex.axis = 1.3,
             cex.lab = 1.3, ylim = c(0, ceiling(max(A$freq) *
                                                  10)/10))
      }
      lgen <- seq(0, length_max, by = 0.01)
      for (k in (1:ngroup)) {
        dens <- dnorm(lgen, mean = mean[[i]][k], sd = sd[[i]][k])
        dens.rel <- dens/sum(dens) * prop[[i]][k] *
          100
        lines(lgen, dens.rel, col = "black", type = "l",
              lty = 1, lwd = 2)
      }
    }
  }
  assign("sd_lm", sd_lm, envir = globalenv())
  assign("data_mix", data_mix, envir = globalenv())
  assign("sd", sd, envir = globalenv())
  assign("mean", mean, envir = globalenv())
  assign("prop", prop, envir = globalenv())
  print("")
  if (step_time != 12) {
    Mat_C <- data.frame(A = rep(1,ngroup*step_time))
    for (k in 1:length(Matrice_Capture)) {
      Matrice_Capture[[k]][4:(4 + ngroup *step_time - 1)][is.na(Matrice_Capture[[k]][4:(4 + ngroup * step_time - 1)])] <- 0
      Mat_C[,k] <- as.vector(round(apply(Matrice_Capture[[k]][4:(4 + ngroup *
                                                                   step_time - 1)], 2, sum)) + 1)

    }
    names(Mat_C) <- unique(data_freq$gear)
  }
  if (step_time == 12) {
    Mat_C <- round(apply(Matrice_Capture[4:(4 + ngroup -
                                              1)], 2, sum)) + 1
  }
  if(!is.null(data_freq$gear)){
    Mat_C <- data.frame(Catch = Mat_C)
  }
  assign("Mat_C", Mat_C, envir = globalenv())
  assign("data_count", data_count, envir = globalenv())
}


