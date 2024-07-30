#'  Polymodal decomposition
#'
#' \code{mixdist_polymod} returns the numbers-at-age using a polymodal decomposition of length frequencies. Can be done by year or other period (quarter, semester, month...) by specifying it.
#'
#' @param   data_freq       Input dataset table. Must include a gear column and total by size and gear in total_gear. It should be extrapolated.
#' @param   title           Title in the plots
#' @param   K               value for K
#' @param   L_inf           value for L_inf
#' @param   t0              value for t0
#' @param   fix_mu          Vector with mean length values-at-age to fix
#' @param   fix_sigma       Vector with standard deviation values-at-age to fix
#' @param   get_lmsd        When TRUE, a linear model is done on sd, values are saved and retruned. Otherwise sigma are fixed accrodingly to "fix_sigma"
#' @param   ngroup          number of age class
#' @param   plot            To plot values.
#' @param   step_class      length step between two length class
#' @param   sigma_adjust    In some case, algorith may not be able to converge, relax hypothesis on sigma (sigma_value = 1 for exemple)
#' @param   age             Age at which begin polymodal decomposition. Set on zero.
#'
#' @examples
#'
#'#--------------------------------------------------
#'
#'
#'                 Data Extrapolation
#'
#'
#'#--------------------------------------------------
#'
#'  data(data_LF_LBB)
#'  data_catch_quarter = data_frame(gear = c(rep("Artisanal",4), rep("Industrial",4)),
#'  step_time = rep(c(1:4),2),
#'  capture = c(1153, 1042, 1046, 1132, 1206, 1124, 1155, 1176))
#'  data_freq <- data_LF_LBB %>%  group_by(lclass, month) %>%  dplyr::summarise(total_sampling=sum(frequence, na.rm = T))
#'# biological parameters from literature or estimated in previous work
#'
#'Linf <- 44.4 # Asymptotic length
#'K <- 0.34 # Growth parameter
#'t0 <- -0.24 # theoric age for length 0 cm
#'
#'a <- 0.006 # parameters from length-size relationship
#'b <- 3.08
#'
#'
#'# 6. Preparing length frequencies data. We remove data from San Pedro because samples weren't consistent.
#'data_freq_mixdist <- data_LF_LBB %>%
#'  filter(data_type == "Data Bio DEMERSTEM", data_from != "San Pedro") %>%
#'  mutate(gear = case_when(
#'    data_from == "Abidjan" ~ 'Industrial',
#'    data_from %in% c("Tadkoradi","Tema") ~ "Artisanal")) %>%
#'  mutate(step_time = ceiling((month)/(12/step_time)))
#'
#'data_freq_mixdist %>%  group_by(lclass, month, gear) %>% mutate(total = sum(frequence,na.rm = T)) %>%  ggplot(aes(x = lclass, y = total)) + geom_bar(stat = 'identity') + facet_grid(month~gear)
#'
#'# 7. Extrapolation of length frequencies
#'data_freq_mixdist <- data_freq_mixdist %>%
#'  group_by(lclass, step_time, gear) %>%
#'  dplyr::summarise(total_sampling=sum(frequence, na.rm = T)) %>%  mutate(weight_sampling = total_sampling * a*lclass^b/1000)
#'
#'
#'
#'data_freq_mixdist <- data_freq_mixdist %>% full_join(data_catch_quarter, by = c("step_time", "gear")) %>% dplyr::rename(Wtot = capture) %>%  mutate(Wtot = 1000 * Wtot)
#'
#'
#'data_freq_mixdist <- data_freq_mixdist %>% group_by(step_time, gear) %>%  dplyr::mutate(W_sampling = sum(weight_sampling),
#'                                                                                        N_sampling = sum(total_sampling),
#'                                                                                        Ntot= floor(Wtot * N_sampling/W_sampling),
#'                                                                                        total_gear = floor(total_sampling*Ntot/N_sampling)) %>%  na.omit()
#'
#'#--------------------------------------------------
#'
#'
#'            Polymodal decomposition
#'
#'
#'#--------------------------------------------------
#'espece <- "P. senegalensis"
#'
#'Linf <- 44.4 # Asymptotic length
#'K <- 0.34 # Growth parameter
#'t0 <- -0.24 # theoric age for length 0 cm
#'
#'ngroup <- 3 # number of age group we want to detect
#'fixmu <- rep(T, ngroup) # We fix the mean length at age using growth parameters estimated/from literature : We specify 'TRUE'
#'fixsigma <- rep(F, ngroup) # We want to get an estimation of sigma : We specify 'FALSE'
#'step_time = 4 # We will try to examine at a quarter level.
#'
#'mixdist_polymod(data_freq = data_freq_mixdist,
#'                K,
#'                Linf,
#'                t0,
#'                step_class = 1, # bin size
#'                step_time = step_time,
#'                ngroup = ngroup,
#'                fix_mu = fixmu,
#'                fix_sigma = fixsigma,
#'                sigma_adjust = 2,
#'                get_lmsd = T, # We will perform a linear model with these data
#'                plot = T)
#'
#'
#'
#' @export
mixdist_polymod <- function (data_freq, K, L_inf, t0, fix_mu, fix_sigma,
                             ngroup, step_class, step_time, get_lmsd = FALSE,
                             plot = FALSE, sigma_adjust = 1, age = 0)
{

  cat("----------------------------------------------

       Polymodal decomposition of length frequencies

      -----------------------------------------------")

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
          Matrice_Capture[[k]][, vect] <- df_gear[,c(1:ngroup)] * df_gear$freq_gear # multpiply by prop for lclass of fleet k at time i
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


