#### re try activation energy with rtpc get e function ####
rm(list = ls())
install.packages("rTPC")
library(rTPC)
subset <- readRDS(here("processed-data", "activation_energy_subset.RDS"))
list_sub <- unique(subset$curve_ID)
curves <- readRDS(here("processed-data", "wild-tpcs-clean.RdS"))
sub_curves <- curves %>%
  filter(curve_ID %in% list_sub)
sub_curves <- sub_curves %>%
  select(curve_ID, test_temp, response_value) %>%
  distinct() %>%
  left_join(subset %>% select(curve_ID, topt_TF, topt, e))


library(dplyr)
library(purrr)

arrhenius_fits <- sub_curves %>%
  group_by(curve_ID) %>%
  group_split() %>%
  map_df(function(df){
    
    topt_val <- unique(df$topt)[1]
    df <- df %>%
      filter(test_temp <= topt_val) %>%
      mutate(K = ifelse(test_temp < 150, test_temp + 273.15, test_temp))
    
    if(nrow(df) < 4){
      return(tibble(
        curve_ID = unique(df$curve_ID),
        e_arr = NA_real_,
        lnc = NA_real_,
        AIC = NA_real_,
        RSS = NA_real_,
        n = nrow(df),
        converged = FALSE
      ))
    }
    
    fit <- try(
      nls(response_value ~ lnc * exp(e_arr/8.62e-05 * (1/median(K) - 1/K)),
          data = df,
          start = c(lnc = median(df$response_value, na.rm = TRUE),
                    e_arr = 1),
          algorithm = "port",
          lower = c(lnc = 0, e_arr = 0),
          upper = c(lnc = Inf, e_arr = 5)),
      silent = TRUE
    )
    
    if(inherits(fit, "try-error")){
      return(tibble(
        curve_ID = unique(df$curve_ID),
        e_arr = NA_real_,
        lnc = NA_real_,
        AIC = NA_real_,
        RSS = NA_real_,
        n = nrow(df),
        converged = FALSE
      ))
    }
    
    rss <- sum(residuals(fit)^2)
    tss <- sum((df$response_value - mean(df$response_value))^2)
    r2 <- 1 - rss/tss
    
    s <- summary(fit)$parameters
    e_est <- s["e_arr", "Estimate"]
    se_e  <- s["e_arr", "Std. Error"]
    tval  <- s["e_arr", "t value"]
    pval  <- s["e_arr", "Pr(>|t|)"]
    
    tibble(
      curve_ID = unique(df$curve_ID),
      e_arr = unname(coef(fit)["e_arr"]),
      lnc = unname(coef(fit)["lnc"]),
      AIC = AIC(fit),
      R2 = r2,
      pval = pval,
      n = nrow(df),
      converged = TRUE
    )
  })

sub_curves <- sub_curves %>%
  left_join(arrhenius_fits, by = "curve_ID")
sub_curves_2 <- sub_curves %>%
  select(curve_ID, topt_TF, topt, e, e_arr, lnc, AIC, R2, pval, n, converged) %>%
  distinct()
sub_curves_2 <- sub_curves_2 %>%
  filter(converged == TRUE)

sub_curves_3 <- sub_curves_2 %>%
  filter(R2 > 0.5)

sub_curves_4 <- sub_curves_3 %>%
  filter(pval < 0.05)

mean(sub_curves_4$e_arr)


saveRDS(sub_curves_4, here("processed-data", "activation_energy_subset.RDS"))
