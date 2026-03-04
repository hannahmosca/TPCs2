#### ============================================================
#### Script info
#### ============================================================
# Title: fit-arrhenius-activation-energy.R
# Description:
#   Estimates Arrhenius activation energy (Ea) for FishTherm curves by fitting an
#   Arrhenius relationship to the increasing-temperature portion of each dataset
#   (temperatures ≤ fitted topt) also using the rTPC package. 
#   Curves are filtered to those with sufficient data and (optionally) to a set of
#   visually screened curve IDs. Fits are performed with nonlinear least squares,
#   and poor fits are removed using convergence, R², and p-value thresholds.
#
install.packages("rTPC")
library(rTPC)

#### 01. load data ####
parameters <- readRDS(here("processed-data", "tpcs_with_fitted_params.RDS"))

#filter out irregular and decreasing to get datasets possibly eligible for act. eng
ee <- parameters %>%
  filter(dataset_type %in% c("full_curve", "left_bound_withopt", "unbounded_increasing", "topt", 
                             "left_bound"))

### visually filtering out ones that are just topt for the activation energy testing###
act_eng <- unique(ee$curve_ID)
remove <- c(56, 438, 439, 208, 53, 54, 126, 108, 144, 177, 179, 182, 185, 190, 186, 199, 201, 207, 217, 219, 207, 292, 321, 323, 344, 373, 374, 368, 369,377, 417, 429, 431, 436, 438, 461)

subset <- ee %>%
  filter(!(curve_ID %in% remove))

Ea_curves <- unique(subset$curve_ID)


curves <- read.csv(here("processed-data", "FishTherm.csv"))
sub_curves <- curves %>%
  filter(curve_ID %in% Ea_curves)
sub_curves <- sub_curves %>%
  select(curve_ID, test_temp, response_value) %>%
  distinct() %>%
  left_join(subset %>% select(curve_ID, topt_TF, topt))


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
  select(curve_ID, topt_TF, topt, e_arr, lnc, AIC, R2, pval, n, converged) %>%
  distinct()
sub_curves_2 <- sub_curves_2 %>%
  filter(converged == TRUE)

sub_curves_3 <- sub_curves_2 %>%
  filter(R2 > 0.5)

sub_curves_4 <- sub_curves_3 %>%
  filter(pval < 0.05)

mean(sub_curves_4$e_arr)

parameters1 <- parameters %>%
  left_join(sub_curves_4 %>% select(curve_ID, e_arr, R2, pval), join_by(curve_ID)) %>%
  rename(R2_earr = R2) %>%
  rename(pval_earr = pval) %>%
  select(curve_ID, model, topt, ctmin, ctmax, thermal_tolerance, breadth, y_value_topt, y_value_ctmax, y_value_ctmin, e_arr, R2_earr, pval_earr, everything()) %>%
  rename(tmin = ctmin) %>%
  rename(tmax = ctmax) %>%
  rename(y_value_tmin = y_value_ctmin) %>%
  rename(y_value_tmax = y_value_ctmax)

saveRDS(parameters1, here("processed-data", "tpcs_with_fitted_params_with_act_eng.RDS"))


