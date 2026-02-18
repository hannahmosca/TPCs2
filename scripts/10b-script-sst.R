

install.packages("lmerTest")
library(lmerTest)
library(car)
install.packages("performance")
library(performance)

install.packages("remotes")
remotes::install_github("bbolker/merTools") 
library(merTools)


#### topt and enviornmental temp ####
ggplot(data = marine1 %>% 
         filter(topt_TF == TRUE),
       aes(x = sst_mean, y = topt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "mean SST",
    y = "Topt",
    title = "mean SST and fish thermal optima"
  )

m_mean <- lmer(topt ~ sst_mean + (1 | study_ID), 
               data = marine1 %>% filter(topt_TF == TRUE))
plot(residuals(m_mean))
qqnorm(resid(m_mean))
qqline(resid(m_mean))
summary(m_mean)
r2(m_mean)
Anova(m_mean) #significant

ggplot(data = marine1 %>% 
         filter(topt_TF == TRUE),
       aes(x = sst_max, y = topt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "max SST",
    y = "Topt",
    title = "max SST and fish thermal optima"
  )
m_max  <- lmer(topt ~ sst_max + (1 | study_ID), 
               data = marine1 %>% filter(topt_TF == TRUE))
plot(residuals(m_max))
qqnorm(resid(m_max))
qqline(resid(m_max))
summary(m_max)
r2(m_max)
Anova(m_max) #significant
AIC(m_mean, m_max) #max temp better correlates with topt
anova(m_mean, m_max)

#predictions
pred_mean <- marine1 %>%
  filter(topt_TF == TRUE) %>%
  dplyr::select(sst_mean) %>%
  distinct() %>%
  mutate(pred_mean = predict(m_mean, newdata = ., re.form = NA))
pred_max <- marine1 %>%
  filter(topt_TF == TRUE) %>%
  dplyr::select(sst_max) %>%
  distinct() %>%
  mutate(pred_max = predict(m_max, newdata = ., re.form = NA))

ggplot(marine1 %>% filter(topt_TF == TRUE)) +
  geom_point(aes(x = sst_mean, y = topt), color = "blue", alpha = 0.5) +
  geom_line(data = pred_mean, aes(x = sst_mean, y = pred_mean), color = "blue", linewidth = 1.2) +
  geom_point(aes(x = sst_max, y = topt), color = "red", alpha = 0.5) +
  geom_line(data = pred_max, aes(x = sst_max, y = pred_max), color = "red", linewidth = 1.2) +
  labs(
    x = "Environmental Temperature",
    y = "Topt",
    title = "Comparison of Mean vs Maximum sst correlated w/ Topt in marine fish",
    subtitle = "Blue = Mean SST, Red = Max SST"
  ) +
  theme_minimal()
##topt is closer to sst max than sst mean##? 

#### performance breadth and tolerance breadth ####
ggplot(data = marine1 %>% 
         filter(thermal_tolerance_TF == TRUE),
       aes(x = sst_sd, y = thermal_tolerance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal tolerance breadth",
    title = "tolerance breadth and Variability"
  )

ggplot(data = marine1 %>% 
         filter(breadth_TF == TRUE),
       aes(x = sst_sd, y = breadth)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "thermal variability (temp_sd)",
    y = "thermal performance breadth",
    title = "Performance Breadth and Variability"
  )
breadth_model <- lmer(breadth ~ sst_mean + (1 | study_ID), 
                      data = marine1 %>%
                        filter(breadth_TF == TRUE))
plot(residuals(breadth_model))
qqnorm(resid(breadth_model))
qqline(resid(breadth_model))
summary(breadth_model)
r2(breadth_model)
Anova(breadth_model) #not sig in marine, but sig in freshwater terrestrial

## deutsch warming tolerance - the difference between ctmax and mean env. temp
marine1 <- marine1 %>%
  mutate(warming_tolerance = ctmax - sst_mean) %>%
  mutate(thermal_safety_margin_duetsch = topt - sst_mean)

ggplot(data = marine1 %>%
         filter(thermal_max_TF == TRUE) %>%
         filter(topt_TF == TRUE),
       aes(x = ctmax, y = (ctmax-topt))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "ctmax",
    y = "ctmax-topt",
    title = "Scatter"
  )

ggplot(data = marine1 %>%
         filter(thermal_max_TF == TRUE),
       aes(x = abs_latitude, y = warming_tolerance)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "latitude",
    y = "warming tolerance",
    title = "Scatter"
  )
##does how close your topt is to your env temp depend on latitude???? ### 
marine1 <- marine1 %>%
  mutate(diff_max = sst_max - topt) %>% 
  mutate(diff_mean = sst_mean - topt) #smaller this # is, the closer topt is to env mean
###topt should be closer to mean water temp in the tropics (ie mag should decrease with abs. latitude), where temps are higher
ggplot(data = marine1 %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = diff_mean)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "abs latitude",
    y = "magnitude of difference between topt and mean water temp",
    title = "latitudinal trends in topt and mean water temp"
  )
dif_model <- lmer(abs_latitude ~ diff_mean + (1 | study_ID), 
                  data = marine1 %>%
                    filter(topt_TF == TRUE))
plot(residuals(dif_model))
qqnorm(resid(dif_model))
qqline(resid(dif_model))
summary(dif_model)
r2(dif_model)
Anova(dif_model) #significant? but looks like the decrease is more in freshwater/terrestrial

### tsm should increase with latitude (deutsch), because fish in tropics are already living close to their physiological optimum
ggplot(data = marine1 %>%
         filter(topt_TF == TRUE),
       aes(x = abs_latitude, y = thermal_safety_margin_duetsch)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "latitude",
    y = "TSM",
    title = "Scatter"
  )
TSM_model <- lmer(abs_latitude ~ thermal_safety_margin_duetsch + (1 | study_ID), 
                  data = marine1 %>%
                    filter(topt_TF == TRUE))
plot(residuals(TSM_model))
qqnorm(resid(TSM_model))
qqline(resid(TSM_model))
summary(TSM_model)
r2(TSM_model)
Anova(TSM_model) #significant


#topt and enviornmental temp
dif <- marine1 %>%
  pivot_longer(
    cols = c(diff_max, diff_mean),
    names_to = "diff_type",
    values_to = "diff_value"
  )
ggplot(dif %>% filter(topt_TF == TRUE)) +
  geom_boxplot(aes(x = diff_type, y = diff_value))

# qq
marine_topt <- marine1 %>%
  filter(topt_TF == TRUE)
qqnorm(marine_topt$diff_mean); qqline(marine_topt$diff_mean)
qqnorm(marine_topt$diff_max); qqline(marine_topt$diff_max)

#topt will be more dif from sst max in enviornemnts with greater thermal variability -- ie mag will increase with sst var
marine_topt <- marine_topt %>%
  mutate(abs_diff_max = abs(diff_max))
marine_topt <- marine_topt %>%
  mutate(abs_diff_mean = abs(diff_mean))

qqnorm(marine_topt$abs_diff_max); qqline(marine_topt$abs_diff_max)
marine_topt <- marine_topt %>%
  mutate(log_abs_diff_max = log1p(abs_diff_max))  # log1p handles 0s safely
qqnorm(marine_topt$log_abs_diff_max); qqline(marine_topt$log_abs_diff_max)

ggplot(data = marine_topt,
       aes(x = sst_sd, y = log_abs_diff_max)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "SST variability (SST sd)",
    y = "magnitude of dif between topt and max temp (logged)",
    title = "thermal variation and topt in relation to max sst"
  )

ggplot(data = marine_topt,
       aes(x = sst_sd, y = abs_diff_max)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "SST variability (SST sd)",
    y = "magnitude of dif between topt and max temp (logged)",
    title = "thermal variation and topt in relation to max sst"
  ) +
  scale_y_continuous(trans = 'log')

qqnorm(marine_topt$abs_diff_mean); qqline(marine_topt$abs_diff_mean)
marine_topt <- marine_topt %>%
  mutate(log_abs_diff_mean = log1p(abs_diff_mean))  # log1p handles 0s safely
qqnorm(marine_topt$log_abs_diff_mean); qqline(marine_topt$log_abs_diff_mean)

ggplot(data = marine_topt, 
       aes(x = sst_sd, y = abs_diff_mean)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "SST variability (SST sd)", 
    y = "magnitude of dif between topt and max temp (logged)",
    title = "thermal variation and topt in relation to mean sst"
  )
#Magnitude-only test
varmean <- (lmer(log_abs_diff_mean ~ sst_sd + (1 | study_ID), data = marine_topt)) #expect dif between sstmean and topt to scale with thermal variation
varmax <- (lmer(log_abs_diff_max ~ sst_sd + (1 | study_ID), data = marine_topt))

plot(residuals(varmean))
qqnorm(resid(varmean))
qqline(resid(varmean))
hist(resid(varmean))
summary(varmean)
r2(varmean)
Anova(varmean) #significant

plot(residuals(varmax))
qqnorm(resid(varmax))
qqline(resid(varmax))
hist(resid(varmax))
summary(varmax)
r2(varmax)
Anova(varmax) #significant

#dif between mean sst and topt is more related to thermal variability than dif between max sst and topt
AIC(varmean, varmax) 
anova(varmean, varmax)

library(ggeffects)
# Predict Topt offset from mean SST over sst_sd range
pred_dif_sstsd_mean <- ggpredict(varmean, terms = "sst_sd")
pred_dif_sstsd_max <- ggpredict(varmax, terms = "sst_sd")
pred_dif_sstsd_mean <- as.data.frame(pred_dif_sstsd_mean)
pred_dif_sstsd_max <- as.data.frame(pred_dif_sstsd_max)


ggplot(marine_topt) +
  geom_point(aes(x = sst_sd, y = log_abs_diff_mean), color = "blue", alpha = 0.5) +
  geom_line(data = pred_dif_sstsd_mean, aes(x = x, y = predicted), color = "blue", linewidth = 1.2) + 
  geom_ribbon(data = pred_dif_sstsd_mean, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) +
  geom_point(aes(x = sst_sd, y = log_abs_diff_max), color = "red", alpha = 0.5) +
  geom_line(data = pred_dif_sstsd_max, aes(x = x, y = predicted), color = "red", linewidth = 1.2) + 
  geom_ribbon(data = pred_dif_sstsd_max, aes(x = x, ymin = conf.low, ymax = conf.high), fill = "red", alpha = 0.2) +
  labs(
    x = "SST_SD",
    y = "Difference between topt and sst",
    title = "Thermal variability and magnitude of difference \n between topt and SST",
    subtitle = "Blue = Mean SST, Red = Max SST"
  ) +
  theme_minimal()


ggplot(data = marine1, 
       aes(x = abs_latitude, y = (sst_max - sst_mean))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm) +
  labs(
    x = "abs lat", 
    y = "dif between topt and mean sst",
    title = "//"
  )

