#Code for model of UA events in Salish Sea
#Zoe Rand and Arial Brewer
#7/28/25
#last updated 10/6/2026
#this version includes suggestions from manuscript revisions

library(tidyverse)
library(corrplot)
library(glmmTMB)
library(DHARMa)
library(marginaleffects)
library(patchwork)

# Read in Data ------------------------------------------------------------

files<-paste0("data/events/Events_", c("AB", "CJF", "DB", "HB","PA", "SB", "TTP", "WJF"), "_2019-2023.csv")
dat<-list()
for(i in 1:length(files)){
  dat[[i]]<-read_csv(files[i])[,1:18] #remove extra columns
  dat[[i]]<-drop_na(dat[[i]]) #remove rows with NAs
}

dat[[8]]<-dat[[8]] %>% rename(`Max vessel distance (km)` = `Max distance (km)`)
dat[[8]]<-dat[[8]] %>% rename(`Start date` = `Start date (local)`)
event_dat<-bind_rows(dat)

table(event_dat$`Vessel type`)
event_dat$`Vessel type`[event_dat$`Vessel type` == "Pleasure Craft"]<-"Pleasure"
table(event_dat$`Vessel type`)

#remove days without enough recording effort and events with unknown vessel type
event_dat<-event_dat %>% filter(`# of daily wav files recorded` >= 138) %>%
  filter(`Vessel type` != "Unknown") %>% filter(`Average vessel SOG (kts)` != "unknown") %>% 
  filter(`Max vessel distance (km)` != "unknown") %>%
  filter(`Median vessel distance (km)` != "unknown") %>%
  filter(`Starting vessel distance (km)` != "unknown") %>%
  mutate(speed = as.numeric(`Average vessel SOG (kts)`), 
         max_distance = as.numeric(`Max vessel distance (km)`),
         med_distance = as.numeric(`Median vessel distance (km)`), 
         start_distance = as.numeric(`Starting vessel distance (km)`),
         site = as.factor(Site), vessel_type = as.factor(`Vessel type`), 
         duration = `# of wav files with UA`, 
         vessel_type_num = ifelse(vessel_type == "Cargo", 2, 
                                  ifelse(vessel_type == "Passenger", 1, 
                                         ifelse(vessel_type == "Pleasure", 3,
                                                4))), 
         UA_type = as.factor(`UA signal type`), 
         UA_type_num = ifelse(UA_type == "Single band", 1,
                              ifelse(UA_type == "Multi-freq", 2,
                                     3))) %>%
  rename("N_detections" = "# of AIS observations") 


summary(event_dat)

#fixing weird median
idx<-which(event_dat$med_distance == 18.2)
event_dat$med_distance[idx]<-event_dat$start_distance[idx] #only one AIS detection for this one so start is the same as median

summary(event_dat)

#total number of events
nrow(event_dat)







# Data exploration plots --------------------------------------------------

ggplot(event_dat) + geom_histogram(aes(x = speed)) + 
  facet_wrap(~vessel_type, ncol = 1) + theme_classic() + 
  scale_y_continuous(expand = c(0, 0))

ggplot(event_dat) + geom_histogram(aes(x = med_distance)) + 
  facet_wrap(~vessel_type, ncol = 1) + theme_classic() + 
  scale_y_continuous(expand = c(0, 0))


ggplot(event_dat) + geom_histogram(aes(x = speed)) + 
  facet_wrap(~UA_type, ncol = 1)+ theme_classic() + 
  scale_y_continuous(expand = c(0, 0))


ggplot(event_dat) + geom_histogram(aes(x = med_distance)) + 
  facet_wrap(~UA_type, ncol = 1)+ theme_classic() + 
  scale_y_continuous(expand = c(0, 0))


# Model -------------------------------------------------------------------

#checking correlations
x <- cor(event_dat[c( "Year","speed", "vessel_type_num", "UA_type_num", "N_detections", "med_distance", "start_distance", "max_distance")])
corrplot(x, type="upper",order="hclust",addCoef.col = "black")


#setting WJF as the reference level
event_dat$site<-relevel(event_dat$site, ref = "WJF")
levels(event_dat$site)

#fitting model without intercept so there are no reference levels
#adding a Vessel ID  random effect
#using median distance instead of max distance 
mod<-glmmTMB(duration ~ -1 + site+speed + med_distance + (1| `Vessel ID`),
             family=truncated_genpois(link = "log"), data=event_dat)

summary(mod)

#double checking model matrix
mtrx<-model.matrix(mod)

#diagnostics
diagnose(mod)

#residuals
mod_res<-simulateResiduals(mod, n = 1000, refit = F) #get residuals
plotQQunif(mod_res) #QQ
plotResiduals(mod_res,) #residuals vs fitted
testDispersion(mod_res, alternative = "greater", type = "DHARMa") #testing for overdispersion


#making plots nice for supplement
qq_plot<-wrap_elements(panel = ~ plotQQunif(mod_res, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE))
res_plot<-wrap_elements(panel = ~plotResiduals(mod_res, quantreg = FALSE))
disp_plot<-wrap_elements(panel = ~ testDispersion(mod_res, alternative = "greater", type = "DHARMa"))

sup_diag_plots<-(qq_plot + res_plot)/disp_plot

sup_diag_plots

ggsave("figures/event_model_diagnostics.pdf", sup_diag_plots, width = 7, height = 6, dpi = 600)

#using profile confidence intervals 
CIs<-confint(mod, method= "profile", level = 0.95, component = "cond") %>% as_tibble(rownames = "parameter") %>% 
    filter(parameter != "theta_1|`VesselID`.1") %>%
    add_column(Estimate = mod$fit$par[1:10]) %>%
    mutate(col = ifelse(`2.5 %` > 0 & `97.5 %` > 0, 1,
                      ifelse(`2.5 %` < 0 & `97.5 %` < 0, 1, 0)), #color based on if overlaps 0
         param_lab = ifelse(parameter == "(Intercept)", "siteWJF", parameter))

#cleaning up for plot

CIs$parameter_factor<-factor(CIs$param_lab, levels = c( "siteAB", "siteSB","siteHB", "siteTTP",
                                                        "siteDB", "sitePA", "siteCJF", "siteWJF",
                                                        "speed", "med_distance"), 
                             labels = c("AB", "SB", "HB", "TTP", "DB", "PA", "CJF", "WJF", "speed", "distance"))


write_csv(CIs, "results/event_model_ests.csv")

#random effect and dispersion are in the summary: 
summary(mod)

#estimate plot
ggplot() + geom_point(aes(x = CIs$parameter_factor, y = CIs$Estimate)) + 
  geom_linerange(aes(x = CIs$parameter_factor, ymin = CIs$`2.5 %`, ymax = CIs$`97.5 %`)) + 
  geom_hline(yintercept = 0, lty = 2, lwd = 0.5, color = "black") +
  coord_flip() + 
  labs(x = "Parameter", y = "Estimate") +
  theme_classic() + 
  theme(legend.position  = "none")

#making it just for sites
CIs %>% filter(!parameter %in% c("speed", "med_distance")) %>% ggplot() + 
  geom_point(aes(x = Estimate, y = parameter_factor, color = factor(col))) + 
  geom_linerange(aes(y = parameter_factor, xmin = `2.5 %`, xmax = `97.5 %`, color = factor(col))) + 
  geom_vline(xintercept = 0, lty = 2, lwd = 0.5, color = "black") +
  labs(x = "Effect", y = "Site") +
  theme_classic() + 
  theme(legend.position  = "none")

#prediction plots
#re.form = NA sets all random effects to 0 (population level predictions)
speed_preds<-plot_predictions(mod,condition=list("speed", "med_distance" = mean(event_dat$med_distance, "site" = "WJF")), re.form=NA, vcov=TRUE, draw = FALSE)
write_csv(speed_preds, "results/speed_predictions.csv")

g1<-ggplot() + geom_point(data = event_dat, aes(x = speed, y = duration), alpha = 0.5)


speed_pred<-g1 + 
  geom_ribbon(data = speed_preds, aes(x = speed, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.5) + 
  geom_line(data = speed_preds, aes(x = speed, y = estimate), color = "blue") +
  theme_classic() + 
  scale_x_continuous(limits = c(0, max(event_dat$speed)), breaks = seq(0, 24, by = 4), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, max(event_dat$duration)), breaks = seq(0, 14, by = 4), expand = c(0, 0)) + 
  labs(x = "Speed (knots)", y = "UA Event duration") +
  theme(legend.position = "none")
speed_pred


distance_preds<-plot_predictions(mod,condition=list("med_distance", "speed" = mean(event_dat$speed), "site" = "WJF"), re.form=NA, vcov=TRUE, draw = FALSE)
write_csv(distance_preds, "results/distance_predictions.csv")


g2<-ggplot() + geom_point(data = event_dat, aes(x = med_distance, y = duration), alpha = 0.5)
pred_dist<-g2 + 
  geom_ribbon(data = distance_preds, aes(x = med_distance, ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.5) + 
  geom_line(data = distance_preds, aes(x = med_distance, y = estimate), color = "darkgreen") +
  theme_classic() + 
  scale_x_continuous(limits = c(0, max(event_dat$med_distance)), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, max(event_dat$duration)), breaks = seq(0, 14, by = 4), expand = c(0, 0)) + 
  labs(x = "Median distance (km)", y = "UA Event duration") +
  theme(legend.position = "none")
pred_dist

speed_pred + pred_dist






