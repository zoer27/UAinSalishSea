#Code for model of UA events in Salish Sea
#Zoe Rand and Arial Brewer
#7/28/25
#last updated 4/15/2026

library(tidyverse)
library(corrplot)
library(glmmTMB)
library(marginaleffects)
library(patchwork)

# Read in Data ------------------------------------------------------------

files<-paste0("data/events/Events_", c("AB", "CJF", "DB", "HB","PA", "SB", "TTP", "WJF"), "_2019-2023.csv")
dat<-list()
for(i in 1:length(files)){
  dat[[i]]<-read_csv(files[i])[, 1:14] #remove extra columns
  dat[[i]]<-drop_na(dat[[i]]) #remove rows with NAs
}

dat[[8]]<-dat[[8]] %>% rename(`Max vessel distance (km)` = `Max distance (km)`)
event_dat<-bind_rows(dat)

table(event_dat$`Vessel type`)
event_dat$`Vessel type`[event_dat$`Vessel type` == "Pleasure Craft"]<-"Pleasure"
table(event_dat$`Vessel type`)

#remove days without enough recording effort and events with unknown vessel type
event_dat<-event_dat %>% filter(`# of daily wav files recorded` >= 138) %>%
  filter(`Vessel type` != "Unknown") %>% filter(`Average vessel SOG (kts)` != "unknown") %>% 
  filter(`Max vessel distance (km)` != "unknown") %>%
  mutate(speed = as.numeric(`Average vessel SOG (kts)`), distance = as.numeric(`Max vessel distance (km)`), 
         speed_c = scale(speed)[,1], 
         distance_c = scale(distance)[,1],
         site = as.factor(Site), vessel_type = as.factor(`Vessel type`), 
         duration = `# of wav files with UA`, 
         vessel_type_num = ifelse(vessel_type == "Cargo", 2, 
                                  ifelse(vessel_type == "Passenger", 1, 
                                         ifelse(vessel_type == "Pleasure", 3,
                                                4))), 
         UA_type = as.factor(`UA signal type`), 
         UA_type_num = ifelse(UA_type == "Single band", 1,
                              ifelse(UA_type == "Multi-freq", 2,
                                     3)))

summary(event_dat)

table(event_dat$`Average vessel SOG (kts)`)
table(event_dat$`Max vessel distance (km)`)


summary(event_dat)


# Model -------------------------------------------------------------------

#checking correlations
x <- cor(event_dat[c( "Year","speed", "distance", "vessel_type_num", "UA_type_num")])
corrplot(x, type="upper",order="hclust",addCoef.col = "black")


#setting WJF as the reference level
event_dat$site<-relevel(event_dat$site, ref = "WJF")
levels(event_dat$site)

#fitting model
mod<-glmmTMB(duration ~ site+speed + distance,
                  family=truncated_poisson(link = "log"), data=event_dat)

summary(mod)

#diagnostics
diagnose(mod)
p_resid<-residuals(mod, type = "pearson")
plot(p_resid)


#using profile confidence intervals
CIs<-confint(mod, method= "profile") %>% as_tibble(rownames = "parameter") %>% 
  add_column(Estimate = mod$fit$par) %>%
  mutate(col = ifelse(`2.5 %` > 0 & `97.5 %` > 0, 1, 
                      ifelse(`2.5 %` < 0 & `97.5 %` < 0, 1, 0)), #color based on if overlaps 0
         param_lab = ifelse(parameter == "(Intercept)", "siteWJF", parameter)) 

#cleaning up for plot
CIs$parameter_factor<-factor(CIs$param_lab, levels = c( "siteAB", "siteSB","siteHB", "siteTTP",
                                                        "siteDB", "sitePA", "siteCJF", "siteWJF",
                                                        "speed", "distance"), 
                             labels = c("AB", "SB", "HB", "TTP", "DB", "PA", "CJF", "WJF", "speed", "distance"))


write_csv(CIs, "results/event_model_ests.csv")

#estimate plot
ggplot() + geom_point(aes(x = CIs$parameter_factor, y = CIs$Estimate, color = as.factor(CIs$col))) + 
  geom_linerange(aes(x = CIs$parameter_factor, ymin = CIs$`2.5 %`, ymax = CIs$`97.5 %`, color = as.factor(CIs$col))) + 
  geom_hline(yintercept = 0, lty = 2, lwd = 0.5, color = "black") +
  coord_flip() + 
  labs(x = "Parameter", y = "Estimate") +
  theme_classic() + 
  theme(legend.position  = "none")



#prediction plots
speed_preds<-plot_predictions(mod,condition=list("speed", "distance" = mean(event_dat$distance, "site" = "WJF")), re.form=NA, vcov=TRUE, draw = FALSE)
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


distance_preds<-plot_predictions(mod,condition=list("distance", "speed" = mean(event_dat$speed), "site" = "WJF"), re.form=NA, vcov=TRUE, draw = FALSE)
write_csv(distance_preds, "results/distance_predictions.csv")


g2<-ggplot() + geom_point(data = event_dat, aes(x = distance, y = duration), alpha = 0.5)
pred_dist<-g2 + 
  geom_ribbon(data = distance_preds, aes(x = distance, ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.5) + 
  geom_line(data = distance_preds, aes(x = distance, y = estimate), color = "darkgreen") +
  theme_classic() + 
  scale_x_continuous(limits = c(0, max(event_dat$distance)), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, max(event_dat$duration)), breaks = seq(0, 14, by = 4), expand = c(0, 0)) + 
  labs(x = "Maximum distance (km)", y = "UA Event duration") +
  theme(legend.position = "none")
pred_dist

speed_pred + pred_dist


