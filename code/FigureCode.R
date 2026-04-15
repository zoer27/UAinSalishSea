#Code for figures used in manuscript
#Zoe Rand
#Last updated 4/15/2026

library(tidyverse)
library(patchwork)


# Colors ------------------------------------------------------------------
year_pal<-c('#fee08b','#bae4bc','#7bccc4','#43a2ca','#0868ac')
sites<-"#80cdc1"
pal3<-c( "#429e42", "#42c3cf", "goldenrod1","#c6d2b9", 'gray90')


# Daily Presence ----------------------------------------------------------

#read in data
files<-paste0("data/dailypresence/DailyPresence_", c("CJF", "TTP", "AB", "DB", "WJF", "SB", "HB", "PA"), "_2019-2023.csv")
dat_list<-list()
for(i in 1:length(files)){
  dat_list[[i]]<-read_csv(files[i])
}
daily<-bind_rows(dat_list)

nsites<-8
#number of days per month with recording effort
effort_ymsite<-daily %>% group_by(Year, Month, Site) %>% mutate(effort_day = ifelse(`# of daily wav files recorded` >= 138, 1, 0)) %>% summarise(effort_days_tot = sum(effort_day))
#adding proportion
effort_ymsite2<-effort_ymsite %>% mutate(Prop_site = ifelse(Month %in% c("November", "April", "June", "September"), effort_days_tot/30, ifelse(Month == "February", effort_days_tot/28, effort_days_tot/31)))
#accounting for leap year
effort_ymsite2<-effort_ymsite2 %>% mutate(Prop_site = ifelse(Month == "February" & Year == 2020, effort_days_tot/29, Prop_site))

#across sites
effort_ym<-effort_ymsite2 %>% group_by(Year, Month) %>% summarise(effort_allsite = sum(effort_days_tot)) %>%
  mutate(Prop_year = ifelse(Month %in% c("November", "April", "June", "September"), effort_allsite/(30*nsites), ifelse(Month == "February", effort_allsite/(28*nsites), effort_allsite/(31*nsites)))) %>% 
  mutate(Prop_year = ifelse(Month == "February" & Year == 2020, effort_allsite/(29*nsites), Prop_year))

#filter out days without enough recording effort
daily<- daily %>% filter(`# of daily wav files recorded` >= 138) #for now using 138 files as the threshold

#Proportion of total days (month*8sites) with recording effort for point size
daily<-daily %>% group_by(Year, Month) %>% 
  mutate(Monthly_UA = sum(`UA absence/presence`), Prop_UA = Monthly_UA/n())

#adding effort
daily<-daily %>% left_join(effort_ymsite2, by = c("Year", "Month", "Site")) %>% left_join(effort_ym, by = c("Year", "Month"))

#by site
daily_site<-daily %>% group_by(Year, Month, Site) %>% 
  mutate(Monthly_UA = sum(`UA absence/presence`), Prop_UA = Monthly_UA/n())

#adjusting site order: WJF-CJF-PA-DB-TTP, HB-SB-AB
daily_site$Site<-factor(daily_site$Site, levels = c("WJF", "CJF", "PA", "DB", "TTP", "HB", "SB", "AB"))

#by type of UA signal
daily2<-daily %>% group_by(Year, Month) %>% 
  pivot_longer(c(`Single band UA`, `Multi-freq UA`, `Sweep UA`), 
               names_to = "UA_type", 
               values_to = "Presence") %>% 
  group_by(Year, Month, UA_type) %>% 
  mutate(Monthly = sum(Presence), Prop = Monthly/n(), 
         UA_type = factor(UA_type, levels = c("Single band UA", "Multi-freq UA", "Sweep UA")))

#by vessel type
daily3<-daily %>% group_by(Year, Month) %>% 
  pivot_longer(c(`Passenger`, `Cargo`, `Tanker`, `Pleasure`, `Unknown vessel`), 
               names_to = "Vessel_type", 
               values_to = "Presence") %>% 
  group_by(Year, Month, Vessel_type) %>% 
  mutate(Monthly = sum(Presence), Prop = Monthly/n(), 
         Vessel_type = factor(Vessel_type, levels = c("Passenger", "Cargo", "Tanker", "Pleasure", "Unknown vessel")))
lbls<-c("Cruise ship", "Cargo", "Tanker", "Pleasure", "Unknown vessel")
names(lbls)<-c("Passenger", "Cargo", "Tanker", "Pleasure", "Unknown vessel")


fig3<-function(){
  signal_plot<-ggplot(daily2) + 
    geom_point(aes(x = Month, y = Prop, color = as.factor(Year), size = Prop_year)) + 
    geom_line(aes(x = Month, y = Prop, color = as.factor(Year), group = as.factor(Year))) + 
    facet_wrap(~UA_type, scale = "free_y") + 
    scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                     labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    scale_color_manual(values = year_pal) + 
    scale_size_continuous(range = c(0.2, 3)) +
    theme_classic() + 
    labs(y = "Proportion of days with UA", color = "Year", size = "Recording effort") + 
    theme(axis.text.x = element_text(angle = 90), 
          strip.background = element_blank())
  
  vessel_plot<-ggplot(daily3) + 
    geom_point(aes(x = Month, y = Prop, color = as.factor(Year), size = Prop_year)) + 
    geom_line(aes(x = Month, y = Prop, color = as.factor(Year), group = as.factor(Year))) + 
    facet_wrap(~Vessel_type, scales = "free_y", labeller = as_labeller(lbls)) + 
    scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                     labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                     expand = c(0.1, 0)) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_color_manual(values = year_pal)+
    scale_size_continuous(range = c(0.2, 3)) + 
    theme_classic() + 
    labs(y = "Proportion of days with UA", color = "Year", size = "Recording effort") + 
    theme(axis.text.x = element_text(angle = 90), 
          strip.background = element_blank())
  
  Fig3<-signal_plot/vessel_plot + plot_layout(axes = "collect", guides = "collect") + 
    plot_annotation(tag_levels = 'a', tag_suffix = ")") & guides(size = "none")
  
  return(print(Fig3))
}

Figure3<-fig3()
ggsave("figures/fig3.png", Figure3, dpi = 600, width = 6, height = 6, units = "in")

fig4<-function(){
  #Total UA across all sites by year
  
  #plot
  tot<-ggplot(daily) + 
    geom_point(aes(x = Month, y = Prop_UA, color = as.factor(Year), size = Prop_year)) + 
    geom_line(aes(x = Month, y = Prop_UA, color = as.factor(Year), group = as.factor(Year))) + 
    scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                     labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) + 
    scale_y_continuous(limits = c(0, 0.8)) +
    scale_color_manual(values = year_pal) +
    scale_size_continuous(range = c(0.2, 3)) +
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 90)) +
    labs(y = "Proportion of days with UA", color = "Year", size = "Recording effort\n (All sites)") #+ 
  
  
  
  site_plot<-ggplot(daily_site) + 
    geom_point(aes(x = Month, y = Prop_UA, color = as.factor(Year), size = Prop_site)) + 
    geom_line(aes(x = Month, y = Prop_UA, color = as.factor(Year), group = as.factor(Year))) + 
    facet_wrap(~Site, ncol = 2, scales = "free_y") + 
    scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                     labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) + 
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    scale_color_manual(values = year_pal) +
    scale_size(range = c(0.05, 2.5), breaks = c(seq(0, 1, by = 0.05))) +
    theme_classic() + 
    labs(y = "Proportion of days with UA", color = "Year", size = "Recording effort\n (by site)") + 
    theme(axis.text.x = element_text(angle = 90), 
          strip.background = element_blank())
  
  des<-"
AAABBBB"
  
  fig4<-tot + site_plot + plot_layout(design = des, axes = "collect", guides = "collect") + 
    plot_annotation(tag_levels = 'a', tag_suffix = ")") & guides(size = "none")
  return(print(fig4))
}

Figure4<-fig4()

ggsave("figures/fig4.png", Figure4, dpi = 600, width = 8, height = 5, units = "in")


# Event model -------------------------------------------------------------

#read in model results
CIs<-read_csv("results/event_model_ests.csv")
CIs$parameter_factor<-factor(CIs$parameter_factor, 
                             levels = c("AB", "SB", "HB", "TTP", "DB", "PA", "CJF", "WJF", "speed", "distance"))
speed_preds<-read_csv("results/speed_predictions.csv")
distance_preds<-read_csv("results/distance_predictions.csv")

fig5<-function(){
  est_plot<-ggplot() + geom_point(aes(x = CIs$parameter_factor, y = CIs$Estimate, color = as.factor(CIs$col))) + 
    geom_linerange(aes(x = CIs$parameter_factor, ymin = CIs$`2.5 %`, ymax = CIs$`97.5 %`, color = as.factor(CIs$col))) + 
    geom_hline(yintercept = 0, lty = 2, lwd = 0.5, color = "black") +
    coord_flip() + 
    labs(x = "Parameter", y = "Estimate") +
    theme_classic() + 
    theme(legend.position  = "none")
  return(print(est_plot))
}

Figure5<-fig5()
ggsave("figures/fig5.png", Figure5, dpi = 600, width = 5, height = 6, units = "in")

fig6<-function(){
  g1<-ggplot() + geom_point(data = event_dat, aes(x = speed, y = duration), alpha = 0.5)
  
  speed_pred<-g1 + 
    geom_ribbon(data = speed_preds, aes(x = speed, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.5) + 
    geom_line(data = speed_preds, aes(x = speed, y = estimate), color = "blue") +
    theme_classic() + 
    scale_x_continuous(limits = c(0, max(event_dat$speed)), breaks = seq(0, 24, by = 4), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 16), breaks = seq(0, 20, by = 4), expand = c(0, 0)) + 
    labs(x = "Average vessel speed (knots)", y = "UA event duration") +
    theme(legend.position = "none")
  
  g2<-ggplot() + geom_point(data = event_dat, aes(x = distance, y = duration), alpha = 0.5)
  pred_dist<-g2 + 
    geom_ribbon(data = distance_preds, aes(x = distance, ymin = conf.low, ymax = conf.high), fill = "darkgreen", alpha = 0.5) + 
    geom_line(data = distance_preds, aes(x = distance, y = estimate), color = "darkgreen") +
    theme_classic() + 
    scale_x_continuous(limits = c(0, max(event_dat$distance)), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 16), breaks = seq(0, 20, by = 4), expand = c(0, 0)) + 
    labs(x = "Maximum vessel distance (km)", y = "UA event duration") +
    theme(legend.position = "none")
  
  Fig6<-speed_pred + pred_dist + plot_annotation(tag_levels = "a", tag_suffix = ")")
  return(print(Fig6))
}

Figure6<-fig6()
ggsave("figures/fig6.png", Figure6, dpi = 600, width = 6, height = 4, units = "in")



# KW and UA overlap -------------------------------------------------------

#monthly overlap proportions
daily_site_kw<-read_csv("results/monthly_overlap_KW.csv")
#daily overlap

fig7<-function(){
  daily_site_kw$Month_fct<-factor(daily_site_kw$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  daily_site_kw$Site<-factor(daily_site_kw$Site, levels = c("WJF", "CJF", "PA", "DB", "TTP", "HB", "SB", "AB"))
  site_plot_kw<-ggplot(daily_site_kw) + 
    geom_col(data = daily_site_kw[daily_site_kw$Zero_days == 0, ], aes(x = Month_fct, y = Prop_SRKW, width = Prop_site), position = "identity" , fill = "gray50") +
    geom_point(data = daily_site_kw[daily_site_kw$Zero_days == 1,], aes(x = Month_fct, y = Prop_SRKW), shape = 4, color = "gray50", size = 2) +
    geom_point(aes(x = Month_fct, y = Prop_UA, size = Prop_site, shape = as.factor(shape_id)), alpha = 0.5, color = sites) + 
    facet_grid(Site~Year, scales = "free_x") + 
    scale_y_continuous(limits = c(-0.05, 1.1)) +
    scale_size_continuous(range = c(0.5, 1.5), guide = "none") +
    scale_shape_manual(values = c(19, 1), guide = "none") +
    theme_minimal() + 
    labs(y = "Proportion of days", size = "Recording effort\n (by site)") + 
    theme(axis.text.x = element_text(angle = 90), 
          strip.background = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray95"))
  
  return(print(site_plot_kw))
}

Figure7<-fig7()
ggsave("figures/fig7.png", Figure7, dpi = 600, width = 7, height = 6, units = "in")
