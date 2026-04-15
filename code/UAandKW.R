#Exploration of UA overlap with Killer whales
#Zoe Rand
#last updated: 4/15/26

library(tidyverse)
#library(ggh4x) # for special scales


# Read in data ------------------------------------------------------------
files<-paste0("data/DailyPresence/DailyPresence_", c("CJF", "TTP", "AB", "DB", "WJF", "SB", "HB", "PA"), "_2019-2023.csv")
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

#adding effort
daily<-daily %>% left_join(effort_ymsite2, by = c("Year", "Month", "Site")) %>% left_join(effort_ym, by = c("Year", "Month"))

daily_site<-daily %>% group_by(Year, Month, Site) %>% 
  mutate(Monthly_UA = sum(`UA absence/presence`), Prop_UA = Monthly_UA/n())

#adjusting site order: WJF-CJF-PA-DB-TTP, HB-SB-AB
daily_site$Site<-factor(daily_site$Site, levels = c("WJF", "CJF", "PA", "DB", "TTP", "HB", "SB", "AB"))

#adding KWs
daily_site_kw<-daily_site %>% 
  mutate(Monthly_SRKW = sum(SRKW), Prop_SRKW = Monthly_SRKW/n())


# Colors ------------------------------------------------------------------

sites<-"#80cdc1"

pal3<-c( "#429e42", "#42c3cf", "goldenrod1","#c6d2b9", 'gray90')


# Overlap by site ---------------------------------------------------------
daily_site_kw$Month_fct<-factor(daily_site_kw$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
#making it so 0 bars actually show up
daily_site_kw<-daily_site_kw %>% mutate(Zero_days = ifelse(Prop_SRKW == 0, 1, 0))
#making 0 days of UA open circles
daily_site_kw$shape_id<-ifelse(daily_site_kw$Prop_UA > 0, 1, 2)

write_csv(daily_site_kw, "results/monthly_overlap_KW.csv")

site_plot_kw<-ggplot(daily_site_kw) + 
  geom_col(data = daily_site_kw[daily_site_kw$Zero_days == 0, ], aes(x = Month_fct, y = Prop_SRKW, width = Prop_site), position = "identity" , fill = "gray50") +
  geom_point(data = daily_site_kw[daily_site_kw$Zero_days == 1,], aes(x = Month_fct, y = Prop_SRKW), shape = 4, color = "gray50", size = 1) +
  geom_point(aes(x = Month_fct, y = Prop_UA, size = Prop_site, shape = as.factor(shape_id)), alpha = 0.5, color = sites) + 
  facet_grid(Site~Year, scales = "free_x") + 
  scale_y_continuous(limits = c(-0.05, 1.1)) +
  scale_size_continuous(range = c(0.01, 1.5), guide = "none") +
  scale_shape_manual(values = c(19, 1), guide = "none") +
  theme_minimal() + 
  labs(y = "Proportion of days", size = "Recording effort\n (by site)") + 
  theme(axis.text.x = element_text(angle = 90), 
        strip.background = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray95"))

site_plot_kw


# Overlap by day ----------------------------------------------------------

daily_site_overlap<-daily_site_kw %>% 
  mutate(bothSRKWandUA = ifelse(SRKW == 1 & `UA absence/presence` == 1, 1, 0), 
         justUA = ifelse(bothSRKWandUA == 0 & `UA absence/presence` ==1, 1, 0), 
         justSRKW = ifelse(bothSRKWandUA == 0 & SRKW == 1, 1, 0), 
         datetime = as_date(mdy(Date)), 
         recording_day = 1)

a<-min(daily_site_overlap$datetime)
b<-max(daily_site_overlap$datetime)
alldays<-tibble(datetime = rep(seq(ymd(a), ymd(b), by = "1 day"), nsites,), 
                Site = rep(c("WJF", "CJF", "PA", "DB", "TTP", "HB", "SB", "AB"),each = length(seq(ymd(a), ymd(b), by = "1 day"))))

daily_site_overlap2<-alldays %>% left_join(daily_site_overlap) %>% 
  mutate(recording_day = ifelse(is.na(recording_day), 0, 1)) %>% 
  mutate(col = ifelse(recording_day == 0, "no recording",
                      ifelse(bothSRKWandUA == 1, "both", 
                             ifelse(justUA == 1, "UA", 
                                    ifelse(justSRKW == 1, "SRKW", "none")))))

daily_site_overlap2$col<-factor(daily_site_overlap2$col, levels = c("both", "SRKW", "UA", "none", "no recording"))  
daily_site_overlap2$Site<-factor(daily_site_overlap2$Site, levels = c("WJF", "CJF", "PA", "DB", "TTP", "HB", "SB", "AB"))

write_csv(daily_site_overlap2, "results/KW_UA_overlap_daily.csv")

labs<-c("Both SRKW and UA", "SRKW only", "UA only", "None","No recording")


kw_overlap_daily<-ggplot(daily_site_overlap2) + 
  geom_tile(aes(x = datetime, y = Site, fill = col)) +
  scale_fill_manual(values = pal3, labels = labs) + 
  theme_classic() + 
  scale_x_date(expand = c(0, 0), date_breaks = "2 months", 
               date_labels  = "%b %Y") + 
  scale_y_discrete(expand = c(0, 0), limits = rev) + 
  labs(x = "Day", y = "Site", fill = NULL) + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.x = element_text(angle = 90))
kw_overlap_daily


#NOTES TO COME BACK TO:
#why doesn't this start at July? 


