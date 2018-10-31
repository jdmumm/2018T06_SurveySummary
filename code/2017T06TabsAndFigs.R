#Format tables for 2017 PWS Survey SUmmary, mstly from 2017T04 script, in turn mstly from 931_popEstAndCPUE_161101.R used for 2016 report

#LOAD ----
library(tidyverse)
library(stats)
library(plotrix)
library (Hmisc)
library(extrafont)
font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

options (scipen = 10)
dat <- read.csv('./data/qP_simp_171127.csv') # N-H
dat_vldz <- read.csv('./data/qP_simp_vldz_171127.csv')# Vldz  
events <- read.csv('./data/events.csv')
events %>% filter (PROJECT_CODE == 'T06', GEAR_PERFORMANCE_CODE == '1') %>%
  select( Event = EVENT_ID,
          year = YEAR,
          Project = PROJECT_CODE, 
          length=TOW_LENGTH_DESIGNATED,
          totCatch = CATCH_WEIGHT) -> event
## Males Main ----

# N-H
m <- dat[dat$PROJECT_CODE == "T06", c(1:7,16,18,20,22,24,26)] 
#combine news and olds for P1s and P2s
m$P1 <- m$MT5_P_ + m$MT6_P_
m$P2 <- m$MT7_P_ + m$MT8_P_
names(m)[c(12,13)] <- c('P3','P4')      #"MT9_P_"  "MT10_P_"
m <- m[,-(8:11)] # remove columns with new old split
#reorder columns
m <- m[,c(1,2,3,9,8,11,10,4:7)]
m
#write.csv(m,'./output/931PopMales_Main.csv')

# Vldz  - samee as above, just differnt file names. 
m <- dat_vldz[dat_vldz$PROJECT_CODE == "T06", c(1:7,16,18,20,22,24,26)] 
#combine news and olds for P1s and P2s
m$P1 <- m$MT5_P_ + m$MT6_P_
m$P2 <- m$MT7_P_ + m$MT8_P_
names(m)[c(12,13)] <- c('P3','P4')      #"MT9_P_"  "MT10_P_"
m <- m[,-(8:11)] # remove columns with new old split
#reorder columns
m <- m[,c(1,2,3,9,8,11,10,4:7)]
m
 #write.csv(m,'./output/931PopMales_Main_vldz.csv')

## Females Main ----
dat %>% filter (PROJECT_CODE == 'T06') %>% select(year = YEAR, tows = n,
                                                     FT11_P_, FT11_P_CI_, MF_P_, MF_P_CI_, TF_P_, TF_P_CI_) -> f

#write.csv(f,'./output/931PopFems_Main.csv')

## Females Vldz ----
dat_vldz %>% filter (PROJECT_CODE == 'T06') %>% select(year = YEAR, tows = n,
                                                  FT11_P_, FT11_P_CI_, MF_P_, MF_P_CI_, TF_P_, TF_P_CI_) -> f_v

write.csv(f_v,'./output/931PopFems_Main_vldz.csv')

##Plot LM ---- 

#Convert to thousands of crabs
dat$LM_P <- dat$LM_P_/1000
dat$LM_P_CI <- dat$LM_P_CI_/1000
dat <- select(dat, "proj" = PROJECT_CODE, "yr" = YEAR,  LM_P, LM_P_CI)
dat %>% filter (proj == "T06", yr > 1991)-> t06

#LM
t06 %>% 
  ggplot(aes(x = yr, y = LM_P) )+
  scale_x_continuous(breaks = seq(1991,2017,1))  +
  scale_y_continuous(breaks = seq(0,300,50)) + 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5))+
  labs( x= 'Year', y = 'Thousands of Crab') +
  geom_point(size = 2)+ 
  geom_errorbar(aes(ymin=ifelse((LM_P - LM_P_CI) > 0 , (LM_P - LM_P_CI), 0), ymax=LM_P+LM_P_CI, width = .5)) + 
  geom_hline(yintercept = 200, lty = 'dashed')

#ggsave("./figs/T06LM.png", dpi=300, height=8, width=6.5, units="in")

#Plot Valdez ---- 
dat <- dat_vldz 
dat$LM_P <- dat$LM_P_/1000
dat$LM_P_CI <- dat$LM_P_CI_/1000
dat <- select(dat, "proj" = PROJECT_CODE, "yr" = YEAR,  LM_P, LM_P_CI)
dat %>% filter (proj == "T06", yr > 1991)-> t06

t06 %>% 
  ggplot(aes(x = yr, y = LM_P) )+
  scale_x_continuous(breaks = seq(1999,2017,1))  +
  scale_y_continuous(breaks = seq(0,250,50)) + 
  theme( axis.text.x  = element_text(angle=90, vjust=0.5))+
  labs( x= 'Year', y = 'Thousands of Crab') +
  geom_point(size = 2)+ 
  geom_errorbar(aes(ymin=ifelse((LM_P - LM_P_CI) > 0 , (LM_P - LM_P_CI), 0), ymax=LM_P+LM_P_CI, width = .5))

#ggsave("./figs/T06LM_vldz.png", dpi=300, height=8, width=6.5, units="in")


