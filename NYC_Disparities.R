rm(list=ls())
library(tidyverse)
library(data.table)
library(tidycensus)
library(readxl)
library(broom)
library(gridExtra)
library(foreign)
library(lubridate)
library(scales)
options(scipen=999)
# obtain a census api key https://api.census.gov/data/key_signup.html
# census_api_key(key="xxx", install = T)
# use this dataset to check variable names
vars<-load_variables(year=2018, "acs5")
# example:
vars %>% filter(grepl("Insurance", concept, ignore.case=T))
# healthcare workers:
vars %>% filter(grepl("C24010", name), grepl("health", label, ignore.case=T)) %>% pull(name)
zipcode_data<-get_acs(geography = "zcta",
                      variables=c(
                        # median household income
                        "B19013_001",
                        # race/ethnicity
                        "B03002_001", 
                        "B03002_012", 
                        "B03002_004",
                        # limited english stuff
                        "C16002_001","C16002_004", "C16002_007", 
                        "C16002_010", "C16002_013",
                        # health insurance (uninsured)
                        "B27010_001","B27010_017", "B27010_033", "B27010_050", "B27010_066",
                        # healthcare workers
                        "C24010_001","C24010_016","C24010_017", 
                        "C24010_018","C24010_020",
                        "C24010_052","C24010_053",
                        "C24010_054","C24010_056",
                        # food service, personal care, and service ocupations
                        "C24010_024","C24010_060","C24010_026","C24010_062",
                        # overcrowding denominator
                        "B25014_001", 
                        #overcrowding 1 and more
                        "B25014_005", "B25014_011",
                        #overcrowding 1.5more
                        "B25014_006", "B25014_012",
                        #overcrowding 2more
                        "B25014_007", "B25014_013",
                        # public transit excluding taxicab
                        "B08006_001","B08006_008"
                      ),
                      year=2018) %>% 
  select(GEOID, variable, estimate) %>% 
  spread(variable, estimate) %>% 
  rowwise() %>% 
  # do some data management to create indicators
  mutate(mhi=B19013_001,
         pct_hisp=B03002_012/B03002_001,
         pct_black=B03002_004/B03002_001,
         total_pop=B03002_001,
         total_hisp=B03002_012,
         total_black=B03002_004,
         limited_engl=sum(c(C16002_004, C16002_007, C16002_010, C16002_013))/C16002_001,
         no_healthins=sum(c(B27010_017, B27010_033, B27010_050, B27010_066))/B27010_001,
         pct_healthcareworkers=sum(c(C24010_016,C24010_017,C24010_018,C24010_020,C24010_052,C24010_053,C24010_054,C24010_056))/C24010_001,
         pct_service=sum(c(C24010_024, C24010_026, C24010_060, C24010_062))/C24010_001,
         pct_overcrowded2=sum(c(B25014_007, B25014_013))/B25014_001,
         pct_overcrowded15=sum(c(B25014_007, B25014_006,
                                 B25014_012, B25014_013))/B25014_001,
         pct_overcrowded1=sum(c(B25014_007, B25014_006,B25014_005,
                                B25014_011,B25014_012, B25014_013))/B25014_001,
         pct_transit=B08006_008/B08006_001,
         GEOID=as.numeric(GEOID)) %>% 
  select(GEOID, mhi, pct_hisp, pct_black, total_pop, total_hisp, total_black,
         limited_engl, no_healthins, pct_healthcareworkers,pct_service,
         pct_transit,
         pct_overcrowded2, pct_overcrowded15, pct_overcrowded1)

# only need to run these two commands once
download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_zcta510_500k.zip",
              destfile ="US Data/cb_2018_us_zcta510_500k.zip" )
unzip("US Data/cb_2018_us_zcta510_500k.zip", exdir = "US Data/zipcodeshp/")

area<-read.dbf("US Data/zipcodeshp/cb_2018_us_zcta510_500k.dbf") %>% 
  mutate(GEOID=as.numeric(as.character(GEOID10)),
         area=ALAND10/1000000/2.59) %>% 
  select(GEOID, area)

# NYC data, import and merge
both<-fread("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/tests-by-zcta.csv") %>% 
  # renaming zipcode identifier to GEOID
  rename(GEOID=MODZCTA,
         all=`Total`, 
         positives=`Positive`) %>% 
  filter(!is.na(GEOID)) %>% 
  left_join(area) %>% 
  left_join(zipcode_data) %>% 
  mutate(density=total_pop/area,
         pct_pos=positives/all,
         pos_pc=positives/total_pop,
         tests_pc=all/total_pop*1000)


# MHI plot
p1<-ggplot(both, aes(x=mhi, y=pct_pos)) +
  stat_smooth(method="loess")+
  geom_point()+
  scale_x_log10(breaks=c(10000, 20000, 30000, 40000,
                         50000, 70000, 100000, 200000))+
  scale_y_continuous(limits=c(NA, NA), labels=percent)+
  annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="Median Household Income\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))
# % Black
p2<-ggplot(both, aes(x=pct_black, y=pct_pos)) +
  stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(NA, NA), labels=percent)+
  scale_x_continuous(limits=c(NA, NA), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% non-Hispanic Black\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))

# % Hispanic and tests
p3<-ggplot(both, aes(x=pct_hisp, y=pct_pos)) +
  stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(NA, NA), labels=percent)+
  scale_x_continuous(limits=c(NA, NA), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Hispanic\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))

# Limited english proficiency
p4<-ggplot(both, aes(x=limited_engl, y=pct_pos)) +
  stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(NA, NA), labels=percent)+
  scale_x_continuous(limits=c(NA, NA), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% households with Limited English Profiency\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))

# health insurance
p5<-ggplot(both, aes(x=no_healthins, y=pct_pos)) +
  stat_smooth(method="loess")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  #annotation_logticks(sides="b")+
  scale_y_continuous(limits=c(NA, NA), labels=percent)+
  scale_x_continuous(labels=percent)+
  theme_bw() +
  labs(x="% with no Health Insurance\n (2014-2018)",
       y="% Positive tests",
       title="")+
  theme(axis.text=element_text(color="black"))
# overcrowding
p6<-ggplot(both, aes(x=pct_overcrowded2, y=pct_pos)) +
  stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(NA, NA), labels=percent)+
  scale_x_continuous(limits=c(NA, NA), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>2 ppl/room)\n (2014-2018)",
       y="% Positive tests",
       title="")
p7<-ggplot(both, aes(x=pct_overcrowded15, y=pct_pos)) +
  stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(NA, NA), labels=percent)+
  scale_x_continuous(limits=c(NA, NA), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>1.5 ppl/room)\n (2014-2018)",
       y="% Positive tests",
       title="")
p8<-ggplot(both, aes(x=pct_overcrowded1, y=pct_pos)) +
  stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(NA, NA), labels=percent)+
  scale_x_continuous(limits=c(NA, NA), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Overcrowding (>1 ppl/room)\n (2014-2018)",
       y="% Positive tests",
       title="")
# service workers
p9<-ggplot(both, aes(x=pct_service, y=pct_pos)) +
  stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(NA, NA), labels=percent)+
  scale_x_continuous(limits=c(NA, NA), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Service Workers [food prep/serving, personal care, \nand service occupations] (2014-2018)",
       y="% Positive tests",
       title="")

library(grid)
library(gridExtra)
pall<-(arrangeGrob(grobs=list(p1, p2, p3, p4, p5, p9, p6, p7, p8), ncol=3, 
                   top=textGrob("% Positive Tests per Zip Code in NYC", gp=gpar(fontsize=20,face="bold",font=8)),
                   bottom=textGrob("Source: NYHealth (github) and 5-year 2014-2018 ACS", gp=gpar(fontsize=10,font=8))))
ggsave("tests_NYC.pdf", pall, width=12, height=12/(20/15))

model<-glm(positives~log(mhi)+I(no_healthins*20)+I(pct_black*20)+
             I(pct_hisp*20)+I(limited_engl*20)+
             I(no_healthins*20)+
             I(pct_overcrowded15*20)+
             I(pct_service*20)+
             I(density/10000)+
             offset(log(all)),
           family="poisson", data=both) %>% 
  tidy %>% 
  mutate(lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error,
         estimate=exp(estimate),
         lci=exp(lci),
         uci=exp(uci)) %>% 
  filter(term!="(Intercept)") %>% 
  mutate(RR=paste0(format(estimate, digits=2, nsmall=2), "", " (",
                   format(lci, digits=2, nsmall=2), "", ";",
                   format(uci, digits=2, nsmall=2), "", ")")) %>% 
  mutate(variable=case_when(
    grepl("mhi", term) ~ "Median Household Income (1% increase)",
    grepl("engl", term) ~ "% HH with Limited English Proficiency (5% increase)",
    grepl("health", term) ~ "% Uninsured (5% increase)",
    grepl("black", term) ~ "% Black (5% increase)",
    grepl("hisp", term) ~ "% Hispanic (5% increase)",
    grepl("ins", term) ~ "% Uninsured (5% increase)",
    grepl("overcr", term) ~ "% Overcrowded [>1.5ppl/room] (5% increase)",
    grepl("dens", term) ~ "Density (10k/sq.mi increase)",
    grepl("service", term) ~ "% Service Workers (5% increase)",
  )) %>% 
  select(variable, RR)
model

lm((pct_pos)~log(mhi)+I(no_healthins*20)+I(pct_black*20)+
     I(pct_hisp*20)+I(limited_engl*20)+
     I(no_healthins*20)+
     I(pct_overcrowded15*20)+
     I(pct_service*20)+
     I(density/10000),data=both) %>% 
  glance
