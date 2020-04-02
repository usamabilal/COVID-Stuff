rm(list=ls())
library(tidyverse)
library(data.table)
library(tidycensus)
library(readxl)
library(broom)
library(gridExtra)
library(foreign)
library(scales)
options(scipen=999)
# obtain a census api key https://api.census.gov/data/key_signup.html
# census_api_key(key="xxx", install = T)
# use this dataset to check variable names
vars<-load_variables(year=2018, "acs5")
# example:
vars %>% filter(grepl("Insurance", concept, ignore.case=T))
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
                        "B27010_001","B27010_017", "B27010_033", "B27010_050", "B27010_066"
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
         GEOID=as.numeric(GEOID)) %>% 
  select(GEOID, mhi, pct_hisp, pct_black, total_pop, total_hisp, total_black,limited_engl, no_healthins)
# philly data, obtained from https://www.phila.gov/programs/coronavirus-disease-2019-covid-19/the-citys-response/monitoring-and-testing/
# new data from April 1st
map<-read_excel("Philly/Mapping_crosstab 040120.xlsx") %>% 
  # renaming zipcode identifier to GEOID
  rename(GEOID=ZIP,
         all=`All Tests along`, 
         negatives=`Negatives along`, 
         positives=`Positives along`, 
         pct_pos=`Positivity % along`) %>% 
  # remove industrial zip codes
  filter(`Zip Label`!="<5") %>% 
  select(-`Zip Label`)


# join philly data with census data
both<-left_join(map, zipcode_data) %>% 
  # calculate tests per capita
  mutate(tests_pc=all/total_pop*1000,
         positives_pc=positives/total_pop*1000) 

# MHI plot
p1<-ggplot(both, aes(x=mhi, y=tests_pc)) +
  stat_smooth(method="loess")+
  geom_point()+
  scale_x_log10(breaks=c(20000, 30000, 40000,
                         50000, 70000, 100000))+
  scale_y_continuous(limits=c(0, NA))+
  annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="Median Household Income (2014-2018)",
       y="Tests conducted per 1000 people",
       title="Number of total tests per Zipcode in Philadelphia",
       caption="Source: PDPH and 5-year ACS. Blue line is a loess smoother")+
  theme(axis.text=element_text(color="black"))



# % Black
p2<-ggplot(both, aes(x=pct_black, y=tests_pc)) +
  #stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(0, NA))+
  scale_x_continuous(limits=c(0, 1), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% non-Hispanic Black (2014-2018)",
       y="Tests conducted per 1000 people",
       title="Number of total tests per Zipcode in Philadelphia",
       caption="Source: PDPH and 5-year ACS")+
  theme(axis.text=element_text(color="black"))

# % Hispanic and tests
p3<-ggplot(both, aes(x=pct_hisp, y=tests_pc)) +
  #stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(0, NA))+
  scale_x_continuous(limits=c(0, .6), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% Hispanic (2014-2018)",
       y="Tests conducted per 1000 people",
       title="Number of total tests per Zipcode in Philadelphia",
       caption="Source: PDPH and 5-year ACS")+
  theme(axis.text=element_text(color="black"))

# Limited english proficiency
p4<-ggplot(both, aes(x=limited_engl, y=tests_pc)) +
  #stat_smooth(method="loess")+
  # annotate("rect", xmin=0, xmax=.20, ymin=0, ymax=15,
  #          alpha=0.3, color="gray")+
  # annotate("rect", xmin=.70, xmax=.95, ymin=2.5, ymax=6,
  #          alpha=0.3, color="gray")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  scale_y_continuous(limits=c(0, NA))+
  scale_x_continuous(limits=c(0, .3), labels=percent)+
  #annotation_logticks(sides="b")+
  theme_bw() +
  labs(x="% households with Limited English Profiency (2014-2018)",
       y="Tests conducted per 1000 people",
       title="Number of total tests per Zipcode in Philadelphia",
       caption="Source: PDPH and 5-year ACS")+
  theme(axis.text=element_text(color="black"))

# health insurance
p5<-ggplot(both, aes(x=no_healthins, y=tests_pc)) +
  stat_smooth(method="loess")+
  geom_point()+
  # scale_x_log10(limits=c(15000, 120000),
  #               breaks=c(10000, 20000, 30000, 40000,
  #                        50000, 70000, 100000))+
  #annotation_logticks(sides="b")+
  scale_y_continuous(limits=c(0, NA)) +
  scale_x_continuous(labels=percent)+
  theme_bw() +
  labs(x="% with no Health Insurance (2014-2018)",
       y="Tests conducted per 1000 people",
       title="Number of total tests per Zipcode in Philadelphia",
       caption="Source: PDPH and 5-year ACS. Blue line is a loess smoother")+
  theme(axis.text=element_text(color="black"))

# and just testing a predictive model
model<-lm(log(tests_pc)~log(mhi)+I(no_healthins*20)+I(pct_black*20)+I(pct_hisp*20)+I(limited_engl*20), 
   data=both) %>% 
  summary %>% tidy %>% 
  mutate(lci=estimate-1.96*std.error,
         uci=estimate+1.96*std.error) %>% 
  mutate(estimate=ifelse(term=="log(mhi)", exp(estimate)-1, estimate)*100,
         lci=ifelse(term=="log(mhi)", exp(lci)-1, lci)*100,
         uci=ifelse(term=="log(mhi)", exp(uci)-1, uci)*100) %>% 
  filter(term!="(Intercept)") %>% 
  mutate(coef=paste0(round(estimate, digits=1), "%", " (",
                     round(lci, digits=1), "%", ";",
                     round(uci, digits=1), "%", ")")) %>% 
  mutate(variable=case_when(
    grepl("mhi", term) ~ "Median Household Income (1% increase)",
    grepl("engl", term) ~ "% HH with Limited English Proficiency (1% increase)",
    grepl("health", term) ~ "% Uninsured (5% increase)",
    grepl("black", term) ~ "% Black (5% increase)",
    grepl("hisp", term) ~ "% Hispanic (5% increase)",
  )) %>% 
  select(variable, coef)

pall<-arrangeGrob(grobs=list(p1, p2, p3, p4, p5, tableGrob(model, rows = NULL)),
            ncol=3)
ggsave("Philly/all_plots.pdf", pall, width=20, height=12.5)
