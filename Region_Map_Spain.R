rm(list=ls())
library(tidyverse)
library(data.table)
library(lubridate)
library(zoo)
library(scales)
library(Siane)
library(rgeos)
library(rgdal)
library(maptools)
dta<-fread("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_datos_isciii_nueva_serie.csv")%>% 
  mutate(nuts1=case_when(
    grepl("Galicia|Asturias|Cantabria", ccaa) ~ "Noroeste",
    grepl("Vasco|Navarra|Rioja|Arag", ccaa) ~ "Noreste",
    grepl("Madrid", ccaa) ~ "Madrid",
    grepl("Castill|Extrema", ccaa) ~ "Centro",
    grepl("Cata|Valenc|Balear", ccaa) ~ "Este",
    grepl("Andalu|Murcia|Ceuta|Melilla", ccaa) ~ "Sur",
    grepl("Canarias", ccaa) ~ "Canarias",
  )) %>% 
  mutate(date=as_date(fecha)) %>% 
  group_by(nuts1, date) %>% 
  summarise_if(is.numeric, sum) %>% 
  select(-cod_ine) %>% 
  group_by(nuts1) %>% 
  mutate(num_casos=rollmean(num_casos_prueba_pcr, k=7, na.pad=T, align="center"))
dta<-left_join(dta, dta %>% group_by(date) %>% 
                 summarise(casos_total=sum(num_casos))) %>% 
  mutate(norm=num_casos/casos_total)

p1<-ggplot(dta, aes(x=date, y=num_casos, fill=nuts1)) +
  geom_area() +
  scale_x_date(limits=c(as_date("2020-02-01"), as_date("2020-07-01")))+
  scale_y_continuous(expand=expansion(mult=0), 
                     limits=c(0, 10000)) +
  scale_fill_brewer(type="qual", palette=2)+
  labs(x="Fecha", y="Casos diarios (suavizados a 7 dias)",
       title="Casos por Region",
       subtitle="Fuente: datadista; casos confirmados por PCR")+
  guides(fill=F)+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        legend.position = "bottom")
p2<-ggplot(dta, aes(x=date, y=num_casos, fill=nuts1)) +
  geom_area() +
  scale_x_date(limits=c(as_date("2020-02-01"), as_date("2020-07-01")))+
  scale_y_continuous(expand=expansion(mult=0), 
                     limits=c(0, 3000)) +
  scale_fill_brewer(type="qual", palette=2)+
  labs(x="Fecha", y="Casos diarios (suavizados a 7 dias)")+
  facet_wrap(~nuts1, ncol=1)+
  guides(fill=F)+
  theme_void()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.border = element_rect(fill=NA))
p3<-ggplot(dta, aes(x=date, y=norm, fill=nuts1)) +
  geom_area() +
  scale_x_date(limits=c(as_date("2020-02-01"), as_date("2020-07-01")))+
  scale_y_continuous(expand=expansion(mult=0), labels=percent) +
  scale_fill_brewer(type="qual", palette=2)+
  labs(x="", y="", title="Proporcion de los casos de todo el pais")+
  guides(fill=F)+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        legend.position = "bottom")

# Siane code from Pedro Gullon
obj<-paste0("Data/Siane")
# fixing a function in the Siane package
# you have to edit the siane map function, and change three arguments
# x= y= and x= [under shift] to dx and dy 
trace(siane_map, edit=TRUE)
shp <- siane_map(obj = obj, 
                 level = "Comunidades", 
                 canarias = TRUE, peninsula = "close")
shp_df<-fortify(shp, region = "id_inec")%>% 
  mutate(id = as.numeric(as.character(id))) %>% 
  mutate(ccaa=case_when(
    id==1 ~ "Andalusia",
    id==2 ~ "Aragon",
    id==3 ~ "Asturias",
    id==4 ~ "Balearic Islands",
    id==5 ~ "Canary Islands",
    id==6 ~ "Cantabria",
    id==7 ~ "Castille and Leon",
    id==8 ~ "Castille-La Mancha",
    id==9 ~ "Catalonia",
    id==10 ~ "Valencian Community",
    id==11 ~ "Extremadura",
    id==12 ~ "Galicia",
    id==13 ~ "Madrid",
    id==14 ~ "Murcia",
    id==15 ~ "Navarra",
    id==16 ~ "Basque Country",
    id==17 ~ "La Rioja",
    id==18 ~ "Ceuta",
    id==19 ~ "Melilla")) %>% 
  mutate(nuts1=case_when(
    grepl("Galicia|Asturias|Cantabria", ccaa) ~ "Noroeste",
    grepl("Basqu|Navarra|Rioja|Arag", ccaa) ~ "Noreste",
    grepl("Madrid", ccaa) ~ "Madrid",
    grepl("Castill|Extrema", ccaa) ~ "Centro",
    grepl("Cata|Valenc|Balear", ccaa) ~ "Este",
    grepl("Andalu|Murcia|Ceuta|Melilla", ccaa) ~ "Sur",
    grepl("Canar", ccaa) ~ "Canarias")) 

world<-readOGR(dsn = 'Data/Siane/world/world.shp')
world_df<-fortify(world)%>% mutate(id = as.numeric(id))
rect <- data.frame (xmin=-1, xmax=4.8, ymin=35.5, ymax=37.4)
map1 =ggplot() +
  geom_polygon(data = world_df, 
               aes(x = long, y = lat, group = group),
               color = 'black', size = .1, fill="grey50", alpha=0.8)+
  
  theme(axis.ticks = element_blank())+
  coord_map() +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#E7F8F9",
            color="black", size=0.2, alpha=1)
map2 =map1 +
  geom_polygon(data = shp_df,
               aes(x = long, y = lat, group = group, fill=nuts1),
               color = 'black', size = .2)+
  scale_fill_brewer(type="qual", palette=2)+
  theme_bw()+
  guides(fill=F)+
  theme(axis.ticks = element_blank(),  
        panel.background = element_rect(fill="#E7F8F9"),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_blank(), 
        axis.text=element_blank(), 
        axis.line=element_blank(),
        legend.position="right")+
  coord_map(xlim = c(-11, 5),ylim = c(35, 44)) 

# insets:
# idea from https://stackoverflow.com/questions/5219671/it-is-possible-to-create-inset-graphs
# for the region plots
xleft   = 0.05
xright  = 0.25
ybottom = 0.2
ytop    = 0.95 
# Calculate position in plot1 coordinates
# Extract x and y values from plot1
l1 = ggplot_build(p1)
x1 = l1$layout$panel_params[[1]]$x.range[1]
x2 = l1$layout$panel_params[[1]]$x.range[2]
y1 = l1$layout$panel_params[[1]]$y.range[1]
y2 = l1$layout$panel_params[[1]]$y.range[2]
xdif = x2-x1
ydif = y2-y1
xmin  = x1 + (xleft*xdif)
xmax  = x1 + (xright*xdif)
ymin  = y1 + (ybottom*ydif)
ymax  = y1 + (ytop*ydif) 
# Get plot2 and make grob
pall = p1 + annotation_custom(grob = ggplotGrob(p2), xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)


# for proportions plot
xleft   = 0.55
xright  = 0.99
ybottom = 0.55
ytop    = 0.25 
# Calculate position in plot1 coordinates
# Extract x and y values from plot1
l1 = ggplot_build(pall)
x1 = l1$layout$panel_params[[1]]$x.range[1]
x2 = l1$layout$panel_params[[1]]$x.range[2]
y1 = l1$layout$panel_params[[1]]$y.range[1]
y2 = l1$layout$panel_params[[1]]$y.range[2]
xdif = x2-x1
ydif = y2-y1
xmin  = x1 + (xleft*xdif)
xmax  = x1 + (xright*xdif)
ymin  = y1 + (ybottom*ydif)
ymax  = y1 + (ytop*ydif) 

pall = pall + annotation_custom(grob = ggplotGrob(p3), xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

# for map of regions
xleft   = 0.60
xright  = 0.99
ybottom = 0.60
ytop    = 0.99 
# Calculate position in plot1 coordinates
# Extract x and y values from plot1
l1 = ggplot_build(pall)
x1 = l1$layout$panel_params[[1]]$x.range[1]
x2 = l1$layout$panel_params[[1]]$x.range[2]
y1 = l1$layout$panel_params[[1]]$y.range[1]
y2 = l1$layout$panel_params[[1]]$y.range[2]
xdif = x2-x1
ydif = y2-y1
xmin  = x1 + (xleft*xdif)
xmax  = x1 + (xright*xdif)
ymin  = y1 + (ybottom*ydif)
ymax  = y1 + (ytop*ydif) 

# Get plot2 and make grob
pall = pall + annotation_custom(grob = ggplotGrob(map2), xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)


ggsave(file="Test_Map_Regions.pdf", pall, width=15, height=12.5)

