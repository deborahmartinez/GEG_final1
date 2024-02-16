
# libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(magrittr)
library(readr)
library(purrr)
library(janitor)
library(lubridate)
library(googlesheets4)
library(ggchicklet)
library(leaflet)
library(sf)


# raw data ----------------------------------------------------------------
setwd("data/transportation/")
# https://climatetrace.org/data
bd <-  list.files(pattern = "\\_country_emissions.csv$") %>% 
  map_df(~read_csv(.))

setwd("../../")
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?view=chart
country_class <- read_csv("data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_125/Metadata_Country_API_NY.GDP.MKTP.CD_DS2_en_csv_v2_125.csv") %>% 
  clean_names()
gdp <- read_csv("data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_125/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_125.csv", skip = 4) %>% 
  clean_names()

# https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/
wordl<- sf::read_sf("data/world-administrative-boundaries/") %>% 
  filter(status == "Member State")

# arrange and join data ---------------------------------------------------

country_class<- country_class %>%  select(country_code:income_group) %>% 
  na.omit()

gdp <-gdp %>%  select(country_code, country_name, gdp=x2022)

aux <- gdp %>%  inner_join(country_class) 

bd <-bd %>%  filter(gas == "co2")  %>% 
  mutate(year = as.numeric(format(start_time,'%Y'))) %>% 
  select(country_code = iso3_country, year, type2 = original_inventory_sector,
         co2 = emissions_quantity) %>% 
  inner_join(aux) %>% 
  mutate( type2 = gsub(pattern = "-transportation", replacement = "", x = type2),
          type2 = gsub(pattern = "-transport", replacement = "", x = type2),
          type2 = str_to_sentence(type2),
          type = case_when(type2 == "Domestic-aviation"~"Aviation", 
                           type2 =="International-aviation"~"Aviation",
                           type2 == "Domestic-shipping"~"Shipping", 
                           type2 =="International-shipping"~"Shipping",
                           T~type2 ) ) 

rm(aux, country_class, gdp)

# exploratory anlisys -----------------------------------------------------
# numers menioned in the paper
bd %>% filter(year == 2022) %>%   
  summarise(co2=sum(co2, na.rm = T))

# General
bd %>%  group_by(type) %>%  summarise(co2=sum(co2, na.rm = T)) %>% 
  ggplot(aes(x = fct_reorder(type, co2), y = co2))+
  geom_chicklet(width = .7, fill = "pink")+
  coord_flip()+
  theme_minimal()

bd %>% filter(!year == 2023) %>%   group_by(year) %>% 
  summarise(co2=sum(co2 /1000000000, na.rm = T)) %>% 
  ggplot(aes(x = year, y = co2))+
  geom_point(aes(x= year, y= co2))+
  geom_line()

bd %>%  group_by(type, year) %>%  summarise(co2=sum(co2/1000000000, na.rm = T)) %>% 
  ggplot(aes(x = year, y = co2, color = type))+
  geom_line()

bd %>%  group_by(income_group, type, year) %>%  summarise(co2=sum(co2, na.rm = T)) %>%
  mutate(income_group = factor(income_group, c("High income", "Upper middle income",
                                               "Lower middle income", "Low income"))) %>% 
  ggplot(aes(x = year, y = co2 %>%  log(), color = type))+
  geom_line() +
  theme(legend.position = "bottom")+
  facet_wrap(~income_group, nrow = 1)


bd %>%  group_by(region, type, year) %>%  summarise(co2=sum(co2, na.rm = T)) %>% 
  ggplot(aes(x = year, y = co2 %>%  log(), color = type))+
  geom_line() +
  facet_wrap(~region)

bd %>%  group_by(type, year) %>%filter(!type == "Road") %>% 
  summarise(co2=sum(co2, na.rm = T)) %>% 
  ggplot(aes(x = year, y = co2, color = type))+
  geom_line()

bd %>%  group_by(country_name) %>% filter(year==2022) %>% 
  summarise(co2 = sum(co2, na.rm = T)) %>%  top_n(10)%>% 
  ggplot(aes(x = fct_reorder(country_name, co2), y = co2))+
  geom_chicklet(width = .7, fill = "pink")+
  coord_flip()+
  theme_minimal()


bd %>% filter(year == 2022) %>% 
  mutate(income_group = factor(income_group, c("High income", "Upper middle income",
                                               "Lower middle income", "Low income"))) %>% 
  ggplot(aes(x= income_group, y = co2 %>%  log() , color= income_group))+
  geom_boxplot(width =.3)+
  geom_jitter(color="gray", size=0.4, alpha=0.7)+
  # facet_wrap(~year)+
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

bd %>% filter(year == 2022) %>% 
  ggplot(aes(x=fct_reorder(str_wrap(region, 10), co2), y = co2 %>%  log() , color= region))+
  geom_boxplot()+
  geom_jitter(color="gray", size=0.4, alpha=0.7)+
  # facet_wrap(~year)+
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

bd %>% filter(year == 2022) %>%  
  filter(co2>0) %>% 
  mutate(income_group = factor(income_group, c("High income", "Upper middle income",
                                               "Lower middle income", "Low income"))) %>% 
  ggplot(aes(x= gdp %>%  log(), y = co2 %>%log(), color= income_group))+
  geom_point()+
  theme(legend.position = "bottom")+
  facet_wrap(~type)

bd %>% filter(year == 2022) %>%  group_by(country_code) %>% 
  summarise(co2= sum(co2, na.rm = T)) %>% 
  left_join(bd %>%  select(country_code,income_group,region, gdp) %>%  unique()) %>% 
  # filter(co2>0) %>% 
  ggplot(aes(x= gdp %>%  log(), y = co2 %>%log(), color = region))+
  geom_point()+
  theme(legend.position = "bottom")






aux <- bd %>%  filter(year == 2022) %>% 
  group_by(country_code) %>%  summarise(co2=sum(co2, na.rm = T))

auxi <- wordl %>%  left_join(aux %>% mutate(co2 = log(co2)) %>%  
                               filter(!is.na(co2), is.finite(co2) ),
                             by= c("iso3" = "country_code"))

bins <- c(0,5,9,12, 17, 21, 22, Inf )
pal <- colorBin("YlOrRd", domain = auxi$co2, bins = bins)

mapita <- leaflet(auxi) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


labels <- sprintf(
  "<strong>%s</strong><br/>%g log CO<sup>2</sup> emissions (tonnes)",
  auxi$name, auxi$co2 %>%  round(1)) %>%
  lapply(htmltools::HTML)

mapita %>% addTiles() %>% 
  addPolygons( fillColor =  ~pal(co2),
               weight = 2,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.7,
               highlightOptions = highlightOptions(
                 weight = 5,
                 color = "#666",
                 dashArray = "",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"))




bd %>%  group_by(type, year) %>%  summarise(co2 =sum(co2, na.rm = T)) %>% 
  ungroup() %>% 
  spread(key = year, value = co2 ) %>% 
  mutate(dif16= `2016`-`2015`,
         dif17= `2017`-`2016`,
         dif18= `2018`-`2017`,
         dif19= `2019`-`2018`,
         dif20= `2020`-`2019`,
         dif21= `2021`-`2020`,
         dif22= `2022`-`2021`,
         dif23= `2023`-`2022`) %>% 
  # gather("year", "co2",`2015`:`2023`)%>% 
  select(-c(`2015`:`2023`)) %>% 
  gather("aux", "dif",dif16:dif23) %>% 
  ggplot(aes(x = aux, y= dif, color= type, group = type))+
  geom_line()+
  theme_minimal()+
  theme(legend.position = "bottom")

bd %>%  group_by(type, year, income_group) %>%  summarise(co2 =sum(co2, na.rm = T)) %>% 
  ungroup() %>% 
  spread(key = year, value = co2 ) %>% 
  mutate(dif16= `2016`-`2015`,
         dif17= `2017`-`2016`,
         dif18= `2018`-`2017`,
         dif19= `2019`-`2018`,
         dif20= `2020`-`2019`,
         dif21= `2021`-`2020`,
         dif22= `2022`-`2021`,
         dif23= `2023`-`2022`) %>% 
  # gather("year", "co2",`2015`:`2023`)%>% 
  select(-c(`2015`:`2023`)) %>% 
  gather("aux", "dif",dif16:dif23) %>% 
  ggplot(aes(x = aux, y= dif, color= type, group = type))+
  geom_line()+
  theme_minimal()+
  theme(legend.position = "bottom")+
  facet_wrap(~income_group)

bd %>%  group_by(region, year, ) %>%  summarise(co2 =sum(co2, na.rm = T)) %>% 
  ungroup() %>% 
  spread(key = year, value = co2 ) %>% 
  mutate(dif16= `2016`-`2015`,
         dif17= `2017`-`2016`,
         dif18= `2018`-`2017`,
         dif19= `2019`-`2018`,
         dif20= `2020`-`2019`,
         dif21= `2021`-`2020`,
         dif22= `2022`-`2021`,
         dif23= `2023`-`2022`) %>% 
  # gather("year", "co2",`2015`:`2023`)%>% 
  select(-c(`2015`:`2023`)) %>% 
  gather("aux", "dif",dif16:dif23) %>% 
  filter(aux%in% c("dif22", "dif21")) %>% 
  ggplot(aes(x = aux, y= dif, color= region, group = region))+
  geom_line()+
  theme_minimal()+
  theme(legend.position = "bottom")

bd %>%  group_by(type, year) %>% 
  summarise(co2 =sum(co2, na.rm = T)) %>% 
  group_by(type) %>% 
  mutate(dif = case_when(year == 2016))



bd %>%  group_by(country_code, year) %>%  summarise(co2 =sum(co2, na.rm = T)) %>% 
  ungroup() %>% 
  spread(key = year, value = co2 ) %>% 
  mutate(dif16= `2016`-`2015`,
         dif17= `2017`-`2016`,
         dif18= `2018`-`2017`,
         dif19= `2019`-`2018`,
         dif20= `2020`-`2019`,
         dif21= `2021`-`2020`,
         dif22= `2022`-`2021`,
         dif23= `2023`-`2022`) %>% 
  # gather("year", "co2",`2015`:`2023`)%>% 
  select(-c(`2015`:`2023`)) %>% 
  gather("aux", "dif",dif16:dif23) %>% 
  ungroup() %>% 
  left_join(bd %>%  select(country_code, gdp) %>%  unique()) %>% 
  ggplot(aes(x = gdp %>% log(), y= dif %>%  log()))+
  geom_point()+
  theme_minimal()+
  theme(legend.position = "bottom")

bd %>%  filter(type =="Aviation") %>%  
  group_by(type2, year) %>%  summarise(co2 =sum(co2, na.rm = T)) %>% 
  ggplot(aes(x = year, y= co2, color = type2))+
  geom_line()


# (pretty) final charts  ---------------------------------------------------

#  figure 1
bd %>%  group_by(country_name) %>% filter(year == 2022) %>% 
  summarise(co2=sum(co2, na.rm = T)) %>% 
  top_n(10) %>% 
  ggplot(aes(x = fct_reorder(country_name, co2), y = co2/ 1000000000))+
  geom_chicklet(width = .6, fill = "#708090")+
  coord_flip()+
  labs(title = "Top 10 country emitters of CO2 \n in 2022",
       x = "Countries", y = "Billions of tonnes")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text =element_text(family = "Times New Roman", size = 14))

#  2

bd %>% filter(!year == 2023) %>%   group_by(year) %>% 
  summarise(co2=sum(co2 /1000000000, na.rm = T)) %>% 
  ggplot(aes(x = year, y = co2))+
  geom_line(color= "#708090", size = 1.5)+
  geom_point(aes(x= year, y= co2), color = "#2F4F4F", size= 3)+
  labs(title = "Yearly CO2 emissions made by the transportation industry",
       x = "", y = "Billions of tonnes")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text =element_text(family = "Times New Roman", size = 15))


bd %>%  group_by(year) %>%  summarise(co2 =sum(co2, na.rm = T)) %>% 
  ungroup() %>% 
  spread(key = year, value = co2 ) %>% 
  mutate(dif16= `2016`-`2015`,
         dif17= `2017`-`2016`,
         dif18= `2018`-`2017`,
         dif19= `2019`-`2018`,
         dif20= `2020`-`2019`,
         dif21= `2021`-`2020`,
         dif22= `2022`-`2021`,
         dif23= `2023`-`2022`) %>% 
  # gather("year", "co2",`2015`:`2023`)%>% 
  select(-c(`2015`:`2023`)) %>% 
  gather("aux", "dif",dif16:dif22) %>% 
  ungroup() %>% select(aux, dif) %>% 
  ggplot(aes(x = aux, y= dif /1000000000, group = 1))+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "#B80300", size=.7)+
  geom_line(color= "#708090", size = 1.5) +
  labs(title = "Yearly differences in emissions made by the transportation industry",
       x = "Year", y = "Billions of tonnes")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text =element_text(family = "Times New Roman", size = 15))


bd %>%  group_by(type, year) %>%  summarise(co2=sum(co2/1000000000, na.rm = T)) %>% 
  ggplot(aes(x = year, y = co2, color = type))+
  geom_line( size = 1)+
  scale_color_manual(values = c("Aviation" = "#6883BA",
                                
                                "Railways" = "#FF6978",
                                "Road" = "#FEB95F",
                                "Shipping" = "#B1EDE8",
                                "Other" = "gray"))+
  labs(title = "Yearly emissions made by type of transport",
       x = "", y = "Billions of tonnes", color = NULL)+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        text =element_text(family = "Times New Roman", size = 15))


bd %>%  group_by(type, year) %>%  summarise(co2 =sum(co2, na.rm = T)) %>% 
  ungroup() %>% 
  spread(key = year, value = co2 ) %>% 
  mutate(dif16= `2016`-`2015`,
         dif17= `2017`-`2016`,
         dif18= `2018`-`2017`,
         dif19= `2019`-`2018`,
         dif20= `2020`-`2019`,
         dif21= `2021`-`2020`,
         dif22= `2022`-`2021`,
         dif23= `2023`-`2022`) %>% 
  # gather("year", "co2",`2015`:`2023`)%>% 
  select(-c(`2015`:`2023`)) %>% 
  gather("aux", "dif",dif16:dif23) %>% 
  ggplot(aes(x = aux, y= dif/1000000000, color= type, group = type))+
  geom_line(size= .7)+
  theme_minimal()+
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Aviation" = "#6883BA",
                                
                                "Railways" = "#FF6978",
                                "Road" = "#FEB95F",
                                "Shipping" = "#B1EDE8",
                                "Other" = "gray"))+
  labs(title = "Yearly change in emissions made by type of transport",
       x = "", y = "Billions of tonnes", color = NULL)+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        text =element_text(family = "Times New Roman", size = 15))

bd %>%  group_by(income_group, type, year) %>%  summarise(co2=sum(co2, na.rm = T)) %>%
  mutate(income_group = factor(income_group, c("High income", "Upper middle income",
                                               "Lower middle income", "Low income"))) %>% 
  ggplot(aes(x = year, y = co2 %>%  log(), color = type))+
  geom_line(size=.8) +
  theme(legend.position = "bottom")+
  facet_wrap(~income_group, nrow = 2) +
  scale_color_manual(values = c("Aviation" = "#6883BA",
                                
                                "Railways" = "#FF6978",
                                "Road" = "#FEB95F",
                                "Shipping" = "#B1EDE8",
                                "Other" = "gray"))+
  labs(title = "Yearly Co2 emissions made by type of transport by income group",
       x =NULL, y = "Tonnes (log)", color = NULL)+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        text =element_text(family = "Times New Roman", size = 15))



bd %>%  group_by(region, year, ) %>%  summarise(co2 =sum(co2, na.rm = T)) %>% 
  ungroup() %>% 
  spread(key = year, value = co2 ) %>% 
  mutate(dif16= `2016`-`2015`,
         dif17= `2017`-`2016`,
         dif18= `2018`-`2017`,
         dif19= `2019`-`2018`,
         dif20= `2020`-`2019`,
         dif21= `2021`-`2020`,
         dif22= `2022`-`2021`,
         dif23= `2023`-`2022`) %>% 
  # gather("year", "co2",`2015`:`2023`)%>% 
  select(-c(`2015`:`2023`)) %>% 
  gather("aux", "dif",dif16:dif23) %>% 
  filter(aux%in% c("dif22", "dif21")) %>% 
  ggplot(aes(x = aux, y= dif/1000000000, color= region, group = region))+
  geom_line(size=1)+
  labs(title = "2021 and 2022 change in emissions made by region",
       x = NULL, y = "Billions of tonnes", color = NULL)+
  scale_color_manual(values = c("#708090", "#FF6347", "#4682B4", "#8A2BE2", "#32CD32", "#BA55D3", "#FFD700"))+
    theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        text =element_text(family = "Times New Roman", size = 15))


