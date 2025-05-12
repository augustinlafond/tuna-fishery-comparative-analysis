install.packages("rfishbase")
library(rfishbase)


# Extract total natural mortality M (predation + senescence + disease) of tuna species from FishBase
m <- rfishbase::popgrowth("Thunnus albacares") %>%
  summarise (m = mean(M, na.rm = T)) %>%
  mutate(species = "yellowfin") %>%
  bind_rows (rfishbase::popgrowth("Katsuwonus pelamis") %>%
               summarise (m = mean(M, na.rm = T)) %>%
               mutate(species = "skipjack")) %>%
  bind_rows (rfishbase::popgrowth("Thunnus obesus") %>%
               summarise (m = mean(M, na.rm = T)) %>%
               mutate (species = "bigeye"))

# Extract natural mortality N (senescence + diseases) of tuna species from Ecopath

# Model 29 for Central Atlantic
# Model 42 of eastern tropical pacific

#To get the ouptput values for model mymodel - 403 

library(RCurl)
library(XML)
library(plyr)

#To get data for the altantic
h=basicTextGatherer()
mymodel<-29
curlPerform(url = paste('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=',mymodel,sep=''),writefunction=h$update,verbose=TRUE)
data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)

n_atlantic <-xpathSApply(data,'//group',function(x) xmlToList(x)) %>%
  as.tibble () %>%
  select(bigeye = V5, skipjack = V27, yellowfin = V38) %>%
  slice (8) %>%
  unnest () %>%
  mutate_all (as.numeric) %>%
  pivot_longer (cols = c(1:3), names_to = "species", values_to = "n_atlantic")


#To get data for the pacific
h=basicTextGatherer()
mymodel<-42
curlPerform(url = paste('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=',mymodel,sep=''),writefunction=h$update,verbose=TRUE)
data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)

n_pacific <-xpathSApply(data,'//group',function(x) xmlToList(x)) %>%
  as.tibble () %>%
  select(large_bigeye = V10, small_bigeye = V29, skipjack = V28, large_yellowfin = V18, small_yellowfin = V37) %>%
  slice (8) %>%
  unnest () %>%
  mutate_all (as.numeric) %>%
  summarise (bigeye = mean(small_bigeye, large_bigeye), skipjack, yellowfin = mean (small_yellowfin, large_yellowfin)) %>%
  pivot_longer (cols = c(1:3), names_to = "species", values_to = "n_pacific")


#To get data for the Indian
# There is only one Ecopath with Ecosim model in the Indian, model 441 for Sri lanka

h=basicTextGatherer()
mymodel<-441
curlPerform(url = paste('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=',mymodel,sep=''),writefunction=h$update,verbose=TRUE)
data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)

n_indian <-xpathSApply(data,'//group',function(x) xmlToList(x)) %>%
  as_tibble () %>%
  select(small_tuna = V32, medium_tuna = V16, large_tuna = V15) %>%
  slice (8) %>%
  unnest () %>%
  mutate_all (as.numeric) %>%
  summarise (bigeye = rowMeans(.), skipjack = rowMeans(.), yellowfin = rowMeans(.)) %>%
  pivot_longer (cols = c(1:3), names_to = "species", values_to = "n_indian")

sequestration_factor <- n_pacific %>%
  left_join (n_atlantic) %>%
  left_join (n_indian) %>%
  left_join (m) %>%
  rowwise () %>%
  mutate(sequestration_pacific = n_pacific/m,
         sequestration_atlantic = n_atlantic/m,
         sequestration_indian = n_indian/m) %>%
  ungroup ()


write.xlsx(sequestration_factor, "output/data/sequestration_factors.xlsx")







# to get the list of models

#To get the list of available Ewe models

library(RCurl)
library(XML)
library(plyr)
library(dplyr)

######################################################
#####################################################
#To obtain the list of available model
library (janitor)
library(stringi)
library(sf)

h=basicTextGatherer()
curlPerform(url = 'http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php',writefunction=h$update)

data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)
liste_mod<-ldply(xmlToList(data),data.frame)%>% filter(model.dissemination_allow =='true')

#liste_mod contains a list and decription of available models in EcoBase    

liste_mod <- liste_mod %>%
  clean_names () %>%
  mutate(coord = stri_extract_all_regex(model_geographic_extent, "(\\-)?([0-9]+)(\\.)?([0-9]+)?")) %>%
  mutate(long1 = as.numeric(map_chr(coord, 1)),
         lat1 = as.numeric(map_chr(coord, 2)),
         long2 = as.numeric(map_chr(coord, 3)),
         lat2 = as.numeric(map_chr(coord, 4))) %>%
  select(model_model_number, long1, lat1, long2, lat2)


liste_mod <- melt(setDT(liste_mod), measure= patterns("long", "lat"), 
     value.name= c("Longitude", "Latitude"), na.rm=TRUE)[,variable:=NULL][] 

liste_mod <- liste_mod %>%
  st_as_sf (coords = c("Longitude", "Latitude"), crs = 4326) %>%
  arrange(model_model_number) 

box_sf <- liste_mod %>% 
  group_by(model_model_number) %>%
  nest()

bbox_wrap <- function(x) st_as_sfc(st_bbox(x))
box_sf <- box_sf %>% 
  mutate(bbox = map(data, bbox_wrap))

liste_mod <- box_sf %>% 
  mutate(geometry = st_as_sfc(do.call(rbind, bbox))) %>% 
  select(-data, -bbox) %>% 
  st_as_sf(crs = 4326)

mapview(liste_mod)