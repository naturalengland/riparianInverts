
library(tidyverse)
library(vegan)

#Import data----
##import wetland coleoptera data 
obs_all <- read_csv("../data/field_data_selected_Apr2019.csv") #all observations
##import species lookup data
pantheon_data <- readxl::read_xlsx("../data/Species and Event data pivot table March with summary.xlsx", sheet = 2)

#Prepare data----

#create event lookup 
event_lookup <- obs_all %>% select(river, event_code) %>% unique()

#create species score lookup
spp_lookup <- pantheon_data %>% select(-event) %>% unique() 
#spp_lookup[is.na(spp_lookup$`Wetland species`)] <- 0
spp_lookup$`Wetland species`[spp_lookup$`Wetland species` == "NA"] <- 0
#convert excavations to handsearch
obs_all <- obs_all %>% 
  mutate(sample_type = as.factor(sample_type)) %>% 
  mutate(sample_type = recode_factor(sample_type, excavation = "hand_search")) %>% 
  droplevels() 
# #remove excavations
# obs_all <- obs_all %>% 
#   filter(sample_type %in% c("pitfall", "hand_search"))


#convert to a frequency matrix----
#This matrix calculates the frequency each species occurs in each replicate. Because it's by 'replicate' each species should only occur once, so in this case the frequency matrix is the same as an occurrence matrix.
obs_all_mat <- obs_all %>% 
  #calculate frequencies
  group_by(river, replicate, event_code, location, sample_type, spp_name) %>% 
  summarise(n = length(spp_name)) %>% 
  #abbreviate species names
  mutate(spp_name = vegan::make.cepnames(spp_name)) %>% 
  #spread to a matrix
  spread(key = spp_name, value = n, fill = 0) %>%
  data.frame() %>%
  arrange(river) 

#split species matrix into data and environment matrices----
selected_data <- obs_all_mat %>%  
  select(-c(river:sample_type))

selected_env <- obs_all_mat %>%  
  select(c(river:sample_type))
unique(selected_env$river)


#create a frequency table----
obs_all_freq <- obs_all %>% 
  select(spp_name, sample_type) %>%
  mutate(presabs = 1) %>%
  group_by(spp_name, sample_type) %>% 
  summarise(freq = sum(presabs)) %>% 
  ungroup()

#sample types----
samptypes <- obs_all_freq %>% 
  select(-freq) %>% 
  mutate(smptp = str_replace_all(sample_type, c("hand_search" = "h", "pitfall" = "p"))) %>% 
  # ungroup() %>% 
  # group_by(spp_name) %>%
  spread(key = sample_type, value = smptp, fill = "") %>% 
  mutate(sample_types = paste0(hand_search, pitfall)) %>% 
  select(spp_name, sample_types)
write_csv(samptypes, "../data/samptypes2.csv")

samp_types_spp_sum <- table(samptypes$sample_types)

obs_all_freq_types <- obs_all_freq %>% 
  spread(key = sample_type, value = freq, fill = 0) %>% 
  full_join(samptypes) %>% 
  left_join(spp_lookup, by = c("spp_name" = "species"))  %>% 
  rename("cons_status" = "Conservation status", 
         "wetland_spp" = "Wetland species", 
         "runningwater_spp" = "running water W23",  
         "peatland_spp" = "peatland W25", 
         "marshland_spp" = "marshland W24", 
         "lake_spp" = "Lake W22",
         "wetwoodland_spp" = "wet woodland W21", 
         "ERS_spp" =  "ERS W312",
         "ripariansand_spp" = "riparian sand W45", 
         "riparianshingle_spp" = "riparian shingle W46",
         "drawdownzone_spp" = "drawdown zone W38",
         "wetlandveg_spp" = "wetland vegetation W34") %>% 
  mutate_at(vars(cons_status:wetlandveg_spp), as.logical)


#test difference in sampling type proportions between wetland and non wetland species
chitest_wetland_props <- obs_all_freq_types %>% 
  group_by(wetland_spp) %>% 
  count(sample_types) %>% 
  filter(wetland_spp %in% c(TRUE, FALSE)) %>% 
  spread(key = wetland_spp, value = n) %>% 
  select(-sample_types) %>% 
  chisq.test()


#extract genus data----
genus_types <- obs_all_freq_types %>% 
  separate(col = spp_name, into = c("genus", NA), remove = T) %>% 
  select(-c(hand_search, pitfall)) %>%
  group_by(genus, sample_types) %>% 
  summarise(freq = length(genus)) %>% 
  spread(key = sample_types, value = freq, fill = 0) %>% 
  mutate(n_spp = h+hp+p) %>% 
  mutate(handsearch = h) %>% 
  mutate(pitfall = p) %>% 
  mutate(handsearch_pitfall = hp) %>% 
  select(genus, handsearch, pitfall, handsearch_pitfall, n_spp) %>% 
  arrange(-n_spp)
##Dissimilarity
#calculate jaccard dissimilarity
adon1 <- vegan::adonis2(selected_data ~ river + event_code + sample_type, data = selected_env, permutations = 120, method = "jaccard", by = "terms")

##Species accumulation curves ----

#extract recent events
event_code_selected <- c("Frome_2017_2", "Frome_2017_1", "Frome_2017_3", "Lugg_2014_1",
                         "Lugg_2014_3", "Lugg_2014_4", "Wye_2014_6", "Wye_2014_7", 
                         "Wye_2014_4", "Wye_2014_3", "Wye_2014_5", "Lugg_2014_2", 
                         "Wye_2014_1", "Dove_2013", "Till_2013", "Beamish_2013", 
                         "Wooler_2013")

obs_rct_mat <- obs_all_mat %>% 
  filter(event_code %in% event_code_selected) %>% droplevels() 



###Grand total accumulation
#species accumulation curve by resampling events
spec_accum_selected <- specaccum(comm = obs_rct_mat %>% select(-c(river:sample_type)))



param_sampletype <- c("hand_search", "pitfall")
param_rivers <- unique(obs_rct_mat$river)
param_eventcode <- unique(obs_rct_mat$event_code)


#create empty data frame as template for accumulation data
spec_accum_empty <- data_frame(
  sample_type = as.character(), 
  river = as.character(), 
  event_code = as.character(),
  n_sites = as.integer(), n_spp = as.double(), sd = as.double())


###accumulation by river
spec_accum_byriver <- spec_accum_empty
spec_accum_temp <- spec_accum_empty

#iterate through rivers
for(selectedriver in param_rivers){
  
  tempdata <- obs_rct_mat %>%
    filter(river == selectedriver) %>%  
    select(-c(river:sample_type))
  
  tempdata <- if(nrow(tempdata)>1){
    
    tempdata <- specaccum(tempdata)
    tempdata <- data.frame(
      sample_type = "all",
      river = selectedriver,
      event_code = "all",
      n_sites = tempdata$sites,
      n_spp = tempdata$richness,
      sd = tempdata$sd) 
    
    spec_accum_temp <- rbind(spec_accum_temp, tempdata)}
}
spec_accum_byriver <-  
  if(nrow(spec_accum_temp)>0){rbind(spec_accum_byriver, spec_accum_temp)}


###accumulation by event

spec_accum_byevent <- spec_accum_empty
spec_accum_temp <- spec_accum_empty

for(selectedevent in param_eventcode){
  tempdata <- obs_rct_mat %>%
    filter(event_code == selectedevent) %>%  
    select(-c(river:sample_type))
  
  tempdata <- if(nrow(tempdata)>1){
    
    tempdata <- specaccum(tempdata)
    
    tempdata <- data.frame(
      sample_type = "all",
      river = event_lookup$river[which(event_lookup$event_code == selectedevent)],
      event_code = selectedevent,
      n_sites = tempdata$sites,
      n_spp = tempdata$richness,
      sd = tempdata$sd) 
    
    spec_accum_temp <- rbind(spec_accum_temp, tempdata)}
}
spec_accum_byevent <-  
  if(nrow(spec_accum_temp)>0){rbind(spec_accum_byevent, spec_accum_temp)}

###accumulation by event and sample type

spec_accum_bysampletype <- spec_accum_empty
spec_accum_temp <- spec_accum_empty

#iterate through events
for(selectedevent in param_eventcode){
  #iterate through samples
  for(sampletype in param_sampletype){
    
    tempdata <- obs_rct_mat %>%
      filter(sample_type == sampletype & 
               event_code == selectedevent) %>%  
      select(-c(river:sample_type))
    
    tempdata <- if(nrow(tempdata)>1){
      
      tempdata <- specaccum(tempdata)
      
      tempdata <- data.frame(
        sample_type = sampletype,
        river = event_lookup$river[which(event_lookup$event_code == selectedevent)],
        event_code = selectedevent,
        n_sites = tempdata$sites,
        n_spp = tempdata$richness,
        sd = tempdata$sd) 
      
      spec_accum_temp <- rbind(spec_accum_temp, tempdata)}
  }
  spec_accum_bysampletype <-  
    if(nrow(spec_accum_temp)>0){rbind(spec_accum_bysampletype, spec_accum_temp)}
}

