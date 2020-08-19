
library(tidyverse)
library(vegan)

#Import data----
##import wetland coleoptera data 
obs_orig <- read_csv("../data/field_data_selected_Aug2019.csv") #processed observations
obs_new <- read_csv("../data/field_data_additional_Oct2019.csv") %>% #new observations
  transmute(spp_name = taxon, abund_num = as.numeric(quantity), abund_char = NA,
            river = river, event =  event, event_code = event_code,
            date = Date, year = year, gridref = grid_ref, sample_type = sample_type,
            location = location_name, repl_dateloc = NA, repl_date = NA, replicate = NA,
            sadler_bell = "Y", search_pitfall = NA,  search_excavation = NA,
            sample_duration = NA, coord_precision = NA, easting = NA, northing = NA)

obs_all <- bind_rows(obs_orig, obs_new)

##import species lookup data

spp_lookup_joined <- read_csv("../data/spp_lookup_with_family.csv")

spp_matched_corrected <- read.csv("../data/speciesmatch_further_correctionsAug2019.csv") 
effort_data <- read_csv("../data/effort_JW_Oct2019.csv")
  #readxl::read_xlsx("../data/Riparian Beetle Assemblages July2019 ck added.xlsx", sheet = 3)

jw_spp_event_data <- readxl::read_xlsx("../data/Riparian Beetle Assemblages July2019 ck added.xlsx", sheet = 2, skip = 1)

#Prepare data----

#create event lookup 
event_lookup <- obs_all %>% select(river, event_code) %>% unique() 
#add a consistent event-year column because record years may vary
event_lookup <- event_lookup %>% 
  mutate(evt_year = str_extract(string = event_lookup$event_code,pattern = "[0-9]{4}"))
event_lookup$evt_year[which(event_lookup$event_code %in% c("Dane_East", "Dane_West"))] <- "2003" #manual fix
#write it back into the main observations table
obs_all <- obs_all %>% full_join(event_lookup, by = c("river", "event_code")) %>% select(spp_name:year, evt_year, everything())

#below not needed, as resulting file has been modified, now imported.  
# #create species score lookup
# pantheon_data <- readxl::read_xlsx("../data/Species and Event data pivot table March with summary.xlsx", sheet = 2)
# spp_lookup <- pantheon_data %>% select(-event) %>% unique() 
# #spp_lookup[is.na(spp_lookup$`Wetland species`)] <- 0
# spp_lookup$`Wetland species`[spp_lookup$`Wetland species` == "NA"] <- 0
# #add further lookup fields
# extra_lookup <- spp_matched_corrected %>% 
#   mutate(species = match) %>% 
#   mutate(is_wetland = wetland) %>% 
#   select(-c(old_name, match, wetland)) %>% 
#   unique() %>% 
#   filter(!duplicated(species)) %>% 
#   select(species, everything())%>% 
#   arrange(species) 
# spp_lookup_joined <- left_join(spp_lookup, extra_lookup, by = "species") 
# #write to csv
# write_csv(spp_lookup_joined, "../data/spp_lookup_joined.csv")

#convert excavations to handsearch
obs_all <- obs_all %>% 
  mutate(sample_type = as.factor(sample_type)) %>% 
  mutate(sample_type = recode_factor(sample_type, excavation = "hand_search")) %>% 
  droplevels() 


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


#This matrix calculates the frequency each species occurs in each event. Because it's by 'event' each species may occur more than once.
obs_all_mat_evt <- obs_all %>% 
  #calculate frequencies
  group_by(river, event_code, location, sample_type, spp_name) %>% 
  summarise(n = length(spp_name)) %>% 
  #abbreviate species names
  mutate(spp_name = vegan::make.cepnames(spp_name)) %>% 
  #spread to a matrix
  spread(key = spp_name, value = n, fill = 0) %>%
  data.frame() %>%
  arrange(river) 


#the previous version makes no sense as it retains sample type as a factor, below proper frequency by event
obs_all_mat_evt2 <- obs_all %>% 
  #calculate frequencies
  group_by(river, event_code, spp_name) %>% 
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
  spread(key = sample_type, value = smptp, fill = "") %>% 
  mutate(sample_types = paste0(hand_search, pitfall)) %>% 
  select(spp_name, sample_types)
write_csv(samptypes, "../data/samptypes2.csv")

samp_types_spp_sum <- table(samptypes$sample_types)

obs_all_freq_types <- obs_all_freq %>% 
  spread(key = sample_type, value = freq, fill = 0) %>% 
  full_join(samptypes) %>% 
  left_join(spp_lookup_joined, by = c("spp_name" = "species"))  %>% 
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
  mutate_at(vars(cons_status:wetlandveg_spp), as.logical) %>% 
  mutate_at(vars(length.min,length.max), as.character)  %>% 
  mutate_at(vars(length.min,length.max), as.double)



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

#prepare effort scores ----
effort_data <- effort_data %>% 
  select(event_code = Event_code_v3, 
         event = Event_name_v3,
         effort = `EFFORT = C+D`,
         pitfall = `number of 2 week pitfall durations (P)`,
         handsearch = `number of timed  searches/excavations (T)`,
         author = Author,
         grid_ref = `Grid Reference`,
         n_records = `total number of records`) %>% 
  filter(event != is.na(event))
  

##Dissimilarity----
#calculate jaccard dissimilarity
adon1 <- vegan::adonis2(selected_data ~ river + event_code + sample_type, data = selected_env, permutations = 120, method = "jaccard", by = "terms")

##Species accumulation curves ----

#extract recent events----
event_code_selected <- c("Frome_2017_a", "Frome_2017_b", "Frome_2017_c", 
                         "Lugg_2014_a", "Lugg_2014_b", "Lugg_2014_c",
                         "Lugg_2014_d", "Wye_2014_a", "Wye_2014_b",
                         "Wye_2014_c", "Wye_2014_d", "Wye_2014_e", 
                         "Wye_2014_f", "Dove_2013", "Till_2013",
                         "Beamish_2013", "Wooler_2013")

obs_rct_mat <- obs_all_mat %>% 
  filter(event_code %in% event_code_selected) %>% droplevels() 

obs_rct_mat_evt <- obs_all_mat_evt %>% filter(event_code %in% event_code_selected) %>% droplevels()

obs_rct_mat_evt2 <- obs_all_mat_evt2 %>% filter(event_code %in% event_code_selected) %>% droplevels()

###Grand total accumulation----
#species accumulation curve by resampling replicates
spec_accum_selected <- specaccum(comm = obs_rct_mat %>% select(-c(river:sample_type)))

spec_accum_event <- specaccum(comm = obs_rct_mat_evt %>% select(-c(river:sample_type)))

param_sampletype <- c("hand_search", "pitfall")
param_rivers <- unique(obs_rct_mat$river)
param_eventcode <- unique(obs_rct_mat$event_code)



###accumulation by function ----
#create empty data frame as template for accumulation data
specaccumby <- function(specaccum_object, 
                        param_eventcode = param_eventcode, 
                        param_rivers = param_rivers, 
                        param_sampletype = param_sampletype){

spec_accum_by <- data_frame(
  sample_type = as.character(), 
  river = as.character(), 
  event_code = as.character(),
  n_sites = as.integer(), n_spp = as.double(), sd = as.double())

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
spec_accum_by <-  
  if(nrow(spec_accum_temp)>0){rbind(spec_accum_by, spec_accum_temp)}

}

###Manual accumulations (i.e. not using above function)


#create empty data frame as template for accumulation data----
spec_accum_empty <- data_frame(
  sample_type = as.character(), 
  river = as.character(), 
  event_code = as.character(),
  n_sites = as.integer(), n_spp = as.double(), sd = as.double())


###accumulation by river (replicate level)---
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

###accumulation by river (event level)----
spec_accum_byriver_evt <- spec_accum_empty
spec_accum_temp <- spec_accum_empty

#iterate through rivers
for(selectedriver in param_rivers){
  
  tempdata <- obs_rct_mat_evt2 %>%
    filter(river == selectedriver) %>%  
    select(-c(river:event_code))
  
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
spec_accum_byriver_evt <-  
  if(nrow(spec_accum_temp)>0){rbind(spec_accum_byriver_evt, spec_accum_temp)}

###accumulation by river (event level - random accumulation)----
spec_accum_byriver_evt_random <- spec_accum_empty
spec_accum_temp <- spec_accum_empty

#iterate through rivers
for(selectedriver in param_rivers){
  
  tempdata <- obs_rct_mat_evt2 %>%
    filter(river == selectedriver) %>%  
    select(-c(river:event_code))
  
  tempdata <- if(nrow(tempdata)>1){
    
    tempdata <- specaccum(tempdata, method = "random")
    tempdata <- data.frame(
      sample_type = "all",
      river = selectedriver,
      event_code = "all",
      n_sites = tempdata$sites,
      n_spp = tempdata$richness,
      sd = tempdata$sd) 
    
    spec_accum_temp <- rbind(spec_accum_temp, tempdata)}
}
spec_accum_byriver_evt_random <-  
  if(nrow(spec_accum_temp)>0){rbind(spec_accum_byriver_evt, spec_accum_temp)}


###accumulation by event----

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

###accumulation by event and sample type----

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

