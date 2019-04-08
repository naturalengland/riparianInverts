
library(tidyverse)
library(vegan)

#Import data----
field_data <- read_csv("../data/field_data_extract_Apr2019.csv")

pantheon_data <- readxl::read_xlsx("../data/Species and Event data pivot table March with summary.xlsx", sheet = 2)

#Prepare data----

#create species score lookup
pantheon_lookup <- pantheon_data %>% select(-event) %>% unique() 

#pantheon_lookup[is.na(pantheon_lookup$`Wetland species`)] <- 0
pantheon_lookup$`Wetland species`[pantheon_lookup$`Wetland species` == "NA"] <- 0

#convert excavations to handsearch
field_data <- field_data %>% 
  mutate(sample_type = as.factor(sample_type)) %>% 
  mutate(sample_type = recode_factor(sample_type, excavation = "hand_search")) %>% 
  droplevels() 
# #remove excavations----
# field_data <- field_data %>% 
#   filter(sample_type %in% c("pitfall", "hand_search"))


#convert to a frequency matrix----
#This matrix calculates the frequency each species occurs in each replicate. Because it's by 'replicate' each species should only occur once, so in this case the frequency matrix is the same as an occurrence matrix.
field_data_mat <- field_data %>% 
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
selected_data <- field_data_mat %>%  
  select(-c(river:sample_type))

selected_env <- field_data_mat %>%  
  select(c(river:sample_type))
unique(selected_env$river)


#create a frequency table----
field_data_freq <- field_data %>% 
  select(spp_name, sample_type) %>%
  mutate(presabs = 1) %>%
  group_by(spp_name, sample_type) %>% 
  summarise(freq = sum(presabs)) %>% 
  ungroup()

#sample types----
samptypes <- field_data_freq %>% 
  select(-freq) %>% 
  mutate(smptp = str_replace_all(sample_type, c("hand_search" = "h", "pitfall" = "p"))) %>% 
  # ungroup() %>% 
  # group_by(spp_name) %>%
  spread(key = sample_type, value = smptp, fill = "") %>% 
  mutate(sample_types = paste0(hand_search, pitfall)) %>% 
  select(spp_name, sample_types)
write_csv(samptypes, "../data/samptypes.csv")

samp_types_spp_sum <- table(samptypes$sample_types)

field_data_freq_types <- field_data_freq %>% 
  spread(key = sample_type, value = freq, fill = 0) %>% 
  full_join(samptypes) %>% 
  left_join(pantheon_lookup, by = c("spp_name" = "species"))  %>% 
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
chitest_wetland_props <- field_data_freq_types %>% 
  group_by(wetland_spp) %>% 
  count(sample_types) %>% 
  filter(wetland_spp %in% c(TRUE, FALSE)) %>% 
  spread(key = wetland_spp, value = n) %>% 
  select(-sample_types) %>% 
  chisq.test()


#extract genus data----
genus_types <- field_data_freq_types %>% 
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




#calculate jaccard dissimilarity
adon1 <- vegan::adonis2(selected_data ~ 
                          river + event_code + sample_type, 
                        data = selected_env, 
                        permutations = 120, method = "jaccard", 
                        by = "terms")


