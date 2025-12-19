setwd("C:\\Users\\Owner")

load("C:\\Users\\Owner\\University of Oregon Dropbox\\Megan Fenner\\cascades-meadows\\data\\spec_net.Rdata")

source("C:\\Users\\Owner\\University of Oregon Dropbox\\Megan Fenner\\cascades-meadows\\dataPrep\\src\\prepNets.R")
source("C:\\Users\\Owner\\University of Oregon Dropbox\\Megan Fenner\\cascades-meadows\\dataPrep\\src\\misc.R")
source("C:\\Users\\Owner\\University of Oregon Dropbox\\Megan Fenner\\cascades-meadows\\dataPrep\\src\\specialization.R")
source("C:\\Users\\Owner\\University of Oregon Dropbox\\Megan Fenner\\cascades-meadows\\dataPrep\\1_specimenPrep.R")

install.packages ("janitor")
library(janitor)
library(dplyr)
library(stringr)
library(tibble)

unique(spec.net$Order)
#create a data frame that only includes the columns from spec.net listed below
pollinators <- spec.net %>%
  #select(c("Genus", "GenusSpecies", "Order", "Family")) I am removing these from the data
  #because they are missing values in the data set
  select(c("Genus", "GenusSpecies", "Order", "Family"))

unique(pollinators$Genus)
unique(pollinators$GenusSpecies)
unique(pollinators$Family)
#regular expression that cleans the data of any occurance of "spp.", "spp", and "sp"- which also excludes
#the instances where there is sp(some number). It also excludes instances where there are three words.
clean_pol <- pollinators %>%
  filter(!grepl("\\b(?:spp\\.?|sp)\\b", GenusSpecies)) %>%
  filter(grepl("^[A-Za-z]+ [A-Za-z]+$", GenusSpecies))


as.character(pollinators$Genus)
as.character(pollinators$GenusSpecies)
as.character(pollinators$Order)
as.character(pollinators$Family)
as.character(clean_pol$Genus)
as.character(clean_pol$GenusSpecies)
as.character(clean_pol$Order)
as.character(clean_pol$Family)
sort(unique(clean_pol$GenusSpecies))
sort(unique(clean_pol$Order))
#from row 33011 to 35653, the data are missing their order (assuming that this is even continuous)
#there are also data missing in the rows for family- however, this is not continuous

#cleaning up the sorted data- changing genus and genus speices names
clean_pol <- clean_pol %>%  
  mutate(Genus= recode(Genus, 
                        "Euphydras" = "Euphydryas",
                        "Glaucopysene" = "Glaucopsyche",
                        "Parnasius" = "Parnassius", 
                        "Polyginia" = "Polygonia",
                        "Sphecomya" = "Sphecomyia",
                       " Eristalis" = "Eristalis"),
         GenusSpecies = recode(GenusSpecies, 
                               "Euphydras colon" = "Euphydryas colon",
                               "Euphydras editha" = "Euphydryas editha",
                               "Glaucopysene lygdamus" = "Glaucopsyche lygdamus",
                               "Parnasius clodius" = "Parnassius clodius",
                               "Polyginia gracilis" = "Polygonia gracilis",
                               "Sphecomya columbiana" = "Sphecomyia columbiana"))
sort(unique(clean_pol$GenusSpecies))

#generating a table for count of each genus species for the data that include both
#genus & species

count_pol_G <- clean_pol %>%
  group_by(Genus) %>%
  summarize(occ = n())

##cleaning up the raw data- changing genus and genus speices names
clean_pol_all <- pollinators %>%  
  mutate(Genus= recode(Genus, 
                       "Euphydras" = "Euphydryas",
                       "Glaucopysene" = "Glaucopsyche",
                       "Parnasius" = "Parnassius", 
                       "Polyginia" = "Polygonia",
                       "Sphecomya" = "Sphecomyia",
                       " Eristalis" = "Eristalis"),
         GenusSpecies = recode(GenusSpecies, 
                               "Euphydras colon" = "Euphydryas colon",
                               "Euphydras editha" = "Euphydryas editha",
                               "Glaucopysene lygdamus" = "Glaucopsyche lygdamus",
                               "Parnasius clodius" = "Parnassius clodius",
                               "Polyginia gracilis" = "Polygonia gracilis",
                               "Sphecomya columbiana" = "Sphecomyia columbiana"))

#counting the occurrence of each genus species
count_pol_all <- clean_pol_all %>%
  group_by(Genus) %>%
  summarize(occ= n())
sort(unique(clean_pol_all$Genus))

#removing the rows that do not have any data in them AT ALL
rem_empty_pall <- clean_pol_all %>%
  mutate(across(where(is.character), ~ na_if(str_squish(.), ""))) %>%
  remove_empty(which = c("rows"), cutoff = 1, quiet = TRUE)

#trying to figure out a way to input data in the columns where data are missing;
#thinking that maybe I could use a for if loop? 
#for(rem_empty_pall in [ , ]){
  #if(rem_empty_pall$Family == "Acroceridae"){ 

rem_empty_pall$Order[rem_empty_pall$Family == "Acroceridae"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Andrenidae"] <- "Hymenoptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Apidae"] <- "Hymenoptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Blera"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Bombyliidae"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Cleridae"] <- "Coleoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Colletes"] <- "Colletidae" #had the wrong Order name
rem_empty_pall$Order[rem_empty_pall$Genus == "Colletes"] <- "Colletidae"
rem_empty_pall$Order[rem_empty_pall$Family == "Dialictus"] <- "Hymenoptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Halictidae"] <- "Hymenoptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Lycaenidae"] <- "Lepidoptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Megachilidae"] <- "Hymenoptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Meloidae"] <- "Coleoptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Nymphalidae"] <- "Lepidoptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Syriphidae"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Tachinidae"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Family == "Vespidae"] <- "Hymenoptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Ceratina"] <- "Hymenoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Ceratina"] <- "Apidae"
rem_empty_pall$Family[rem_empty_pall$Genus == "Erynnis"] <- "Hesperiidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Chlosyne"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Chlosyne"] <- "Nymphalidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Chrysotoxum"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Colias"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Colias"] <- "Pieridae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Didea"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Eristalis"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Erynnis"] <- "Lepidoptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Euphydryas"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Euphydryas"] <- "Nymphalidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Glaucopsyche"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Glaucopsyche"] <- "Lycaenidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Hadromyia"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Hesperia"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Hesperia"] <- "Hesperiidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Hylaeus"] <- "Hymenoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Hylaeus"] <- "Colletidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Icaricia"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Icaricia"] <- "Lycaenidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Lasioglossum"] <- "Hymenoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Lasioglossum"] <- "Halictidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Lepturobosca"] <- "Coleoptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Lycaena"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Lycaena"] <- "Lycaenidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Neophasia"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Neophasia"] <- "Pieridae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Ochlodes"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Ochlodes"] <- "Hesperiidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Parnassius"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Parnassius"] <- "Papilionidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Platycheirus"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Phyciodes"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Phyciodes"] <- "Nymphalidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Polygonia"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Polygonia"] <- "Nymphalidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Pyrgus"] <- "Lepidoptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Sericomyia"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Sphaerophoria"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Sphecomyia"] <- "Diptera"
rem_empty_pall$Order[rem_empty_pall$Genus == "Speyeria"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Speyeria"] <- "Nymphalidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Strymon"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Strymon"] <- "Lycaenidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Synanthedon"] <- "Lepidoptera"
rem_empty_pall$Family[rem_empty_pall$Genus == "Synanthedon"] <- "Sesiidae"
rem_empty_pall$Order[rem_empty_pall$Genus == "Xylota"] <- "Diptera"

sort(unique(rem_empty_pall$Order))
sort(unique(rem_empty_pall$Family))
#checking to make sure there are no NAs in the dataframe anymore
sum(is.na(rem_empty_pall))
print(paste("the number of NAs is", sum(is.na(rem_empty_pall))))

cleancount_pall <- rem_empty_pall %>%
  group_by(Genus) %>%
  summarize(occ= n())
sort(unique(rem_empty_pall$Genus))

nrow(count_pol_G) #87
sort(unique(count_pol_G$Genus))
nrow(cleancount_pall) #101
sort(unique(cleancount_pall$Genus))

#testing to see which values are different between the data frames
unique_between_clean <-setdiff(count_pol_G, cleancount_pall)
unique_between_pall <-setdiff(cleancount_pall, count_pol_G)

#this is actually kind of wild; I have told R to look in the data frame count_pol_G and
#compare this data frame to clean_count_pall. It has looked instead to see where the
#numbers of occurences differs. Because I have told it that I am comparing count_pol_G to 
#cleancount_pall, the number that it outputs is the number for count_pol_G. So, in theory, 
#not only would this show which Genuses are not in the data, but also the instances where
#the counts are not the same! Might make future analysis easier; I didn't expect R to do this

#created a simple table which highlights which genuses are missing from the cleaned data
unique_spec <-as_tibble(symdiff(count_pol_G$Genus, cleancount_pall$Genus)) #the 
#math checks out with above- 14 values (87+14=101)





