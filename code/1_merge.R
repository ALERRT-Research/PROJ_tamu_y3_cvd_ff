source("./0_packages.R")

#import data (clean TAMU data dir)
data_clean <- "../../DATA_tamu_fire_police/data_clean/"

anthro  <- import(paste0(data_clean, "anthropometrics/anthro_clean.rds"))
biomet  <- import(paste0(data_clean, "biometrics/biomet_clean.rds"))
biospec <- import(paste0(data_clean, "biospecimen/biospec_clean.rds"))
demo    <- import(paste0(data_clean, "demographics/demo_clean.rds"))
dexa    <- import(paste0(data_clean, "dexa/dexa_clean.rds"))
fitness <- import(paste0(data_clean, "fitness/fitness_clean.rds"))
id      <- import(paste0(data_clean, "ids/ids_clean.rds"))

#merge data
data_merge <- list(anthro,  
                   biomet,  
                   biospec, 
                   demo,    
                   dexa,    
                   fitness, 
                   id) |> 
  reduce(full_join)

#drop individual dataframes
rm(data_clean,
   anthro,  
   biomet,  
   biospec, 
   demo,    
   dexa,    
   fitness, 
   id)


