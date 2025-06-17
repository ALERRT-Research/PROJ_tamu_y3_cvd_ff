source("./0_packages.R")

#=Get analysis data============================================================
data_23 <- import("../input/tamu_23.rds") 

#====List IVs and DVs

#body comp
mod1.1_ivs <- paste0("bodycomp_PC", 1:2)
mod1.2_ivs <- c("age", mod1.1_ivs)

#fitness
mod2.1_ivs <- c("fitness_PC1")
mod2.2_ivs <- c("age", mod2.1_ivs)

#cardiometabolic
mod3.1_ivs <- paste0("cardiomet_PC", 1:3)
mod3.2_ivs <- c("age", mod3.1_ivs)

#DVs
dvs <- c("aopp_log", "homa_ir_log", "crp_ngml_log", "a_amylase_log", "cort_blood", "cort_saliv")

#=====Model====================================================================

#all dv models (except homa_ir for second set of IVs)
mod_f1.1 <- get_models(dataframe = data_23, IVs = mod1.1_ivs, DVs = dvs, mod_num = "1.1")
mod_f1.2 <- get_models(dataframe = data_23, IVs = mod1.2_ivs, DVs = dvs, mod_num = "1.2")
mod_f2.1 <- get_models(dataframe = data_23, IVs = mod2.1_ivs, DVs = dvs, mod_num = "2.1")
mod_f2.2 <- get_models(dataframe = data_23, IVs = mod2.2_ivs, DVs = dvs, mod_num = "2.2")
mod_f3.1 <- get_models(dataframe = data_23, IVs = mod3.1_ivs, DVs = dvs, mod_num = "3.1")
mod_f3.2 <- get_models(dataframe = data_23, IVs = mod3.2_ivs, DVs = dvs, mod_num = "3.2")


#bind models & export
mods_all <- c(mod_f1.1,
              mod_f1.2,
              mod_f2.1,
              mod_f2.2,
              mod_f3.1,
              mod_f3.2) 

#export
export(mods_all, "../output/results/mods_all.rds")

#clean up
rm(mod_f1.1,
   mod_f1.2,
   mod_f2.1,
   mod_f2.2,
   mod_f3.1,
   mod_f3.2,
   mods_all)

#=====Results==================================================================

#all dv models (except homa_ir for second set of IVs)
res_f1.1 <- get_results(dataframe = data_23, IVs = mod1.1_ivs, DVs = dvs, mod_num = "1.1")
res_f1.2 <- get_results(dataframe = data_23, IVs = mod1.2_ivs, DVs = dvs, mod_num = "1.2")
res_f2.1 <- get_results(dataframe = data_23, IVs = mod2.1_ivs, DVs = dvs, mod_num = "2.1")
res_f2.2 <- get_results(dataframe = data_23, IVs = mod2.2_ivs, DVs = dvs, mod_num = "2.2")
res_f3.1 <- get_results(dataframe = data_23, IVs = mod3.1_ivs, DVs = dvs, mod_num = "3.1")
res_f3.2 <- get_results(dataframe = data_23, IVs = mod3.2_ivs, DVs = dvs, mod_num = "3.2")

#bind results
res_all <- bind_rows(res_f1.1,
                     res_f1.2,
                     res_f2.1,
                     res_f2.2,
                     res_f3.1,
                     res_f3.2) |> 
  mutate(dv = str_sub(mod, 1, -5)) |> 
  mutate(submod = ifelse(str_sub(mod, start=-1)=="1", "unadjusted", "age adjusted")) |> 
  group_by(dv, submod) |> 
  mutate(p.value_fdr = p.adjust(p.value, method = "fdr")) |> 
  ungroup() 

#export
export(res_all, "../output/results/res_all.rds")
export(res_all, "../output/results/res_all.csv")


#=Correlations between DVs=====================================================

#lists of variables by group
vars_bodycomp  <- c("android_pct", "gynoid_pct", "fat_lbs_log", "fat_pct",
                        "lean_lbs", "waist_cm", "whr", "weight_lbs_log")
vars_fitness   <- c("tte_min", "pushup", "situp", "sit_reach")
vars_cardiomet <- c("chol_hdl", "chol_ldl", "chol_tot", "trig_log",
                        "apob", "gluc", "insulin_log", "hba1c")
dvs <- c("aopp_log", "homa_ir_log", "crp_ngml_log", "a_amylase_log",
         "cort_blood", "cort_saliv_log")

#correlate variables
cor_bodycomp <- data_23 |>
  select(all_of(vars_bodycomp)) |>
  correlate() 

cor_fitness <- data_23 |>
  select(all_of(vars_fitness)) |>
  correlate()

cor_cardiomet <- data_23 |>
  select(all_of(vars_cardiomet)) |>
  correlate()

cor_dvs <- data_23 |>
  select(all_of(dvs)) |>
  correlate()


#export all together
cor_all <- c("r_bodycomp" = cor_bodycomp, 
             "r_fitness" = cor_fitness, 
             "r_cardiomet" = cor_cardiomet, 
             "r_dvs" = cor_dvs)

export(cor_all,       "../output/results/cor_all.rds")

#export individually
export(cor_bodycomp,  "../output/results/cor_bodycomp.csv")
export(cor_fitness,   "../output/results/cor_fitness.csv")
export(cor_cardiomet, "../output/results/cor_cardiomet.csv")
export(cor_dvs,       "../output/results/cor_dvs.csv")




#=END==========================================================================

