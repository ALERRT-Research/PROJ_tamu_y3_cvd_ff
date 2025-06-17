
#=Set up=======================================================================
source("./0_packages.R")

#=START========================================================================

#=Descriptives=================================================================

full_samp <- import("../input/tamu_23.rds") |> 
  select(-c(id, year, type,
            test_blood,
            test_saliv,
            test_cort,
            vo2_foster,
            grip)) |> 
  select(-ends_with("_log")) |> 
  select(-matches("PC[123]$")) |> 
  var_labels(age = "Age (years)",
             #body com = "p
             waist_cm = "Waist cir. (cm)",
             whr = "Waist-hip ratio",
             weight_lbs = "Weight (lbs)",
             fat_pct = "Fat mass (%)",
             fat_lbs = "Fat mass (lbs)",
             lean_lbs = "Lean mass (lbs)",
             android_pct = "Android fat (%)",
             gynoid_pct = "Gynoid fat (%)",
             #fitness
             tte_min = "Time to exhaustion",
             situp = "Sit ups",
             pushup = "Push ups",
             sit_reach = "Sit and reach",
             #cardiometabolic
             chol_hdl = "HDL cholesterol",
             chol_ldl = "LDL cholesterol",
             chol_tot = "Total cholesterol",
             trig = "Triglycerides (blood)",
             apob = "ApoB",
             gluc = "Glucose (blood)",
             insulin = "Insulin",
             hba1c = "HbA1c (%)",
             homa_ir = "HOMA-IR",
             #stress markers
             a_amylase = "A-amylase",
             cort_blood = "Cortisol (blood)",
             cort_saliv = "Cortisol (saliva)",
             #inflammation and oxidative stress
             aopp = "AOPPs",
             crp_ngml = "CRP")


#generate table of descriptives
tab_fire <- desc_tab(full_samp, filter = F)
# tab_fire

#Add grouping headers
tab_grouped <- tab_fire |> 
  modify_table_body(mutate,
                    groupname_col = case_when(
                      (variable %in% c("age")) ~ "Demographics",
                      #Body comp markers
                      (variable %in% c("waist_cm", 
                                       "whr", 
                                       "weight_lbs", 
                                       "fat_pct", 
                                       "fat_lbs", 
                                       "lean_lbs", 
                                       "android_pct", 
                                       "gynoid_pct")) ~ "Body Composition",
                      #Physical Fitness
                      (variable %in% c("tte_min",
                                       "situp", 
                                       "pushup", 
                                       "sit_reach")) ~ "Physical Fitness",
                      #Cardiometabolic
                      (variable %in% c("chol_hdl", 
                                       "chol_ldl", 
                                       "chol_tot", 
                                       "trig", 
                                       "apob",
                                       "gluc", 
                                       "insulin", 
                                       "hba1c",
                                       "homa_ir")) ~ "Cardiometabolic",
                      #Stress
                      (variable %in% c("a_amylase", 
                                       "cort_blood", 
                                       "cort_saliv")) ~ "Stress Markers",
                      #Inflammation & Oxidative 
                      (variable %in% c("aopp", 
                                       "crp_ngml")) ~ "Inflammation & Oxidative Stress Markers"))
# tab_grouped

tab_grouped |> 
  as_gt() |> 
  gtsave("../output/results/tab1_desc.docx")

rm(full_samp, 
   tab_fire,
   tab_grouped)

#=PCA==========================================================================

#get PCA results
pca_data <- import("../output/results/pca_output.rds")

#PCs
pcs_bodycomp  <- pca_data[[1]][[1]]
pcs_fitness   <- pca_data[[2]][[1]]
pcs_cardiomet <- pca_data[[3]][[1]]

#loadings
loadings <- bind_rows(pca_data[[1]][[2]],
                      pca_data[[2]][[2]],
                      pca_data[[3]][[2]]) |> 
  mutate(column = case_when((column=="waist_cm"      ) ~ "Waist cir. (cm)",
                            (column=="whr"           ) ~ "Waist-hip ratio",
                            (column=="weight_lbs_log") ~ "Weight (lbs)*",
                            (column=="fat_pct"       ) ~ "Fat mass (%)",
                            (column=="fat_lbs_log"   ) ~ "Fat mass (lbs)*",
                            (column=="lean_lbs"      ) ~ "Lean mass (lbs)",
                            (column=="android_pct"   ) ~ "Android fat (%)",
                            (column=="gynoid_pct"    ) ~ "Gynoid fat (%)",
                            #fitness
                            (column=="tte_min"       ) ~ "Time to exhaustion",
                            (column=="situp"         ) ~ "Sit ups",
                            (column=="pushup"        ) ~ "Push ups",
                            (column=="sit_reach"     ) ~ "Sit and reach",
                            #cardiometabolic
                            (column=="chol_hdl"      ) ~ "HDL cholesterol",
                            (column=="chol_ldl"      ) ~ "LDL cholesterol",
                            (column=="chol_tot"      ) ~ "Total cholesterol",
                            (column=="trig_log"      ) ~ "Triglycerides (blood)*",
                            (column=="apob"          ) ~ "ApoB",
                            (column=="gluc"          ) ~ "Glucose (blood)",
                            (column=="insulin_log"   ) ~ "Insulin*",
                            (column=="hba1c"         ) ~ "HbA1c (%)")) |> 
  mutate(across(c(pc_1, pc_2, pc_3), ~round(., 2))) |> 
  mutate(pc_1 = case_when((group %in% c("bodycomp", "cardiomet")) ~ pc_1*-1,
                           TRUE ~ pc_1))
  


tab_bodycomp <- loadings |> 
  filter(group=="bodycomp") |> 
  select(-c(group, pc_3)) |> 
  var_labels(pc_1 = "General adiposity (PC1)",
             pc_2 = "Lean mass/fat dist. (PC2)",
             column = "Body composition") |>
  gt()

tab_fitness <- loadings |> 
  filter(group=="fitness") |> 
  select(-c(group, pc_2, pc_3)) |> 
  var_labels(pc_1 = "General fitness (PC1)",
             column = "Fitness") |>
  gt()

tab_cardiomet <- loadings |> 
  filter(group=="cardiomet") |> 
  select(-c(group)) |> 
  var_labels(pc_1 = "Lipid metabolism (PC1)",
             pc_2 = "Glucose control (PC2)",
             pc_3 = "HDL cholesterol (PC3)",
             column = "Cardiometabolic") |>
  gt()

gtsave(tab_bodycomp, "../output/results/loading_bodycomp.docx")
gtsave(tab_fitness, "../output/results/loading_fitness.docx")
gtsave(tab_cardiomet, "../output/results/loading_cardiomet.docx")

#=Models=======================================================================

# read in list of model objects
mods_all <- import("../output/results/mods_all.rds")

#define DV names and labels
names <- c("aopp_log", "homa_ir_log", "a_amylase_log", "crp_ngml_log", "cort_blood", "cort_saliv")
dv_labs <- c("AOPP", "HOMA-IR", "Alpha-amylase", "CRP", "Cortisol (blood)", "Cortisol (saliva)")

#define
iv1_labs <- list(bodycomp_PC1 = "General adiposity (PC1)",
                 bodycomp_PC2 = "Lean mass/fat dist. (PC2)")
iv2_labs <- list(fitness_PC1 = "General fitness (PC1)")
iv3_labs <- list(cardiomet_PC1 = "Lipid metabolism (PC1)",
                 cardiomet_PC2 = "Glucose control (PC2)",
                 cardiomet_PC3 = "HDL cholesterol (PC3)")

#get tables
tab_bodycomp <- create_model_table(mods_all, 
                                   model_names=names, 
                                   model_labs=dv_labs, 
                                   model_num = 1, 
                                   var_labs=iv1_labs)
# tab_bodycomp

tab_fitness <- create_model_table(mods_all, 
                                  model_names=names, 
                                  model_labs=dv_labs, 
                                  model_num = 2, 
                                  var_labs=iv2_labs)
# tab_fitness

tab_cardiomet <- create_model_table(mods_all, 
                                    model_names=names, 
                                    model_labs=dv_labs, 
                                    model_num = 3, 
                                    var_labs=iv3_labs)

# tab_cardiomet

#export tables

tab_bodycomp  |> as_gt() |> gtsave("../output/results/tab2_bodycomp.docx")
tab_fitness   |> as_gt() |> gtsave("../output/results/tab3_fitness.docx")
tab_cardiomet |> as_gt() |> gtsave("../output/results/tab4_cardiomet.docx")

