source("./1_merge.R")

#filter to '23
#reorder dataset by Matt's groupings ("../temp/var_list_matt.txt")
data_23 <- data_merge |>
  filter(year==2023) |> 
  select(id, year,
         #demographics
         age, sex, type,
         #Body comp markers
         waist_cm, whr, weight_lbs, fat_pct, fat_lbs, lean_lbs, android_pct, gynoid_pct,
         #Cardiorespiratory fitness markers
         tte_min, vo2_foster,
         #Muscular fitness
         situp, pushup, sit_reach, grip,
         #Classic CVD biomarkers
         chol_hdl, chol_ldl, chol_tot, trig, apob,
         #Insulin resistance markers
         gluc, insulin, hba1c, homa_ir,
         #Stress biomarkers
         a_amylase, cort_blood, cort_saliv, test_saliv, test_blood, test_cort,
         #Novel CVD biomarkers
         aopp, #crp_mgdl, 
         crp_ngml
  ) 


#=====drop females=============================================================

data_23_males <- data_23 |> 
  filter(sex=="male") |> 
  select(-sex)

# #=====check missingness========================================================
# p2 <- gg_miss_var(data_23_males, show_pct = TRUE) + labs(title = "TAMU data, 2023 (n=171)")
# p3 <- gg_miss_var(data_23_males |>
#                     mutate(type = ifelse(type=="fire", "fire (n=98)", "police (n=73)")),
#                   show_pct = TRUE, facet = type)
# #combine plots
# miss_plot <- p2 + p3 &
#   theme(axis.title.y = element_blank())
# miss_plot
# 
# #There are definitely some variables that need to be dropped bc of excessive
# #missingness. Missingness also seems to vary by fire/police.
# 
# #save output
# ggsave("../temp/miss_plot.tiff", miss_plot, height = 5, width = 7, bg="white")

#=====drop PD==================================================================

data_23_fire <- data_23_males |> 
  filter(type=="fire") #removed 66 rows (40%), 97 rows remaining


#=====adjust for skewness======================================================

#deskew
deskewed <- data_23_fire|> 
  pivot_longer(-c(id, year, age, type)) |> 
  group_by(name) |> 
  mutate(value_2 = normalize_skewed(value, skew_threshold=1)) |> 
  mutate(skew = skewness(value, na.rm=TRUE)) |> 
  ungroup() |> 
  mutate(name_2 = case_when((skew >  1) ~ paste0(name, "_log"),
                            (skew < -1) ~ paste0(name, "_sqrt"),
                            TRUE ~ name)) |> 
  filter(grepl("_log|_sqrt", name_2)) |> 
  select(id, year, age, type, value=value_2, name=name_2)

#add deskewed vars back in
data_23_deskewed <- data_23_fire |> 
  pivot_longer(-c(id, year, age, type)) |> 
  bind_rows(deskewed) |> 
  pivot_wider(names_from = name,
              values_from = value)

# ridge_years(df=data_23)
# plot_deskewed <- ridge_years(df=data_23_deskewed)
# plot_deskewed
# ggsave("../temp/dist_plot.tiff", height = 7, width = 10, bg="white")

#====PCA=======================================================================

#Set vars lists
pca_vars_bodycomp  <- c("android_pct", "gynoid_pct", "fat_lbs_log", "fat_pct", 
                        "lean_lbs", "waist_cm", "whr", "weight_lbs_log")
pca_vars_fitness   <- c("tte_min", "pushup", "situp", "sit_reach")
pca_vars_cardiomet <- c("chol_hdl", "chol_ldl", "chol_tot", "trig_log", 
                        "apob", "gluc", "insulin_log", "hba1c")

#Run PCA
pca_bodycomp  <- get_pcs(data_23_deskewed, vars = pca_vars_bodycomp,  "bodycomp")
pca_fitness   <- get_pcs(data_23_deskewed, vars = pca_vars_fitness,   "fitness")
pca_cardiomet <- get_pcs(data_23_deskewed, vars = pca_vars_cardiomet, "cardiomet")

pca_output_bodycomp  <- get_pcs_output(data_23_deskewed, vars = pca_vars_bodycomp,  "bodycomp")
pca_output_fitness   <- get_pcs_output(data_23_deskewed, vars = pca_vars_fitness,   "fitness")
pca_output_cardiomet <- get_pcs_output(data_23_deskewed, vars = pca_vars_cardiomet, "cardiomet")

#save PCA output
pca_output <- list(pca_output_bodycomp, pca_output_fitness, pca_output_cardiomet)
export(pca_output, "../output/results/pca_output.rds")


#merge PCs back in
data_23_pca <- data_23_deskewed |> 
  left_join(pca_bodycomp) |> 
  left_join(pca_fitness) |> 
  left_join(pca_cardiomet)

#reverse code specific PCs to ease interpretation
data_23_rev <- data_23_pca |> 
  mutate(across(c(bodycomp_PC1, cardiomet_PC1), ~.*-1))

#=====save out dataframe=======================================================

data_23 <- data_23_rev

rm(data_merge,
   data_23_males,
   data_23_fire,
   deskewed,
   data_23_deskewed,
   data_23_pca,
   data_23_rev)
export(data_23, "../input/tamu_23.rds")





