#=Packages=====================================================================

library(pacman)

# p_load_gh("dustinfife/flexplot")
p_load(rio,
       tidyverse,
       tidylog,
       ###
       broom,
       broom.helpers,
       corrr,
       cowplot,
       flexplot,
       ggpubr,
       ggrepel,
       ggtext,
       gt,
       gtsummary,
       Hmisc,
       janitor,
       naniar,
       patchwork,
       psych,
       sjlabelled
)

#=2_recode.R===================================================================

#=====ridge_years: Check for outliers - Density by year========================
ridge_years <- function(id=id, df=df) {
  require(ggpubr)
  df |> 
    select(id, where(is.numeric)) |> 
    pivot_longer(-c(id),
                 names_to="vars",
                 values_to="vals") |>
    ggplot(aes(x=vals, y=after_stat(density))) + 
    geom_density() +
    stat_overlay_normal_density(color="red", linetype="dashed") + #this is neat
    facet_wrap(~vars, scales = "free") 
}


#=====normalize_skewed: function to normalize skewed data using threshold======
normalize_skewed <- function(variable, skew_threshold = 1) {
  require(dplyr)
  require(e1071)
  
  # Calculate skewness
  skew_value <- skewness(variable, na.rm = TRUE)
  
  # Apply transformations based on skewness threshold
  if (abs(skew_value) > skew_threshold) {
    if (skew_value > 0) {
      # Positive skew - apply log transformation
      return(log(variable + 1))  # log(x+1) to handle zero or negative values
    } else {
      # Negative skew - apply sqrt transformation
      return(sqrt(variable))
    }
  } else {
    # If skewness is within the threshold, return the variable unchanged
    return(variable)
  }
}

#=3_analysis.R==========================================================

#=====tidy_cor: calculate correlations and merge results into tibble===========
tidy_cor <- function(df, cluster=TRUE, method="HC", drop=TRUE, lab_first=TRUE){
  require(corrr)
  require(Hmisc)
  
  #create cormat[r,n,P]
  cormat <- df |>
    cor(use = "pairwise.complete.obs") |>
    rcorr() 
  
  #extract r mat, transform to cor_df, rearrange (cluster), tidy format
  cormat_r <- cormat$r |> 
    as_cordf()
  
  #cluster data using method
  if (cluster){
    cormat_r <- cormat_r |> 
      rearrange(method = method) |> 
      stretch()
  } else {
    cormat_r <- cormat_r |> 
      stretch()
  }
  
  #extract r mat, transform to cor_df, tidy format
  cormat_p <- cormat$P |> 
    as_cordf() |> 
    stretch() |> 
    rename(p=r)
  
  #merge results
  cormat_complete <- cormat_r |> 
    left_join(cormat_p, by=c("x", "y"))  
  
  #drop top triangle
  if (drop){
    cormat_complete <- cormat_complete |> 
      mutate(across(c(x, y), ~as_factor(.))) |>
      filter(as.numeric(x) > as.numeric(y)) |>
      mutate(x=fct_rev(x))
  }
  
  #add label to first instance of pair
  if (lab_first) {
    cormat_complete <- cormat_complete |> 
      arrange(desc(y)) |> 
      group_by(y) |> 
      mutate(lab_first = ifelse(row_number()==1, as.character(x), NA)) |> 
      ungroup() 
  }
  return(cormat_complete)
}


#=====tidy_cor: calculate partial correlations and merge results into tibble===========
tidy_cor_part <- function(df, x, y, cluster=TRUE, method="HC", lab_first=TRUE){
  require(corrr)
  require(Hmisc)
  require(psych)
  
  #create cormat[r,n,P]
  cormat <- df |>
    partial.r(x=x, y=y) |> 
    rcorr() 
  
  #extract r mat, transform to cor_df, rearrange (cluster), tidy format
  cormat_r <- cormat$r |> 
    as_cordf()
  
  if (cluster){
    cormat_r <- cormat_r |> 
      rearrange(method = method) |> 
      stretch() |> 
      rename(r_part=r)
  } else {
    cormat_r <- cormat_r |> 
      stretch() |> 
      rename(r_part=r)
  }
  
  #extract r mat, transform to cor_df, tidy format
  cormat_p <- cormat$P |> 
    as_cordf() |> 
    stretch() |> 
    rename(p_part=r)
  
  #merge results
  cormat_complete <- cormat_r |> 
    left_join(cormat_p, by=c("x", "y")) 
  # mutate(across(c(x, y), ~as_factor(.))) |>
  # filter(as.numeric(x) > as.numeric(y)) |>
  # mutate(x=fct_rev(x))
  
  if (lab_first) {
    cormat_complete <- cormat_complete |> 
      arrange(desc(y)) |> 
      group_by(y) |> 
      mutate(lab_first = ifelse(row_number()==1, as.character(x), NA)) |> 
      ungroup() 
  }
  return(cormat_complete)
}

#=====get_results: get results from linear models using lists of IVs/DVs=======
get_results <- function(dataframe, IVs, DVs, mod_num, standardize=TRUE) {
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop over each dependent variable
  for (DV in DVs) {
    # Filter the dataframe based on the group
    filtered_data <- dataframe %>%
      drop_na(all_of(c(DV, IVs)))
    
    # standardize all vars
    filtered_data_std <- filtered_data %>%
      mutate(across(all_of(c(DV, IVs)), scale))
    
    # Create a formula for the linear model
    formula <- as.formula(paste(DV, "~", paste(IVs, collapse = " + ")))
    
    # Estimate the linear model
    if(standardize){
      model <- lm(formula, data = filtered_data_std)
    } else {
      model <- lm(formula, data = filtered_data)
    }
    
    # Tidy the model output and extract model statistics
    model_tidy <- tidy(model)
    
    model_glance <- glance(model) |> select(-c(p.value, statistic))
    
    # Bind the results together and add to the list
    result <- bind_cols(model_tidy, model_glance) |> 
      mutate(mod=paste0(DV, "_", mod_num)) |> 
      select(mod, term, everything())
    results_list[[DV]] <- as_tibble(result)
  }
  
  # Combine all results into a single tibble
  final_results <- bind_rows(results_list)
  
  return(final_results)
}


#=====get_models: get model objects from linear models using lists of IVs/DVs====
get_models <- function(dataframe, IVs, DVs, mod_num, standardize=TRUE) {
  require(tidyverse)
  
  # Initialize an empty list to store model objects
  models_list <- list()
  
  # Loop over each dependent variable
  # Loop over each dependent variable
  for (DV in DVs) {
    # Filter the dataframe based on the group
    filtered_data <- dataframe %>%
      drop_na(all_of(c(DV, IVs)))
    
    # standardize all vars
    filtered_data_std <- filtered_data %>%
      mutate(across(all_of(c(DV, IVs)), scale))
    
    # Create a formula for the linear model
    formula <- as.formula(paste(DV, "~", paste(IVs, collapse = " + ")))
    
    # Estimate the linear model
    if(standardize){
      model <- lm(formula, data = filtered_data_std)
    } else {
      model <- lm(formula, data = filtered_data)
    }
    
    # Add DV information to the model object
    model_name <- paste0(DV, "_", mod_num)
    models_list[[model_name]] <- model
  }
  
  return(models_list)
}

#=====get_pcs: Get PCs from (varimax) PCA======================================
get_pcs <- function(data, vars, prefix) {
  # Perform PCA
  pca_mod <- data |> 
    select(all_of(vars)) |> 
    drop_na() |> 
    prcomp(scale. = TRUE, center = TRUE)
  
  # Get PCs with std.dev > 1
  
  pcs_to_keep <- tidy(pca_mod, "pcs") |> 
    filter(std.dev > 1) |> 
    summarise(PC = max(PC)) |> 
    pull(PC)
  
  #print PCs with cumulative variance explained
  print(tidy(pca_mod, "pcs"))
  
  print(tidy(pca_mod, "loadings") |> 
          filter(PC<=pcs_to_keep) |> 
          pivot_wider(names_from=PC,
                      values_from=value,
                      names_prefix="pc_"))
  
  # Project these PCs into the full dataset
  data_pca <- augment(pca_mod, newdata = data) |> 
    select(id, num_range(".fittedPC", 1:pcs_to_keep)) |> 
    rename_with(~str_replace(., ".fitted", paste0(prefix, "_")), -id)
  
  return(data_pca)
}

get_pcs_output <- function(data, vars, prefix) {
  # Perform PCA
  pca_mod <- data |> 
    select(all_of(vars)) |> 
    drop_na() |> 
    prcomp(scale. = TRUE, center = TRUE)
  
  # Get PCs with std.dev > 1
  
  pcs_to_keep <- tidy(pca_mod, "pcs") |> 
    filter(std.dev > 1) |> 
    summarise(PC = max(PC)) |> 
    pull(PC)
  
  #print PCs with cumulative variance explained
  pcs <- tidy(pca_mod, "pcs") |> mutate(group=prefix)
  
  loadings <- tidy(pca_mod, "loadings") |> 
    filter(PC<=pcs_to_keep) |> 
    pivot_wider(names_from=PC,
                values_from=value,
                names_prefix="pc_")|> mutate(group=prefix)
  
  output <- list(pcs, loadings)
  return(output)
}


#=5_tables.R===================================================================

#=====desc_tab: create descriptives table======================================
desc_tab <- function(data, filter=FALSE, group){
  
  #set compact theme
  theme_gtsummary_compact()
  
  #filter?
  if (filter) {
    data <- data |> 
      filter(type == !!group)
  }
  
  stats <- c("N" = "{length}", 
             "Mean" = "{mean}", 
             "SD" = "{sd}",
             "Range" = "{min}, {max}")
  
  tbl <- purrr::imap(
    stats,
    ~data |> 
      tbl_summary(missing = "no", 
                  statistic = ~.x) |> 
      modify_header(all_stat_cols() ~ stringr::str_glue("**{.y}**"))
  ) |> 
    tbl_merge(tab_spanner = FALSE) |> 
    modify_footnote(~NA)
  return(tbl)
}

#=create_model_table: merge and stack model objects in table===================
create_model_table <- function(mods_all, model_names, model_labs, model_num, var_labs) {
  
  #set compact theme
  theme_gtsummary_compact()
  
  # Create an empty list to store regression tables for the unadjusted and adjusted models
  tables_unadjusted <- list()
  tables_adjusted <- list()
  
  # Iterate over each outcome variable and create regression tables
  for (name in model_names) {
    # Construct the model names dynamically
    model_unadjusted <- paste0(name, "_", model_num, ".1")
    model_adjusted <- paste0(name, "_", model_num, ".2")
    
    # Generate regression tables
    tab_unadjusted <- tbl_regression(mods_all[[model_unadjusted]],
                                     label=var_labs,
                                     intercept=F) |> 
      modify_header(label ~ "", p.value="**P**") |> 
      add_glance_table(
        include = c(r.squared)) |> 
      add_significance_stars(hide_ci = T, hide_p = FALSE) 
    
    tab_adjusted <- tbl_regression(mods_all[[model_adjusted]], 
                                   include = -age,
                                   intercept=F,
                                   label=var_labs) |> 
      modify_header(label ~ "", p.value="**P**") |> 
      add_glance_table(
        include = c(r.squared, nobs)) |> 
      add_significance_stars(hide_ci = T, hide_p = FALSE) 
    
    # Store the tables in their respective lists
    tables_unadjusted[[name]] <- tab_unadjusted
    tables_adjusted[[name]] <- tab_adjusted
  }
  
  # Merge the unadjusted and adjusted tables
  tab_1.1 <- tbl_merge(tables_unadjusted,
                       tab_spanner = model_labs)
  tab_1.2 <- tbl_merge(tables_adjusted,
                       tab_spanner = model_labs)
  
  # Stack the merged tables and add group headers
  final_table <- tbl_stack(list(tab_1.1, tab_1.2),
                           group_header = c("Unadjusted", "Age Adjusted"))
  
  return(final_table)
}

#=END==========================================================================


