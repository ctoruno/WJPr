diffmeans <- function(data, target_vars, group_vars, geo_var, type = "categorical", t = 0.1, collapse = TRUE, verbose = FALSE){
  
  # Looping through grouping alternatives
  groloop <- lapply(
    group_vars %>% set_names(group_vars),
    function(grouping){
      
      if (verbose == TRUE){
        print("=================")
        print(grouping)
      }
      
      # Looping through target variables
      varloop <- lapply(
        target_vars %>% set_names(target_vars),
        function(target){
          
          if (verbose == TRUE){
            print("--------------")
            print(target)
          }
          
          # Preparing data for tests
          data_subset <- data %>%
            select(
              grouping = all_of(grouping),
              target   = all_of(target),
              geovar   = all_of(geo_var)
            )
          
          # Defining a function to estimate the results.
          if (type == "continuous") {
            stat_function <- function(df) {
              group_A <- df %>% filter(grouping == 1) %>% pull(target)
              group_B <- df %>% filter(grouping == 0) %>% pull(target)
              mean_A  <- mean(group_A, na.rm = T)
              mean_B  <- mean(group_B, na.rm = T)
              ttest_result <- t.test(group_A, group_B, paired = FALSE)
              
              data.frame(
                mean_A  = mean_A,
                mean_B  = mean_B,
                diff    = mean_A - mean_B,
                stat    = ttest_result$statistic,
                p_value = ttest_result$p.value
              )
            }
          }
          if (type == "categorical") {
            stat_function <- function(df) {
              count_table      <- table(df$grouping, df$target)
              prop_test_result <- prop.test(count_table)
              mean_A <- mean(df$target[df$grouping == 1], na.rm = T)
              mean_B <- mean(df$target[df$grouping == 0], na.rm = T)
              
              data.frame(
                mean_A  = mean_A,
                mean_B  = mean_B,
                diff    = mean_A - mean_B,
                stat    = prop_test_result$statistic,
                p_value = prop_test_result$p.value
              )
            }
          }
          
          results <- data_subset %>%
            group_by(geovar) %>%
            nest() %>%
            mutate(
              dim_results = map(data, stat_function)
            ) %>%
            unnest(dim_results) %>%
            select(-data) %>%
            mutate(
              across(
                c(stat, p_value),
                ~if_else(is.na(diff), NA_real_, .x)
              ),
              stat_sig = if_else(p_value <= t, TRUE, FALSE, missing = FALSE),
            )
        }
      )
      
      if (collapse == TRUE){
        varloop <- imap_dfr(
          varloop,
          function(df, var){
            df %>%
              mutate(
                variable = var
              ) %>%
              relocate(variable)
          }
        ) 
      } else {
        varloop <- varloop
      }
      
      return(varloop)
      
    }
  )
}


