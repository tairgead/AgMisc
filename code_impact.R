#' @param x If is.null(df) a character string giving the name of the high 
#' cardinality column. If df is provided, x is a high cardinality vector.
#' @param y If is.null(df), a character string giving the name of the target 
#' column. If df is provided, y is a vector of length = length(x) containing the
#' target values.
#' @return A dataframe with the groups from x and the re-coded value.
#' @examples
#' code_impact("group", "value", df = my_df)
#' code_impact(my_df$group, my_df$value)

# based on 'Preprocessing Scheme for High Cardinality Attributes'
# by Daniele Micci-Barreca @ ClearCommerce Corporation
# and some work on the WinVector blog (John Mount & Nina Zumel)

# This function takes high cardinality variables and creates an 'impact score'
# for each level, allowing us to use the variable in a model

# The general idea is to estimate the probability of a positive event
# for each group, but pulling groups with low counts toward the 'grand mean'
# probability 

# TODO: add ability to aggregate at levels other than the top-level. 
#   example: zip codes should be able to aggrate at the zip4 level instead of 
#   the grand mean level. 
# TODO: add parallel option

code_impact <- function(x, y, df = NULL, mean_name = "grand_mean") {
  if (is.null(df)) {
    df <- data.frame(x, y)
    names(df) <- c("group", "value")
    x <- names(df)[[1]]
    y <- names(df)[[2]]
  }
  
  group <- unique(df[[x]])

  assertthat::assert_that(sum(group == "grand_mean" & mean_name == "grand_mean") == 0,
                          msg = paste("One of your groups is named 'grand_mean'",
                                      "\nPlease specify the mean_name arg to",
                                      "be a name that doesn't exist in your",
                                      "group vector"))
  
  grand_mean <- mean_name
  
  df[y] <- as.integer(df[[y]]) # make sure y is a number
  
  n = nrow(df)
  p = sum(df[[y]])/n
  # duplicate output for NA so NA shows as a group with the impact value 
  # equal to the grand mean impact value
  
  df[x] <- as.character(df[[x]])
  df <- df[rep(seq_len(nrow(df)), 2), ]
 
  df[(n + 1):(n *( 2)), x] <- grand_mean
  
  # add NA to the list of groups
  group <- c(as.character(group), grand_mean)

  # create empty list to store results for each value of group, including NA
  s_i <- vector("list", length(group))
  
  for (grp in 1:length(group)) {
    # count of rows with x = group
    n_i <- sum(df[x] == group[[grp]]) 
    
    # count of rows with x = group and y in target class
    n_iy <- sum(df[x] == group[[grp]] & df[y] == 1) 
    
    # total count of rows with y in target class
    n_y <- sum(df[x] != grand_mean & df[y] == 1)
    
    # total length of x
    n_t <- nrow(df) / 2
    
    # Calculate lambda --------------------------------------------------------  
    df_n <- df[which(df[x] == group[[grp]]), ]
    n <- nrow(df_n)
    
    # I have modified how m is calculated by adding 1 to both sides
    # this is to prevent an estimate being zero when there are small
    # n sizes for a cell and all have the same value and therefore zero variance.
    # Also, if n == 1 (thus variance is NA), force var(df_n[y]) = 0
    m <- ifelse(n == 1, 1 / (var(df[y]) + 1), (var(df_n[y]) + 1) / (var(df[y]) + 1))
    
    lambda_val <- n / (m + n)
    
    
    # Calculate impact (s_i) --------------------------------------------------  
    cell_ratio <-  n_iy / n_i
    global_ratio <- n_y / n_t
    
    s_i_val <- as.numeric(lambda_val * (cell_ratio) + (1 - lambda_val) * (global_ratio))
    s_i[[grp]] <- data.frame("group" = group[[grp]], "impact" = s_i_val)
  }
  
  
  # list(n_i, n_iy, n_y, n_t, cell_ratio, global_ratio, lambda_val, s_i)
  s_i <- do.call(rbind, s_i)
  return(s_i)
}