# Function to calculate the Urban Wildness Score
# authors: Maria Espin, Maud Bernard-Verdier


column_names = read_excel(path = "Raw/column_names.xlsx")

calc_uws <- function(df, ref.col = column_names) {
 if (!all( column_names$Column_corr  %in% names(df))) stop()

 
    df <- df %>%
    mutate(STR_score = rowSums(across(starts_with("str_" )))/6 *5,
           SPA_score = rowSums(across(starts_with("spa_" )))/5 *5,
           ANT_score = rowSums(across(starts_with("ant_" )))/5 *5,
           UWS = (STR_score + SPA_score + ANT_score)/3
           ) %>%
      select(STR_score,SPA_score, ANT_score,UWS)
return(df)
}

