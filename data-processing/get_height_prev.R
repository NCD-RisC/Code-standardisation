## get_height_prev ##
# Gives 1 if an individual falls in a given height category, else 0 (subjects with NA values in height_data were removed)
get_height_prev = function(height_data) {
    
    height_data$prev_height120 <- ifelse(height_data$height_clean < 120, 1, 0)
    height_data$prev_height120_140 <- ifelse(height_data$height_clean >= 120 & height_data$height_clean < 140, 1, 0)
    height_data$prev_height140_145 <- ifelse(height_data$height_clean >= 140 & height_data$height_clean < 145, 1, 0)
    height_data$prev_height145_150 <- ifelse(height_data$height_clean >= 145 & height_data$height_clean < 150, 1, 0)
    height_data$prev_height150_155 <- ifelse(height_data$height_clean >= 150 & height_data$height_clean < 155, 1, 0)
    height_data$prev_height155_160 <- ifelse(height_data$height_clean >= 155 & height_data$height_clean < 160, 1, 0)
    height_data$prev_height160_165 <- ifelse(height_data$height_clean >= 160 & height_data$height_clean < 165, 1, 0)
    height_data$prev_height165_170 <- ifelse(height_data$height_clean >= 165 & height_data$height_clean < 170, 1, 0)
    height_data$prev_height170_175 <- ifelse(height_data$height_clean >= 170 & height_data$height_clean < 175, 1, 0)
    height_data$prev_height175_180 <- ifelse(height_data$height_clean >= 175 & height_data$height_clean < 180, 1, 0)
    height_data$prev_height180_185 <- ifelse(height_data$height_clean >= 180 & height_data$height_clean < 185, 1, 0)
    height_data$prev_height185_190 <- ifelse(height_data$height_clean >= 185 & height_data$height_clean < 190, 1, 0)
    height_data$prev_height190_195 <- ifelse(height_data$height_clean >= 190 & height_data$height_clean < 195, 1, 0)
    height_data$prev_height195_200 <- ifelse(height_data$height_clean >= 195 & height_data$height_clean < 200, 1, 0)
    height_data$prev_height200 <- ifelse(height_data$height_clean >= 200, 1, 0)
    
    return(height_data)
}