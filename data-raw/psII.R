## code to prepare `psII` dataset goes here

psIIfile = 'data-raw/output_psII_level1.csv'
data = read_csv(psIIfile, #reading the data from the image processing
                col_types = cols(gtype = col_character())) 

psII = data %>%
  mutate(idate = jobdate-min(jobdate)+1, #days after treatment begins
         parameter = as_factor(parameter))

usethis::use_data(psII, overwrite = TRUE)