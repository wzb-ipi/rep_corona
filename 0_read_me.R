
# Compiling 1_1_dataprep.Rmd recompiles the data including most recent Corona data
# All data is either in raw or is pulled from public soures. Transformed data is saved in folder saved\
# Compiling 2_dashboard.Rmd generates the dashboard as a .html file. This also creates figures used in the "long" paper
# Compiling 3_1_long_paper.Rmd and 3_2_short_paper.Rmd compile pdfs of the two papers using most recently available data and figures 

corona_data <- TRUE # Use to download today's data  
rmarkdown::render(paste0(getwd(), "/1_1_dataprep.Rmd"), output_file = paste0(getwd(), "1_data_prep_report.html")) 
rmarkdown::render(paste0(getwd(), "/2_dashboard.Rmd"),   output_file = paste0(getwd(), "5_dashboard.html")) 
