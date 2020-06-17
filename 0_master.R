
corona_data <- TRUE
rmarkdown::render(paste0(getwd(), "/1_dataprep.Rmd"), output_file = paste0(getwd(), "1_data_prep_report.html")) 
rmarkdown::render(paste0(getwd(), "/5_report.Rmd"),   output_file = paste0(getwd(), "5_dashboard.html")) 
