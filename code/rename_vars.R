rename_df <- read_xlsx(path = "./raw/rename_vars.xlsx", sheet = "varsdfs") %>% 
  select(new_var_name, old_var_name) %>% 
  unique() %>% 
  filter(!is.na(new_var_name) & !(new_var_name == old_var_name)) 

.file_find_replace <- function(filepath, pattern, replacement) {
  file_contents <- readLines(filepath)
  updated_contents <- gsub(x = file_contents, pattern = pattern,
                           replacement = replacement)
  writeLines(updated_contents, filepath)
}

my_r_scripts <- list.files(path = "./code/", pattern = "(r|R)$")
my_r_scripts <- my_r_scripts[!(my_r_scripts == "rename_vars.R")] 

for (r_script in my_r_scripts ) {
  for (i in 1:nrow(rename_df)) {
  .file_find_replace(paste0("./code/",r_script),rename_df$old_var_name[i],rename_df$new_var_name[i])
  }
}


for (i in 1:nrow(rename_df)) {
  .file_find_replace("9_paper.Rmd",rename_df$old_var_name[i],rename_df$new_var_name[i])
}

