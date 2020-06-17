
## Function to put line break in long labels
add_linebreak <- function(min_length = 10, 
                          string,
                          add_multiple_linebreaks = F) {
  
  if (nchar(string) > min_length) {
    
    if (!add_multiple_linebreaks) {
      
      ##
      l <- nchar(string)
      find_space <- str_locate_all(string, ' |\\-') %>%
        .[[1]] %>% data.frame() %>%
        pull(start) %>% .[which.min(abs(. - (nchar(string) / 2)))]
      
      ## add line break
      substr(string, find_space, find_space) <- '\n'
      string
    } else {
      find_space <- str_locate_all(string, ' |\\-') %>%
        .[[1]] %>% data.frame() %>% slice(-1) %>% 
        pull(1)
      
      for (i in find_space) {
        substr(string, i, i) <- '\n'
      }
      string
    }
  } else {
    string
  }
}
  