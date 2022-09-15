#### LOAD PACKAGES ####
library(rvest)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(readr)
library(stringr)
library(stringi)


#### LOAD URL AND READ HTML ####
main_url <- "https://www.golem.es/golem/vangolem"
html_data <- read_html(main_url)


#### HANDS ON SCRAPING ####

## Get the films titles 
films <- html_data %>% 
  html_nodes("span.txtNegXL a") %>% 
  html_text2()
films <- as.data.frame(films)

## Create the dataframe to store the time information
for (i in 1:4){
  new <- rep(i, nrow(films))
  films[ , ncol(films)+1] <- new
  colnames(films)[ncol(films)] <- paste0("time", i)
}

## Fill the time values
z <- 0
# The i iterator is for the films
for (i in seq(1, 3+(nrow(films)-1)*2, 2)){
  print(paste0("i es: ", i))
  z <- z + 1
  if (z > nrow(films)) break
  print(paste0("z es: ", z))
  # The j iterator is for the film times
  for (j in 2:5){ 
    print(paste0("j es: ", j))
    css_tag <- paste0(".tabContenido table:nth-child(",i,") table table table td:nth-child(",j-1,") td .CajaVentasSup .horaXXXL .horaXXXL")
    films[[j]][[z]] <- if (length(
                                  html_data %>% 
                                  html_nodes(css_tag) %>% 
                                  html_text2()
                                  )
                           ) html_data %>% 
                             html_nodes(css_tag) %>% 
                             html_text2() else NA
    }
  }


#### APPEND DATA DAY TO DAY TO A .CSV FILE ####
write.table(films, "data/pelis_van_golem.csv", fileEncoding = "UTF-8", sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
