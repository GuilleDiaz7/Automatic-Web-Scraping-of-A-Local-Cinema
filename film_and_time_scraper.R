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
html_data <- read_html("https://www.golem.es/golem/vangolem")


#### HANDS ON SCRAPING ####

## Get the films titles 
films_url <- tibble(
  films = html_data %>% 
            html_nodes("span.txtNegXL a") %>% 
            html_text2(),
  urls = html_data %>% 
    html_nodes(".m5") %>% 
    html_attr("href")
)
  
films_url <- films_url %>% 
  mutate(
  urls = paste0("https://www.golem.es", urls)
  )

films <- films_url %>% 
  select(-urls)

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
  # print(paste0("i es: ", i))
  z <- z + 1
  if (z > nrow(films)) break
  # print(paste0("z es: ", z))
  # The j iterator is for the film times
  for (j in 2:5){ 
    # print(paste0("j es: ", j))
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

## Add a date column
films <- films %>% 
  mutate(date = Sys.Date()) %>% 
  relocate(.before = films, date)

prueba <- left_join(films_url, films)
mapeador <- tibble(
  html_result = map(prueba$urls,
                    ~ {
                      Sys.sleep(2)
                      .x %>% 
                        read_html()
                    }),
  url = prueba$urls
)

# VAMOS BIEN, ES AÑADOR EL RESTO DE DATOS AQUÍ, DOY POR HECHO QUE NO CAMBIAN DE ORDEN
results_by_film_url <- tibble(
  url = mapeador$url,
  cas_titulo = map(mapeador$html_result,
                 ~ .x %>% 
                   html_nodes(".txtNegL tr:nth-child(2) .txtLectura:nth-child(1)") %>% 
                   html_text2()
                 ),
  dato_titulo = map(mapeador$html_result,
                    ~ .x %>% 
                      html_nodes(".txtNegL tr:nth-child(2) .txtLectura:nth-child(2)") %>% 
                      html_text2()
                    ),
  
  )

results_by_film_url %>% 
  unnest(casillas) %>% 
  filter(casillas != "Ficha Técnica:")


#### APPEND DATA DAY TO DAY TO A .CSV FILE ####
write.table(films, "data/films_van_golem.csv", fileEncoding = "UTF-8", sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
