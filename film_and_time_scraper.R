#### EXPLORATORY DATA ANALYSIS OF THE SCRAPED DATA ####
#### BASIC CODE ####

## Clean data memory
rm(list = ls())
rm("results_by_film_url_casillas") # Clean a particular R object

## Remove plots
dev.off(dev.list()["RStudioGD"]) # Apply dev.off() & dev.list()

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
  url = html_data %>% 
    html_nodes(".m5") %>% 
    html_attr("href")
)
  
films_url <- films_url %>% 
  mutate(
  url = paste0("https://www.golem.es", url)
  )

films <- films_url %>% 
  select(-url)

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
  html_result = map(prueba$url,
                    ~ {
                      Sys.sleep(2)
                      .x %>% 
                        read_html()
                    }),
  url = prueba$url
)

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
  cas_director = map(mapeador$html_result,
                   ~ .x %>% 
                     html_nodes(".txtNegL tr:nth-child(3) .txtLectura:nth-child(1)") %>% 
                     html_text2()
  ),
  dato_director = map(mapeador$html_result,
                    ~ .x %>% 
                      html_nodes(".txtNegL tr:nth-child(3) .txtLectura:nth-child(2)") %>% 
                      html_text2()
  ),
  cas_length = map(mapeador$html_result,
                   ~ .x %>% 
                     html_nodes(".txtNegL tr:nth-child(4) .txtLectura:nth-child(1)") %>% 
                     html_text2()
  ),
  dato_length = map(mapeador$html_result,
                    ~ .x %>% 
                      html_nodes(".txtNegL tr:nth-child(4) .txtLectura:nth-child(2)") %>% 
                      html_text2()
  ),
  cas_country = map(mapeador$html_result,
                   ~ .x %>% 
                     html_nodes(".txtNegL tr:nth-child(5) .txtLectura:nth-child(1)") %>% 
                     html_text2()
  ),
  dato_country = map(mapeador$html_result,
                    ~ .x %>% 
                      html_nodes(".txtNegL tr:nth-child(5) .txtLectura:nth-child(2)") %>% 
                      html_text2()
  )
  )

results_by_film_url <- results_by_film_url %>% 
  unnest(2:9)

df <- left_join(prueba, results_by_film_url)

df <- df %>% 
  pivot_longer(
    cols = 8:15,
    names_to = "casillas",
    names_prefix = "cas_",
    values_to = "datos"
  ) %>% 
  filter(
    str_detect(casillas, "dato_")
  ) %>% 
  mutate(
    casillas = str_remove(casillas, "dato_")
  ) %>% 
  pivot_wider(names_from = casillas, 
              values_from = datos)

df <- df %>% 
  dplyr::rename(
    original_title = titulo
  ) %>% 
  relocate(
    date, .before = films
  ) %>% 
  select(-url)

#### APPEND DATA DAY TO DAY TO A .CSV FILE ####
write.table(df, "data/films_van_golem.csv", fileEncoding = "UTF-8", sep = ",", row.names = FALSE, col.names = F, append = T)

