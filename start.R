
pacman::p_load(knitr, tidyverse, openxlsx, sf, rmarkdown, rvest)
# setwd("C:/Users/fabio/Dropbox/postdoc/microdashboards/wtm_iq/")
# setwd("..")
# getwd()

source("cntry.R")
source("utils.R")

linkie <- function(.x, sets) {
  download.file(glue::glue("https://github.com/favstats/meta_ad_reports/releases/download/TR-lifelong/{.x}.rds"), destfile = "data.rds", quiet = T)
  
  fin <- read_rds("data.rds")
  
  Sys.sleep(0.1)
  file.remove("data.rds")
  
  return(fin)
}

linkiesf <- possibly(linkie,otherwise = NULL, quiet = T)

spending_dat <- seq.Date(from = as.Date("2024-02-01"), to = Sys.Date(), by = "day") %>% 
  map_dfr(~{linkiesf(.x, sets)})

saveRDS(spending_dat, file = "data/spending_dat.rds")


sets <- jsonlite::fromJSON("settings.json")

full_cntry_list <- read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry) %>% 
  sample_n(n()) %>% 
  filter(iso2c == "TR")

cntryy <- "TR"

render_it <- function(...) {
  print("Now rendering:")
  print(...)
  quarto::quarto_render(..., quiet = T)
}
render_it <- possibly(render_it, otherwise = NULL, quiet = F)


advertiser_dat <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTVkw2cJ5IqeOTBKOfBXpDZDftW9g_nlN-ZKdqDK42wvcxZYHbkVBKDsxfB8r7V88RVef3zHIxBbDOw/pub?output=csv") %>% 
  janitor::clean_names()# %>% 

city_list <- advertiser_dat %>% 
  group_by(city) %>% 
  summarize(spend_30days_bf_march4_2024 = sum(spend_30days_bf_march4_2024)) %>% 
  arrange(desc(spend_30days_bf_march4_2024)) %>% 
  drop_na() %>% 
  pull(city)# %>% .[1]

# city_list <- advertiser_dat$city

# for (cities in city_list) {
  # print(cntryy)
  counter <- 0
city_list %>% 
  # keep(~str_detect(.x, "adana")) %>% 
  # .[1:5] %>% 
  walk_progress(~{
    
    the_city <- .x
    counter <- counter + 1
    # print(the_city)
    print(paste0(the_city, ": ", counter))
    try({
      
      sets$the_country <- full_cntry_list$country[which(full_cntry_list$iso2c==cntryy)]
      sets$cntry <- cntryy
      
      jsonlite::write_json(sets, "settings.json",  simplifyVector = TRUE)
      
      print(the_city)
      
      # title_txt <- read_lines("_quarto.yml")
      # title_txt[which(str_detect(title_txt, "title"))[1]] <- glue::glue("  title: \"Targeting Dashboard - {sets$the_country}\"")
      # # title_txt[which(str_detect(title_txt, "output-dir"))[1]] <- glue::glue("  output-dir: ../docs/{sets$cntry}")  
      # # Sys.sleep(1)
      # write_lines(title_txt, "_site/_quarto.yml")
      
      
      # all_dat <- readRDS("data/all_dat.rds")
      # color_dat <- readRDS("data/color_dat.rds")
      
      color_dat <- tibble()
      already_happened <- F
      if(read_lines("cntry.R") %>% length() > 5){
        election_dat30 <- readRDS("data/election_dat30.rds")  %>% 
          select(-contains("party")) %>%
          left_join(all_dat %>% select(page_id, party))
        already_happened <- T
      }
      
      if(!already_happened){
        out <- sets$cntry %>% 
          map(~{
            .x %>% 
              paste0(c("-last_30_days"))
          }) %>% 
          unlist() %>% 
          # keep(~str_detect(.x, tf)) %>% 
          # .[100:120] %>% 
          map_dfr_progress(~{
            the_assets <- httr::GET(paste0("https://github.com/favstats/meta_ad_targeting/releases/expanded_assets/", .x))
            
            the_assets %>% httr::content() %>% 
              rvest::html_elements(".Box-row") %>% 
              rvest::html_text()  %>%
              tibble(raw = .)   %>%
              # Split the raw column into separate lines
              mutate(raw = strsplit(as.character(raw), "\n")) %>%
              # Extract the relevant lines for filename, file size, and timestamp
              transmute(
                filename = sapply(raw, function(x) trimws(x[3])),
                file_size = sapply(raw, function(x) trimws(x[6])),
                timestamp = sapply(raw, function(x) trimws(x[7]))
              ) %>% 
              filter(filename != "Source code") %>% 
              mutate(release = .x) %>% 
              mutate_all(as.character)
          })
        
        thosearethere <- out %>% 
          rename(tag = release,
                 file_name = filename)  %>% 
          arrange(desc(tag)) %>% 
          separate(tag, into = c("cntry", "tframe"), remove = F, sep = "-") %>%
          filter(cntry == sets$cntry) %>% 
          mutate(ds  = str_remove(file_name, "\\.rds|\\.zip|\\.parquet")) %>% 
          distinct(cntry, ds, tframe) %>% 
          drop_na(ds) %>% 
          arrange(desc(ds))
        
        print(thosearethere)
        
        if(is.na(thosearethere$ds[1])){
          print("go next")
          next
        } 
        # try({
        election_dat30 <- arrow::read_parquet(paste0("https://github.com/favstats/meta_ad_targeting/releases/download/", sets$cntry, "-last_", 30,"_days/", thosearethere$ds[1], ".parquet"))
        # })
        
      }
      
      
      raw <- election_dat30 %>%
        rename(internal_id = contains("page_id"))# %>%
      # filter(is.na(no_data)) %>% 
      # filter(sources == "wtm")
      
      if(nrow(raw)==0){
        
        if(read_lines("cntry.R") %>% length() > 5){
          election_dat30 <- election_dat30 %>%
            rename(internal_id = contains("page_id")) %>%
            # filter(is.na(no_data)) %>% 
            drop_na(party) #%>% 
          # filter(party %in% color_dat$party) 
        } else {
          election_dat30 <- tibble()
        }
        
        
      } else {
        election_dat30 <- raw %>% 
          drop_na(party) #%>% 
        # filter(party %in% color_dat$party) 
      }
      
      
      
      # rstudioapi::jobRunScript("fbadlibrary.R")
      try({
        
        
        
        
        if(nrow(election_dat30)!=0){
          
          
          print("election_dat30 is not 0")
          # Sys.sleep(60*7)
          # all_dat <- readRDS("data/all_dat.rds")
          
          write_lines(nrow(distinct(election_dat30, internal_id)), file = "n_advertisers.txt")
          
          params <- list(the_city = the_city)
          # params_json <- jsonlite::toJSON(params, auto_unbox = TRUE)
          
          dir("_site", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>%
            discard(~str_detect(.x, "blog|map|about")) %>% 
            walk(~render_it(.x, execute_params = params))
          # dir("_site", full.names = T) %>% keep(~str_detect(.x, "index")) %>% walk(~render_it(.x, execute_params = params))
          
          if(!(fs::dir_exists( glue::glue("docs/{the_city}/")))){
            fs::dir_create( glue::glue("docs/{the_city}/"))
          }
          
          dir("docs", full.names = T) %>%
            keep(~str_detect(.x, "site|files")) %>%
            walk(~fs::dir_copy(.x, str_replace(.x, "docs/", glue::glue("docs/{the_city}/")), overwrite = T))
          
          dir("docs", full.names = T) %>%
            keep(~str_detect(.x, "html|json|logo")) %>%
            walk(~fs::file_copy(.x, str_replace(.x, "docs/", glue::glue("docs/{the_city}/")), overwrite = T))
          
          
          
          
          unlink("node_modules", recursive = T, force = T)
          unlink("out", recursive = T, force = T)
          
          dir("docs", full.names = T) %>%
            keep(~str_detect(.x, "site|files")) %>%
            walk(fs::dir_delete)
          
          dir("docs", full.names = T) %>%
            keep(~str_detect(.x, "html|json")) %>%
            walk(fs::file_delete)
          
        } else {
          
          print("election_dat30 is 0")
          
          rmarkdown::render("logs/index.Rmd")
          dir.create(glue::glue("docs/{sets$cntry}"), recursive = T)
          file.copy(from = "logs/index.Rmd", to = glue::glue("docs/index.html"), overwrite = T, recursive = T)
          
          unlink("node_modules", recursive = T, force = T)
          unlink("out", recursive = T, force = T)
          
        }
        
        
        
        
        
      })
      
    })
    
    # dir("docs", full.names = T) %>% 
    #   keep(~str_detect(.x, "site|files")) %>% 
    #   walk(fs::dir_delete)
    # 
    # dir("docs", full.names = T) %>% 
    #   keep(~str_detect(.x, "html|json")) %>% 
    #   walk(fs::file_delete)
    
    # file.remove("_site/_quarto.yml")
    
    rm(election_dat30)
    gc()
    
    
  })


# }

knitr::knit("README.Rmd")

params <- list(the_city = "all")
rmarkdown::render("logs/overview.Rmd", params = params)

file.copy(from = "logs/overview.html", to = glue::glue("docs/overview.html"), overwrite = T)

dir("_site", full.names = T,recursive = T) %>% keep(~str_detect(.x, "qmd")) %>% walk(~render_it(.x, execute_params = params))
# dir("_site", full.names = T) %>% keep(~str_detect(.x, "index")) %>% walk(~render_it(.x, execute_params = params))
# dir("_site", full.names = T) %>% keep(~str_detect(.x, "location")) %>% walk(~render_it(.x, execute_params = params))
# dir("_site", full.names = T) %>% keep(~str_detect(.x, "targeting")) %>% walk(~render_it(.x, execute_params = params))




city_list %>%
  # .[1] %>% 
  walk_progress( ~ {
    city_name <- .x
    dir("docs", full.names = T) %>%
      keep( ~ str_detect(.x, "map|blog|about")) %>%
      walk( ~ fs::file_copy(.x, str_replace(
        .x, "docs/", glue::glue("docs/{city_name}/")
      ), overwrite = T))
    
    # dir("docs", full.names = T) %>%
    #   keep( ~ str_detect(.x, "blog")) %>%
    #   walk( ~ fs::file_copy(.x, str_replace(
    #     .x, "docs/", glue::glue("docs/{city_name}/")
    #   ), overwrite = T))
    
    dir("docs", full.names = T) %>%
      keep( ~ str_detect(.x, "post")) %>%
      walk( ~ fs::dir_copy(.x, str_replace(
        .x, "docs/", glue::glue("docs/{city_name}/")
      ), overwrite = T))
  })

rmarkdown::render("index.Rmd")
# dir.create(glue::glue("docs/{sets$cntry}"), recursive = T)
# file.copy(from = "index.html", to = glue::glue("docs/index.html"), overwrite = T, recursive = T)
# dir(full.names = F) %>%
#   keep(~str_detect(.x, "_libs")) %>%
#   walk(~fs::dir_copy(.x, "docs/site_libs", overwrite = T))
# 

if(!("docs/map.html"  %in% fs::dir_ls("docs"))){
  dir("_site", full.names = T,recursive = T) %>% keep(~str_detect(.x, "map")) %>% walk(~render_it(.x, execute_params = params))
}

dir("docs/istanbul", full.names = T) %>% keep(~str_detect(.x, "map")) %>% walk(~file.copy(.x, str_remove(.x, "istanbul/")))

file.copy(from = "docs/istanbul/map.html", to = glue::glue("docs/map.html"), overwrite = T)

if (Sys.info()[["effective_user"]] == "fabio") {
  system("git pull")
  system("git add -A")
  system('git commit -m "update"')
  system("git push")
}




