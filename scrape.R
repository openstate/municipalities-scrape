## LIBRARIES

library(rvest)
library(dplyr)
library(Rcrawler)
library(stringr)

## SET WD
setwd("/Users/Nele/Desktop/Internship/R/")

## SET UTF
Sys.setlocale(locale="UTF-8" )

## load in data
library(rjson)
#gemeentes <- read.csv2("json")

# EXTRACT MUNICIPALITY WEBSITES ----------------------------------------------------

base <- read_html('https://almanak.overheid.nl/Gemeenten/') # basis URL

urls <- base %>%
  html_nodes('a') %>%
  html_attr('href') # extract almanak contact website per municipality

urls <- urls[(which(urls == "/Gemeenten")+1):(which(urls == "/Gemeenten/A-Z/A")-2)] # subset to only get municipalities

urls <- paste0("https://almanak.overheid.nl",urls) # add string to make it the full website

tbl <- list()
j <- 1
for (j in seq_along(urls)) {
  
  gemeente <- urls[[j]] %>%   # tbl[[j]] assigns each table from the urls as an element in the tbl list
    read_html() %>% 
    html_node("h1") %>%
    html_text()
  
  gemeente <- sub("\\).*", "", sub("^[^\\(]*\\(", "", gemeente))
  
  website <- urls[[j]] %>% 
    read_html() %>% 
    html_nodes("a") %>%
    html_attr('href')
  
  website <- ifelse(is.na(website[which(grepl(gsub(" ", "", tolower(sub("\\).*", "", sub(".*\\(", "", gemeente)))), website))[1]]),
                    # take lower case and collapse website into one word, grepl checks if website contains that word somewhere
                    website[(which(grepl("mailto", website))-1)], 
                    # if its NA, then give me the element before the element which contains mailto, because mail is listed often behind the munipilacity website
                    website[which(grepl(gsub(" ", "", tolower(sub("\\).*", "", sub(".*\\(", "", gemeente)))), website))[1]])
  # if its not NA, then give me that element (which is the URL)
  
  tbl[[j]] <- c(gemeente, website)
  
  j <- j+1                    # j <- j+1 iterates over each url in turn and assigns the table from the second url as an element of tbl list
}

tbl_bind <- do.call(rbind, tbl) # bind to one matrix
gemeentes <- as.data.frame(tbl_bind) # convert to data frame
gemeentes$V2 <- as.character(gemeentes$V2) # needed for next step
# gemeentes[326,2] <- "https://www.westerkwartier.nl/home" # westerkwatier has a strange string as their website, so change it here by hand
colnames(gemeentes) <- c("Gemeente", "Website")

gemeentes$Gemeente <- as.character(gemeentes$Gemeente)
gemeentes$Gemeente[32] <- "Bergen L" # bergen has their name double in their, causing the loop making the name 'L', here it is manually set to the right name

# # EXTRACT INFORMATION FROM URLS -------------------------------------------
# 
# urls <- as.list(gemeentes$Website)
# 
# tbl_2 <- list()
# j <- 1
# for (j in seq_along(gemeentes$Website)) {
#   
#   naam <- urls[[j]] %>%   # tbl[[j]] assigns each table from the urls as an element in the tbl list
#     read_html() %>% 
#     html_node("h1") %>%
#     html_text()
#   
#   all <- urls[[j]] %>% 
#     read_html() %>% 
#     html_node("#main_content_wrapper") %>%
#     html_text()
#   
#   tbl_2[[j]] <- c(naam,all)
#   
#   j <- j+1                    # j <- j+1 iterates over each url in turn and assigns the table from the second url as an element of tbl list
# }



# CHECK FOR SITEMAP --------------------------------------------

gemeentes$Sitemaps <- paste0(gemeentes$Website, "/sitemap.xml")
for (i in seq_along(gemeentes$Sitemaps)) {
  
  gemeentes$Sitemaps[[i]] <- ifelse((tryCatch({
    gemeentes$Sitemaps[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Sitemaps[[i]], NA)
  
  gemeentes$Sitemaps[[i]] <- ifelse(!is.na(gemeentes$Sitemaps[[i]]), ifelse(length(gemeentes$Sitemaps[[i]] %>%
                                                                                     read_html() %>%
                                                                                     html_nodes("loc")) > 1, 
                                                                            gemeentes$Sitemaps[[i]], NA),
                                    NA)
  
}



# CHECK FOR EXTERNAL WEBSITE "https://ris2.ibabs.eu/" AND WHICH IS RAADSLEDEN SITE --------------------------------------

gemeentes$ibabs <- sub("\\.nl.*", "", sub(".*\\www.", "", gemeentes$Website))
gemeentes$ibabs <- paste0("https://ris2.ibabs.eu/", gemeentes$ibabs)

for (i in seq_along(gemeentes$ibabs)) {
  
  gemeentes$ibabs[[i]] <- ifelse((tryCatch({
    gemeentes$ibabs[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  ifelse(length(gemeentes$ibabs[[i]] %>%
                  read_html() %>%
                  html_nodes("a")) > 3,
         gemeentes$ibabs[[i]], NA), NA)
  
  # filter sites that are only for raadsleden themself
  gemeentes$ibabs[[i]] <- ifelse(!is.na(gemeentes$ibabs[[i]]), 
                                 ifelse(length(which(grepl("Wie is wie", gemeentes$ibabs[[i]] %>% # only hold ibabs if "Wie is wie" is part of the website
                                                             read_html() %>%
                                                             html_nodes("a") %>%
                                                             html_text()))) != 0,
                                        gemeentes$ibabs[[i]], NA), gemeentes$ibabs[[i]])
  
  # search for site with "Wie is wie"
  gemeentes$ibabs[[i]] <- ifelse(!is.na(gemeentes$ibabs[[i]]), 
                                 (gemeentes$ibabs[[i]] %>%
                                    read_html() %>%
                                    html_nodes("a") %>%
                                    html_attr("href"))[which(grepl("/People/Index/", gemeentes$ibabs[[i]] %>%
                                                                     read_html() %>%
                                                                     html_nodes("a") %>%
                                                                     html_attr("href")))],
                                 gemeentes$ibabs[[i]]); gemeentes$ibabs[[i]]
  
  gemeentes$ibabs[[i]] <- ifelse(!is.na(gemeentes$ibabs[[i]]),
                                 paste0("https://ris2.ibabs.eu", gemeentes$ibabs[[i]]),
                                 gemeentes$ibabs[[i]])
  
  # is there the possiblity to go to different sites from "Wie is wie"?
  doorklik <- ifelse(!is.na(gemeentes$ibabs[[i]]),
                     ifelse(length(gemeentes$ibabs[[i]] %>%
                                     read_html() %>%
                                     html_nodes("h2") %>%
                                     html_text()) > 0,
                            0,
                            1), gemeentes$ibabs[[i]])
  
  # to which sites can you
  doorklik_mogelijkheden <- if (isTRUE(doorklik == 1)) {
    (gemeentes$ibabs[[i]] %>%
       read_html() %>%
       html_nodes("a") %>%
       html_attr("href"))[which(grepl("/People/Profiles/|/People/Details/", gemeentes$ibabs[[i]] %>%
                                        read_html() %>%
                                        html_nodes("a") %>%
                                        html_attr("href")))]
  } else {
    gemeentes$ibabs[[i]]
  }
  
  # make it an url
  doorklik_mogelijkheden <- if (isTRUE(doorklik == 1 & length(doorklik_mogelijkheden) != 0)) {
    
    doorklik_mogelijkheden <- paste0("https://ris2.ibabs.eu", doorklik_mogelijkheden)
    
  } else {
    
    0
    
  }
  
  # if you can go to different sites from "Wie is wie" and there are more than 0 possibilities, than check which one is Raadsleden|Gemeenteraad|Raad
  headers <- character()
  
  doorklik_mogelijkheden <- if (isTRUE(doorklik == 1 & length(doorklik_mogelijkheden) > 0)) {
    
    for (j in seq_along(doorklik_mogelijkheden)) {
      
      headers[[j]] <- doorklik_mogelijkheden[[j]] %>%
        read_html() %>%
        html_nodes("h2") %>%
        html_text()
      
    }
    
    gemeentes$ibabs[[i]] <- doorklik_mogelijkheden[which.min(nchar(grepl("Raadsleden|raadsleden|Gemeenteraad|gemeenteraad|De raad", headers)))]
    
  } else {
    
    0
    
  }
  # remove everything that is not used anymore (/that do not have content on their sites)
  if (!is.na(gemeentes$ibabs[[i]])) {
    gemeentes$ibabs[[i]] <- ifelse(
      length(gemeentes$ibabs[[i]] %>%
               read_html() %>% 
               html_nodes("a") %>%
               html_attr("href")) < 10,
      NA,
      gemeentes$ibabs[[i]])
  }
  
}


# CHECK FOR EXTERNAL WEBSITE "notubiz" ------------------------------

gemeentes$notubiz <- str_replace(str_replace(gemeentes$Website, "www.", ""), ".nl", ".notubiz.nl/leden")

for (i in seq_along(gemeentes$notubiz)) {
  
  # check if websites exist
  gemeentes$notubiz[[i]] <- ifelse((tryCatch({
    gemeentes$notubiz[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$notubiz[[i]], NA)
  
  # filter to only get websites of members
  if (!is.na(gemeentes$notubiz[i])) {
    
    # filter website that don't have any content
    gemeentes$notubiz[[i]] <- ifelse(length(gemeentes$notubiz[[i]] %>%
                                              read_html() %>%
                                              html_nodes("a") %>%
                                              html_text()) < 20,
                                     NA,
                                     gemeentes$notubiz[[i]])
    # for all websites that do have content...
    if (!is.na(gemeentes$notubiz[i])) {
      # store all urls that include "lid" in url (all urls from members of municipality council)
      urls <- (gemeentes$notubiz[[i]] %>%
                 read_html() %>%
                 html_nodes("a") %>%
                 html_attr("href"))[which(grepl("lid", (gemeentes$notubiz[[i]] %>%
                                                          read_html() %>%
                                                          html_nodes("a") %>%
                                                          html_attr("href"))))]
      # for all websites that do own urls of members of municipality council (others are mostly just a connection to https://notubiz.nl/)... 
      if (length(urls) > 1) {
        dates <- NA
        # read in to dates of last acitivity of all members
        for (j in seq_along(urls)) {
          dates[[j]] <- urls[[j]] %>%
            read_html() %>%
            html_node("time") %>%
            html_attr("datetime")
          
          # compare those dates to the current yeaar to make sure to only get websites that are up to date
          if (grepl(substr(dates[[j]], 1, 4), substr(Sys.Date(), 1, 4)) & !is.na(dates[[j]])) {
            gemeentes$notubiz[[i]] <- gemeentes$notubiz[[i]]
          } else {
            gemeentes$notubiz[[i]] <- NA
          }
        }
      } else {
        gemeentes$notubiz[[i]] <- NA
      }
    }
  }
  
  i <- i + 1
}

# example
# gemeentes$notubiz[[18]] <- ifelse((tryCatch({
#   gemeentes$notubiz[[18]] %>%
#     read_html()
#   TRUE
# }, error=function(e) FALSE)),
# gemeentes$notubiz[[18]], NA)
# 
# if (!is.na(gemeentes$notubiz[18])) {
#   gemeentes$notubiz[[18]] <- ifelse(length(gemeentes$notubiz[[18]] %>%
#                                              read_html() %>%
#                                              html_nodes("a") %>%
#                                              html_text()) < 20,
#                                     NA,
#                                     gemeentes$notubiz[[18]])
#   if (!is.na(gemeentes$notubiz[18])) {
#     urls <- (gemeentes$notubiz[[18]] %>%
#                read_html() %>%
#                html_nodes("a") %>%
#                html_attr("href"))[which(grepl("lid", (gemeentes$notubiz[[18]] %>%
#                                                         read_html() %>%
#                                                         html_nodes("a") %>%
#                                                         html_attr("href"))))]
#     if (length(urls) > 1) {
#       dates <- NA
#       # read in to dates of last acitivity of all members
#       for (j in seq_along(urls)) {
#         dates[[j]] <- urls[[j]] %>%
#           read_html() %>%
#           html_node("time") %>%
#           html_attr("datetime")
#         
#         # compare those dates to the current yeaar to make sure to only get websites that are up to date
#         if (grepl(substr(dates[[j]], 1, 4), substr(Sys.Date(), 1, 4)) & !is.na(dates[[j]])) {
#           gemeentes$notubiz[[18]] <- gemeentes$notubiz[[18]]
#         } else {
#           gemeentes$notubiz[[18]] <- NA
#         }
#       }
#     } else {
#       gemeentes$notubiz[[18]] <- NA
#     }
#   }
# }

# CHECK FOR EXTERNAL WEBSITE "gementeraad." --------------------------------------

gemeentes$Gemeenteraad. <- stringr::str_replace(gemeentes$Website, "www.", "gemeenteraad.")

for (i in seq_along(gemeentes$Gemeenteraad.)) {
  
  gemeentes$Gemeenteraad.[[i]] <- ifelse((tryCatch({
    gemeentes$Gemeenteraad.[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Gemeenteraad.[[i]], NA)
  
}


# CHECK FOR EXTERNAL WEBSITE "gemeenteraad" -------------------------

gemeentes$Gemeenteraad <- stringr::str_replace(gemeentes$Website, "www.", "gemeenteraad")

for (i in seq_along(gemeentes$Gemeenteraad)) {
  
  gemeentes$Gemeenteraad[[i]] <- ifelse((tryCatch({
    gemeentes$Gemeenteraad[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Gemeenteraad[[i]], NA)
  
}

# filter voor dingen

# CHECK FOR EXTERNAL WEBSITE "raad." --------------------------------------

gemeentes$Website[which(!grepl("www", gemeentes$Website))] <- sub("://", "://www.", gemeentes$Website[which(!grepl("www", gemeentes$Website))])

gemeentes$Raad <- stringr::str_replace(gemeentes$Website, "www.", "raad.")

for (i in seq_along(gemeentes$Raad)) {
  
  gemeentes$Raad[[i]] <- ifelse((tryCatch({
    gemeentes$Raad[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Raad[[i]], NA)
  
}


# CHECK FOR EXTERNAL WEBSITE "ris" ----------------------------------
gemeentes$ris <- str_replace(gemeentes$Website, "www.", "ris.")
for (i in seq_along(gemeentes$ris)) {
  
  gemeentes$ris[[i]] <- ifelse((tryCatch({
    gemeentes$ris[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$ris[[i]], NA)
  
  gemeentes$ris[[i]] <- ifelse(!is.na(gemeentes$ris[[i]]), ifelse(length(which(grepl("raad", (gemeentes$ris[[i]] %>%
                                                                                                read_html() %>%
                                                                                                html_nodes("a") %>%
                                                                                                html_attr("href"))))) > 0,
                                                                  gemeentes$ris[[i]], NA),
                               NA)
  
}
# length(which(grepl("raad", (gemeentes$ris[[178]] %>%
#   read_html() %>%
#   html_nodes("a") %>%
#   html_attr("href"))))) > 0
# ding <- as.data.frame(LinkExtractor("http://ris.olst-wijhe.nl/besturen")$InternalLinks)
# ding2 <- as.data.frame(LinkExtractor(gemeentes$Website[[222]])$InternalLinks)
# CHECK FOR EXTERNAL WEBSITE "bestuur.gemeente.nl" ------------------
gemeentes$Bestuur <- str_replace(gemeentes$Website, "www.", "bestuur.")
for (i in seq_along(gemeentes$Bestuur)) {
  
  gemeentes$Bestuur[[i]] <- ifelse((tryCatch({
    gemeentes$Bestuur[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Bestuur[[i]], NA)
  
  # gemeentes$Bestuur[[i]] <- ifelse(!is.na(gemeentes$Bestuur[[i]]), ifelse(length(gemeentes$Bestuur[[i]] %>%
  #                                                                                    read_html() %>%
  #                                                                                    html_nodes("loc")) > 1, 
  #                                                                         gemeentes$Bestuur[[i]], NA),
  #                                   NA)
  
}


# CHECK FOR EXTERNAL WEBSITE "raadsinformatie" ------------------------------
gemeentes$Raadsinformatie <- sub("www.", "", gemeentes$Website)
gemeentes$Raadsinformatie <- sub("[.]nl", ".raadsinformatie.nl", gemeentes$Raadsinformatie)
gemeentes$Raadsinformatie <- sub("[.]eu", ".raadsinformatie.eu", gemeentes$Raadsinformatie)
gemeentes$Raadsinformatie <- sub("[.]org", ".raadsinformatie.org", gemeentes$Raadsinformatie)
gemeentes$Raadsinformatie <- sub("[.]info", ".raadsinformatie.info", gemeentes$Raadsinformatie)
gemeentes$Raadsinformatie <- sub("[.]net", ".raadsinformatie.net", gemeentes$Raadsinformatie)

for (i in seq_along(gemeentes$Raadsinformatie)) {
  
  gemeentes$Raadsinformatie[[i]] <- ifelse((tryCatch({
    gemeentes$Raadsinformatie[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Raadsinformatie[[i]], NA)
  
}

ifelse((tryCatch({
  gemeentes$Raadsinformatie[179] %>%
    read_html()
  TRUE
}, error=function(e) FALSE)),
gemeentes$Raadsinformatie[[i]], NA)

# https://lochem.raadsinformatie.nl/leden
# CHECK IF WEBSITE IS PART OF "GO" --------------------------------------
gemeentes$Website[136] <- "https://www.h-i-ambacht.nl/"
gemeentes$Website[65] <- "http://www.dantumadiel.frl"

gemeentes$go <- NA
# for raads websites
for (i in seq_along(gemeentes$Raad)){
  if (!is.na(gemeentes$Raad[[i]])) {
    gemeentes$go[[i]] <- ifelse(length(gemeentes$Raad[[i]] %>%
                                         read_html() %>%
                                         html_nodes("a.go") %>%
                                         html_text()) != 0 |
                                  length(gemeentes$Raad[[i]] %>%
                                           read_html() %>%
                                           html_nodes("span.go") %>%
                                           html_text()) != 0 |
                                  length(grepl("GemeenteOplossingen", (gemeentes$Raad[[i]] %>%
                                                                         read_html() %>%
                                                                         html_nodes("a.right") %>%
                                                                         html_text()))) > 0,
                                gemeentes$Raad[[i]], NA)
  }
}
length(which(!is.na(gemeentes$go)))

# for gemeenteraad websites
for (i in seq_along(gemeentes$Gemeenteraad)){
  if (is.na(gemeentes$go[[i]]) & !is.na(gemeentes$Gemeenteraad[[i]])) {
    gemeentes$go[[i]] <- ifelse(length(gemeentes$Gemeenteraad[[i]] %>%
                                         read_html() %>%
                                         html_nodes("a.go") %>%
                                         html_text()) != 0 |
                                  length(gemeentes$Gemeenteraad[[i]] %>%
                                           read_html() %>%
                                           html_nodes("span.go") %>%
                                           html_text()) != 0 |
                                  length(grepl("GemeenteOplossingen", (gemeentes$Gemeenteraad[[i]] %>%
                                                                         read_html() %>%
                                                                         html_nodes("a.right") %>%
                                                                         html_text()))) > 0,
                                gemeentes$Gemeenteraad[[i]], NA)
  }
}

# for bestuur websites
for (i in seq_along(gemeentes$Bestuur)){
  if (is.na(gemeentes$go[[i]]) & !is.na(gemeentes$Bestuur[[i]])) {
    gemeentes$go[[i]] <- ifelse(length(gemeentes$Bestuur[[i]] %>%
                                         read_html() %>%
                                         html_nodes("a.go") %>%
                                         html_text()) != 0 |
                                  length(gemeentes$Bestuur[[i]] %>%
                                           read_html() %>%
                                           html_nodes("span.go") %>%
                                           html_text()) != 0 |
                                  length(grepl("GemeenteOplossingen", (gemeentes$Bestuur[[i]] %>%
                                                                         read_html() %>%
                                                                         html_nodes("a.right") %>%
                                                                         html_text()))) > 0,
                                gemeentes$Bestuur[[i]], NA)
  }
}

# for regular gemeente websites
for (i in seq_along(gemeentes$Website)) {
  if (is.na(gemeentes$go[[i]])) {
    gemeentes$go[[i]] <- ifelse(length(gemeentes$Website[[i]] %>%
                                         read_html() %>%
                                         html_nodes("a.go") %>%
                                         html_text()) != 0 |
                                  length(gemeentes$Website[[i]] %>%
                                           read_html() %>%
                                           html_nodes("span.go") %>%
                                           html_text()) != 0 |
                                  length(grepl("GemeenteOplossingen", (gemeentes$Website[[i]] %>%
                                                                         read_html() %>%
                                                                         html_nodes("a.right") %>%
                                                                         html_text()))) > 0,
                                gemeentes$Website[[i]], NA)
  }
}

# example
# gemeentes$go[[1]] <- ifelse(length(gemeentes$Website[[1]] %>%
#                                      read_html() %>%
#                                      html_nodes("a.go") %>%
#                                      html_text()) != 0 |
#                               length(grepl("GemeenteOplossingen", (gemeentes$Website[[1]] %>%
#                                                               read_html() %>%
#                                                               html_nodes("a.right") %>%
#                                                               html_text()))) > 0,
#                             gemeentes$Website[[1]], NA)
# 
# ifelse(length(gemeentes$Raad[[275]] %>%
#                 read_html() %>%
#                 html_nodes("a.go") %>%
#                 html_text()) != 0 |
#          length(gemeentes$Raad[[275]] %>%
#                   read_html() %>%
#                   html_nodes("span.go") %>%
#                   html_text()) != 0 |
#          length(grepl("GemeenteOplossingen", (gemeentes$Raad[[275]] %>%
#                                                 read_html() %>%
#                                                 html_nodes("a.right") %>%
#                                                 html_text()))) > 0,
#        gemeentes$Raad[[275]], NA)





# SCRAPE WEBSITES USING IBABS ---------------------------------------------

# municipality websites of municipality council
gemeentes$ibabs_url <- NA

j <- 1
for (j in seq_along(gemeentes$ibabs)) {
  
  if (!is.na(gemeentes$ibabs[[j]])) {
    
    gemeentes$ibabs_url[j] <- list(unique((gemeentes$ibabs[[j]] %>%  
                                             read_html() %>% 
                                             html_nodes("a") %>%
                                             html_attr("href"))[which(grepl("categoryId", (gemeentes$ibabs[[j]] %>%   
                                                                                             read_html() %>% 
                                                                                             html_nodes("a") %>%
                                                                                             html_attr("href"))))]))
    
    
    gemeentes$ibabs_url[[j]] <- paste0("https://ris2.ibabs.eu", gemeentes$ibabs_url[[j]])
    
  } else {
    
    NA
    
  }
  
  j <- j+1
  
}

list <- list()
labels <- list()
text <- list()
add.text <- list()
naam <- numeric(0)
politieke_partij <- numeric(0)
adres <- numeric(0)
emailadres <- numeric(0)
telefoonnummer <- numeric(0)
hoofdfunctie <- numeric(0)
nevenfunctie <- numeric(0)
for (i in seq_along(gemeentes$ibabs_url)) {
  #gemeentes$ibabs_info[i] <- list(0)
  if (!is.na(gemeentes$ibabs_url[i])) {
    for (j in seq_along(gemeentes$ibabs_url[[i]])) {
      naam[j] <- gemeentes$ibabs_url[[i]][j] %>%
        read_html() %>%
        html_nodes("h2.profile-name") %>%
        html_text()
      labels[[j]] <- gemeentes$ibabs_url[[i]][j] %>%
        read_html() %>%
        html_nodes("div.col-sm-5.detail-row-label") %>%
        html_text()
      text[[j]] <- gemeentes$ibabs_url[[i]][j] %>%
        read_html() %>%
        html_nodes("div.col-sm-7.detail-row-text") %>%
        html_text()
      politieke_partij[[j]] <- ifelse(grepl("Politieke partij", labels[j]),
                                      text[[j]][which(grepl("Politieke partij", labels[[j]]))],
                                      NA)
      adres[[j]] <- ifelse(grepl("Adres", labels[j]),
                           text[[j]][which(grepl("Adres", labels[[j]]))],
                           NA)
      emailadres[[j]] <- ifelse(grepl("Emailadres", labels[j]),
                                text[[j]][which(grepl("Emailadres", labels[[j]]))],
                                NA)
      telefoonnummer[[j]] <- ifelse(grepl("Telefoonnummer", labels[j]),
                                    text[[j]][which(grepl("Telefoonnummer", labels[[j]]))],
                                    NA)
      hoofdfunctie[[j]] <- ifelse(grepl("Hoofdfunctie", labels[j]),
                                  text[[j]][which(grepl("Hoofdfunctie", labels[[j]]))],
                                  NA)
      nevenfunctie[[j]] <- ifelse(grepl("Nevenfunctie", labels[j]),
                                  text[[j]][which(grepl("Nevenfunctie", labels[[j]]))],
                                  NA)
    }
    gemeentes$ibabs_info[[i]] <- list()
    gemeentes$ibabs_info[[i]] <- list(naam = naam, politieke_partij = politieke_partij, adres = adres, emailadres= emailadres,
                                      telefoonnummer = telefoonnummer, hoofdfunctie = hoofdfunctie, nevenfunctie = nevenfunctie)
  } else {
    gemeentes$ibabs_info[[i]] <- NA
  }
}


gemeentes$ibabs_url[which(gemeentes$ibabs_url == "https://ris2.ibabs.eu")] <- NA
length(which(gemeentes$ibabs_url == "https://ris2.ibabs.eu"))


naam[23] <- ifelse(length(gemeentes$ibabs_url[[34]][1] %>%
                            read_html() %>%
                            html_nodes("h2.profile-name") %>%
                            html_text()) != 0,
                   gemeentes$ibabs_url[[34]][1] %>%
                     read_html() %>%
                     html_nodes("h2.profile-name") %>%
                     html_text(),
                   NA)
labels[[23]] <- gemeentes$ibabs_url[[34]][23] %>%
  read_html() %>%
  html_nodes("div.col-sm-5.detail-row-label") %>%
  html_text()
text[[23]] <- gemeentes$ibabs_url[[34]][23] %>%
  read_html() %>%
  html_nodes("div.col-sm-7.detail-row-text") %>%
  html_text()
politieke_partij[[23]] <- ifelse(grepl("Politieke partij", labels[23]),
                                 text[[23]][which(grepl("Politieke partij", labels[[23]]))],
                                 NA)
adres[[23]] <- ifelse(grepl("Adres", labels[23]),
                      text[[23]][which(grepl("Adres", labels[[23]]))],
                      NA)
emailadres[[23]] <- ifelse(grepl("Emailadres", labels[23]),
                           text[[23]][which(grepl("Emailadres", labels[[23]]))],
                           NA)
telefoonnummer[[23]] <- ifelse(grepl("Telefoonnummer", labels[23]),
                               text[[23]][which(grepl("Telefoonnummer", labels[[23]]))],
                               NA)
hoofdfunctie[[23]] <- ifelse(grepl("Hoofdfunctie", labels[23]),
                             text[[23]][which(grepl("Hoofdfunctie", labels[[23]]))],
                             NA)
nevenfunctie[[23]] <- ifelse(grepl("Nevenfunctie", labels[23]),
                             text[[23]][which(grepl("Nevenfunctie", labels[[23]]))],
                             NA)
gemeentes$ibabs_info[[34]] <- list()
gemeentes$ibabs_info[[34]] <- list(naam = naam, politieke_partij = politieke_partij, adres = adres, emailadres= emailadres,
                                   telefoonnummer = telefoonnummer, hoofdfunctie = hoofdfunctie, nevenfunctie = nevenfunctie)

# SCRAPE WEBSITES USING NOTUBIZ -------------------------------------------

# municipality websites of municipality council
gemeentes$notubiz_url <- NA

j <- 1
for (j in seq_along(gemeentes$notubiz)) {
  
  if (!is.na(gemeentes$notubiz[[j]])) {
    
    gemeentes$notubiz_url[j] <- list(unique((gemeentes$notubiz[[j]] %>%  
                                               read_html() %>% 
                                               html_nodes("a") %>%
                                               html_attr("href"))[which(grepl("nl/leden/lid/", (gemeentes$notubiz[[j]] %>%   
                                                                                                  read_html() %>% 
                                                                                                  html_nodes("a") %>%
                                                                                                  html_attr("href"))))]))
    
  } else {
    
    NA
    
  }
  
  j <- j+1
  
}

# use those urls to finally scrape information of members

list <- list()
for (i in seq_along(gemeentes$notubiz_url)) {
  naam <- character(0)
  telefoonnummer <- character(0)
  functie <- character(0)
  email <- character(0)
  nevenactiviteiten <- character(0)
  for (j in seq_along(gemeentes$notubiz_url[[i]])) {
    naam[[j]] <- ifelse(is.na(gemeentes$notubiz_url[i]),
                        NA,
                        (unique(gemeentes$notubiz_url[[i]][j] %>%
                                  read_html() %>%
                                  html_nodes("dd.volledige_naam") %>%
                                  html_text())))
    
    telefoonnummer[[j]] <- ifelse(is.na(gemeentes$notubiz_url[i]),
                                  NA,
                                  (unique(gemeentes$notubiz_url[[i]][j] %>%
                                            read_html() %>%
                                            html_nodes("dd.telefoonnummer") %>%
                                            html_text())))
    
    functie[[j]] <- ifelse(is.na(gemeentes$notubiz_url[i]),
                           NA,
                           (unique(gemeentes$notubiz_url[[i]][j] %>%
                                     read_html() %>%
                                     html_nodes("dd.functie") %>%
                                     html_text())))
    
    email[[j]] <- ifelse(is.na(gemeentes$notubiz_url[i]),
                         NA,
                         (unique(gemeentes$notubiz_url[[i]][j] %>%
                                   read_html() %>%
                                   html_nodes("dd.email") %>%
                                   html_text())))
    
    nevenactiviteiten[[j]] <- ifelse(is.na(gemeentes$notubiz_url[i]),
                                     NA,
                                     (unique(gemeentes$notubiz_url[[i]][j] %>%
                                               read_html() %>%
                                               html_nodes("div#nevenactiviteiten") %>%
                                               html_text())))
    
    j <- j+1
  }
  list[[i]] <- list(naam = naam, telefoonnummer = telefoonnummer, functie = functie, email = email, nevenactiviteiten = nevenactiviteiten)
  i <- i+1
}



# example
# gemeentes$notubiz_url[[72]][2] %>%
#   read_html() %>%
#   html_nodes("dd.volledige_naam") %>%
#   html_text()
# 
# gemeentes$notubiz_url[[72]][2] %>%
#   read_html() %>%
#   html_nodes("dd.telefoonnummer") %>%
#   html_text()
# 
# gemeentes$notubiz_url[[72]][2] %>%
#   read_html() %>%
#   html_nodes("dd.functie") %>%
#   html_text()
# 
# gemeentes$notubiz_url[[72]][2] %>%
#   read_html() %>%
#   html_nodes("dd.email") %>%
#   html_text()
# 
# gemeentes$notubiz_url[[72]][2] %>%
#   read_html() %>%
#   html_nodes("dd.email") %>%
#   html_text()
# 
# gemeentes$notubiz_url[[30]][1] %>%
#   read_html() %>%
#   html_nodes("dd.url") %>%
#   html_text()
# 
# gemeentes$notubiz_url[[30]][1] %>%
#   read_html() %>%
#   html_nodes("div#nevenactiviteiten") %>%
#   html_text()
# SCRAPE WEBSITES USING AN EXTERNAL WEBSITE OR REGULAR WEBSITE ------------
# gather urls of websites that use an external website
gemeentes$extern <- NA
for (i in seq_along(gemeentes$Website)) {
  
  if (is.na(gemeentes$notubiz[i])
      & is.na(gemeentes$notubiz[i])
  ) {
    
    if (!is.na(gemeentes$ris[i])) {
      gemeentes$extern[i] <- gemeentes$ris[i]
    }
    if (!is.na(gemeentes$Bestuur[i])) {
      gemeentes$extern[i] <- gemeentes$Bestuur[i]
    }
    if (!is.na(gemeentes$Raad[i])) {
      gemeentes$extern[i] <- gemeentes$Raad[i]
    }
    if (!is.na(gemeentes$Gemeenteraad[i])) {
      gemeentes$extern[i] <- gemeentes$Gemeenteraad[i]
    }
    if (!is.na(gemeentes$Gemeenteraad.[i])) {
      gemeentes$extern[i] <- gemeentes$Gemeenteraad.[i]
    } else {
      gemeentes$extern[i] <- gemeentes$Website[i]
    } 
  }
  i <- i + 1
}

# get url per member of each municipality
gemeentes$extern_url <- NA
i <- 1
for (i in seq_along(gemeentes$extern)) {
  if (!is.na(gemeentes$extern[i])) {
    if (tryCatch({
      LinkExtractor(gemeentes$extern[i])
      TRUE
    }, error=function(e) FALSE)) {
      links <- LinkExtractor(gemeentes$extern[i])$InternalLinks
      links_sub <- sub(".*nl", "", links)
      links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|organisatie|Organisatie|
                                       raadsleden|Raadsleden", 
                                       links_sub) & !grepl("Vergaderingen|vergaderingen", links_sub))]
      
      for (t in seq_along(links_grepl)) {
        links2 <- LinkExtractor(links_grepl[t])$InternalLinks
        links_sub2 <- sub(".*nl", "", links2)
        links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|organisatie|Organisatie|
                                           raadsleden|Raadsleden", 
                                           links_sub2) & !grepl("Vergaderingen|vergaderingen", links_sub2))]
      }
      
      links_merge <- unique(c(links_grepl, links_grepl2))
      
      if (length(links_merge > 0) & tryCatch({
        for (l in seq_along(links_merge))
          links_merge[l] %>%
          read_html()
        TRUE
      }, error=function(e) FALSE)) {
        lengths <- NA
        for (j in seq_along(links_merge)) {
          lengths[j] <- length(links_merge[j] %>%
                                 read_html() %>%
                                 html_nodes("a") %>%
                                 html_attr("href"))
        }
        gemeentes$extern_url[i] <- links_merge[which.max(lengths)] 
      }
    }
    }
  i <- i + 1
}




# breda 50, beverwijk 39,
# gemeentes$Website[16] <- "http://www.amsterdam.nl/"
# gemeentes$extern[16] <- "https://www.amsterdam.nl/"
# tryCatch({
#   LinkExtractor(gemeentes$extern[44])
#   TRUE
# }, error=function(e) FALSE)
# links <- LinkExtractor(gemeentes$extern[44])$InternalLinks
# links_sub <- sub(".*nl", "", links)
# links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|organisatie|Organisatie|
#                                  raadsleden|Raadsleden", 
#                                  links_sub) & !grepl("Vergaderingen|vergaderingen", links_sub))]
# for (i in seq_along(links_grepl)) {
#   links2 <- LinkExtractor(links_grepl[i])$InternalLinks
#   links_sub2 <- sub(".*nl", "", links2)
#   links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|organisatie|Organisatie|
#                                  raadsleden|Raadsleden", 
#                                    links_sub2) & !grepl("Vergaderingen|vergaderingen", links_sub2))]
# }
# 
# links_merge <- unique(c(links_grepl, links_grepl2))
# 
# # 53 is FALSE (kann nicht gelesen werden), 45 is TRUE (kann gelesen werden)
# tryCatch({
#   for (i in seq_along(links_grepl))
#     links_grepl[i] %>%
#     read_html()
#   TRUE
# }, error=function(e) FALSE)
# 
# lengths <- NA
# for (i in seq_along(links_grepl)) {
#   lengths[i] <- length(links_grepl[i] %>%
#            read_html() %>%
#            html_nodes("a") %>%
#            html_attr("href"))
# }
# links_grepl[which.max(lengths)]
# 
# length(gemeentes$extern[16] %>%
#   read_html() %>%
#   html_nodes("a") %>%
#   html_attr("href"))
# 
# length(links_grepl[2] %>%
#   read_html() %>%
#   html_nodes("a") %>%
#   html_attr("href"))
# 
# length(links_grepl[3] %>%
#   read_html() %>%
#   html_nodes("a") %>%
#   html_attr("href"))
# 
# length(links_grepl[4] %>%
#   read_html() %>%
#   html_nodes("a") %>%
#   html_attr("href"))



# NOTITIES ----------------------------------------------------------------

# EXTRACT RAADSLEDEN WEBSITE with sitemap
links <- gemeentes$Sitemaps[19] %>%
  read_html() %>%
  html_nodes("loc") %>%
  html_text(); links

links <- unique(links[which(grepl("gemeenteraad|samenstelling|wie-is-wie", links))]); links
links <- unique(links[which(grepl("gemeenteraad|samenstelling|wie-is-wie|ibabs", links))]); links

website <- ifelse(length(grepl("gemeenteraad", links) & grepl("samenstelling", links)) == 1,
                  links[which(grepl("gemeenteraad", links) & grepl("samenstelling", links))],
                  links[which(grepl("gemeenteraad", links) & grepl("samenstelling", links))][which.min(nchar(links[which(grepl("gemeenteraad", links) & grepl("samenstelling", links))]))]
)

ifelse(is.na(wbsite),
       links[which.min(nchar(links[which(grepl("gemeenteraad|samenstelling|wie-is-wie", links))]))])
LinkExtractor("https://www.arnhem.nl/Bestuur/gemeenteraad/over_de_gemeenteraad", ExternalLInks = T)


# EXTRACT RAADSLEDEN SITE FROM BERGEN (XML SITEMAP) ---------------------------
links <- gemeentes$Sitemaps[33] %>%
  read_html() %>%
  html_nodes("loc") %>%
  html_text()

links <- unique(links[which(grepl("gemeenteraad", links))]); links

# EXTRACT RAADSLEDEN SITE FROM ALBRANDSWAARD (XML SITEMAP) --------------------------- wie-is-wie

links <- gemeentes$Sitemaps[6] %>%
  read_html() %>%
  html_nodes("loc") %>%
  html_text()

links <- unique(links[which(grepl("gemeenteraad|samenstelling|wie-is-wie", links))]); links
gemeenteraad <- links[which.min(nchar(links))] ;gemeenteraad

# EXTRACT RAADSLEDEN SITE FROM ALMELO (XML SITEMAP) --------------------------- SAMENSTELLING

links <- gemeentes$Sitemaps[8] %>%
  read_html() %>%
  html_nodes("loc") %>%
  html_text()

links <- unique(links[which(grepl("gemeenteraad|samenstelling", links))]); links
gemeenteraad <- links[which.min(nchar(links))] ;gemeenteraad

# EXTRACT RAADSLEDEN SITE FROM ZWOLLE (NO SITEMAP & IBABS) ------------------------------------------------------------------
# algemeen > "gemeenteraad" > "ibabs"
links <- unlist(purrr::flatten(LinkExtractor(gemeentes$Website[355], ExternalLInks = TRUE)[c(2,3)])) ;links
links <- unique(links[which(grepl("gemeenteraad", links))]) ;links
gemeenteraad <- links[which.min(nchar(links))] ;gemeenteraad

links_bestuur <- unlist(purrr::flatten(LinkExtractor(gemeenteraad, ExternalLInks = TRUE)[c(2,3)])) ;links_bestuur
links_ibabs <- links_bestuur[which(grepl("ibabs", links_bestuur))] ;links_ibabs


storage <- vector()
i <- 1
for (i in seq_along(links_ibabs)) {
  links_gemeenteraad <- ifelse((links_ibabs[i] %>% 
                                  read_html() %>%
                                  html_nodes("h2") %>%
                                  html_text()) == "Raadsleden",
                               links_ibabs[i], "no"
                               
  )
  
  storage[[i]] <- links_gemeenteraad
  
  i <- i+1
  
}

link_gemeenteraad <- storage[storage != "no"]
gemeentes <- gemeentes %>% select(-c(use, use_external))

# USE GEMEENTERAAD IF RAAD EN GEMEENTERAAD ARE BOTH AVAILABLE ------------------

gemeentes$use <- ifelse(is.na(gemeentes$Gemeenteraad), gemeentes$Raad, gemeentes$Gemeenteraad)
gemeentes$use <- ifelse(is.na(gemeentes$Gemeenteraad), gemeentes$Raad, gemeentes$Gemeenteraad) # als gemeenteraad en raad en sitemap niet aanwezig, gebruik website

extern <- gemeentes %>%
  filter(!is.na(use))


x <- list( alpha = 1:5, beta = "Bravo", 
           gamma = list(a=1:3, b=NULL), 
           delta = c(TRUE, FALSE) )
json <- toJSON( x )
jason <- fromJSON( json )
str(json)

#named vectors are treated as JSON objects (lists)
toJSON(islands[1:4])


#data.frames must be converted into a list before converting into JSON
plot(cars, pch=2)
json_cars <- toJSON(as.list(cars))
points( data.frame( fromJSON( json_cars ) ), col="red", pch=3 )

#special R types are encoded as strings
testString <- c(1,2,3,4,NA,NaN,Inf,8,9);
toJSON(testString);


# scrape iBabs eerste versie
i <- 1
j <- 1
for (i in seq_along(gemeentes$ibabs_url)) {
  if (!is.na(gemeentes$ibabs_url[[i]])) {
    tbl <- list()
    naam <- character()
    all <- character()
    for (j in seq_along(gemeentes$ibabs_url[[i]])) {
      
      naam <- unique(gemeentes$ibabs_url[[i]][j] %>%
                       read_html() %>%
                       html_nodes("h2.profile-name") %>%
                       html_text())
      
      all <- gemeentes$ibabs_url[[i]][j] %>%
        read_html() %>%
        html_nodes("div.profile-details.col-sm-9") %>%
        html_text()
      
      tbl[[j]] <- c(naam, all)
      
      j <- j+1
    }
  }
  
  else {
    naam <- NA
    all <- NA
    
    tbl[[j]] <- c(naam, all)
    
  }
  
  i <- i+1
  
}
tbl_bind <- do.call(rbind, tbl)
ding <- as.data.frame(tbl_bind); View(ding)

## voorbeeld
# gemeentes$ibabs_info <- NA
# i <- 1
# j <- 1
# for (i in seq_along(gemeentes$ibabs_url[1:5])) {
#   tbl <- list()
#   naam <- character()
#   all <- character()
#   if (!is.na(gemeentes$ibabs_url[[i]])) {
#     for (j in seq_along(gemeentes$ibabs_url[[i]])) {
#       
#       gemeente <- as.character(gemeentes$Gemeente[i])
#       
#       naam <- unique(gemeentes$ibabs_url[[i]][j] %>%
#                        read_html() %>%
#                        html_nodes("h2.profile-name") %>%
#                        html_text())
#       
#       all <- gemeentes$ibabs_url[[i]][j] %>%
#         read_html() %>%
#         html_nodes("div.profile-details.col-sm-9") %>%
#         html_text()
#       
#       tbl[[j]] <- c(gemeente, naam, all)
#       
#       j <- j+1
#     }
#   }
#   
#   else {
#     gemeente[i] <- as.character(gemeentes$Gemeente[i])
#     naam[i] <- NA
#     all[i] <- NA
#     
#     tbl[[j]] <- c(gemeente, naam, all)
#     
#   }
#   gemeentes$ibabs_info[i] <- tbl
#   i <- i+1
#   
# }
# tbl_bind <- do.call(rbind, tbl)
# ding <- as.data.frame(tbl_bind); View(ding)
# 
# 
# if (!is.na(gemeentes$ibabs_url[[4]])) {
#   tbl <- list()
#   naam <- character()
#   all <- character()
#   for (j in seq_along(gemeentes$ibabs_url[[4]])) {
#     
#     gemeente <- as.character(gemeentes$Gemeente[4])
#     
#     naam <- unique(gemeentes$ibabs_url[[4]][j] %>%
#                      read_html() %>%
#                      html_nodes("h2.profile-name") %>%
#                      html_text())
#     
#     all <- gemeentes$ibabs_url[[4]][j] %>%
#       read_html() %>%
#       html_nodes("div.profile-details.col-sm-9") %>%
#       html_text()
#     
#     tbl[4][[j]] <- c(gemeente, naam, all)
#     
#     j <- j+1
#   }
#   
#   else {
#     naam <- NA
#     all <- NA
#     
#     tbl[[j]] <- c(naam, all)
#     
#   }
# }
# tbl_bind <- do.call(rbind, tbl)
# ding <- as.data.frame(tbl_bind); View(ding)




# JSON --------------------------------------------------------------------
library(rjson)
json <- toJSON(gemeentes)

# TO DO -------------------------------------------------------------------

# categorisieren welche gemeente welche Art website hat (--> gemeenteraad website finden)
# nog checken:
# ris https://ris.delft.nl/ DONE
# raadsinformatie https://lochem.raadsinformatie.nl/leden DONE
# gemeenteraad'gemeente'.nl (dus zonder punt) DONE
# ibabs
# ibabs nicht mit [1] selektieren, sondern mit == "Gemeenteraad" | "Raad" (sie line 200) DONE
# liste mit name und all DONE
# Hoofdfuncties toevoegen DONE

# how to detect update/change in URL 
# samenfuehren gemeentes extern en gemeentes die nicht ibabs und nicht notubiz sind --> in eine colummn DONE
# extern en extern_url vergelijken en naar errors kijken
# notubiz scrape schreiben, was kopieren von ibabs?


length(which(!is.na(gemeentes$extern)))/355*100
length(which(!is.na(gemeentes$ibabs)))/355*100
length(which(!is.na(gemeentes$notubiz)))/355*100

gemeentes_ibabs <- gemeentes %>%
  select(Gemeente, ibabs_info)
save(gemeentes,file="gemeentes")

json <- toJSON(gemeentes_ibabs)
write.csv2(json, "gemeentes_ibabs")

gemeentes$ibabs_url[355][[1]][1] %>%
  read_html() %>%
  html_nodes(xpath = "//div[contains(@class,'detail-row row')] and //") %>%
  html_text()

