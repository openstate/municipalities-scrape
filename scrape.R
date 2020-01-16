# SET UP ------------------------------------------------------------------

## LIBRARIES
library(rvest)
library(dplyr)
library(Rcrawler)
library(stringr)
library(rjson)
library(miceadds)
library(stringdist)

## SET WD
setwd("/Users/Nele/Desktop/Internship/R/")

## SET UTF
Sys.setlocale(locale="UTF-8")

## read in data set from wd
#gemeentes <- load.Rdata2("gemeentes", path=getwd())

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

# municipalities where website or name is not registered right
gemeentes$Gemeente <- as.character(gemeentes$Gemeente)
gemeentes$Gemeente[32] <- "Bergen L" # bergen has their name double in their, causing the loop making the name 'L', here it is manually set to the right name
gemeentes$Website[16] <- "https://www.amsterdam.nl/" # was https://www.amsterdam.nl/contact
gemeentes$Website[136] <- "https://www.h-i-ambacht.nl/" # was http://www.hendrik-ido-ambacht.nl
gemeentes$Website[65] <- "http://www.dantumadiel.frl" # was http://www.dantumadiel.fr
gemeentes$Website[68] <- "https://gemeente.derondevenen.nl/" # was http://www.derondevenen.nl
gemeentes$Website[16] <- "https://www.amsterdam.nl/" # was https://www.amsterdam.nl/contact

# detect domain change
for (i in seq_along(gemeentes$Website)) {
  session_url <- html_session(gemeentes$Website[[i]])$url
  if (session_url != gemeentes$Website[[i]]) {
    print(gemeentes$Website[[i]])
    print(session_url)
    gemeentes$Website[i] <- session_url
  }
}

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

for (i in seq_along(gemeentes$ibabs)) {
  if (!is.na(gemeentes$ibabs[i])) {
    session_url <- html_session(gemeentes$ibabs[[i]])$url
    if (session_url != gemeentes$ibabs[[i]]) {
      print(gemeentes$ibabs[[i]])
      print(session_url)
      gemeentes$ibabs[i] <- session_url
    } 
  }
}

# CHECK FOR EXTERNAL WEBSITE "notubiz" ------------------------------

gemeentes$notubiz <- str_replace(str_replace(gemeentes$Website, "www.", ""), ".nl", ".notubiz.nl/leden")
i <- 1
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
        }
        dates <- na.omit(dates)
        # compare those dates to the current yeaar to make sure to only get websites that are up to date
        for (l in seq_along(dates)) {
          if (grepl(substr(dates[[l]], 1, 4), substr(Sys.Date(), 1, 4)) |
              grepl(substr(dates[[l]], 1, 4), as.character((as.numeric(substr(Sys.Date(), 1, 4)) - 1)))) {
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


for (i in seq_along(gemeentes$notubiz)) {
  if (!is.na(gemeentes$notubiz[i])) {
    session_url <- html_session(gemeentes$notubiz[[i]])$url
    if (session_url != gemeentes$notubiz[[i]]) {
      print(gemeentes$notubiz[[i]])
      print(session_url)
      gemeentes$notubiz[i] <- session_url
    } 
  }
}

# CHECK FOR EXTERNAL WEBSITE "gementeraad." --------------------------------------

gemeentes$Gemeenteraad. <- str_replace(gemeentes$Website, "www.", "gemeenteraad.")

for (i in seq_along(gemeentes$Gemeenteraad.)) {
  
  gemeentes$Gemeenteraad.[[i]] <- ifelse((tryCatch({
    gemeentes$Gemeenteraad.[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Gemeenteraad.[[i]], NA)
  
}

# detect domain change
for (i in seq_along(gemeentes$Gemeenteraad.)) {
  if (!is.na(gemeentes$Gemeenteraad.[i])) {
    session_url <- html_session(gemeentes$Gemeenteraad.[[i]])$url
    if (session_url != gemeentes$Gemeenteraad.[[i]]) {
      print(gemeentes$Gemeenteraad.[[i]])
      print(session_url)
      gemeentes$Gemeenteraad.[i] <- session_url
    } 
  }
}

# CHECK FOR EXTERNAL WEBSITE "gemeenteraad" -------------------------

gemeentes$Gemeenteraad <- str_replace(gemeentes$Website, "www.", "gemeenteraad")

for (i in seq_along(gemeentes$Gemeenteraad)) {
  
  gemeentes$Gemeenteraad[[i]] <- ifelse((tryCatch({
    gemeentes$Gemeenteraad[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Gemeenteraad[[i]], NA)
  
}

for (i in seq_along(gemeentes$Gemeenteraad)) {
  if (!is.na(gemeentes$Gemeenteraad[i])) {
    session_url <- html_session(gemeentes$Gemeenteraad[[i]])$url
    if (session_url != gemeentes$Gemeenteraad[[i]]) {
      print(gemeentes$Gemeenteraad[[i]])
      print(session_url)
      gemeentes$Gemeenteraad[i] <- session_url
    } 
  }
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

for (i in seq_along(gemeentes$Raad)) {
  if (!is.na(gemeentes$Raad[i])) {
    session_url <- html_session(gemeentes$Raad[[i]])$url
    if (session_url != gemeentes$Raad[[i]]) {
      print(gemeentes$Raad[[i]])
      print(session_url)
      gemeentes$Raad[i] <- session_url
    } 
  }
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

for (i in seq_along(gemeentes$ris)) {
  if (!is.na(gemeentes$ris[i])) {
    session_url <- html_session(gemeentes$ris[[i]])$url
    if (session_url != gemeentes$ris[[i]]) {
      print(gemeentes$ris[[i]])
      print(session_url)
      gemeentes$ris[i] <- session_url
    } 
  }
}

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

for (i in seq_along(gemeentes$Bestuur)) {
  if (!is.na(gemeentes$Bestuur[i])) {
    session_url <- html_session(gemeentes$Bestuur[[i]])$url
    if (session_url != gemeentes$Bestuur[[i]]) {
      print(gemeentes$Bestuur[[i]])
      print(session_url)
      gemeentes$Bestuur[i] <- session_url
    } 
  }
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

for (i in seq_along(gemeentes$Raadsinformatie)) {
  if (!is.na(gemeentes$Raadsinformatie[i])) {
    session_url <- html_session(gemeentes$Raadsinformatie[[i]])$url
    if (session_url != gemeentes$Raadsinformatie[[i]]) {
      print(gemeentes$Raadsinformatie[[i]])
      print(session_url)
      gemeentes$Raadsinformatie[i] <- session_url
    } 
  }
}

gemeentes$Raadsinformatie[which(gemeentes$Raadsinformatie == "https://notubiz.nl/")] <- NA


gemeentes$Gemeenteraad. <- str_replace(gemeentes$Website, "www.", "gemeenteraad.")

for (i in seq_along(gemeentes$Gemeenteraad.)) {
  
  gemeentes$Gemeenteraad.[[i]] <- ifelse((tryCatch({
    gemeentes$Gemeenteraad.[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Gemeenteraad.[[i]], NA)
  
}

# detect domain change
for (i in seq_along(gemeentes$Gemeenteraad.)) {
  if (!is.na(gemeentes$Gemeenteraad.[i])) {
    session_url <- html_session(gemeentes$Gemeenteraad.[[i]])$url
    if (session_url != gemeentes$Gemeenteraad.[[i]]) {
      print(gemeentes$Gemeenteraad.[[i]])
      print(session_url)
      gemeentes$Gemeenteraad.[i] <- session_url
    } 
  }
}

# CHECK FOR EXTERNAL WEBSITE "gemeente." -------------------------

gemeentes$Gemeente. <- str_replace(gemeentes$Website, "www.", "gemeente.")

for (i in seq_along(gemeentes$Gemeente.)) {
  
  gemeentes$Gemeente.[[i]] <- ifelse((tryCatch({
    gemeentes$Gemeente.[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$Gemeente.[[i]], NA)
  
}

for (i in seq_along(gemeentes$Gemeente.)) {
  if (!is.na(gemeentes$Gemeente.[i])) {
    session_url <- html_session(gemeentes$Gemeente.[[i]])$url
    if (session_url != gemeentes$Gemeente.[[i]]) {
      print(gemeentes$Gemeente.[[i]])
      print(session_url)
      gemeentes$Gemeente.[i] <- session_url
    } 
  }
}

# CHECK FOR EXTERNAL WEBSITE "gemeentebestuur." -------------------------

gemeentes$gemeentebestuur. <- str_replace(gemeentes$Website, "www.", "gemeentebestuur.")

for (i in seq_along(gemeentes$gemeentebestuur.)) {
  
  gemeentes$gemeentebestuur.[[i]] <- ifelse((tryCatch({
    gemeentes$gemeentebestuur.[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$gemeentebestuur.[[i]], NA)
  
}

for (i in seq_along(gemeentes$gemeentebestuur.)) {
  if (!is.na(gemeentes$gemeentebestuur.[i])) {
    session_url <- html_session(gemeentes$gemeentebestuur.[[i]])$url
    if (session_url != gemeentes$gemeentebestuur.[[i]]) {
      print(gemeentes$gemeentebestuur.[[i]])
      print(session_url)
      gemeentes$gemeentebestuur.[i] <- session_url
    } 
  }
}


# CHECK FOR EXTERNAL WEBSITE „besluitvorming" -----------------------------

gemeentes$besluitvorming <- str_replace(gemeentes$Website, "www.", "besluitvorming.")

for (i in seq_along(gemeentes$besluitvorming)) {
  
  gemeentes$besluitvorming[[i]] <- ifelse((tryCatch({
    gemeentes$besluitvorming[[i]] %>%
      read_html()
    TRUE
  }, error=function(e) FALSE)),
  gemeentes$besluitvorming[[i]], NA)
  
}

for (i in seq_along(gemeentes$besluitvorming)) {
  if (!is.na(gemeentes$besluitvorming[i])) {
    session_url <- html_session(gemeentes$besluitvorming[[i]])$url
    if (session_url != gemeentes$besluitvorming[[i]]) {
      print(gemeentes$besluitvorming[[i]])
      print(session_url)
      gemeentes$besluitvorming[i] <- session_url
    } 
  }
}

# CHECK IF WEBSITE IS PART OF "GO" --------------------------------------

# gemeentes$go <- NA
# # for raads websites
# for (i in seq_along(gemeentes$Raad)){
#   if (!is.na(gemeentes$Raad[[i]])) {
#     gemeentes$go[[i]] <- ifelse(length(gemeentes$Raad[[i]] %>%
#                                          read_html() %>%
#                                          html_nodes("a.go") %>%
#                                          html_text()) != 0 |
#                                   length(gemeentes$Raad[[i]] %>%
#                                            read_html() %>%
#                                            html_nodes("span.go") %>%
#                                            html_text()) != 0 |
#                                   length(grepl("GemeenteOplossingen", (gemeentes$Raad[[i]] %>%
#                                                                          read_html() %>%
#                                                                          html_nodes("a.right") %>%
#                                                                          html_text()))) > 0,
#                                 gemeentes$Raad[[i]], NA)
#   }
# }
# length(which(!is.na(gemeentes$go)))
# 
# # for gemeenteraad websites
# for (i in seq_along(gemeentes$Gemeenteraad)){
#   if (is.na(gemeentes$go[[i]]) & !is.na(gemeentes$Gemeenteraad[[i]])) {
#     gemeentes$go[[i]] <- ifelse(length(gemeentes$Gemeenteraad[[i]] %>%
#                                          read_html() %>%
#                                          html_nodes("a.go") %>%
#                                          html_text()) != 0 |
#                                   length(gemeentes$Gemeenteraad[[i]] %>%
#                                            read_html() %>%
#                                            html_nodes("span.go") %>%
#                                            html_text()) != 0 |
#                                   length(grepl("GemeenteOplossingen", (gemeentes$Gemeenteraad[[i]] %>%
#                                                                          read_html() %>%
#                                                                          html_nodes("a.right") %>%
#                                                                          html_text()))) > 0,
#                                 gemeentes$Gemeenteraad[[i]], NA)
#   }
# }
# 
# # for bestuur websites
# for (i in seq_along(gemeentes$Bestuur)){
#   if (is.na(gemeentes$go[[i]]) & !is.na(gemeentes$Bestuur[[i]])) {
#     gemeentes$go[[i]] <- ifelse(length(gemeentes$Bestuur[[i]] %>%
#                                          read_html() %>%
#                                          html_nodes("a.go") %>%
#                                          html_text()) != 0 |
#                                   length(gemeentes$Bestuur[[i]] %>%
#                                            read_html() %>%
#                                            html_nodes("span.go") %>%
#                                            html_text()) != 0 |
#                                   length(grepl("GemeenteOplossingen", (gemeentes$Bestuur[[i]] %>%
#                                                                          read_html() %>%
#                                                                          html_nodes("a.right") %>%
#                                                                          html_text()))) > 0,
#                                 gemeentes$Bestuur[[i]], NA)
#   }
# }
# 
# # for regular gemeente websites
# for (i in seq_along(gemeentes$Website)) {
#   if (is.na(gemeentes$go[[i]])) {
#     gemeentes$go[[i]] <- ifelse(length(gemeentes$Website[[i]] %>%
#                                          read_html() %>%
#                                          html_nodes("a.go") %>%
#                                          html_text()) != 0 |
#                                   length(gemeentes$Website[[i]] %>%
#                                            read_html() %>%
#                                            html_nodes("span.go") %>%
#                                            html_text()) != 0 |
#                                   length(grepl("GemeenteOplossingen", (gemeentes$Website[[i]] %>%
#                                                                          read_html() %>%
#                                                                          html_nodes("a.right") %>%
#                                                                          html_text()))) > 0,
#                                 gemeentes$Website[[i]], NA)
#   }
# }



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

#length(which(gemeentes$ibabs_url == "https://ris2.ibabs.eu"))
gemeentes$ibabs_url[which(gemeentes$ibabs_url == "https://ris2.ibabs.eu")] <- NA

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



# SCRAPE WEBSITES USING NOTUBIZ -------------------------------------------

# municipality member websites of municipality council
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

gemeentes$notubiz_info <- NA
for (i in seq_along(gemeentes$notubiz_url)) {
  if (!is.na(gemeentes$notubiz_url[i])) {
    naam <- character(0)
    telefoonnummer <- character(0)
    functie <- character(0)
    email <- character(0)
    nevenactiviteiten <- character(0)
    for (j in seq_along(gemeentes$notubiz_url[[i]])) {
      naam[[j]] <- (unique(gemeentes$notubiz_url[[i]][j] %>%
                                    read_html() %>%
                                    html_nodes("dd.volledige_naam") %>%
                                    html_text()))
      
      telefoonnummer[[j]] <- ifelse(length((unique(gemeentes$notubiz_url[[i]][j] %>%
                                              read_html() %>%
                                              html_nodes("dd.telefoonnummer") %>%
                                              html_text()))) == 0,
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
    gemeentes$notubiz_info[[i]] <- list(0)
    gemeentes$notubiz_info[[i]] <- list(naam = naam, telefoonnummer = telefoonnummer, functie = functie, email = email, nevenactiviteiten = nevenactiviteiten)
    i <- i+1
  } else {
    gemeentes$ibabs_info[[i]] <- NA
  }
}



# bekostigd onbekostigd


# SCRAPE WEBSITES USING AN EXTERNAL WEBSITE OR REGULAR WEBSITE ------------

## LOOP TROUGH ALL POSSIBLE SITES TO GET URL OF GEMEENTERAAD

# REGULAR GEMEENTE
i <- 1
t <- 1
z <- 1
c <- 1
l <- 1

remove(comsub)
rm(find.string())
remove(gsub())
remove(lcPrefix())
remove(reps())

gemeentes$url_regular <- 0
for (i in seq_along(gemeentes$Website)) {
  if (is.na(gemeentes$ibabs[i]) & is.na(gemeentes$notubiz[i]) & !is.na(gemeentes$url_regular[i])) {
    if (!is.na(gemeentes$Website[i]) & gemeentes$url_regular[i] == 0) {
      if (tryCatch({
        LinkExtractor(gemeentes$Website[i])
        TRUE
      }, error=function(e) FALSE)) {
        
        links_internal <- LinkExtractor(gemeentes$Website[i])$InternalLinks
        links_external <- LinkExtractor(gemeentes$Website[i])$ExternalLinks
        links <- c(links_internal, links_external)
        links_sub <- sub(".*nl", "", links)
        links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                         organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                         raadslid|Raadslid|gemeente|councilperiod", links_sub) 
                                   & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                            bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                            Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                            notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                            brieven|actueel|gast|financien", links_sub))]
        links2 <- list()
        links_sub2 <- list()
        links_grepl2 <- list()
        for (t in seq_along(links_grepl)) {
          if (length(LinkExtractor(links_grepl[t])$InternalLinks) != 0) {
            links2[[t]] <- LinkExtractor(links_grepl[t])$InternalLinks
            links_sub2[[t]] <- gsub(".*nl", "", links2[[t]])
          } else {
            links_grepl2[[t]] <- NA
          }
        }
        links2 <- unlist(links2)
        links_sub2 <- unlist(links_sub2)
        links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                         organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente|councilperiod", 
                                           links_sub2) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                            bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                                brieven|actueel|gast|financien", links_sub2))]
        links_grepl2 <- unique(unlist(links_grepl2))
        
        links3 <- list()
        links_sub3 <- list()
        links_grepl3 <- list()
        for (z in seq_along(links_grepl2)) {
          if (tryCatch({
            LinkExtractor(links_grepl2[z])$InternalLinks
            TRUE
          }, error=function(e) FALSE)) {
            if (length(LinkExtractor(links_grepl2[z])$InternalLinks) != 0) {
              links3[[z]] <- LinkExtractor(links_grepl2[z])$InternalLinks
              links_sub3[[z]] <- gsub(".*nl", "", links3[[z]])
            }
          } else {
            links_sub3[[z]] <- NA
          }
        }
        links3 <- unlist(links3)
        links_sub3 <- unlist(links_sub3)
        links_grepl3 <- links3[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                           organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente|councilperiod",
                                           links_sub3) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                                brieven|actueel|gast|financien", links_sub3))]
        links_grepl3 <- unique(unlist(links_grepl3))
        
        # links4 <- list()
        # links_sub4 <- list()
        # links_grepl4 <- list()
        # for (z in seq_along(links_grepl3)) {
        #   if (tryCatch({
        #     LinkExtractor(links_grepl3[z])$InternalLinks
        #     TRUE
        #   }, error=function(e) FALSE)) {
        #     if (length(LinkExtractor(links_grepl3[z])$InternalLinks) != 0) {
        #       links4[[z]] <- LinkExtractor(links_grepl3[z])$InternalLinks
        #       links_sub4[[z]] <- gsub(".*nl", "", links4[[z]])
        #     }
        #   } else {
        #     links_sub4[[z]] <- NA
        #   }
        # }
        # links4 <- unlist(links4)
        # links_sub4 <- unlist(links_sub4)
        # links_grepl4 <- links4[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
        #                                    organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
        #                                    raadslid|Raadslid|gemeente",
        #                                    links_sub4) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
        #                                                         bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
        #                                                         Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
        #                                                         notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
        #                                                         brieven|actueel|gast", links_sub4))]
        # links_grepl4 <- unique(unlist(links_grepl4))
        # 
        links_merge <- unique(c(links_grepl, links_grepl2, links_grepl3#, links_grepl4
        ))
        links_merge <- links_merge[which(!grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                brieven|actueel|gast|financien", links_merge))]
        
        for (c in seq_along(links_merge)) {
          if (tryCatch({
            links_merge[c] %>%
              read_html()
            FALSE
          }, error=function(e) TRUE)) {
            links_merge[c] <- NA
          } 
        }
        links_merge <- na.omit(links_merge)
        
        samenstelling_gemeenteraad <- links_merge[which(grepl("samenstelling-gemeenteraad", links_merge))]
        gemeenteraad_samenstelling <- links_merge[which(grepl("gemeenteraad-samenstelling", links_merge))]
        samenstelling_raad <- links_merge[which(grepl("samenstelling-raad", links_merge))]
        Samenstelling_gemeenteraad <- links_merge[which(grepl("Samenstelling_gemeenteraad|
                                                              samenstelling_gemeenteraad|
                                                              samenstelling-van-de-gemeenteraad", links_merge))]
        wie <- links_merge[which(grepl("Wie_zitten_er_in_de_gemeenteraad|wie_zitten_er_in_de_gemeenteraad|
                                       wie-zitten-er-in-de-gemeenteraad|Wie-zitten-er-in-de-gemeenteraad|
                                       wie-zit-er-in-de-raad|Wie-zit-er-in-de-raad ", links_merge))]
        
        gemeentes$url_regular[i] <- ifelse(length(samenstelling_gemeenteraad) > 0, samenstelling_gemeenteraad,
                                           ifelse(length(gemeenteraad_samenstelling) > 0, gemeenteraad_samenstelling, 
                                                  ifelse(length(samenstelling_raad) > 0, samenstelling_raad,
                                                         ifelse(length(Samenstelling_gemeenteraad) > 0, Samenstelling_gemeenteraad, 
                                                                ifelse(length(wie) > 0, wie, NA)))))
        
        if (length(links_merge) > 0 & is.na(gemeentes$url_regular[i]) & tryCatch({
          for (l in seq_along(links_merge)) {
            links_merge[l] %>%
              read_html()
          }
          TRUE
        }, error=function(e) FALSE)) {
          lengths <- NA
          for (j in seq_along(links_merge)) {
            lengths[j] <- length(links_merge[j] %>%
                                   read_html() %>%
                                   html_nodes("a") %>%
                                   html_attr("href"))
          }
          gemeentes$url_regular[i] <- links_merge[which.max(lengths)] 
        }
      } else {
        gemeentes$url_regular[i] <- NA
      }
    } else {
      gemeentes$url_regular[i] <- NA
    }
  } else {
    gemeentes$url_regular[i] <- NA
  }
}

gemeentes$url_regular[346] <- "https://www.gemeenteraadzundert.nl/contactgegevens-raadsleden/"

#|financie

if (is.na(gemeentes$ibabs[17]) & is.na(gemeentes$notubiz[17]) & is.na(gemeentes$url_regular[17]) & is.na(gemeentes$url_regular[17])) {
  if (!is.na(gemeentes$Website[17])) {
    if (tryCatch({
      LinkExtractor(gemeentes$Website[17])
      TRUE
    }, error=function(e) FALSE)) {
      
      links_internal <- LinkExtractor(gemeentes$Website[17])$InternalLinks
      links_external <- LinkExtractor(gemeentes$Website[17])$ExternalLinks
      links <- c(links_internal, links_external)
      links_sub <- sub(".*nl", "", links)
      links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                       organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                       raadslid|Raadslid|gemeente", links_sub) 
                                 & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                          bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                          Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                          notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                          brieven|actueel|gast|financien", links_sub))]
      links2 <- list()
      links_sub2 <- list()
      links_grepl2 <- list()
      for (t in seq_along(links_grepl)) {
        if (length(LinkExtractor(links_grepl[t])$InternalLinks) != 0) {
          links2[[t]] <- LinkExtractor(links_grepl[t])$InternalLinks
          links_sub2[[t]] <- gsub(".*nl", "", links2[[t]])
        } else {
          links_grepl2[[t]] <- NA
        }
      }
      links2 <- unlist(links2)
      links_sub2 <- unlist(links_sub2)
      links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                         organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                         raadslid|Raadslid|gemeente|councilperiod", 
                                         links_sub2) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                              bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                              Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                              notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                              brieven|actueel|gast|financien", links_sub2))]
      links_grepl2 <- unique(unlist(links_grepl2))
      
      links3 <- list()
      links_sub3 <- list()
      links_grepl3 <- list()
      for (z in seq_along(links_grepl2)) {
        if (tryCatch({
          LinkExtractor(links_grepl2[z])$InternalLinks
          TRUE
        }, error=function(e) FALSE)) {
          if (length(LinkExtractor(links_grepl2[z])$InternalLinks) != 0) {
            links3[[z]] <- LinkExtractor(links_grepl2[z])$InternalLinks
            links_sub3[[z]] <- gsub(".*nl", "", links3[[z]])
          }
        } else {
          links_sub3[[z]] <- NA
        }
      }
      links3 <- unlist(links3)
      links_sub3 <- unlist(links_sub3)
      links_grepl3 <- links3[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                         organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                         raadslid|Raadslid|gemeente|councilperiod",
                                         links_sub3) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                              bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                              Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                              notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                              brieven|actueel|gast|financien", links_sub3))]
      links_merge <- unique(unlist(links_grepl3))
      
      # links4 <- list()
      # links_sub4 <- list()
      # links_grepl4 <- list()
      # for (z in seq_along(links_grepl3)) {
      #   if (tryCatch({
      #     LinkExtractor(links_grepl3[z])$InternalLinks
      #     TRUE
      #   }, error=function(e) FALSE)) {
      #     if (length(LinkExtractor(links_grepl3[z])$InternalLinks) != 0) {
      #       links4[[z]] <- LinkExtractor(links_grepl3[z])$InternalLinks
      #       links_sub4[[z]] <- gsub(".*nl", "", links4[[z]])
      #     }
      #   } else {
      #     links_sub4[[z]] <- NA
      #   }
      # }
      # links4 <- unlist(links4)
      # links_sub4 <- unlist(links_sub4)
      # links_grepl4 <- links4[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
      #                                    organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
      #                                    raadslid|Raadslid|gemeente",
      #                                    links_sub4) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
      #                                                         bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
      #                                                         Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
      #                                                         notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
      #                                                         brieven|actueel|gast", links_sub4))]
      # links_grepl4 <- unique(unlist(links_grepl4))
      # 
      # links_merge <- unique(c(links_grepl, links_grepl2, links_grepl3, links_grepl4))
      links_merge <- links_merge[which(!grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                              bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                              Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                              notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                              brieven|actueel|gast|financien", links_merge))]
      
      for (c in seq_along(links_merge)) {
        if (tryCatch({
          links_merge[c] %>%
            read_html()
          FALSE
        }, error=function(e) TRUE)) {
          links_merge[c] <- NA
        } 
      }
      links_merge <- na.omit(links_merge)
      
      samenstelling_gemeenteraad <- links_merge[which(grepl("samenstelling-gemeenteraad", links_merge))]
      gemeenteraad_samenstelling <- links_merge[which(grepl("gemeenteraad-samenstelling", links_merge))]
      samenstelling_raad <- links_merge[which(grepl("samenstelling-raad", links_merge))]
      Samenstelling_gemeenteraad <- links_merge[which(grepl("Samenstelling_gemeenteraad|
                                                            samenstelling_gemeenteraad|
                                                            samenstelling-van-de-gemeenteraad", links_merge))]
      wie <- links_merge[which(grepl("Wie_zitten_er_in_de_gemeenteraad|wie_zitten_er_in_de_gemeenteraad|
                                     wie-zitten-er-in-de-gemeenteraad|Wie-zitten-er-in-de-gemeenteraad|
                                     wie-zit-er-in-de-raad|Wie-zit-er-in-de-raad ", links_merge))]
      
      gemeentes$url_regular[17] <- ifelse(length(samenstelling_gemeenteraad) > 0, samenstelling_gemeenteraad,
                                         ifelse(length(gemeenteraad_samenstelling) > 0, gemeenteraad_samenstelling, 
                                                ifelse(length(samenstelling_raad) > 0, samenstelling_raad,
                                                       ifelse(length(Samenstelling_gemeenteraad) > 0, Samenstelling_gemeenteraad, 
                                                              ifelse(length(wie) > 0, wie, NA)))))
      
      if (length(links_merge) > 0 & is.na(gemeentes$url_regular[17]) & tryCatch({
        for (l in seq_along(links_merge)) {
          links_merge[l] %>%
            read_html()
        }
        TRUE
      }, error=function(e) FALSE)) {
        lengths <- NA
        for (j in seq_along(links_merge)) {
          lengths[j] <- length(links_merge[j] %>%
                                 read_html() %>%
                                 html_nodes("a") %>%
                                 html_attr("href"))
        }
        gemeentes$url_regular[17] <- links_merge[which.max(lengths)] 
      }
    } else {
      gemeentes$url_regular[17] <- NA
    }
  } else {
    gemeentes$url_regular[17] <- NA
  }
  } else {
    gemeentes$url_regular[17] <- NA
  }



# GEMEENTERAAD.
gemeentes$url_gemeenteraad. <- 0
for (i in seq_along(gemeentes$`Gemeenteraad.`)) {
  if (is.na(gemeentes$ibabs[i]) & is.na(gemeentes$notubiz[i])) {
    if (!is.na(gemeentes$Gemeenteraad.[i])) {
      if (tryCatch({
        LinkExtractor(gemeentes$Gemeenteraad.[i])
        TRUE
      }, error=function(e) FALSE)) {
        
        links_internal <- LinkExtractor(gemeentes$Gemeenteraad.[i])$InternalLinks
        links_external <- LinkExtractor(gemeentes$Gemeenteraad.[i])$ExternalLinks
        links <- c(links_internal, links_external)
        links_sub <- sub(".*nl", "", links)
        links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                         Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                         raadslid|Raadslid|gemeente", links_sub) & 
                                  !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                         bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                         Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                         notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                         brieven|actueel|gast", links_sub))]
        links2 <- list()
        links_sub2 <- list()
        links_grepl2 <- list()
        for (t in seq_along(links_grepl)) {
          if (length(LinkExtractor(links_grepl[t])$InternalLinks) != 0) {
            links2[[t]] <- LinkExtractor(links_grepl[t])$InternalLinks
            links_sub2[[t]] <- gsub(".*nl", "", links2[[t]])
          } else {
            links_grepl2[[t]] <- NA
          }
        }
        links2 <- unlist(links2)
        links_sub2 <- unlist(links_sub2)
        links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                          Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                          raadslid|Raadslid|gemeente", links_sub2) & 
                                    !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                           bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                           Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                           notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                           brieven|actueel|gast", links_sub2))]
        links_grepl2 <- unique(unlist(links_grepl2))
        
        links3 <- list()
        links_sub3 <- list()
        links_grepl3 <- list()
        for (z in seq_along(links_grepl2)) {
          if (tryCatch({
            LinkExtractor(links_grepl2[z])$InternalLinks
            TRUE
          }, error=function(e) FALSE)) {
            if (length(LinkExtractor(links_grepl2[z])$InternalLinks) != 0) {
              links3[[z]] <- LinkExtractor(links_grepl2[z])$InternalLinks
              links_sub3[[z]] <- gsub(".*nl", "", links3[[z]])
            }
          } else {
            links_sub3[[z]] <- NA
          }
        }
        links3 <- unlist(links3)
        links_sub3 <- unlist(links_sub3)
        links_grepl3 <- links3[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                           organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente",
                                           links_sub3) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                                brieven|actueel|gast", links_sub3))]
        links_grepl3 <- unique(unlist(links_grepl3))
        
        links4 <- list()
        links_sub4 <- list()
        links_grepl4 <- list()
        for (z in seq_along(links_grepl3)) {
          if (tryCatch({
            LinkExtractor(links_grepl3[z])$InternalLinks
            TRUE
          }, error=function(e) FALSE)) {
            if (length(LinkExtractor(links_grepl3[z])$InternalLinks) != 0) {
              links4[[z]] <- LinkExtractor(links_grepl3[z])$InternalLinks
              links_sub4[[z]] <- gsub(".*nl", "", links4[[z]])
            }
          } else {
            links_sub4[[z]] <- NA
          }
        }
        links4 <- unlist(links4)
        links_sub4 <- unlist(links_sub4)
        links_grepl4 <- links4[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                           organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente",
                                           links_sub4) & 
                                    !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                           bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                           Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                           notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                           brieven|actueel|gast", links_sub4))]
        links_grepl4 <- unique(unlist(links_grepl4))
        
        links_merge <- unique(c(links_grepl, links_grepl2, links_grepl3, links_grepl4))
        links_merge <- links_merge[which(!grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                brieven|actueel|gast", links_merge))]
        
        samenstelling_gemeenteraad <- links_merge[which(grepl("samenstelling-gemeenteraad", links_merge))]
        gemeenteraad_samenstelling <- links_merge[which(grepl("gemeenteraad-samenstelling", links_merge))]
        samenstelling_raad <- links_merge[which(grepl("samenstelling-raad", links_merge))]
        Samenstelling_gemeenteraad <- links_merge[which(grepl("Samenstelling_gemeenteraad|samenstelling_gemeenteraad|
                                                              samenstelling-van-de-gemeenteraad", links_merge))]
        wie <- links_merge[which(grepl("Wie_zitten_er_in_de_gemeenteraad|wie_zitten_er_in_de_gemeenteraad|
                                       wie-zitten-er-in-de-gemeenteraad|Wie-zitten-er-in-de-gemeenteraad|
                                       wie-zit-er-in-de-raad|Wie-zit-er-in-de-raad ", links_merge))]
        
        gemeentes$url_gemeenteraad.[i] <- ifelse(length(samenstelling_gemeenteraad) > 0, samenstelling_gemeenteraad,
                                   ifelse(length(gemeenteraad_samenstelling) > 0, gemeenteraad_samenstelling, 
                                          ifelse(length(samenstelling_raad) > 0, samenstelling_raad,
                                                 ifelse(length(Samenstelling_gemeenteraad) > 0, Samenstelling_gemeenteraad, 
                                                        ifelse(length(wie) > 0, wie, NA)))))
        
        if (length(links_merge) > 0 & is.na(gemeentes$url_gemeenteraad.[i]) & tryCatch({
          for (l in seq_along(links_merge)) {
            links_merge[l] %>%
              read_html()
          }
          TRUE
        }, error=function(e) FALSE)) {
          lengths <- NA
          for (j in seq_along(links_merge)) {
            lengths[j] <- length(links_merge[j] %>%
                                   read_html() %>%
                                   html_nodes("a") %>%
                                   html_attr("href"))
          }
          gemeentes$url_gemeenteraad.[i] <- links_merge[which.max(lengths)] 
        }
      } else {
        gemeentes$url_gemeenteraad.[i] <- NA
      }
    } else {
      gemeentes$url_gemeenteraad.[i] <- NA
    }
  } else {
    gemeentes$url_gemeenteraad.[i] <- NA
  }
}


# GEMEENTERAAD
gemeentes$url_gemeenteraad <- 0
for (i in seq_along(gemeentes$Gemeenteraad)) {
  if (is.na(gemeentes$ibabs[i]) & is.na(gemeentes$notubiz[i])) {
    if (!is.na(gemeentes$Gemeenteraad[i])) {
      if (tryCatch({
        LinkExtractor(gemeentes$Gemeenteraad[i])
        TRUE
      }, error=function(e) FALSE)) {
        
        links_internal <- LinkExtractor(gemeentes$Gemeenteraad[i])$InternalLinks
        links_external <- LinkExtractor(gemeentes$Gemeenteraad[i])$ExternalLinks
        links <- c(links_internal, links_external)
        links_sub <- sub(".*nl", "", links)
        links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                         Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                         raadslid|Raadslid|gemeente", links_sub) & 
                                     !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                            bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                            Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                            notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                            brieven|actueel|gast", links_sub))]
        links2 <- list()
        links_sub2 <- list()
        links_grepl2 <- list()
        for (t in seq_along(links_grepl)) {
          if (length(LinkExtractor(links_grepl[t])$InternalLinks) != 0) {
            links2[[t]] <- LinkExtractor(links_grepl[t])$InternalLinks
            links_sub2[[t]] <- gsub(".*nl", "", links2[[t]])
          } else {
            links_grepl2[[t]] <- NA
          }
        }
        links2 <- unlist(links2)
        links_sub2 <- unlist(links_sub2)
        links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                           Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente", 
                                           links_sub2) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                                brieven|actueel|gast", links_sub2))]
        links_grepl2 <- unique(unlist(links_grepl2))
        
        links3 <- list()
        links_sub3 <- list()
        links_grepl3 <- list()
        for (z in seq_along(links_grepl2)) {
          if (tryCatch({
            LinkExtractor(links_grepl2[z])$InternalLinks
            TRUE
          }, error=function(e) FALSE)) {
            if (length(LinkExtractor(links_grepl2[z])$InternalLinks) != 0) {
              links3[[z]] <- LinkExtractor(links_grepl2[z])$InternalLinks
              links_sub3[[z]] <- gsub(".*nl", "", links3[[z]])
            }
          } else {
            links_sub3[[z]] <- NA
          }
        }
        links3 <- unlist(links3)
        links_sub3 <- unlist(links_sub3)
        links_grepl3 <- links3[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                           organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente",
                                           links_sub3) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                                brieven|actueel|gast", links_sub3))]
        links_grepl3 <- unique(unlist(links_grepl3))
        
        links4 <- list()
        links_sub4 <- list()
        links_grepl4 <- list()
        for (z in seq_along(links_grepl3)) {
          if (tryCatch({
            LinkExtractor(links_grepl3[z])$InternalLinks
            TRUE
          }, error=function(e) FALSE)) {
            if (length(LinkExtractor(links_grepl3[z])$InternalLinks) != 0) {
              links4[[z]] <- LinkExtractor(links_grepl3[z])$InternalLinks
              links_sub4[[z]] <- gsub(".*nl", "", links4[[z]])
            }
          } else {
            links_sub4[[z]] <- NA
          }
        }
        links4 <- unlist(links4)
        links_sub4 <- unlist(links_sub4)
        links_grepl4 <- links4[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                           organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente",
                                           links_sub4) & 
                                       !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                              bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                              Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                              notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                              brieven|actueel|gast", links_sub4))]
        links_grepl4 <- unique(unlist(links_grepl4))
        
        links_merge <- unique(c(links_grepl, links_grepl2, links_grepl3, links_grepl4))
        links_merge <- links_merge[which(!grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                brieven|actueel|gast", links_merge))]
        
        for (g in seq_along(links_merge)) {
          if (str_count(links_merge[g], ".html") > 1) {
            string <- str_split(links_merge[g], "/")[[1]][(which(grepl(".html", str_split(links_merge[g], "/")[[1]]))[1])]
            links_merge[g] <- gsub(paste0("/", string), "", links_merge[g])
          }
        }
        
        teller <- 1
        delete <- numeric()
        for (q in seq_along(links_merge)) {
          if (tryCatch({
            links_merge[q] %>%
              read_html()
            FALSE
          }, error=function(e) TRUE)) {
            delete[teller] <- q
            teller <- teller + 1
          }
        }
        if (length(delete) > 0) {         
          links_merge <- links_merge[-delete]        
          }
        
        samenstelling_gemeenteraad <- links_merge[which(grepl("samenstelling-gemeenteraad", links_merge))]
        gemeenteraad_samenstelling <- links_merge[which(grepl("gemeenteraad-samenstelling", links_merge))]
        samenstelling_raad <- links_merge[which(grepl("samenstelling-raad", links_merge))]
        Samenstelling_gemeenteraad <- links_merge[which(grepl("Samenstelling_gemeenteraad|samenstelling_gemeenteraad|samenstelling-van-de-gemeenteraad", links_merge))]
        wie <- links_merge[which(grepl("Wie_zitten_er_in_de_gemeenteraad|wie_zitten_er_in_de_gemeenteraad|wie-zitten-er-in-de-gemeenteraad|
                                     Wie-zitten-er-in-de-gemeenteraad|Wie-zit-er-in-de-raad", links_merge))]
        
        gemeentes$url_gemeenteraad[i] <- ifelse(length(samenstelling_gemeenteraad) > 0, samenstelling_gemeenteraad,
                                   ifelse(length(gemeenteraad_samenstelling) > 0, gemeenteraad_samenstelling, 
                                          ifelse(length(samenstelling_raad) > 0, samenstelling_raad,
                                                 ifelse(length(Samenstelling_gemeenteraad) > 0, Samenstelling_gemeenteraad, 
                                                        ifelse(length(wie) > 0, wie, NA)))))
        
        
        if (length(links_merge) > 0 & is.na(gemeentes$url_gemeenteraad[i])) {
          lengths <- NA
          for (j in seq_along(links_merge)) {
            lengths[j] <- length(links_merge[j] %>%
                                   read_html() %>%
                                   html_nodes("a") %>%
                                   html_attr("href"))
          }
          gemeentes$url_gemeenteraad[i] <- links_merge[which.max(lengths)] 
        }
      } else {
        gemeentes$url_gemeenteraad[i] <-  NA
      } 
    } else {
      gemeentes$url_gemeenteraad[i] <-  NA
    }
  } else {
    gemeentes$url_gemeenteraad[i] <-  NA
  }
}


# RAAD
gemeentes$url_raad <- 0
for (i in seq_along(gemeentes$Raad)) {
  if (is.na(gemeentes$ibabs[i]) & is.na(gemeentes$notubiz[i])) {
    if (!is.na(gemeentes$Raad[i])) {
      if (tryCatch({
        LinkExtractor(gemeentes$Raad[i])
        TRUE
      }, error=function(e) FALSE)) {
        
        links_internal <- LinkExtractor(gemeentes$Raad[i])$InternalLinks
        links_external <- LinkExtractor(gemeentes$Raad[i])$ExternalLinks
        links <- c(links_internal, links_external)
        links_sub <- sub(".*nl", "", links)
        links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                         Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                         raadslid|Raadslid|gemeente", links_sub) & 
                                     !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                            bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                            Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                            notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                            brieven|actueel", links_sub))]
        links2 <- list()
        links_sub2 <- list()
        links_grepl2 <- list()
        for (t in seq_along(links_grepl)) {
          if (length(LinkExtractor(links_grepl[t])$InternalLinks) != 0) {
            links2[[t]] <- LinkExtractor(links_grepl[t])$InternalLinks
            links_sub2[[t]] <- gsub(".*nl", "", links2[[t]])
          } else {
            links_grepl2[[t]] <- NA
          }
        }
        links2 <- unlist(links2)
        links_sub2 <- unlist(links_sub2)
        links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                           Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente", 
                                           links_sub2) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                                brieven|actueel", links_sub2))]
        links_grepl2 <- unique(unlist(links_grepl2))
        
        links_merge <- unique(c(links_grepl, links_grepl2))
        links_merge <- links_merge[which(!grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                bekendmakingen|whatsapp|tel:|mailto:", links_merge))]
        years <- as.character(c(2000:(as.numeric(substr(Sys.Date(), 1, 4))-1)))
        years <- paste(years,collapse = '|')
        links_merge <- links_merge[which(!grepl(years, links_merge))]
        
        for (g in seq_along(links_merge)) {
          if (str_count(links_merge[g], ".html") > 1) {
            string <- str_split(links_merge[g], "/")[[1]][(which(grepl(".html", str_split(links_merge[g], "/")[[1]]))[1])]
            links_merge[g] <- gsub(paste0("/", string), "", links_merge[g])
          }
        }
        
        teller <- 1
        delete <- numeric()
        for (q in seq_along(links_merge)) {
          if (tryCatch({
            links_merge[q] %>%
              read_html()
            FALSE
          }, error=function(e) TRUE)) {
            delete[teller] <- q
            teller <- teller + 1
          }
        }
        
        if (length(delete) > 0) {
          links_merge <- links_merge[-delete] 
        }
        
        samenstelling_gemeenteraad <- links_merge[which(grepl("samenstelling-gemeenteraad", links_merge))]
        gemeenteraad_samenstelling <- links_merge[which(grepl("gemeenteraad-samenstelling", links_merge))]
        samenstelling_raad <- links_merge[which(grepl("samenstelling-raad", links_merge))]
        Samenstelling_gemeenteraad <- links_merge[which(grepl("Samenstelling_gemeenteraad|samenstelling_gemeenteraad|samenstelling-van-de-gemeenteraad", links_merge))]
        wie <- links_merge[which(grepl("Wie_zitten_er_in_de_gemeenteraad|wie_zitten_er_in_de_gemeenteraad|wie-zitten-er-in-de-gemeenteraad|
                                     Wie-zitten-er-in-de-gemeenteraad|wie-zit-er-in-de-raad|Wie-zit-er-in-de-raad", links_merge))]
        
        gemeentes$url_raad[i] <- ifelse(length(samenstelling_gemeenteraad) > 0, samenstelling_gemeenteraad,
                                   ifelse(length(gemeenteraad_samenstelling) > 0, gemeenteraad_samenstelling, 
                                          ifelse(length(samenstelling_raad) > 0, samenstelling_raad,
                                                 ifelse(length(Samenstelling_gemeenteraad) > 0, Samenstelling_gemeenteraad, 
                                                        ifelse(length(wie) > 0, wie, NA)))))
        
        
        if (length(links_merge) > 0 & is.na(gemeentes$url_raad[i])) {
          lengths <- NA
          for (j in seq_along(links_merge)) {
            lengths[j] <- length(links_merge[j] %>%
                                   read_html() %>%
                                   html_nodes("a") %>%
                                   html_attr("href"))
          }
          gemeentes$url_raad[i] <- links_merge[which.max(lengths)] 
        }
      } else {
        gemeentes$url_raad[i] <- NA
      }
    } else {
      gemeentes$url_raad[i] <- NA
    }
  } else {
    gemeentes$url_raad[i] <- NA
  }
}


# RIS
gemeentes$url_ris <- 0
for (i in seq_along(gemeentes$ris)) {
  if (is.na(gemeentes$ibabs[i]) & is.na(gemeentes$notubiz[i])) {
    if (!is.na(gemeentes$ris[i])) {
      if (tryCatch({
        LinkExtractor(gemeentes$ris[i])
        TRUE
      }, error=function(e) FALSE)) {
        
        links_internal <- LinkExtractor(gemeentes$ris[i])$InternalLinks
        links_external <- LinkExtractor(gemeentes$ris[i])$ExternalLinks
        links <- c(links_internal, links_external)
        links_sub <- sub(".*nl", "", links)
        links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                         Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                         raadslid|Raadslid|gemeente", links_sub) & 
                                     !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                            bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                            Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                            notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                            brieven|actueel", links_sub))]
        links2 <- list()
        links_sub2 <- list()
        links_grepl2 <- list()
        for (t in seq_along(links_grepl)) {
          if (length(LinkExtractor(links_grepl[t])$InternalLinks) != 0) {
            links2[[t]] <- LinkExtractor(links_grepl[t])$InternalLinks
            links_sub2[[t]] <- gsub(".*nl", "", links2[[t]])
          } else {
            links_grepl2[[t]] <- NA
          }
        }
        links2 <- unlist(links2)
        links_sub2 <- unlist(links_sub2)
        links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                         Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente", links_sub2) & 
                                       !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                              bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                              Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                              notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                              brieven|actueel", links_sub2))]
        links_grepl2 <- unique(unlist(links_grepl2))
        
        links_merge <- unique(c(links_grepl, links_grepl2))
        links_merge <- links_merge[which(!grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|Documenten|
                                                Burgemeester|burgemeester|brieven|Brieven", links_merge))]
        years <- as.character(c(2000:(as.numeric(substr(Sys.Date(), 1, 4))-1)))
        years <- paste(years,collapse = '|')
        links_merge <- links_merge[which(!grepl(years, links_merge))]
        
        for (g in seq_along(links_merge)) {
          if (str_count(links_merge[g], ".html") > 1) {
            string <- str_split(links_merge[g], "/")[[1]][(which(grepl(".html", str_split(links_merge[g], "/")[[1]]))[1])]
            links_merge[g] <- gsub(paste0("/", string), "", links_merge[g])
          }
        }
        
        teller <- 1
        delete <- numeric()
        for (q in seq_along(links_merge)) {
          if (tryCatch({
            links_merge[q] %>%
              read_html()
            FALSE
          }, error=function(e) TRUE)) {
            delete[teller] <- q
            teller <- teller + 1
          }
        }
        if (length(delete) > 0) {         links_merge <- links_merge[-delete]        }
        
        samenstelling_gemeenteraad <- links_merge[which(grepl("samenstelling-gemeenteraad", links_merge))]
        gemeenteraad_samenstelling <- links_merge[which(grepl("gemeenteraad-samenstelling", links_merge))]
        samenstelling_raad <- links_merge[which(grepl("samenstelling-raad", links_merge))]
        Samenstelling_gemeenteraad <- links_merge[which(grepl("Samenstelling_gemeenteraad|samenstelling_gemeenteraad|samenstelling-van-de-gemeenteraad", links_merge))]
        wie <- links_merge[which(grepl("Wie_zitten_er_in_de_gemeenteraad|wie_zitten_er_in_de_gemeenteraad|wie-zitten-er-in-de-gemeenteraad|
                                     Wie-zitten-er-in-de-gemeenteraad|wie-zit-er-in-de-raad|Wie-zit-er-in-de-raad", links_merge))]
        
        gemeentes$url_ris[i] <- ifelse(length(samenstelling_gemeenteraad) > 0, samenstelling_gemeenteraad,
                                   ifelse(length(gemeenteraad_samenstelling) > 0, gemeenteraad_samenstelling, 
                                          ifelse(length(samenstelling_raad) > 0, samenstelling_raad,
                                                 ifelse(length(Samenstelling_gemeenteraad) > 0, Samenstelling_gemeenteraad, 
                                                        ifelse(length(wie) > 0, wie, NA)))))
        
        
        if (length(links_merge) > 0 & is.na(gemeentes$url_ris[i])) {
          lengths <- NA
          for (j in seq_along(links_merge)) {
            lengths[j] <- length(links_merge[j] %>%
                                   read_html() %>%
                                   html_nodes("a") %>%
                                   html_attr("href"))
          }
          gemeentes$url_ris[i] <- links_merge[which.max(lengths)] 
        }
      } else {
        gemeentes$url_ris[i] <- NA
      }
    } else {
      gemeentes$url_ris[i] <- NA
    }
  } else {
    gemeentes$url_ris[i] <- NA
  }
}


# BESTUUR
gemeentes$url_bestuur <- 0
for (i in seq_along(gemeentes$Bestuur)) {
  if (is.na(gemeentes$ibabs[i]) & is.na(gemeentes$notubiz[i])) {
    if (!is.na(gemeentes$Bestuur[i])) {
      if (tryCatch({
        LinkExtractor(gemeentes$Bestuur[i])
        TRUE
      }, error=function(e) FALSE)) {
        
        links_internal <- LinkExtractor(gemeentes$Bestuur[i])$InternalLinks
        links_external <- LinkExtractor(gemeentes$Bestuur[i])$ExternalLinks
        links <- c(links_internal, links_external)
        links_sub <- sub(".*nl", "", links)
        links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                         Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                         raadslid|Raadslid|gemeente", links_sub) & 
                                     !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                            bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                            Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                            notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                            brieven|actueel|publicaties_3937", links_sub))]
        links2 <- list()
        links_sub2 <- list()
        links_grepl2 <- list()
        for (t in seq_along(links_grepl)) {
          if (length(LinkExtractor(links_grepl[t])$InternalLinks) != 0) {
            links2[[t]] <- LinkExtractor(links_grepl[t])$InternalLinks
            links_sub2[[t]] <- gsub(".*nl", "", links2[[t]])
          } else {
            links_grepl2[[t]] <- NA
          }
        }
        links2 <- unlist(links2)
        links_sub2 <- unlist(links_sub2)
        links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|
                                         Raad|organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                           raadslid|Raadslid|gemeente", links_sub2) & 
                                       !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                              bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                              Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                              notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                              brieven|actueel|publicaties_3937", links_sub2))]
        links_grepl2 <- unique(unlist(links_grepl2))
        
        links_merge <- unique(c(links_grepl, links_grepl2))
        links_merge <- links_merge[which(!grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                                bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                                Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                                notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                                brieven|actueel|publicaties_3937", links_merge))]
        years <- as.character(c(2000:(as.numeric(substr(Sys.Date(), 1, 4))-1)))
        years <- paste(years,collapse = '|')
        links_merge <- links_merge[which(!grepl(years, links_merge))]
        
        for (g in seq_along(links_merge)) {
          if (str_count(links_merge[g], ".html") > 1) {
            string <- str_split(links_merge[g], "/")[[1]][(which(grepl(".html", str_split(links_merge[g], "/")[[1]]))[1])]
            links_merge[g] <- gsub(paste0("/", string), "", links_merge[g])
          }
        }
        
        teller <- 1
        delete <- numeric()
        for (q in seq_along(links_merge)) {
          if (tryCatch({
            links_merge[q] %>%
              read_html()
            FALSE
          }, error=function(e) TRUE)) {
            delete[teller] <- q
            teller <- teller + 1
          }
        }
        if (length(delete) > 0) {         links_merge <- links_merge[-delete]        }
        
        samenstelling_gemeenteraad <- links_merge[which(grepl("samenstelling-gemeenteraad", links_merge))]
        gemeenteraad_samenstelling <- links_merge[which(grepl("gemeenteraad-samenstelling", links_merge))]
        samenstelling_raad <- links_merge[which(grepl("samenstelling-raad", links_merge))]
        Samenstelling_gemeenteraad <- links_merge[which(grepl("Samenstelling_gemeenteraad|samenstelling_gemeenteraad|samenstelling-van-de-gemeenteraad", links_merge))]
        wie <- links_merge[which(grepl("Wie_zitten_er_in_de_gemeenteraad|wie_zitten_er_in_de_gemeenteraad|wie-zitten-er-in-de-gemeenteraad|
                                     Wie-zitten-er-in-de-gemeenteraad|wie-zit-er-in-de-raad|Wie-zit-er-in-de-raad ", links_merge))]
        
        gemeentes$url_bestuur[i] <- ifelse(length(samenstelling_gemeenteraad) > 0, samenstelling_gemeenteraad,
                                   ifelse(length(gemeenteraad_samenstelling) > 0, gemeenteraad_samenstelling, 
                                          ifelse(length(samenstelling_raad) > 0, samenstelling_raad,
                                                 ifelse(length(Samenstelling_gemeenteraad) > 0, Samenstelling_gemeenteraad, 
                                                        ifelse(length(wie) > 0, wie, NA)))))
        
        
        if (length(links_merge) > 0 & is.na(gemeentes$url_bestuur[i])) {
          lengths <- NA
          for (j in seq_along(links_merge)) {
            lengths[j] <- length(links_merge[j] %>%
                                   read_html() %>%
                                   html_nodes("a") %>%
                                   html_attr("href"))
          }
          gemeentes$url_bestuur[i] <- links_merge[which.max(lengths)] 
        }
      } else {
        gemeentes$url_bestuur[i] <- NA
      }
    } else {
      gemeentes$url_bestuur[i] <- NA
    }
  } else {
    gemeentes$url_bestuur[i] <- NA
  }
}


# RAADSINFORMATIE
# if (is.na(gemeentes$ibabs[179]) & is.na(gemeentes$notubiz[179]) & is.na(gemeentes$url[179])) {
#   if (!is.na(gemeentes$Raadsinformatie[179])) {
#     if (tryCatch({
#       LinkExtractor(gemeentes$Raadsinformatie[179])
#       TRUE
#     }, error=function(e) FALSE)) {
#       
#       links_internal <- LinkExtractor(gemeentes$Raadsinformatie[179])$InternalLinks
#       links_external <- LinkExtractor(gemeentes$Raadsinformatie[179])$ExternalLinks
#       links <- c(links_internal, links_external)
#       links_sub <- sub(".*nl", "", links)
#       links_grepl <- links[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|organisatie|Organisatie|
#                                        raadsleden|Raadsleden|Bestuur|bestuur|inwoners", 
#                                        links_sub) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|bekendmakingen|whatsapp|tel:|mailto:", links_sub))]
#       links2 <- list()
#       links_sub2 <- list()
#       links_grepl2 <- list()
#       for (t in seq_along(links_grepl)) {
#         if (length(LinkExtractor(links_grepl[t])$InternalLinks) != 0) {
#           links2[[t]] <- LinkExtractor(links_grepl[t])$InternalLinks
#           links_sub2[[t]] <- gsub(".*nl", "", links2[[t]])
#         } else {
#           links_grepl2[[t]] <- NA
#         }
#       }
#       links2 <- unlist(links2)
#       links_sub2 <- unlist(links_sub2)
#       links_grepl2 <- links2[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|organisatie|Organisatie|
#                                          raadsleden|Raadsleden|bestuur", 
#                                          links_sub2) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|bekendmakingen|whatsapp|tel:|mailto:", links_sub2))]
#       links_grepl2 <- unique(unlist(links_grepl2))
#       
#       links_merge <- unique(c(links_grepl, links_grepl2))
#       links_merge <- links_merge[which(!grepl("whatsapp", links_merge))]
#       years <- as.character(c(2000:(as.numeric(substr(Sys.Date(), 1, 4))-1)))
#       years <- paste(years,collapse = '|')
#       links_merge <- links_merge[which(!grepl(years, links_merge))]
#       
#       for (g in seq_along(links_merge)) {
#         if (str_count(links_merge[g], ".html") > 1) {
#           string <- str_split(links_merge[g], "/")[[1]][(which(grepl(".html", str_split(links_merge[g], "/")[[1]]))[1])]
#           links_merge[g] <- gsub(paste0("/", string), "", links_merge[g])
#         }
#       }
#       
#       teller <- 1
#       delete <- numeric()
#       for (q in seq_along(links_merge)) {
#         if (tryCatch({
#           links_merge[q] %>%
#             read_html()
#           FALSE
#         }, error=function(e) TRUE)) {
#           delete[teller] <- q
#           teller <- teller + 1
#         }
#       }
#       if (length(delete) > 0) {         links_merge <- links_merge[-delete]        }
#       
#       samenstelling_gemeenteraad <- links_merge[which(grepl("samenstelling-gemeenteraad", links_merge))]
#       gemeenteraad_samenstelling <- links_merge[which(grepl("gemeenteraad-samenstelling", links_merge))]
#       samenstelling_raad <- links_merge[which(grepl("samenstelling-raad", links_merge))]
#       Samenstelling_gemeenteraad <- links_merge[which(grepl("Samenstelling_gemeenteraad|samenstelling_gemeenteraad", links_merge))]
#       wie <- links_merge[which(grepl("Wie_zitten_er_in_de_gemeenteraad|wie_zitten_er_in_de_gemeenteraad|wie-zitten-er-in-de-gemeenteraad|
#                                      Wie-zitten-er-in-de-gemeenteraad", links_merge))]
#       
#       gemeentes$url[179] <- ifelse(length(samenstelling_gemeenteraad) > 0, samenstelling_gemeenteraad,
#                                    ifelse(length(gemeenteraad_samenstelling) > 0, gemeenteraad_samenstelling, 
#                                           ifelse(length(samenstelling_raad) > 0, samenstelling_raad,
#                                                  ifelse(length(Samenstelling_gemeenteraad) > 0, Samenstelling_gemeenteraad, 
#                                                         ifelse(length(wie) > 0, wie, NA)))))
#       
#       
#       if (length(links_merge) > 0 & is.na(gemeentes$url[179])) {
#         lengths <- NA
#         for (j in seq_along(links_merge)) {
#           lengths[j] <- length(links_merge[j] %>%
#                                  read_html() %>%
#                                  html_nodes("a") %>%
#                                  html_attr("href"))
#         }
#         gemeentes$url[179] <- links_merge[which.max(lengths)] 
#       }
#     }
#   }
# }


# IF MULTIPLE WEBSITES, REDUCE TO THE ONE THAT SHOULD BE USED

all <- list(gemeentes$url_regular,
            gemeentes$url_gemeenteraad,
            gemeentes$url_gemeenteraad.,
            gemeentes$url_raad,
            gemeentes$url_bestuur,
            gemeentes$url_ris)
multiple <- numeric()
emails <- list()
gemeentes$use_url <- 0

for (i in seq_along(gemeentes$Gemeente)) {
  multiple <- numeric()
  emails <- list()
  if (length(which(c(!is.na(gemeentes$url_regular[i]),
                     !is.na(gemeentes$url_gemeenteraad[i]),
                     !is.na(gemeentes$url_gemeenteraad.[i]),
                     !is.na(gemeentes$url_raad[i]),
                     !is.na(gemeentes$url_bestuur[i]),
                     !is.na(gemeentes$url_ris[i])))) > 1) {
    for (j in (which(c(!is.na(gemeentes$url_regular[i]),
                       !is.na(gemeentes$url_gemeenteraad[i]),
                       !is.na(gemeentes$url_gemeenteraad.[i]),
                       !is.na(gemeentes$url_raad[i]),
                       !is.na(gemeentes$url_bestuur[i]),
                       !is.na(gemeentes$url_ris[i]))))) {
      multiple[j] <- all[[j]][i]
    }
    multiple <- multiple[which(!is.na(multiple))]
    if (length(unique(multiple)) > 1) {
      print(gemeentes$Gemeente[i])
      for (t in seq_along(na.omit(unique(multiple)))) {
        emails[[t]] <- multiple[[t]] %>%
          read_html() %>% 
          html_nodes("a") %>%
          html_attr("href")
        emails[t] <- str_count(emails[t], "mailto")
      }
      gemeentes$use_url[i] <- multiple[which.max(emails)]
    } else {
      gemeentes$use_url[i] <- unique(multiple)
    }
  } 
  if (length(which(c(!is.na(gemeentes$url_regular[i]),
                     !is.na(gemeentes$url_gemeenteraad[i]),
                     !is.na(gemeentes$url_gemeenteraad.[i]),
                     !is.na(gemeentes$url_raad[i]),
                     !is.na(gemeentes$url_bestuur[i]),
                     !is.na(gemeentes$url_ris[i])))) == 1) {
    gemeentes$use_url[i] <- all[[which(c(!is.na(gemeentes$url_regular[i]),
                                           !is.na(gemeentes$url_gemeenteraad[i]),
                                           !is.na(gemeentes$url_gemeenteraad.[i]),
                                           !is.na(gemeentes$url_raad[i]),
                                           !is.na(gemeentes$url_bestuur[i]),
                                           !is.na(gemeentes$url_ris[i])))]][i]
  } else {
  gemeentes$use_url[i] <- NA
  }
}

## use above to actually filter on something if there are multpiple possiblities
# take double municipalities and use all "a"'s and check if e.g. e-mail %in% the urls

for (i in seq_along(gemeentes$Gemeente)) {
  multiple <- numeric()
  emails <- list()
  if (length(which(c(!is.na(gemeentes$url_regular[281]),
                     !is.na(gemeentes$url_gemeenteraad[281]),
                     !is.na(gemeentes$url_gemeenteraad.[281]),
                     !is.na(gemeentes$url_raad[281]),
                     !is.na(gemeentes$url_bestuur[281]),
                     !is.na(gemeentes$url_ris[281])))) > 1) {
    for (j in (which(c(!is.na(gemeentes$url_regular[281]),
                       !is.na(gemeentes$url_gemeenteraad[281]),
                       !is.na(gemeentes$url_gemeenteraad.[281]),
                       !is.na(gemeentes$url_raad[281]),
                       !is.na(gemeentes$url_bestuur[281]),
                       !is.na(gemeentes$url_ris[281]))))) {
      multiple[j] <- all[[j]][281]
    }
    multiple <- multiple[which(!is.na(multiple))]
    if (length(unique(multiple)) > 1) {
      print(gemeentes$Gemeente[281])
      for (t in seq_along(na.omit(unique(multiple)))) {
        emails[[t]] <- multiple[[t]] %>%
          read_html() %>% 
          html_nodes("a") %>%
          html_attr("href")
      emails[t] <- str_count(emails[t], "mailto")
      }
      gemeentes$use_url[281] <- multiple[which.max(emails)]
    } else {
      gemeentes$use_url[281] <- unique(multiple)
    }
  } 
  # if just one website:
  if (length(which(c(!is.na(gemeentes$url_regular[281]),
                     !is.na(gemeentes$url_gemeenteraad[281]),
                     !is.na(gemeentes$url_gemeenteraad.[281]),
                     !is.na(gemeentes$url_raad[281]),
                     !is.na(gemeentes$url_bestuur[281]),
                     !is.na(gemeentes$url_ris[281])))) == 1) {
    gemeentes$use_url[i] <- all[[which(c(!is.na(gemeentes$url_regular[281]),
                                         !is.na(gemeentes$url_gemeenteraad[281]),
                                         !is.na(gemeentes$url_gemeenteraad.[281]),
                                         !is.na(gemeentes$url_raad[281]),
                                         !is.na(gemeentes$url_bestuur[281]),
                                         !is.na(gemeentes$url_ris[281])))]][281]
  } else {
    gemeentes$use_url[281] <- NA
  }
}



## GET URLS OF ALL MEMBERS

# MAKE COLUMN THAT SHOWS IF ALL INFORMATION OF MEMBERS IS ON ONE OR MULTIPLE PAGES
gemeentes$one_page <- 0
for (i in seq_along(gemeentes$use_url)) {
  if (!is.na(gemeentes$use_url[i])) {
    if (str_count(LinkExtractor(gemeentes$use_url[[i]])$Info$Source_page, pattern = "mailto") > 10) {
      gemeentes$one_page[i] <- 1
    } else {
      gemeentes$one_page[i] <- 0
    }  
  }
}

# GET LIST OF URLS OF ALL MUNICIPALITIES THAT USE MULTIPLE PAGES (ONE_PAGE = 0)


# find reference page
gemeentes$reference <- "0"
years <- as.character(c(2000:(as.numeric(substr(Sys.Date(), 1, 4)))))
years <- paste(years,collapse = '|')
ignore <- paste(c(years, "mailto"), collapse = "|")
for (i in seq_along(gemeentes$use_url)) {
  if (!is.na(gemeentes$use_url[i]) & gemeentes$one_page[i] == 0) {
    links <- na.omit(unique((gemeentes$use_url[[i]] %>%
                               read_html() %>% 
                               html_nodes("a") %>%
                               html_attr("href"))))
    for (l in seq_along(links)) {
      if(grepl(ignore, links[l])) {
        links[l] <- NA
      }
    }
    links <- na.omit(links)
    df <- data.frame(Link = numeric(length(links)),
                     Similarity = numeric(length(links)))
    df$Similarity <- as.list(df$Similarity)
    for (j in seq_along(links)) {
      df$Link[j] <- j
      for (l in seq_along(df$Similarity)) {
        df$Similarity[[j]][l] <- stringsim(links[j], links[l])
      }
    }
    for (t in seq_along(df$Similarity)) {
      df$Mean[t] <- mean(df$Similarity[[t]])
    }
    gemeentes$reference[i] <- links[which.max(df$Mean)]
  } else {
    gemeentes$reference[i] <- NA
  }
}

# neem path, gooi laatste /gedeelte weg, tel per uniek pad op (Map + reduce), 
# filter alle paden eruit met count = 1, sorteer aflopend (9->0) op count, console log


years <- as.character(c(2000:(as.numeric(substr(Sys.Date(), 1, 4)))))
years <- paste(years,collapse = '|')
grepl(years, links)
ignore <- paste(c(years, "mailto"), collapse = "|")

gemeentes$use_url[160]
paste(str_split(gemeentes$use_url[[320]], "/")[[1]][1:3] , collapse = "/")
prefix <- paste(str_split(gemeentes$use_url[167], "/")[[1]][4:(length(str_split(gemeentes$use_url[160], "/")[[1]])-1)] , collapse = "/")
count(paste(str_split(gemeentes$use_url[167], "/")[[1]][4:(length(str_split(gemeentes$use_url[160], "/")[[1]])-1)] , collapse = "/") %in% links)

if (!is.na(gemeentes$use_url[167]) & gemeentes$one_page[160] == 0) {
  links <- na.omit(unique((gemeentes$use_url[[167]] %>%
                             read_html() %>% 
                             html_nodes("a") %>%
                             html_attr("href"))))
  for (i in seq_along(links)) {
    if(grepl(ignore, links[i])) {
      links[i] <- NA
    }
  }
  links <- na.omit(links)
  df <- data.frame(Link = numeric(length(links)),
                   Similarity = numeric(length(links)))
  df$Similarity <- as.list(df$Similarity)
  for (j in seq_along(links)) {
    df$Link[j] <- j
    for (l in seq_along(df$Similarity)) {
      df$Similarity[[j]][l] <- stringsim(links[j], links[l])
    }
  }
  for (t in seq_along(df$Similarity)) {
    df$Mean[t] <- mean(df$Similarity[[t]])
  }
  gemeentes$reference[186] <- links[which.max(df$Mean)]
} else {
  gemeentes$reference[160] <- NA
}
str(links)
paste(links[1:90], collapse = '" "')
str_c(links, collapse = T, sep = '       ')
library(Biostrings)
longestCommonPrefix(paste(links[1:90], collapse = '" "'))
library("PTXQC")
longestCommonPrefix(links)
# longest prefix match group R
# nur eigene Domein

lcPrefix <- function (x, ignore.case = FALSE) 
{
  x <- as.character(x)
  if (ignore.case) 
    x <- toupper(x)
  nc <- nchar(x, type = "char")
  for (i in 1:min(nc)) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) {
      return(substr(x[1], 1, i - 1))
    }
  }
  substr(x[1], 1, i)
}

substr(links[1],1,which.max(apply(do.call(rbind,lapply(strsplit(links,''),`length<-`,nchar(links[1]))),2,function(i)!length(unique(i))==1))-1)
lcPrefix(links)
source("http://bioconductor.org/biocLite.R")
biocLite("Biostrings")
library(Biostrings)

gemeentes$reference

if (!is.na(gemeentes$use_url[212])) {
  links <- unique((gemeentes$use_url[[9]] %>%
                     read_html() %>% 
                     html_nodes("a") %>%
                     html_attr("href")))
  df <- data.frame(Link = numeric(length(links)),
                   Similarity = numeric(length(links)))
  df$Similarity <- as.list(df$Similarity)
  for (j in seq_along(links)) {
    df$Link[j] <- j
    for (l in seq_along(df$Similarity)) {
      df$Similarity[[j]][l] <- stringsim(links[j], links[l])
    }
  }
  for (t in seq_along(df$Similarity)) {
    df$Mean[t] <- mean(df$Similarity[[t]])
  }
  gemeentes$reference[9] <- links[which.max(df$Mean)]
}








i <- 1
for (i in seq_along(gemeentes$use_url)) {
  if (gemeentes$one_page[i] == 0) {
    
  }
  
  if (!is.na(gemeentes$use_url[[i]])) {
    
    gemeentes$use_urls[i] <- list(unique((gemeentes$use_url[[i]] %>%  
                                                read_html() %>% 
                                                html_nodes("a") %>%
                                                html_attr("href"))[which(grepl("nl/leden/lid/", (gemeentes$use_url[[i]] %>%   
                                                                                                   read_html() %>% 
                                                                                                   html_nodes("a") %>%
                                                                                                   html_attr("href"))))]))
  } else {
    NA
  }
  i <- i+1
}


unique((gemeentes$use_url[[9]] %>%  # per party
          read_html() %>% 
          html_nodes("a") %>%
          html_attr("href")))

unique((gemeentes$use_url[[12]] %>%  
          read_html() %>% 
          html_nodes("a") %>%
          html_attr("href")))

unique((gemeentes$use_url[[339]] %>%  # directs to websites of each party
          read_html() %>% 
          html_nodes("a") %>%
          html_attr("href")))

unique((gemeentes$use_url[[324]] %>%  # directs to websites of each party
          read_html() %>% 
          html_nodes("a") %>%
          html_attr("href")))



# FETCH SITES
for (i in seq_along(gemeentes$extern_url)) {
  naam <- numeric()
  if (!is.na(gemeentes$extern_url)) {
    
  }
}

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


# EXTRACT RAADSLEDEN SITE FROM BERGEN (XML SITEMAP)
links <- gemeentes$Sitemaps[33] %>%
  read_html() %>%
  html_nodes("loc") %>%
  html_text()

links <- unique(links[which(grepl("gemeenteraad", links))]); links

# EXTRACT RAADSLEDEN SITE FROM ALBRANDSWAARD (XML SITEMAP)

links <- gemeentes$Sitemaps[6] %>%
  read_html() %>%
  html_nodes("loc") %>%
  html_text()

links <- unique(links[which(grepl("gemeenteraad|samenstelling|wie-is-wie", links))]); links
gemeenteraad <- links[which.min(nchar(links))] ;gemeenteraad

# EXTRACT RAADSLEDEN SITE FROM ALMELO (XML SITEMAP)

links <- gemeentes$Sitemaps[8] %>%
  read_html() %>%
  html_nodes("loc") %>%
  html_text()

links <- unique(links[which(grepl("gemeenteraad|samenstelling", links))]); links
gemeenteraad <- links[which.min(nchar(links))] ;gemeenteraad

# EXTRACT RAADSLEDEN SITE FROM ZWOLLE (NO SITEMAP & IBABS)
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

## DEERDE DOORKLIK CODE
links3 <- list()
links_sub3 <- list()
links_grepl3 <- list()
for (z in seq_along(links_grepl2)) {
  if (tryCatch({
    LinkExtractor(links_grepl2[z])$InternalLinks
    TRUE
  }, error=function(e) FALSE)) {
    if (length(LinkExtractor(links_grepl2[z])$InternalLinks) != 0) {
      links3[[z]] <- LinkExtractor(links_grepl2[z])$InternalLinks
      links_sub3[[z]] <- gsub(".*nl", "", links3[[z]])
    }
  } else {
    links_sub3[[z]] <- NA
  }
}
links3 <- unlist(links3)
links_sub3 <- unlist(links_sub3)
links_grepl3 <- links3[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|organisatie|Organisatie|raadsleden|Raadsleden",
                                   links_sub3) & !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|bekendmakingen|whatsapp|tel:|mailto:", links_sub3))]
links_grepl3 <- unique(unlist(links_grepl3))

gemeente <- gemeentes$general_url[[78]] %>%
  read_html() %>%
  html_nodes("a") %>%
  html_attr("href")
#gemeente_url <- gsub(".*nl", "", gemeentes$general_url[[114]])
gemeente_url <- strsplit(gemeentes$general_url[[140]], split = "/")[[1]][4]
# on different sites:
# 84 [80]
# 140 [81]
# 155 [1]

# per fraction
# 48
# 50 [25]

# on same site:
# 74
# 72
# 68 [78]
# 98 [76]
# 88 [3]
# 56 [5]
i <- 1
match <- character()
for (i in seq_along(gemeente)) {
  if (!is.na(gemeente[i])) {
    if (nchar(gemeente_url) != nchar(gemeente[i])) {
      match[i] <- ifelse(grepl(gemeente_url, gemeente[i]), gemeente[i], NA)
    } else {
      match[i] <- NA
    }
  }
}
if (length(which(!is.na(match))) < 10) {
  gemeentes$general_url[[99]] %>%
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href")
}
ding <- gemeentes$general_url[[50]] %>%
  read_html()

session_url <- html_session(gemeentes$Website[[68]])$url
if (session_url != gemeentes$Website[[39]]) {
  gemeentes$Website[[39]] <- session_url
}

# DIFFERENT SITES FOR ALL MEMBERS
blaricum <- gemeentes$extern_url[[41]] %>%
  read_html() %>%
  html_nodes("a") %>%
  html_attr("href")
blaricum_url <- gsub(".*nl", "", gemeentes$extern_url[[41]])

i <- 1
match <- character()
for (i in seq_along(blaricum)) {
  if (!is.na(blaricum[i])) {
    if (nchar(blaricum_url) != nchar(blaricum[i])) {
      match[i] <- ifelse(grepl(blaricum_url, blaricum[i]), blaricum[i], NA)
    } else {
      match[i] <- NA
    }
  }
}


gemeentes$url_regular # mist heel veel
gemeentes$url_gemeenteraad # mist heel veel
gemeentes$url_gemeenteraad. # volledig
gemeentes$url_raad # op drie na volledig
gemeentes$url_bestuur # volledig
gemeentes$url_ris # volledig

gemeentes$url_gemeenteraad <- ifelse(gemeentes$url_regular == gemeentes$url_gemeenteraad, "double", gemeentes$url_gemeenteraad)

for (i in seq_along(gemeentes$url_regular)) {
  if (!is.na(gemeentes$url_regular[i]) & !is.na(gemeentes$url_gemeenteraad[i])) {
    if (gemeentes$url_regular[i] == gemeentes$url_gemeenteraad[i]) {
      gemeentes$url_gemeenteraad[i] <- "double"
    } 
  }
}

for (i in seq_along(gemeentes$url_regular)) {
  if (!is.na(gemeentes$url_regular[i]) & !is.na(gemeentes$url_gemeenteraad.[i])) {
    if (gemeentes$url_regular[i] == gemeentes$url_gemeenteraad.[i]) {
      gemeentes$url_gemeenteraad.[i] <- "double"
    } 
  }
}

# Neder-Betuwe was laatste
!grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
       bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|Documenten|
       Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|notulen|Sociale|sociale|
       inwoners|Inwoners|fragmenten|vergunning|vragen|brieven|actueel|gast", links_sub)
grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|organisatie|Organisatie|
      raadsleden|Raadsleden|Bestuur|bestuur|inwoners|raadslid|Raadslid", 
      links_sub)
grepl("Wie_zitten_er_in_de_gemeenteraad|wie_zitten_er_in_de_gemeenteraad|wie-zitten-er-in-de-gemeenteraad|
      Wie-zitten-er-in-de-gemeenteraad|wie-zit-er-in-de-raad|Wie-zit-er-in-de-raad|gemeente", links_merge)
links_grepl3 <- unique(unlist(links_grepl3))

links4 <- list()
links_sub4 <- list()
links_grepl4 <- list()
for (z in seq_along(links_grepl3)) {
  if (tryCatch({
    LinkExtractor(links_grepl3[z])$InternalLinks
    TRUE
  }, error=function(e) FALSE)) {
    if (length(LinkExtractor(links_grepl3[z])$InternalLinks) != 0) {
      links4[[z]] <- LinkExtractor(links_grepl3[z])$InternalLinks
      links_sub4[[z]] <- gsub(".*nl", "", links4[[z]])
    }
  } else {
    links_sub4[[z]] <- NA
  }
}
links4 <- unlist(links4)
links_sub4 <- unlist(links_sub4)
links_grepl4 <- links4[which(grepl("gemeenteraad|Gemeenteraad|samenstelling|Samenstelling|wie-is-wie|raad|Raad|
                                   organisatie|Organisatie|raadsleden|Raadsleden|Bestuur|bestuur|inwoners|
                                   raadslid|Raadslid|gemeente",
                                   links_sub4) & 
                               !grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                      bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                      Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                      notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                      brieven|actueel|gast", links_sub4))]
links_grepl4 <- unique(unlist(links_grepl4))

links_merge <- unique(c(links_grepl, links_grepl2, links_grepl3, links_grepl4))
links_merge <- links_merge[which(!grepl("Vergaderingen|vergaderingen|verordeningen|whatsapp|RSS|rss|Nieuws|nieuws|
                                        bekendmakingen|whatsapp|tel:|mailto:|regelingen|Regelingen|documenten|
                                        Documenten|Burgemeester|burgemeester|brieven|Brieven|advies|Advies|Notulen|
                                        notulen|Sociale|sociale|inwoners|Inwoners|fragmenten|vergunning|vragen|
                                        brieven|actueel|gast", links_merge))]


# TO DO -------------------------------------------------------------------

# categorisieren welche gemeente welche Art website hat (--> gemeenteraad website finden)
# nog checken:
# ris https://ris.delft.nl/ DONE
# raadsinformatie https://lochem.raadsinformatie.nl/leden DONE
# gemeenteraad'gemeente'.nl (dus zonder punt) DONE
# ibabs
# ibabs nicht mit [4] selektieren, sondern mit == "Gemeenteraad" | "Raad" (sie line 200) DONE
# liste mit name und all DONE
# Hoofdfuncties toevoegen DONE

# how to detect update/change in URL (schauen nach Domain change)
      # Cuijk [62], De Ronde Venen [68], 
# samenfuehren gemeentes extern en gemeentes die nicht ibabs und nicht notubiz sind --> in eine colummn DONE
# extern en extern_url vergelijken en naar errors kijken
# notubiz scrape schreiben, was kopieren von ibabs? DONE
# general scrape schreiben
# zwijndrecht komt niet bij notubiz op, maar is wel notubiz (https://zwijndrecht.notubiz.nl/leden)
# beverwijk muss normale website genommen worden, nicht gemeenteraad.
      # Anstelle von filter, lieber alle nach einander im loop ausprobieren. Also ersrt normale gemeentewebsite, wenn die length(==0) ist, dann gemeenteraad oder so
# gemeente.emmen --> gemeente. categorisatie checken
# gemeentebestuur.beuningen --> gemeentebestuur. categorisatie checken
# line 740 (gemeente epe, erster loop funktioniert nicht)

# ibabs DONE
# notubiz
# general

# etwas fuür die Auswahl von Kategorien einfallen lassen.
        # Anstelle von filter, lieber alle nach einander im loop ausprobieren. Also ersrt normale gemeentewebsite, 
        # wenn die length(==0) ist, dann gemeenteraad oder so


(length(which(!is.na(gemeentes$use_url))))
(length(which(!is.na(gemeentes$use_url))))/355*100 + length(which(!is.na(gemeentes$ibabs)))/355*100 + length(which(!is.na(gemeentes$notubiz)))/355*100
(length(which(!is.na(gemeentes$extern))))
(length(which(!is.na(gemeentes$extern_url))))
json <- toJSON(gemeentes_ibabs)
write.csv2(json, "gemeentes_ibabs")

# save data
save(gemeentes,file="gemeentes")


(9*3*4*10.5) + (6*6*9) - 500 - (8.8*3*4) - 200
649 + 538 - 430 - (8.8*3*4) - 200
4/6
2/4