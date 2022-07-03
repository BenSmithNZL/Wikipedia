library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)

base = "https://en.wikipedia.org/wiki/"

results <- list()

targets <- c("Auckland", "Wellington", "Christchurch",
             "Sydney", "Brisbane",
             "London", "Manchester",
             "Dublin", "Belfast",
             "Cape_Town", "Durban",
             "Munich", "Berlin",
             "Socks", "T-shirt")

names(results) <- targets


# Gets all links off the Wikipedia page
scraplinks <- function(url){
  
  webpage <- xml2::read_html(url)

  url_ <- webpage %>%
    html_nodes("a") %>%
    html_attr("href")

  link_ <- webpage %>%
    html_nodes("a") %>%
    html_text()
  
  return(tibble(link = link_, url = url_))
  
}

# Uses scraplinks to get the links, then cleans them
get_article_links <- function(article){
  
  url = paste(base, article, sep = "")
  
  d <- scraplinks(url)
  
  d <- d[!is.na(d$url), ]
  d <- d[which(substr(d$url, 1, 6) == "/wiki/"),]
  d$url <- gsub("/wiki/","", d$url)
  
  d <- d[-grep("Category:", d$url),]
  d <- d[-grep("File:", d$url),]
  d <- d[-grep("Help:", d$url),]
  d <- d[-grep("Portal:", d$url),]
  d <- d[-grep("Special:", d$url),]
  d <- d[-grep("Talk:", d$url),]
  d <- d[-grep("Template:", d$url),]
  d <- d[-grep("Template_talk:", d$url),]
  d <- d[-grep("Wikipedia:", d$url),]
  d <- d[-grep("Main_Page", d$url),]
  
  d <- d[-grep(article, d$url),]
  
  d$url <- gsub("\\#.*", "", d$url)
  
  return(unique(d$url))
  
}

# Store the results in a list
results <- sapply(targets, get_article_links)

# Calculate their similarity 
similarity <- matrix(nrow = length(targets),
                     ncol = length(targets))

rownames(similarity) <- targets
colnames(similarity) <- targets

for (r in 1:length(targets)) {
  for(c in 1:length(targets)) {
    similarity[r, c] <- sum(results[[r]] %in% results[[c]]) / length(results[[r]])
  }
}

dist_mat <- dist(similarity, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

View(round(similarity, 4))

