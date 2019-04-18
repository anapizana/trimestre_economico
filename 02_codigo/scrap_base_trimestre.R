library(rvest)
library(httr)
# Librerías

library(tidyverse)
library(stringr)
library(magrittr)


# Este loop scrapea todos los links a los números de la revista.
# Guarda dichos links en un objeto llamado "total_links".

total_links <- NULL
links <- NULL

for(i in 1:3){
  
  cat(i, "... ")
  
  url <- paste0("http://www.eltrimestreeconomico.com.mx/index.php/te/issue/archive?issuesPage=",
                i)
  pr <- read_html(url)
  links <- pr %>%
    html_nodes("h4 a") %>% 
    html_attr('href')
  links <- as.data.frame(links)
  
  total_links <- rbind(total_links, links)
  
}


# Dado que sólo desde el número 85 tenemos xmls, vamos a obtenerlos.

xmllinks <- total_links[3:nrow(total_links),]


xmllinks  <-  as.character(xmllinks )

total_xmllinks <- NULL
xmllinks_complete <- NULL

for(i in 1:57){
  
  cat(i, "... ")
  
  url <- xmllinks[i]
  
  pr <- read_html(url)
  xmllinks_complete <- pr %>%
    html_nodes(".file+ .file") %>% 
    html_attr('href')
  xmllinks_complete <- as.data.frame(xmllinks_complete)
  
  total_xmllinks <- rbind(total_xmllinks, xmllinks_complete)
  
}

write.csv(total_xmllinks, "links_xml.csv")





#Extraer el contenido

art <- as.tibble(read_csv("links_xml.csv"))

base <- NULL
bind <- NULL

for(i in 1:435) {
  
  cat(i, "... ")
  
  url <- art$xmllinks_complete[i]
  
  #x <- GET(url, add_headers('use-agent'='holi'))
  
  pr <- read_html(url)
  
  p_numero <- pr %>%
    html_nodes("#breadcrumb a:nth-child(2)") %>%
    html_text()
  
  if(length(p_numero) == 0) {
    p_numero <- NA 
  }
  
  
  p_titulo <- pr %>%
    html_nodes("#content h1") %>%
    html_text()
  
  if(length(p_titulo) == 0) {
    p_titulo <- NA 
  }
  

  p_apellido <- pr %>%
    html_nodes(".surname") %>% 
    html_text() 
  
  
  if(length(p_apellido) == 0) {
    p_apellido <- NA 
  }

  p_nombre <- pr %>%
    html_nodes(".given_names") %>% 
    html_text() 
  
  
  if(length(p_nombre) == 0) {
    p_nombre <- NA 
  }

  p_afiliacion <- pr %>%
    html_nodes(".original") %>% 
    html_text() 
  
  
  if(length(p_afiliacion) == 0) {
    p_afiliacion <- NA 
  } 

  p_abstract <- pr %>%
    html_nodes("#abstract+ .panel-default .panel-body") %>% 
    html_text() 
  
  
  if(length(p_abstract) == 0) {
    p_abstract <- NA 
  }
  

  p_kwords <- pr %>%
    html_nodes(".panel-default:nth-child(8) .list-inline") %>% 
    html_text() 
  
  
  if(length(p_kwords) == 0) {
    p_kwords <- NA 
  }
  
  p_kwordsalt <- pr %>%
    html_nodes("#keywords+ .panel-default .panel-body") %>% 
    html_text() 
  
  
  if(length(p_kwordsalt) == 0) {
    p_kwordsalt <- NA 
  }
  
  
  bind <- cbind(url,p_titulo, p_numero, p_apellido, p_nombre,
                p_afiliacion, p_abstract,p_kwords, p_kwordsalt)
  
  base <- rbind(base, bind) 
}


base <- as.tibble(base)


write.csv(base, "base.csv")



