#******************************************************************************#
#                                                                              #
#                    Lab 3 - Data Acquisition & Analysis                       #
#                                                                              #
#                   Pablo Cid Ramos - Data Driven Securty                      #
#                                                                              #
#******************************************************************************#

## Crawling y Scrapping
# A�adimos el enlace a buscar
url <- "https://www.mediawiki.org/wiki/MediaWiki"

#options(repos="https://cran.rstudio.com" )
 #Instalamos los paquetes que utilizaremos
install.packages("httr",repos='http://cran.us.r-project.org')
install.packages("xml2", repos='http://cran.us.r-project.org')
install.packages("XML", repos='http://cran.us.r-project.org')
install.packages("plyr", repos='http://cran.us.r-project.org')

library(XML)
library(xml2)
library(plyr)
library(httr)
library(ggplot2)


### 1.1 Obtención de la página web
get_title <- function(url){
  doc <- read_html(url)
  return(doc)
}
#doc <- read_html(url)


### 1.2 Analisis de el contenido de la web
web_content <- function(doc){
  titulo <- rvest::html_text(rvest::xml_nodes(doc,
                                              xpath = "//head/title"))
  return(titulo)
}
#titulo <- rvest::html_text(rvest::xml_nodes(doc,
 #                                           xpath = "//head/title"))


### 1.3.	Extracción de enlaces
get_links <- function(doc){
  enlaces <- rvest::html_text(rvest::xml_nodes(doc,
                                               xpath="//a/@href"))
  project <- data.frame(enlace=enlaces)
  return(project)
}
#enlaces <- rvest::html_text(rvest::xml_nodes(doc,
 #                           xpath="//a/@href"))
#project <- data.frame(enlace=enlaces)

### 1.4 Exploración de enlaces

expl_links <- function(project){
  status <- rbind()
  type <- rbind()
  for(link in project$enlace){
    if(grepl("https",link)){
      response<-httr::HEAD(link)
      status <- rbind(status,response$status_code)
      type <- rbind(type,"absolute")
    }else if(startsWith(link,"/")){
      link2 <- paste("https://www.mediawiki.org",link)
      link2 <- gsub(" ","",link2)
      response<-httr::HEAD(link2)
      status <- rbind(status,response$status_code)
      type <- rbind(type,"relative")
    }else{
      status <- rbind(status,NA)
      type <- rbind(type,NA)
    }
    Sys.sleep(2)
  }
  project <- cbind(project,status)
  project <-cbind(project,type)
  return(project)
}


###############################################

#a <- 0
#status <- rbind()
#type <- rbind()
#for(link in project$enlace){
#  print(link)
#  if(grepl("https",link)){
#    response<-httr::HEAD(link)
#    status <- rbind(status,response$status_code)
#    type <- rbind(type,"absolute")
#  }else if(startsWith(link,"/")){
#      print(link)
#      link2 <- paste("https://www.mediawiki.org",link)
#      link2 <- gsub(" ","",link2)
#      print(link2)
#      response<-httr::HEAD(link2)
#      status <- rbind(status,response$status_code)
#      type <- rbind(type,"relative")
#  }else{
#    status <- rbind(status,NA)
#    type <- rbind(type,NA)
#  }
#  a <- a + 1
  #print(link)
#  Sys.sleep(2)
#}
#project <- cbind(project,status)
#project <-cbind(project,type)


#################################################


### Gráficos en R

### 2.1 Histograma
hist <- function(project){
  project.absolute <- project[project$type == "absolute",]
  project.absolute <- project.absolute[!is.na(project.absolute$type),]
  g <- ggplot2::ggplot(project.absolute, aes(x=enlace))
  g + geom_bar()

  project.relative <- project[project$type == "relative",]
  project.relative<- project.relative[!is.na(project.relative$type),]
  g <- ggplot2::ggplot(project.relative, aes(x=enlace))
  g + geom_bar()

}
### 2.2 Un gráfico de barras
bar <- function(project){
  project.absolute <- project[project$type == "absolute",]
  project.absolute <- project.absolute[!is.na(project.absolute$type),]

  project.relative <- project[project$type == "relative",]
  project.relative<- project.relative[!is.na(project.relative$type),]

  external <- rbind()
  for(link in project.absolute$enlace){
    if(grepl("https://www.mediawiki.org",link)){
      external <- rbind(external,"internal")
    }else{
      external <- rbind(external,"external")
    }
  }
  project.absolute <- cbind(project.absolute,external)
  bar.plot <- plyr::count(project.absolute,"external")
  bar.plot[bar.plot$external == "internal",]$freq <- bar.plot[bar.plot$external == "internal",]$freq + nrow(project.relative)
  g <- ggplot2::ggplot(bar.plot, aes(x=external,y=freq,fill=external))
  g + geom_bar(stat = "identity")
}


### 2.3 Pie Chart
chart <- function(project){
  pie.chart <- plyr::count(project,"status")
  pie.chart.graphic <- ggplot2::ggplot(pie.chart, aes(x="",y=freq,fill=status))
  pie.chart.graphic + geom_bar(width=1,stat = "identity") + coord_polar("y", start=0)
}

