library(tidyverse)
library(rvest)
library(RSelenium)



rD <- rsDriver(port=4444L, browser=c("chrome"), 
               chromever="83.0.4103.39",
               verbose=F)
remDr <- rD[["client"]]
remDr$navigate("https://www.confaz.fazenda.gov.br/balanca-comercial-interestadual")


Sys.sleep(3)

webElem1 <- remDr$findElement(using = 'css',".external-link")
webElem1$clickElement()

Sys.sleep(3)


webElem2 <- remDr$findElement(using = 'xpath',
                              "//*[@id='pbiAppPlaceHolder']/ui-view/div/div[2]/logo-bar/div/div/div/logo-bar-navigation/span/a[2]")
                              
                              
webElem2$clickElement()

Sys.sleep(3)

webElem3 <- remDr$findElement(using = 'xpath',
                             "/html/body/div[1]/ui-view/div/div[2]/logo-bar/div/div/div/logo-bar-navigation/section/div[1]/div/div/ul/li[6]/a")

webElem3$clickElement()

Sys.sleep(3)

webElem_table <- remDr$findElement(using="xpath",
                          "/html/body/div[1]/ui-view/div/div[1]/div/div/div/div/exploration-container/exploration-container-modern/div/div/exploration-host/div/div/exploration/div/explore-canvas-modern/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container-modern[1]/transform/div/div[3]/div/visual-modern/div/div/div[2]/div[1]/div[1]/div/div")
webElem_table$clickElement()

Sys.sleep(3)

webElem4 <- remDr$findElement(using = 'xpath',
                              "/html/body/div[1]/ui-view/div/div[1]/div/div/div/div/exploration-container/exploration-container-modern/div/div/exploration-host/div/div/exploration/div/explore-canvas-modern/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container-modern[1]/transform/div/visual-container-header-modern/div/div[1]/div/visual-header-item-container/div/button/i"
)
webElem4$clickElement()

Sys.sleep(3)

temp <- read_html(remDr$getPageSource()[[1]])

temp %>% write_html("automated_table_scrape.html")


remDr$close()
# stop the selenium server
rD$server$stop()

rm(rD, remDr)
gc()

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
