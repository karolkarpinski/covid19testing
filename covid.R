library(rvest)
library(tidyr)
library(readr)
library(readtext)
library(curl)
library(httr)
library(magick)
library(tesseract)
library(stringr)
library(magrittr)


covid <- data.frame()

# New York

ny <- read_html("https://www.health.ny.gov/diseases/communicable/coronavirus/")

ny <- ny %>%
  html_node("#case_count_table") %>%
  html_table()

covid["New York", "Positive test results"] <- as.numeric(ny[1,2] + ny[1,3])
covid["New York", "Negative test results"] <- as.numeric(ny[2,2] + ny[2,3])
covid["New York", "Pending"] <- as.numeric(ny[3,2] + ny[3,3])
covid["New York", "Total tested"] <- covid["New York", "Positive test results"] + covid["New York", "Negative test results"] + covid["New York", "Pending"]

# Arizona

az_session <- GET("https://tableau.azdhs.gov/views/COVID-19Table/COVID-19table?:isGuestRedirectFromVizportal=y&:embed=y")
az_session_1 <- POST(paste0("https://tableau.azdhs.gov/vizql/w/COVID-19Table/v/COVID-19table/bootstrapSession/sessions/", headers(az_session)$`x-session-id`), body = "worksheetPortSize=%7B%22w%22%3A1740%2C%22h%22%3A750%7D&dashboardPortSize=%7B%22w%22%3A1740%2C%22h%22%3A750%7D&clientDimension=%7B%22w%22%3A1740%2C%22h%22%3A336%7D&renderMapsClientSide=true&isBrowserRendering=true&browserRenderingThreshold=100&formatDataValueLocally=false&clientNum=&navType=Nav&navSrc=Top&devicePixelRatio=1.1041666269302368&clientRenderPixelLimit=25000000&sheet_id=COVID-19%2520table&showParams=%7B%22checkpoint%22%3Afalse%2C%22refresh%22%3Afalse%2C%22refreshUnmodified%22%3Afalse%7D&stickySessionKey=%7B%22featureFlags%22%3A%22%7B%7D%22%2C%22isAuthoring%22%3Afalse%2C%22isOfflineMode%22%3Afalse%2C%22lastUpdatedAt%22%3A1583337655421%2C%22viewId%22%3A234%2C%22workbookId%22%3A26%7D&filterTileSize=200&locale=en_US&language=en&verboseMode=false&:session_feature_flags=%7B%7D&keychain_version=1", add_headers(.headers = c(
  "Connection" = "keep-alive",
  "X-Tsi-Active-Tab" = "COVID-19%20table",
  "Content-Type" = "application/x-www-form-urlencoded"
)), encode = "raw")
az <- read_csv(paste0("https://tableau.azdhs.gov/vizql/w/COVID-19Table/v/COVID-19table/exportcrosstab/sessions/", headers(az_session)$`x-session-id`, "/views/8275719771277684273_2738315765867498942?charset=utf8&download=true"), col_names = FALSE)

covid["Arizona", "Positive test results"] <- as.numeric(az[2,2] + az[3,2])
covid["Arizona", "Negative test results"] <- as.numeric(az[5,2])
covid["Arizona", "Pending"] <- as.numeric(az[4,2])
covid["Arizona", "Total tested"] <- covid["Arizona", "Positive test results"] + covid["Arizona", "Negative test results"] + covid["Arizona", "Pending"]

# Arkansas

ar <- read_html("https://www.healthy.arkansas.gov/programs-services/topics/novel-coronavirus")

ar <- ar %>% 
  html_node("table") %>%
  html_table()

covid["Arkansas", "Positive test results"] <- as.numeric(ar[2,1])
covid["Arkansas", "Negative test results"] <- as.numeric(str_extract(ar[2,4], "[0-9]+"))
covid["Arkansas", "Pending"] <- as.numeric(ar[2,2])
covid["Arkansas", "Total tested"] <- covid["Arkansas", "Positive test results"] + covid["Arkansas", "Negative test results"] + covid["Arkansas", "Pending"]

# Colorado

co <- read_html("https://www.colorado.gov/pacific/cdphe/2019-novel-coronavirus")

co <- co %>%
  html_node("table") %>%
  html_table()

covid["Colorado", "Positive test results"] <- as.numeric(co[1,2])
covid["Colorado", "Negative test results"] <- as.numeric(co[2,2])
covid["Colorado", "Pending"] <- as.numeric(co[3,2])
covid["Colorado", "Total tested"] <- covid["Colorado", "Positive test results"] + covid["Colorado", "Negative test results"] + covid["Colorado", "Pending"]

# Florida

fl <- read_delim("http://www.floridahealth.gov/diseases-and-conditions/COVID-19/_documents/covid19-daily-numbers.txt", "*", col_names = FALSE)
covid["Florida", "Positive test results"] <- as.numeric(fl[1,2]) + as.numeric(fl[1,3])
covid["Florida", "Negative test results"] <- as.numeric(fl[1,5])
covid["Florida", "Pending"] <- as.numeric(fl[1,4])
covid["Florida", "Total tested"] <- covid["Florida", "Positive test results"] + covid["Florida", "Negative test results"] + covid["Florida", "Pending"]

# Idaho

id <- read_html("https://coronavirus.idaho.gov/")

id <- id %>%
  html_node("table") %>%
  html_table

covid["Idaho", "Positive test results"] <- as.numeric(id[4,2])
covid["Idaho", "Negative test results"] <- as.numeric(id[3,2]) - as.numeric(id[4,2])
covid["Idaho", "Pending"] <- 0
covid["Idaho", "Total tested"] <- covid["Idaho", "Positive test results"] + covid["Idaho", "Negative test results"] + covid["Idaho", "Pending"]

# Illinois 

il <- read_html("http://dph.illinois.gov/topics-services/diseases-and-conditions/diseases-a-z-list/coronavirus")

il <- il %>%
  html_node("table") %>%
  html_table

covid["Illinois", "Positive test results"] <- as.numeric(il[1,2]) + as.numeric(il[2,2])
covid["Illinois", "Negative test results"] <- as.numeric(il[3,2])
covid["Illinois", "Pending"] <- as.numeric(il[4,2])
covid["Illinois", "Total tested"] <- covid["Illinois", "Positive test results"] + covid["Illinois", "Negative test results"] + covid["Illinois", "Pending"]

# Maryland

md <- read_html("https://phpa.health.maryland.gov/Pages/Novel-coronavirus.aspx")


covid["Maryland", "Positive test results"] <- md %>%
  html_node("#ctl00_PlaceHolderMain_ctl02__ControlWrapper_RichHtmlField") %>%
  html_nodes("div div") %>%
  extract(9) %>%
  html_text() %>%
  str_split(":") %>%
  extract2(1) %>%
  extract(2) %>%
  str_extract("[0-9]+") %>%
  as.numeric()
  
covid["Maryland", "Negative test results"] <- md %>%
  html_node("#ctl00_PlaceHolderMain_ctl02__ControlWrapper_RichHtmlField") %>%
  html_nodes("div div") %>%
  extract(7) %>%
  html_text() %>%
  str_split(":") %>%
  extract2(1) %>%
  extract(2) %>%
  str_extract("[0-9]+") %>%
  as.numeric()

covid["Maryland", "Pending"] <- md %>%
  html_node("#ctl00_PlaceHolderMain_ctl02__ControlWrapper_RichHtmlField") %>%
  html_nodes("div div") %>%
  extract(6) %>%
  html_text() %>%
  str_split(":") %>%
  extract2(1) %>%
  extract(2) %>%
  str_extract("[0-9]+") %>%
  as.numeric()

covid["Maryland", "Total tested"] <- covid["Maryland", "Positive test results"] + covid["Maryland", "Negative test results"] + covid["Maryland", "Pending"]

# Michigan

mi <- read_html("https://www.michigan.gov/coronavirus")

mi <- mi %>%
  html_node("table") %>%
  html_table()

covid["Michigan", "Positive test results"] <- as.numeric(mi[4,2])
covid["Michigan", "Negative test results"] <- as.numeric(mi[3,2])
covid["Michigan", "Pending"] <- as.numeric(mi[5,2])
covid["Michigan", "Total tested"] <- covid["Michigan", "Positive test results"] + covid["Michigan", "Negative test results"] + covid["Michigan", "Pending"]

# Minnesota

mn <- read_html("https://www.health.state.mn.us/diseases/coronavirus/situation.html")

mn <- mn %>%
  html_node("table") %>%
  html_table()

covid["Minnesota", "Positive test results"] <- as.numeric(mn[1,2])
covid["Minnesota", "Negative test results"] <- as.numeric(mn[2,2])
covid["Minnesota", "Pending"] <- as.numeric(mn[3,2])
covid["Minnesota", "Total tested"] <- covid["Minnesota", "Positive test results"] + covid["Minnesota", "Negative test results"] + covid["Minnesota", "Pending"]

# Mississippi

ms <- read_html("https://msdh.ms.gov/msdhsite/_static/14,0,420.html")

covid["Mississippi", "Positive test results"] <- ms %>%
  html_node("ul.shadedBlue") %>%
  html_nodes("li") %>%
  extract(1) %>%
  html_node("strong") %>%
  html_text() %>%
  as.numeric()

covid["Mississippi", "Negative test results"] <- 0 # To update once their website is updated

covid["Mississippi", "Pending"] <- ms %>%
  html_node("ul.shadedBlue") %>%
  html_nodes("li") %>%
  extract(2) %>%
  html_node("strong") %>%
  html_text() %>%
  as.numeric()

# Montana - to OCR

# Nebraska

ne <- read_html("http://dhhs.ne.gov/Pages/Coronavirus.aspx")

covid["Nebraska", "Positive test results"] <- ne %>%
  html_node("#ctl00_PlaceHolderMain_ctl08__ControlWrapper_RichHtmlField") %>%
  html_nodes("ul") %>%
  extract(3) %>%
  html_nodes("li") %>%
  extract(1) %>%
  html_text() %>%
  str_split(" – ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()
  
covid["Nebraska", "Negative test results"] <- ne %>%
  html_node("#ctl00_PlaceHolderMain_ctl08__ControlWrapper_RichHtmlField") %>%
  html_nodes("ul") %>%
  extract(4) %>%
  html_nodes("li") %>%
  extract(1) %>%
  html_text() %>%
  str_split(" – ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["Nebraska", "Pending"] <- ne %>%
  html_node("#ctl00_PlaceHolderMain_ctl08__ControlWrapper_RichHtmlField") %>%
  html_nodes("ul") %>%
  extract(3) %>%
  html_nodes("li") %>%
  extract(2) %>%
  html_text() %>%
  str_split(" - ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["Nebraska", "Total tested"] <- covid["Nebraska", "Positive test results"] + covid["Nebraska", "Negative test results"] + covid["Nebraska", "Pending"]

# New Hampshire

nh <- read_html("https://www.dhhs.nh.gov/dphs/cdcs/2019-ncov.htm")

nh <- nh %>%
  html_node("table") %>%
  html_table()

covid["New Hampshire", "Positive test results"] <- as.numeric(nh[1,2]) + as.numeric(nh[3,2])
covid["New Hampshire", "Negative test results"] <- as.numeric(nh[4,2])
covid["New Hampshire", "Pending"] <- as.numeric(nh[2,2])
covid["New Hampshire", "Total tested"] <- covid["New Hampshire", "Positive test results"] + covid["New Hampshire", "Negative test results"] + covid["New Hampshire", "Pending"]

# Ohio

oh <- read_html("https://odh.ohio.gov/wps/portal/gov/odh/know-our-programs/Novel-Coronavirus/2019-nCoV")

oh <- oh %>%
  html_node("table") %>%
  html_table()

covid["Ohio", "Positive test results"] <- as.numeric(str_split(oh[1,1], ": ")[[1]][2])
covid["Ohio", "Negative test results"] <- as.numeric(str_split(oh[1,3], ": ")[[1]][2])
covid["Ohio", "Pending"] <- as.numeric(str_split(oh[1,2], ": ")[[1]][2])
covid["Ohio", "Total tested"] <- covid["Ohio", "Positive test results"] + covid["Ohio", "Negative test results"] + covid["Ohio", "Pending"]

# Oregon

or <- read_html("https://www.oregon.gov/oha/PH/DISEASESCONDITIONS/DISEASESAZ/Pages/emerging-respiratory-infections.aspx")

or <- or %>%
  html_node("table") %>%
  html_table()

covid["Oregon", "Positive test results"] <- as.numeric(or[2,2])
covid["Oregon", "Negative test results"] <- as.numeric(or[3,2])
covid["Oregon", "Pending"] <- as.numeric(or[4,2])
covid["Oregon", "Total tested"] <- covid["Oregon", "Positive test results"] + covid["Oregon", "Negative test results"] + covid["Oregon", "Pending"]

# South Dakota

sd <- read_html("https://doh.sd.gov/news/Coronavirus.aspx")

sd <- sd %>%
  html_node("table") %>%
  html_table()

covid["South Dakota", "Positive test results"] <- as.numeric(sd[1,2]) + as.numeric(sd[3,2])
covid["South Dakota", "Negative test results"] <- as.numeric(sd[2,2])
covid["South Dakota", "Pending"] <- as.numeric(sd[4,2])
covid["South Dakota", "Total tested"] <- covid["South Dakota", "Positive test results"] + covid["South Dakota", "Negative test results"] + covid["South Dakota", "Pending"]

# Wisconsin

wi <- read_html("https://www.dhs.wisconsin.gov/outbreaks/index.htm")

wi <- wi %>%
  html_node("table") %>%
  html_table()

covid["Wisconsin", "Positive test results"] <- as.numeric(wi[1,2])
covid["Wisconsin", "Negative test results"] <- as.numeric(wi[2,2])
covid["Wisconsin", "Pending"] <- as.numeric(wi[3,2])
covid["Wisconsin", "Total tested"] <- covid["Wisconsin", "Positive test results"] + covid["Wisconsin", "Negative test results"] + covid["Wisconsin", "Pending"]

# District of Columbia

dc <- read_html("https://coronavirus.dc.gov/page/coronavirus-surveillance-data")

covid["District of Columbia", "Positive test results"] <- dc %>%
  html_nodes("div.field-item") %>%
  html_node("ul") %>%
  html_nodes("li") %>%
  extract(4) %>%
  html_text() %>%
  str_split(": ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["District of Columbia", "Negative test results"] <- dc %>%
  html_nodes("div.field-item") %>%
  html_node("ul") %>%
  html_nodes("li") %>%
  extract(2) %>%
  html_text() %>%
  str_split(": ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["District of Columbia", "Pending"] <- dc %>%
  html_nodes("div.field-item") %>%
  html_node("ul") %>%
  html_nodes("li") %>%
  extract(3) %>%
  html_text() %>%
  str_split(": ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["District of Columbia", "Total tested"] <- covid["District of Columbia", "Positive test results"] + covid["District of Columbia", "Negative test results"] + covid["District of Columbia", "Pending"]
  