library(rvest)
library(tidyr)
library(readr)
library(readtext)
library(curl)
library(httr)
library(stringr)
library(magrittr)
library(janitor)
library(tidycensus)
library(dplyr)

old_file <- './testnumbers/2020-03-09.csv'
population <- read.csv('./populations.csv')



covid <- data.frame()


# Arizona

az_session <- GET("https://tableau.azdhs.gov/views/COVID-19Table/COVID-19table?:isGuestRedirectFromVizportal=y&:embed=y")
az_session_1 <- POST(paste0("https://tableau.azdhs.gov/vizql/w/COVID-19Table/v/COVID-19table/bootstrapSession/sessions/", headers(az_session)$`x-session-id`), body = "worksheetPortSize=%7B%22w%22%3A1740%2C%22h%22%3A750%7D&dashboardPortSize=%7B%22w%22%3A1740%2C%22h%22%3A750%7D&clientDimension=%7B%22w%22%3A1740%2C%22h%22%3A336%7D&renderMapsClientSide=true&isBrowserRendering=true&browserRenderingThreshold=100&formatDataValueLocally=false&clientNum=&navType=Nav&navSrc=Top&devicePixelRatio=1.1041666269302368&clientRenderPixelLimit=25000000&sheet_id=COVID-19%2520table&showParams=%7B%22checkpoint%22%3Afalse%2C%22refresh%22%3Afalse%2C%22refreshUnmodified%22%3Afalse%7D&stickySessionKey=%7B%22featureFlags%22%3A%22%7B%7D%22%2C%22isAuthoring%22%3Afalse%2C%22isOfflineMode%22%3Afalse%2C%22lastUpdatedAt%22%3A1583337655421%2C%22viewId%22%3A234%2C%22workbookId%22%3A26%7D&filterTileSize=200&locale=en_US&language=en&verboseMode=false&:session_feature_flags=%7B%7D&keychain_version=1", add_headers(.headers = c(
  "Connection" = "keep-alive",
  "X-Tsi-Active-Tab" = "COVID-19%20table",
  "Content-Type" = "application/x-www-form-urlencoded"
)), encode = "raw")
az <- read_csv(paste0("https://tableau.azdhs.gov/vizql/w/COVID-19Table/v/COVID-19table/exportcrosstab/sessions/", headers(az_session)$`x-session-id`, "/views/8275719771277684273_2738315765867498942?charset=utf8&download=true"), col_names = FALSE)

covid["Arizona", "positive"] <- as.numeric(az[2,2] + az[3,2])
covid["Arizona", "negative"] <- as.numeric(az[5,2])
covid["Arizona", "pending"] <- as.numeric(az[4,2])
covid["Arizona", "total"] <- covid["Arizona", "positive"] + covid["Arizona", "negative"] + covid["Arizona", "pending"]

# Arkansas

ar <- read_html("https://www.healthy.arkansas.gov/programs-services/topics/novel-coronavirus")

ar <- ar %>% 
  html_node("table") %>%
  html_table()

covid["Arkansas", "positive"] <- as.numeric(ar[1,2])

covid["Arkansas", "negative"] <- as.numeric(ar[4,2])

covid["Arkansas", "pending"] <- as.numeric(ar[2,2])

covid["Arkansas", "total"] <- covid["Arkansas", "positive"] + covid["Arkansas", "negative"] + covid["Arkansas", "pending"]

# Colorado

co <- read_html("https://docs.google.com/document/d/e/2PACX-1vRSxDeeJEaDxir0cCd9Sfji8ZPKzNaCPZnvRCbG63Oa1ztz4B4r7xG_wsoC9ucd_ei3--Pz7UD50yQD/pub")

co <- co %>%
  html_node("ul") %>%
  html_nodes("li")

covid["Colorado", "positive"] <- as.numeric(str_extract(html_text(co[1]),"[0-9]+")) + as.numeric(str_extract(html_text(co[2]),"[0-9]+"))
covid["Colorado", "negative"] <- as.numeric(str_extract(html_text(co[3]),"[0-9]+"))
covid["Colorado", "pending"] <- 0
covid["Colorado", "total"] <- covid["Colorado", "positive"] + covid["Colorado", "negative"] + covid["Colorado", "pending"]

# Delaware

de <- read_html("https://dhss.delaware.gov/dhss/dph/epi/2019novelcoronavirus.html")

de <- de %>%
  html_node("table") %>%
  html_table()

covid["Delaware", "positive"] <- as.numeric(de[2,2])
covid["Delaware", "negative"] <- as.numeric(de[3,2])
covid["Delaware", "pending"] <- as.numeric(de[4,2])
covid["Delaware", "total"] <- covid["Delaware", "positive"] + covid["Delaware", "negative"] + covid["Delaware", "pending"]

# Florida

fl <- read_html("http://www.floridahealth.gov/diseases-and-conditions/COVID-19/index.html")
fl_positive_1 <- fl %>%
  html_node("block") %>%
  html_nodes("div") %>%
  extract(1) %>%
  html_text() %>%
  str_split(" – ") %>%
  extract2(1) %>%
  extract(1) %>%
  as.numeric()

fl_positive_2 <- fl %>%
  html_node("block") %>%
  html_nodes("div") %>%
  extract(2) %>%
  html_text() %>%
  str_split(" – ") %>%
  extract2(1) %>%
  extract(1) %>%
  as.numeric()

fl_positive_3 <- fl %>%
  html_node("block") %>%
  html_nodes("div") %>%
  extract(3) %>%
  html_text() %>%
  str_split(" – ") %>%
  extract2(1) %>%
  extract(1) %>%
  as.numeric()


  

covid["Florida", "positive"] <- fl_positive_1 + fl_positive_2 + fl_positive_3
covid["Florida", "negative"] <- fl %>%
  html_node("block") %>%
  html_nodes("div") %>%
  extract(5) %>%
  html_text() %>%
  as.numeric()

covid["Florida", "pending"] <- fl %>%
  html_node("block") %>%
  html_nodes("div") %>%
  extract(6) %>%
  html_text() %>%
  as.numeric()

covid["Florida", "total"] <- covid["Florida", "positive"] + covid["Florida", "negative"] + covid["Florida", "pending"]

# Idaho

id <- read_html("https://coronavirus.idaho.gov/")

id <- id %>%
  html_node("table") %>%
  html_table

covid["Idaho", "positive"] <- as.numeric(id[4,2])
covid["Idaho", "negative"] <- as.numeric(id[3,2]) - as.numeric(id[4,2])
covid["Idaho", "pending"] <- 0
covid["Idaho", "total"] <- covid["Idaho", "positive"] + covid["Idaho", "negative"] + covid["Idaho", "pending"]

# Illinois 

il <- read_html("http://dph.illinois.gov/topics-services/diseases-and-conditions/diseases-a-z-list/coronavirus")

il <- il %>%
  html_node("table") %>%
  html_table

covid["Illinois", "positive"] <- as.numeric(il[1,2]) + as.numeric(il[2,2])
covid["Illinois", "negative"] <- as.numeric(il[3,2])
covid["Illinois", "pending"] <- as.numeric(il[4,2])
covid["Illinois", "total"] <- covid["Illinois", "positive"] + covid["Illinois", "negative"] + covid["Illinois", "pending"]

# Iowa

covid["Iowa", "positive"] <- 13
covid["Iowa", "negative"] <- 46
covid["Iowa", "pending"] <- 27
covid["Iowa", "total"] <- covid["Iowa", "positive"] + covid["Iowa", "negative"] + covid["Iowa", "pending"]

# Maryland

md <- read_html("https://phpa.health.maryland.gov/Pages/Novel-coronavirus.aspx")


covid["Maryland", "positive"] <- md %>%
  html_node("#ctl00_PlaceHolderMain_ctl02__ControlWrapper_RichHtmlField") %>%
  html_nodes("div div") %>%
  extract(8) %>%
  html_text() %>%
  str_split(":") %>%
  extract2(1) %>%
  extract(2) %>%
  str_extract("[0-9]+") %>%
  as.numeric()
  
covid["Maryland", "negative"] <- md %>%
  html_node("#ctl00_PlaceHolderMain_ctl02__ControlWrapper_RichHtmlField") %>%
  html_nodes("div div") %>%
  extract(7) %>%
  html_text() %>%
  str_split(":") %>%
  extract2(1) %>%
  extract(2) %>%
  str_extract("[0-9]+") %>%
  as.numeric()

covid["Maryland", "pending"] <- as.numeric(0)

covid["Maryland", "total"] <- covid["Maryland", "positive"] + covid["Maryland", "negative"] + covid["Maryland", "pending"]

# Michigan

mi <- read_html("https://www.michigan.gov/coronavirus")

mi <- mi %>%
  html_node("table") %>%
  html_table()

covid["Michigan", "positive"] <- as.numeric(mi[4,2])
covid["Michigan", "negative"] <- as.numeric(mi[3,2])
covid["Michigan", "pending"] <- as.numeric(mi[5,2])
covid["Michigan", "total"] <- covid["Michigan", "positive"] + covid["Michigan", "negative"] + covid["Michigan", "pending"]

# Minnesota

mn <- read_html("https://www.health.state.mn.us/diseases/coronavirus/situation.html")

mn <- mn %>%
  html_node("table") %>%
  html_table()

covid["Minnesota", "positive"] <- as.numeric(mn[1,2])
covid["Minnesota", "negative"] <- as.numeric(mn[2,2]) - as.numeric(mn[1,2])
covid["Minnesota", "pending"] <- 0
covid["Minnesota", "total"] <- covid["Minnesota", "positive"] + covid["Minnesota", "negative"] + covid["Minnesota", "pending"]

# Mississippi


# Montana - MANUAL

# Montana

covid["Montana", "positive"] <- 0
covid["Montana", "negative"] <- 21
covid["Montana", "pending"] <- 0
covid["Montana", "total"] <- covid["Montana", "positive"] + covid["Montana", "negative"] + covid["Montana", "pending"]


# Nebraska

ne <- read_html("http://dhhs.ne.gov/Pages/Coronavirus.aspx")

covid["Nebraska", "positive"] <- ne %>%
  html_node("#ctl00_PlaceHolderMain_ctl08__ControlWrapper_RichHtmlField") %>%
  html_nodes("ul") %>%
  extract(1) %>%
  html_nodes("li") %>%
  extract(1) %>%
  html_text() %>%
  str_split(" – ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()
  
covid["Nebraska", "negative"] <- ne %>%
  html_node("#ctl00_PlaceHolderMain_ctl08__ControlWrapper_RichHtmlField") %>%
  html_nodes("ul") %>%
  extract(1) %>%
  html_nodes("li") %>%
  extract(5) %>%
  html_text() %>%
  str_split(" – ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["Nebraska", "pending"] <- ne %>%
  html_node("#ctl00_PlaceHolderMain_ctl08__ControlWrapper_RichHtmlField") %>%
  html_nodes("ul") %>%
  extract(1) %>%
  html_nodes("li") %>%
  extract(4) %>%
  html_text() %>%
  str_split(" - ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["Nebraska", "total"] <- covid["Nebraska", "positive"] + covid["Nebraska", "negative"] + covid["Nebraska", "pending"]

# Nevada

nv <- read_html("http://dpbh.nv.gov/Programs/OPHIE/dta/Hot_Topics/Coronavirus/")

nv <- nv %>%
  html_node("table") %>%
  html_table()

covid["Nevada", "positive"] <- as.numeric(nv[3,2])
covid["Nevada", "negative"] <- as.numeric(nv[4,2])
covid["Nevada", "pending"] <- 0
covid["Nevada", "total"] <- covid["Nevada", "positive"] + covid["Nevada", "negative"] + covid["Nevada", "pending"]


# New Hampshire

nh <- read_html("https://www.dhhs.nh.gov/dphs/cdcs/2019-ncov.htm")

nh <- nh %>%
  html_node("table") %>%
  html_table()

covid["New Hampshire", "positive"] <- as.numeric(nh[1,2]) + as.numeric(nh[3,2])
covid["New Hampshire", "negative"] <- as.numeric(nh[4,2])
covid["New Hampshire", "pending"] <- as.numeric(nh[2,2])
covid["New Hampshire", "total"] <- covid["New Hampshire", "positive"] + covid["New Hampshire", "negative"] + covid["New Hampshire", "pending"]

# New Jersey

covid["New Jersey", "positive"] <- 15
covid["New Jersey", "negative"] <- 44
covid["New Jersey", "pending"] <- 20
covid["New Jersey", "total"] <- covid["New Jersey", "positive"] + covid["New Jersey", "negative"] + covid["New Jersey", "pending"]

# New Mexico

nm <- read_html("https://cv.nmhealth.org/")

nm <- nm %>%
  html_node("table") %>%
  html_table()

covid["New Mexico", "positive"] <- as.numeric(nm[1,2])
covid["New Mexico", "negative"] <- as.numeric(nm[2,2])
covid["New Mexico", "pending"] <- 0
covid["New Mexico", "total"] <- covid["New Mexico", "positive"] + covid["New Mexico", "negative"] + covid["New Mexico", "pending"]


# Ohio

oh <- read_html("https://odh.ohio.gov/wps/portal/gov/odh/know-our-programs/Novel-Coronavirus/2019-nCoV")

oh <- oh %>%
  html_node("table") %>%
  html_table()

covid["Ohio", "positive"] <- as.numeric(str_split(oh[1,1], ": ")[[1]][2])
covid["Ohio", "negative"] <- as.numeric(str_split(oh[1,3], ": ")[[1]][2])
covid["Ohio", "pending"] <- as.numeric(str_split(oh[1,2], ": ")[[1]][2])
covid["Ohio", "total"] <- covid["Ohio", "positive"] + covid["Ohio", "negative"] + covid["Ohio", "pending"]

# Oregon

or <- read_html("https://www.oregon.gov/oha/PH/DISEASESCONDITIONS/DISEASESAZ/Pages/emerging-respiratory-infections.aspx")

or <- or %>%
  html_node("table") %>%
  html_table()

covid["Oregon", "positive"] <- as.numeric(str_extract(or[2,2],"[0-9]+"))
covid["Oregon", "negative"] <- as.numeric(or[3,2])
covid["Oregon", "pending"] <- as.numeric(or[4,2])
covid["Oregon", "total"] <- covid["Oregon", "positive"] + covid["Oregon", "negative"] + covid["Oregon", "pending"]

# South Carolina

sc <- read_html("https://www.scdhec.gov/health/infectious-diseases/viruses/coronavirus-disease-2019-covid-19/monitoring-testing-covid-19")

sc <- sc %>%
  html_node("table") %>%
  html_table()

covid["South Carolina", "positive"] <- sc[4,2] + sc[5,2]
covid["South Carolina", "negative"] <- sc[3,2]
covid["South Carolina", "pending"] <- 0
covid["South Carolina", "total"] <- covid["South Carolina", "positive"] + covid["South Carolina", "negative"] + covid["South Carolina", "pending"]

# South Dakota

sd <- read_html("https://doh.sd.gov/news/Coronavirus.aspx")

sd <- sd %>%
  html_node("table") %>%
  html_table()

covid["South Dakota", "positive"] <- as.numeric(sd[1,2])
covid["South Dakota", "negative"] <- as.numeric(sd[2,2])
covid["South Dakota", "pending"] <- as.numeric(sd[3,2])
covid["South Dakota", "total"] <- covid["South Dakota", "positive"] + covid["South Dakota", "negative"] + covid["South Dakota", "pending"]

# Vermont

vt <- read_html("https://www.healthvermont.gov/response/infectious-disease/2019-novel-coronavirus")

vt <- vt %>%
  html_node("table") %>%
  html_table()

covid["Vermont", "positive"] <- as.numeric(vt[1,2])
covid["Vermont", "negative"] <- as.numeric(vt[2,2])
covid["Vermont", "pending"] <- 0
covid["Vermont", "total"] <- covid["Vermont", "positive"] + covid["Vermont", "negative"] + covid["Vermont", "pending"]

# Virginia

covid["Virginia", "positive"] <- 8
covid["Virginia", "negative"] <- 53
covid["Virginia", "pending"] <- 0
covid["Virginia", "total"] <- covid["Virginia", "positive"] + covid["Virginia", "negative"] + covid["Virginia", "pending"]

# Wisconsin

wi <- read_html("https://www.dhs.wisconsin.gov/outbreaks/index.htm")

wi <- wi %>%
  html_node("table") %>%
  html_table()

covid["Wisconsin", "positive"] <- as.numeric(wi[1,2])
covid["Wisconsin", "negative"] <- as.numeric(wi[2,2])
covid["Wisconsin", "pending"] <- 0
covid["Wisconsin", "total"] <- covid["Wisconsin", "positive"] + covid["Wisconsin", "negative"] + covid["Wisconsin", "pending"]

# District of Columbia

dc <- read_html("https://coronavirus.dc.gov/page/coronavirus-surveillance-data")

covid["District of Columbia", "positive"] <- dc %>%
  html_nodes("div.field-item") %>%
  html_node("ul") %>%
  html_nodes("li") %>%
  extract(4) %>%
  html_text() %>%
  str_split(": ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["District of Columbia", "negative"] <- dc %>%
  html_nodes("div.field-item") %>%
  html_node("ul") %>%
  html_nodes("li") %>%
  extract(2) %>%
  html_text() %>%
  str_split(": ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["District of Columbia", "pending"] <- dc %>%
  html_nodes("div.field-item") %>%
  html_node("ul") %>%
  html_nodes("li") %>%
  extract(3) %>%
  html_text() %>%
  str_split(": ") %>%
  extract2(1) %>%
  extract(2) %>%
  as.numeric()

covid["District of Columbia", "total"] <- covid["District of Columbia", "positive"] + covid["District of Columbia", "negative"] + covid["District of Columbia", "pending"]

covid$state <- as.character(row.names(covid))


old_tests <- read.csv(old_file)

covid <- inner_join(population, covid)


covid["Total", "positive"] <- sum(covid[, "positive"], na.rm = TRUE)
covid["Total", "negative"] <- sum(covid[, "negative"], na.rm = TRUE)
covid["Total", "pending"] <- sum(covid[, "pending"], na.rm = TRUE)
covid["Total", "total"] <- sum(covid[, "total"], na.rm = TRUE)
covid["Total", "population"] <- sum(covid[, "population"], na.rm = TRUE)
covid["Total", "state"] <- "Total"

delta <- old_tests %>%
  select(state, total) %>%
  right_join(covid, by = "state") %>%
  mutate(delta = (total.y/total.x - 1)) %>%
  select(state,delta)

covid <- left_join(covid, delta)

covid <- covid %>%
  mutate(tests_per_million = total/(population/1000000)) %>%
  mutate(pct_positive = positive/(positive+negative))
  

write.csv(covid, paste0("./testnumbers/", Sys.Date(),".csv"), row.names = FALSE)
covid$date <- Sys.Date()
all <- read.csv("./testnumbers/all.csv")
all$date <- as.Date(all$date)
all <- all %>%
  bind_rows(covid)
write.csv(all, './testnumbers/all.csv', row.names = FALSE)
