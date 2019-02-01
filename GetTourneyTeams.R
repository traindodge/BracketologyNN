library(XML)
library(httr)

fetch_NCAA_CBB_Adv_Ratings <- function(year) {
  url = paste0("https://www.sports-reference.com/cbb/seasons/", year, "-advanced-school-stats.html")
  NCAA_CBB_Adv_Ratings = GET(url)
  NCAA_CBB_Adv_Ratings = readHTMLTable(rawToChar(NCAA_CBB_Adv_Ratings$content))
  NCAA_CBB_Adv_Ratings = NCAA_CBB_Adv_Ratings[[1]]
  NCAA_CBB_Adv_Ratings$year = year
  NCAA_CBB_Adv_Ratings
}

library(plyr)
NCAA_CBB_Adv_Ratings = ldply(2000:2018, fetch_NCAA_CBB_Adv_Ratings, .progress = "text")

NCAA_CBB_Adv_Ratings = NCAA_CBB_Adv_Ratings[ grep("Rk", NCAA_CBB_Adv_Ratings$Rk, invert = TRUE) , ]
NCAA_CBB_Adv_Ratings = NCAA_CBB_Adv_Ratings[ grep("Home", NCAA_CBB_Adv_Ratings$W, invert = TRUE) , ]

library(stringi)

NCAABirth = stri_sub(NCAA_CBB_Adv_Ratings$School, -4, -1)
NCAABirth = as.character(NCAABirth)

NCAA_CBB_Adv_Ratings$NCAABirth = NCAABirth

NCAA_CBB_Adv_Ratings$School = as.character(NCAA_CBB_Adv_Ratings$School)

NCAA_CBB_Adv_Ratings$School = gsub('NCAA', '', NCAA_CBB_Adv_Ratings$School)

NCAA_Tourney = NCAA_CBB_Adv_Ratings[(NCAA_CBB_Adv_Ratings$NCAABirth == "NCAA"),]

NCAA_Tourney = NCAA_Tourney[-c(1,11)]

NCAA_Tourney$END = NCAA_Tourney$School

write.csv(NCAA_Tourney, file = "D:/R_Practice/NCAA_Tourney/Teams.csv")


