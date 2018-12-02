if(!file.exists("Lesson2")) dir.create("C:/Users/Admin/Documents/myRcourse/Lesson2")
setwd("C:/Users/Admin/Documents/myRcourse/Lesson2")

sber_url <- "http://sbrfdata.ru/opendata.zip"
if(!file.exists("opendata.rar")) download.file(sber_url,"opendata.rar")
#unzip("opendata.rar")
sber <- read.csv("opendata.csv")

library(dplyr)
sber <- filter(sber, name=="Средний чек в формате Фастфуд")
sber$date <- format(as.Date(sber$date), format = "%Y")
sber <- filter(sber, date %in% c(2017, 2018))
sber <- group_by(sber, region)
sber <- summarize(sber, cheque=round(mean(value), 0))

regions <- read.csv("Субъекты РФ.csv", header = TRUE, sep = ";")
colnames(regions) <- c("region", "city")

sber$region <- gsub("Республика ", '', sber$region, fixed = TRUE)
sber$region <- gsub("автономный округ", "АО", sber$region, fixed = TRUE)
sber$region <- gsub("автономная область", "АО", sber$region, fixed = TRUE)
sber$region <- gsub("Алтай (Горный Алтай)", "Алтай", sber$region, fixed = TRUE)
sber$region <- gsub("^Кабардино.*", "Кабардино-Балкария", sber$region, fixed = FALSE)
sber$region <- gsub("Черкессия", "Черкесия", sber$region, fixed = TRUE)
sber$region <- gsub("^Северная Осетия.*", "Северная Осетия — Алания", sber$region, 
                    fixed = FALSE)
sber$region <- gsub("^Удмуртская.*", "Удмуртия", sber$region, fixed = FALSE)
sber$region <- gsub("^Чеченская.*", "Чечня", sber$region, fixed = FALSE)
sber$region <- gsub("^Чувашская.*", "Чувашия", sber$region, fixed = FALSE)
sber$region <- trimws(sber$region)

no_map <- sber$region[which(!sber$region %in% regions$region)]
no_map

sber <- merge(sber, regions, by = 'region')

population <- read.csv("Численность занятых.csv", skip = 2, 
                       header = TRUE, sep = ";", dec = ",")
population <- population[, c(1, 9)]
colnames(population) <- c("region", "population")

included_lines <- which(population$region=="в том числе: "| 
                                population$region=="в том числе:")-1
population <- population[-included_lines, ]

urban <- read.csv("Доля городского населения.csv", skip = 4, 
                  header = TRUE, sep = ";", dec = ",")
urban <- urban[, c(1, 8)]
colnames(urban) <- c("region", "urban_population")

included_lines <- which(urban$region=="в том числе: "| 
                                urban$region=="в том числе:")-1
urban <- urban[-included_lines, ]

income_strata <- read.csv("Распределение по величине доходов.csv", skip = 6, 
                          header = FALSE, sep = ";", dec = ",")
income_strata <- income_strata[, c(1, 8, 9)]
colnames(income_strata) <- c("region", "from_45", "above_60")
income_strata <- mutate(income_strata, target_income = from_45 + above_60)
income_strata <- select(income_strata, region, target_income)

included_lines <- which(income_strata$region=="в том числе: "| 
                                income_strata$region=="в том числе:")-1
income_strata <- income_strata[-included_lines, ]

population$region <- gsub("Республика ", "", population$region, fixed = TRUE)
population$region <- gsub("автономный округ", "АО", population$region, fixed = TRUE)
population$region <- gsub("автономная область", "АО", population$region, fixed = TRUE)
population$region <- gsub("Алтай (Горный Алтай)", "Алтай", population$region, fixed = TRUE)
population$region <- gsub("^Кабардино.*", "Кабардино-Балкария", population$region, fixed = FALSE)
population$region <- gsub("Черкессия", "Черкесия", population$region, fixed = TRUE)
population$region <- gsub("^Северная Осетия.*", "Северная Осетия — Алания", population$region, 
                          fixed = FALSE)
population$region <- gsub("^Удмуртская.*", "Удмуртия", population$region, fixed = FALSE)
population$region <- gsub("^Чеченская.*", "Чечня", population$region, fixed = FALSE)
population$region <- gsub("^Чувашская.*", "Чувашия", population$region, fixed = FALSE)
population$region <- gsub("г.", "", population$region, fixed = TRUE)
population$region <- trimws(population$region)

population$region <- gsub("^Ханты.*", "Ханты-Мансийский АО — Югра", 
                          population$region, fixed = FALSE)
population$region <- gsub("^Карачаево.*", "Карачаево-Черкесия", 
                          population$region, fixed = FALSE)

total <- merge(sber, population, by = 'region')

urban$region <- gsub("Республика ", "", urban$region, fixed = TRUE)
urban$region <- gsub("автономный округ", "АО", urban$region, fixed = TRUE)
urban$region <- gsub("автономная область", "АО", urban$region, fixed = TRUE)
urban$region <- gsub("Алтай (Горный Алтай)", "Алтай", urban$region, fixed = TRUE)
urban$region <- gsub("^Кабардино.*", "Кабардино-Балкария", urban$region, fixed = FALSE)
urban$region <- gsub("Черкессия", "Черкесия", urban$region, fixed = TRUE)
urban$region <- gsub("^Северная Осетия.*", "Северная Осетия — Алания", urban$region, 
                     fixed = FALSE)
urban$region <- gsub("^Удмуртская.*", "Удмуртия", urban$region, fixed = FALSE)
urban$region <- gsub("^Чеченская.*", "Чечня", urban$region, fixed = FALSE)
urban$region <- gsub("^Чувашская.*", "Чувашия", urban$region, fixed = FALSE)
urban$region <- gsub("г.", "", urban$region, fixed = TRUE)
urban$region <- trimws(urban$region)

urban$region <- gsub("^Ханты.*", "Ханты-Мансийский АО — Югра", 
                     urban$region, fixed = FALSE)
urban$region <- gsub("^Карачаево.*", "Карачаево-Черкесия", 
                     urban$region, fixed = FALSE)
urban$region <- gsub("^Еврейская.*", "Еврейская АО", urban$region, fixed = FALSE)
urban$region <- gsub("^Ямало.*", "Ямало-Ненецкий АО", urban$region, fixed = FALSE)

total <- merge(total, urban, by = "region")

income_strata$region <- gsub("Республика ", "", income_strata$region, fixed = TRUE)
income_strata$region <- gsub("автономный округ", "АО", income_strata$region, fixed = TRUE)
income_strata$region <- gsub("автономная область", "АО", income_strata$region, fixed = TRUE)
income_strata$region <- gsub("Алтай (Горный Алтай)", "Алтай", income_strata$region, fixed = TRUE)
income_strata$region <- gsub("^Кабардино.*", "Кабардино-Балкария", income_strata$region, fixed = FALSE)
income_strata$region <- gsub("Черкессия", "Черкесия", income_strata$region, fixed = TRUE)
income_strata$region <- gsub("^Северная Осетия.*", "Северная Осетия — Алания", income_strata$region, 
                             fixed = FALSE)
income_strata$region <- gsub("^Удмуртская.*", "Удмуртия", income_strata$region, fixed = FALSE)
income_strata$region <- gsub("^Чеченская.*", "Чечня", income_strata$region, fixed = FALSE)
income_strata$region <- gsub("^Чувашская.*", "Чувашия", income_strata$region, fixed = FALSE)
income_strata$region <- gsub("г.", "", income_strata$region, fixed = TRUE)
income_strata$region <- trimws(income_strata$region)

income_strata$region <- gsub("^Ханты.*", "Ханты-Мансийский АО — Югра", 
                             income_strata$region, fixed = FALSE)
income_strata$region <- gsub("^Карачаево.*", "Карачаево-Черкесия", 
                             income_strata$region, fixed = FALSE)
income_strata$region <- gsub("^Еврейская.*", "Еврейская АО", income_strata$region, 
                             fixed = FALSE)
income_strata$region <- gsub("^Ямало.*", "Ямало-Ненецкий АО", income_strata$region, 
                             fixed = FALSE)
income_strata$region <- gsub("^Ямало.*", "Ямало-Ненецкий АО", income_strata$region, 
                             fixed = FALSE)
income_strata$region <- gsub("^Тюменская.*", "Тюменская область", income_strata$region, 
                             fixed = FALSE)
total <- merge(total, income_strata, by = "region")

total <- mutate(total, market = round(population*urban_population/100*target_income/100, 1))
total <- select(total, region, city, population, market, cheque)

moscow <- which(total$region=="Москва")
moscow_reg <- which(total$region=="Московская область")
spb <- which(total$region=="Санкт-Петербург")
lenobl <- which(total$region=="Ленинградская область")
arkhangel <- which(total$region=="Архангельская область")
nenets <- which(total$region=="Ненецкий АО")
tyumen <- which(total$region=="Московская область")
yamal <- which(total$region=="Ямало-Ненецкий АО")
khanty <- which(total$region=="Ханты-Мансийский АО — Югра")

total[moscow_reg, "population"] <- total[moscow_reg, "population"] + 
        total[moscow, "population"]
total[lenobl, "population"] <- total[spb, "population"] + total[lenobl, "population"]
total[arkhangel, "population"] <- total[arkhangel, "population"] + total[nenets, "population"]
total[tyumen, "population"] <- total[tyumen, "population"] + total[yamal, "population"] + 
        total[khanty, "population"]

total[moscow_reg, "market"] <- total[moscow_reg, "market"] + total[moscow, "market"]
total[lenobl, "market"] <- total[spb, "market"] + total[lenobl, "market"]
total[arkhangel, "market"] <- total[arkhangel, "market"] + total[nenets, "market"]
total[tyumen, "market"] <- total[tyumen, "market"] + total[yamal, "market"] + 
        total[khanty, "market"]

moscow_share <- total[moscow, "population"] / 
        (total[moscow, "population"] + total[moscow_reg, "population"])
moscow_reg_share <- total[moscow_reg, "population"] / 
        (total[moscow, "population"] + total[moscow_reg, "population"])

spb_share <- total[spb, "population"] / 
        (total[spb, "population"] + total[lenobl, "population"])
lenobl_share <- total[lenobl, "population"] / 
        (total[spb, "population"] + total[lenobl, "population"])

arkhangel_share <- total[arkhangel, "population"] / 
        (total[arkhangel, "population"] + total[nenets, "population"])
nenets_share <- total[nenets, "population"] / 
        (total[arkhangel, "population"] + total[nenets, "population"])

tyumen_share <- total[tyumen, "population"] / 
        (total[tyumen, "population"] + total[yamal, "population"] + 
                 total[khanty, "population"])
yamal_share <- total[yamal, "population"] / 
        (total[tyumen, "population"] + total[yamal, "population"] + 
                 total[khanty, "population"])
khanty_share <- total[khanty, "population"] / 
        (total[tyumen, "population"] + total[yamal, "population"] + 
                 total[khanty, "population"])

total[moscow_reg, "cheque"] <- total[moscow_reg, "cheque"]*moscow_reg_share + 
        total[moscow, "cheque"]*moscow_share
total[lenobl, "cheque"] <- total[lenobl, "cheque"]*lenobl_share + 
        total[spb, "cheque"]*spb_share
total[arkhangel, "cheque"] <- total[arkhangel, "cheque"]*arkhangel_share + 
        total[nenets, "cheque"]*nenets_share
total[tyumen, "cheque"] <- total[tyumen, "cheque"]*tyumen_share + 
        total[yamal, "cheque"]*yamal_share + 
        total[khanty, "cheque"]*khanty_share

total <- total[-c(moscow, spb, nenets, yamal, khanty), ]

total <- arrange(total, -market)

distance <- read.csv("distance.csv", skip = 1, sep = ";", na.strings = " ")
colnames(distance) <- c("city", as.character(distance$city))

no_map <- total$city[which(!total$city %in% distance$city)]
no_map


report <- NULL

for (i in 1:nrow(total)) {
        HQ <- as.character(total$city[i])
        proximity <- filter(distance, city == HQ)
        proximity <- proximity[HQ, which(proximity %in% c(1:500))]
        proximity_list <- colnames(proximity)
        proximity_list <- proximity_list[which(proximity_list %in% total$city)]
        cluster <- c(HQ, proximity_list)
        
        city_population <- vector()
        city_market <- vector()
        city_cheque <- vector()
        city_region <- vector()
        
        for (n in seq_along(cluster)) {
                city_population[n] <- total$population[which(total$city==cluster[n])] 
                city_market[n] <- total$market[which(total$city==cluster[n])] 
                city_cheque[n] <- total$cheque[which(total$city==cluster[n])]
                city_region[n] <- total$region[which(total$city==cluster[n])]
                }

        cluster_entry <- data.frame(HQ, cluster, city_population, city_market, 
                             city_cheque, city_region)
        
        report <- rbind(report, cluster_entry)
        }


report_fin <- group_by(report, HQ)
report_fin <- summarise(report_fin, population=sum(city_population), 
                    market=sum(city_market), 
                    cheque=round(weighted.mean(city_cheque, city_population), 0))
report_fin <- select(report_fin, -population)
report_fin <- arrange(report_fin, -market)
report_fin <- filter(report_fin, market >= 500)


report_fin


par(mar=c(7,4,2,1))

barplot(height = report_fin$market, 
        names.arg = report_fin$HQ, 
        ylim = c(0, 7000),
        ylab = "Размер рынка, тыс. потребителей", 
        cex.names = 0.7,
        las = 2, 
        main = "Размер рынков кластеров",
        col = 'blue')

report_small <- filter(report_fin, market<2000)
barplot(height = report_small$market, 
        names.arg = report_small$HQ, 
        ylim = c(0, 1200),
        ylab = "Размер рынка, тыс. потребителей", 
        cex.names = 0.8,
        las = 2, 
        main = "Размер рынков кластеров, без Москвы")

barplot(height = report_fin$cheque, 
        names.arg = report_fin$HQ, 
        cex.names = 0.8,
        las = 2, 
        ylab = "Средний чек в фастфуд", 
        main = "Средний чек в выбранных кластерах")

hist(total$cheque, main = "Распределение чека по всем регионам",
     xlab = "Средний чек в категории фастфуд",
     ylab = "Частота")

print("Справка по составу кластеров регионов")
for (i in seq_along(report_fin$HQ)) {
        regions <- report$city_region[which(report$HQ==report_fin$HQ[i])]
        print(paste("Кластер", i))
        print(paste("Штаб-квартира", report_fin$HQ[i]))
        print(paste("Включает", length(regions), "регионов:", 
                    paste(regions, collapse = ", ")))
        print("")
        print("")
}
