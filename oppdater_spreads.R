library(pacman)

p_load(tidyverse, readxl, RODBC, lubridate, pryr)

setwd("F:/MB/MOA/Likviditet/Analyser/Dataprosjekt/Stamdata/JJP")


spreads_old <- spreads


last_date_spreads <- as.Date(max(spreads_old$Today))

issuetype_nt <- read_excel("issuetype.xlsx")
riskclassrisk_nt <- read_excel("riskclassrisk.xlsx")
riskclasstype_nt <- read_excel("riskclasstype.xlsx")



dbhandle <- odbcDriverConnect('driver={SQL Server};server=wm-x-s-31;database = NBDataHub;trusted_connection=true')


ISIN_spreads <- sqlQuery(dbhandle,
                         
                         paste0("
SELECT [ISIN]
      ,[PriceDate]
      ,[Spread]
      ,[CreditDuration]
  FROM [NBDataHub].[NordicBondPricing].[ISINPrices]
                             
  WHERE Pricedate > '", last_date_spreads, "'"))

# Laster ned info om ISIN fra Nordic Trustee
ISIN_info_nt <- sqlQuery(dbhandle,
                         paste0("
SELECT DISTINCT
       [ISIN]
      ,[Currency]
      ,[RiskClassRisk]
      ,[RiskClassType]
  FROM [NBDataHub].[NordicTrustee].[Issue]"))


# Koble fra server
close(dbhandle)

# Map info om ISIN med spreads og opprett løpetidsbolker
spreads_new <- ISIN_spreads%>%
  left_join(ISIN_info_nt, by = "ISIN")%>%
  left_join(riskclassrisk_nt, by = "RiskClassRisk") %>%
  left_join(riskclasstype_nt, by = "RiskClassType") %>%
  filter(Currency == "NOK") %>%
  mutate(PriceDate = as.Date(PriceDate),
         Tenor = ifelse(CreditDuration > 0.000 & CreditDuration < 0.375, "3m", NA),
         Tenor = ifelse(CreditDuration > 0.375 & CreditDuration < 0.625, "6m", Tenor),
         Tenor = ifelse(CreditDuration > 0.625 & CreditDuration < 0.875, "9m", Tenor),
         Tenor = ifelse(CreditDuration > 0.875 & CreditDuration < 1.500, "1y", Tenor),
         Tenor = ifelse(CreditDuration > 1.500 & CreditDuration < 2.500, "2y", Tenor),
         Tenor = ifelse(CreditDuration > 2.500 & CreditDuration < 3.500, "3y", Tenor),
         Tenor = ifelse(CreditDuration > 3.500 & CreditDuration < 4.500, "4y", Tenor),
         Tenor = ifelse(CreditDuration > 4.500 & CreditDuration < 5.500, "5y", Tenor),
         Tenor = ifelse(CreditDuration > 5.500 & CreditDuration < 6.500, "6y", Tenor),
         Tenor = ifelse(CreditDuration > 6.500 & CreditDuration < 7.500, "7y", Tenor),
         Tenor = ifelse(CreditDuration > 7.500 & CreditDuration < 8.500, "8y", Tenor),
         Tenor = ifelse(CreditDuration > 8.500 & CreditDuration < 9.500, "9y", Tenor),
         Tenor = ifelse(CreditDuration > 9.500 & CreditDuration < 10.500, "10y", Tenor),
         Tenor = ifelse(CreditDuration > 10.50 ,">10y", Tenor))

# Beregn daglige spreader per løpetid per risikoklasso. Bruker median for å begrense utslag av ekstremverdier i halene.
spreads_new <- spreads_new%>%
  rename(Today = PriceDate,
         RiskClass = risk_class)%>%
  group_by(Today, Tenor, RiskClass)%>%
  summarise(Spread = median(Spread, na.rm  = T))%>%
  filter(is.na(Tenor) == F,
         is.na(RiskClass) == F)%>%
  mutate(Tenor = factor(Tenor, levels = c("3m", "6m", "9m", "1y", "2y", "3y", "4y", 
                                          "5y", "6y", "7y", "8y", "9y", "10y", ">10y")),
         Spread = as.numeric(Spread))

# Slå sammen gammel og ny data
spreads <- rbind(spreads_old, spreads_new)


