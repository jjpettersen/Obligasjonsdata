library(pacman)

p_load(tidyverse, readxl, RODBC, lubridate, Rblpapi, pryr)

setwd("F:/MB/MOA/Likviditet/Analyser/Dataprosjekt/Stamdata/JJP")

# Last inn gammelt datasett
load("obligasjonsdata_dashboard.rda")

# Lag dataframe som inneholder gammel spread-data
spreads_old <- spreads

# Siste dato i gammel spread-data. Brukes i innhenting av ny data gjennom SQL
last_date_spreads <- as.Date(max(spreads_old$Today))

#laster inn mapping mellom numeric og tekst for issue type, issue risk etc.
issuetype_nt <- read_excel("issuetype.xlsx")
riskclassrisk_nt <- read_excel("riskclassrisk.xlsx")
riskclasstype_nt <- read_excel("riskclasstype.xlsx")
country_codes <- read_excel("country_codes.xlsx")


# Tranche-data hentet fra Stamdata. Ingenting er filtrert bort, Observasjoner ut 2015.
tranche_excel <- read_xlsx("tranche_ut_2015.xlsx")

tranche_excel$Issuer <- str_remove(tranche_excel$Issuer, "_")


# Se kun på NO-ISIN (i databasen har vi bare NO-ISIN)
# Fiks kolonne-navn slik at de samsvarer med kolonnene i databasen, fiks noen andre småting. 
tranche_excel <- tranche_excel%>%
  filter(substr(ISIN, 1, 2) == "NO")%>%
  rename(Today = `Settlement Date`,
         CurrentCouponRate       = `Current Coupon`,
         MaturityDate            = `Maturity Date`,
         IssuedAmount            = `Issued Amount`,
         Issuer_Name             =  Issuer,
         Issuer_Country          =  Country,
         IssueType               = `Issue Type`,
         CurrentInterestType     = `Current Return Type`,
         RiskClassRisk           =  Risk,
         Issuer_IndustryGrouping =  `Industry Group`)%>%
  group_by(ISIN)%>%
  mutate(IssueDate = min(Today))%>%
  select(Today, ISIN, Issuer_IndustryGrouping, Issuer_Country, IssueDate, Issuer_Name, IssueType, Currency, CurrentCouponRate, CurrentInterestType, MaturityDate, IssuedAmount, RiskClassRisk)%>%
  group_by(Today, ISIN, IssueDate, Issuer_IndustryGrouping, Issuer_Country, Currency, IssueType, MaturityDate, Issuer_Name, CurrentInterestType, RiskClassRisk)%>%
  summarise(CurrentCouponRate = mean(CurrentCouponRate),
            IssuedAmount = sum(IssuedAmount, na.rm = T))%>%
  group_by(ISIN)%>%
  mutate(CurrentOutstandingAmount = cumsum(IssuedAmount),
         CurrentInterestType = ifelse(CurrentInterestType == "Zero Coupon", "Zero", CurrentInterestType))




# BLOOMBERG -----------------------------------------------------------------------------------

# Koble til Bloomberg
blpConnect()

# Last inn gammel Bloomberg-data
load("data_raw_bb.rda")
load("tranche_bb.rda")

tranche_bb_old <- tranche_bb


# INFO -----------------------------------------------------------------------------------------------------------------------------------------------------


info_old <- info_raw

# Henter tickere fra Bloomberg SRCH, "norske". Fjerner tickere vi allerede har info om
tickers_norske_new <- bsrch("FI:norske")%>%
  filter(!(id %in% info_old$TICKER))


tickers_norske_new <- tickers_norske_new$id %>% as.character()

# Hent disse kolonnene fra Bloomberg
info_norske_new <- bdp(tickers_norske_new,
                c("LONG_COMP_NAME",
                  "TICKER",
                  "ID_ISIN",
                  "DES_NOTES",
                  "CRNCY",
                  "ISSUE_DT",
                  "ORIGINAL_AMOUNT_SOLD",
                  "AMT_ISSUED",
                  "MATURITY",
                  "CALLED_DT",
                  "RESET_IDX",
                  "REFIX_FREQ",
                  "FIRST_CPN_DT",
                  "CPN_TYP",
                  "CPN",
                  "BICS_LEVEL_1_NAME",
                  "BICS_LEVEL_2_INDUSTRY_GROUP_NAME",
                  "COLLAT_TYP",
                  "PAYMENT_RANK",
                  "BASEL_III_DESIGNATION"))%>%
  filter(substr(ID_ISIN, 1,2) != "NO")%>%
  mutate(Issuer_Country = "Norway")

info_norske_new <- rownames_to_column(info_norske_new) %>% rename(id = rowname)


# Henter tickere fra Bloomberg SRCH, "utenlandske_nok". Fjerner tickere vi allerede har info om

tickers_utenlandske_NOK_new <- bsrch("FI:utenlandske_nok")%>%
  filter(!(substr(id,1,8) %in% substr(info_old$id,1,8)))

tickers_utenlandske_NOK_new <- tickers_utenlandske_NOK_new$id %>% as.character()

# Hent disse kolonnene
info_utenlandske_NOK_new <- bdp(tickers_utenlandske_NOK_new,
                            c("LONG_COMP_NAME",
                              "CNTRY_OF_INCORPORATION",
                              "TICKER",
                              "ID_ISIN",
                              "ID_BB",
                              "DES_NOTES",
                              "CRNCY",
                              "ISSUE_DT",
                              "ORIGINAL_AMOUNT_SOLD",
                              "AMT_ISSUED",
                              "MATURITY",
                              "CALLED_DT",
                              "RESET_IDX",
                              "REFIX_FREQ",
                              "FIRST_CPN_DT",
                              "CPN_TYP",
                              "CPN",
                              "BICS_LEVEL_1_NAME",
                              "BICS_LEVEL_2_INDUSTRY_GROUP_NAME",
                              "COLLAT_TYP",
                              "PAYMENT_RANK",
                              "BASEL_III_DESIGNATION"))%>%
  filter(substr(ID_ISIN, 1,2) != "NO")%>%
  rename(Issuer_CountryCode = CNTRY_OF_INCORPORATION)%>%
  left_join(country_codes)%>%
  select(-Issuer_CountryCode)%>%
  rename(id = ID_BB)
    

info_raw <- bind_rows(info_old, info_norske_new, info_utenlandske_NOK_new)%>%
  ungroup()%>%
  unique()%>%
  filter(ISSUE_DT <= Sys.Date())

# Mistenker at info er lagt inn feil i Bloomberg for disse ISIN (2000 mrd NOK i én av dem f.eks)
info_raw <- info_raw%>%
  mutate(AMT_ISSUED = ifelse(ID_ISIN %in% c("DE000UE9J9J8",
                                            "XS1517311988",
                                            "XS1491002025",
                                            "XS1391088843",
                                            "XS1425265144",
                                            "XS1491002025",
                                            "XS1491001993",
                                            "XS1515377742",
                                            "XS1427243594"
                                            
                                            ),
                             AMT_ISSUED / 10^3,
                             AMT_ISSUED))

# Noen utstedelser er "funged". Fanger dette opp gjennom description notes-kolonnen, hvis den inneholder ordet "FUNGED".
# Finner datoen i description notes og lager FungedDate, leter på to mest brukte dato-formater
info <- info_raw%>%
  mutate(funged = ifelse(str_detect(DES_NOTES, "FUNGED"), 1, 0),
         FungedDate = ifelse(funged == 1, gsub("[^0-9./-]", "", DES_NOTES), NA),
         FungedDate = as.Date(parse_date_time(FungedDate, c("%m/%d/%y", "%Y-%m-%d"))))

info <- info%>%
  select(-DES_NOTES)%>%
  rename(Issuer_Name         = LONG_COMP_NAME,
         ISIN                = ID_ISIN,
         Currency            = CRNCY,
         IssueDate           = ISSUE_DT,
         IssuedAmount        = AMT_ISSUED,
         MaturityDate        = MATURITY,
         EarlyRedeemedDate   = CALLED_DT,
         ReferenceRate       = RESET_IDX,
         CurrentInterestType = CPN_TYP,
         CurrentCouponRate   = CPN,
         Industry            = BICS_LEVEL_1_NAME,
         IssueType           = PAYMENT_RANK,
         RiskClassRisk       = COLLAT_TYP
         
  )

info <- info%>%
  mutate(Today = IssueDate)



# Henter tickere for reopenings for norske og utenlandske, gjennom SRCH "norske_funged_reopened" og "utenlandske_funged_reopened"
tickers_reopenings_norske <- bsrch("FI:norske_funged_reopened")
tickers_reopenings_utenlandske <- bsrch("FI:utenlandske_funged_reopened")


isin_reopenings <- info%>%filter(id %in% tickers_reopenings_norske$id | 
                                   substr(id,1,8) %in% substr(tickers_reopenings_utenlandske$id,1,8))

tickers_reopenings <- unique(isin_reopenings$id)


# Tickere for flytrentepapirer
tickers_floating <- info%>%select(id, CurrentInterestType)%>%filter(CurrentInterestType == "FLOATING")
tickers_floating <- unique(tickers_floating$id)



# sett til NA kupongrente om papiret er flyt og NA volum dersom reopened (henter denne infoen separat senere)
info <- info%>%
  mutate(CurrentCouponRate        = ifelse(id %in% tickers_floating, NA, CurrentCouponRate),
         IssuedAmount = ifelse(is.na(ORIGINAL_AMOUNT_SOLD), IssuedAmount, ORIGINAL_AMOUNT_SOLD),
         CurrentOutstandingAmount = ifelse(id %in% tickers_reopenings, NA, IssuedAmount),
         Today = as.Date(Today),
         IssueDate = as.Date(IssueDate))%>%
  group_by(id)%>%
  mutate(MaturityDate = as.Date(ifelse(is.na(EarlyRedeemedDate) & is.na(FungedDate), 
                                       as.Date(MaturityDate), 
                                       as.Date(min(EarlyRedeemedDate, FungedDate, na.rm = T))), 
                                origin = "1970-01-01"))%>%
  ungroup()%>%
  filter(is.na(MaturityDate) == F,
         is.na(IssuedAmount) == F,
         !(is.na(REFIX_FREQ)   == T & CurrentInterestType == "FLOATING"))

# Brukes til å lage oversikt over reset-datoer for flytrentepapirer (brukes til å filtrere søk for å begrense data pull fra Bloomberg)
reset_frequencies <- data.frame(REFIX_FREQ = c(1, 2, 4, 12, 52, 360),
                                frequency = c("year", "2 quarter", "quarter", "month", "week", "day"))

# Antar fastrentepapir hvis kolonnen om rentetype er FUNGED eller DEFAULTED
info <- info%>%
  left_join(reset_frequencies)%>%
  mutate(CurrentInterestType = ifelse(CurrentInterestType %in% c("FUNGED", "DEFAULTED"),
                                      "FIXED",
                                      CurrentInterestType))


# FIKSE VARIABELNAVN, MAPPING MOT STAMDATA-KLASSIFISERING ------------------------------------------------------------------------------------

# Fikser navn som inneholder æ,ø, å
sd_names_industry <- tranche_daily%>%ungroup()%>%select(Issuer_Name, Issuer_IndustryGrouping)%>%
  unique()%>%
  mutate(Issuer_Name_sd = Issuer_Name,
         
         Issuer_Name_temp = tolower(Issuer_Name),
         
         Issuer_Name_temp_1 = str_replace_all(Issuer_Name_temp, "å", "aa"),
         Issuer_Name_temp_1 = str_replace_all(Issuer_Name_temp_1, "æ", "ae"),
         Issuer_Name_temp_1 = str_replace_all(Issuer_Name_temp_1, "ø", "oe"),
         
         Issuer_Name_temp_2 = str_replace_all(Issuer_Name_temp, "å", "a"),
         Issuer_Name_temp_2 = str_replace_all(Issuer_Name_temp_2, "ø", "o"))%>%
  select(-Issuer_Name)


info <- info%>%
  mutate(Issuer_Name_low = tolower(Issuer_Name))%>%
  left_join(sd_names_industry%>%select(-Issuer_Name_temp, -Issuer_Name_temp_2), by = c("Issuer_Name_low" = "Issuer_Name_temp_1"))%>%
  left_join(sd_names_industry%>%select(-Issuer_Name_temp, -Issuer_Name_temp_1), by = c("Issuer_Name_low" = "Issuer_Name_temp_2"))%>%
  mutate(Issuer_Name = ifelse(is.na(Issuer_Name_sd.x),
                              ifelse(is.na(Issuer_Name_sd.y),
                                     Issuer_Name, Issuer_Name_sd.y),
                              Issuer_Name_sd.x))%>%
  select(-Issuer_Name_low, -Issuer_Name_sd.x, -Issuer_Name_sd.y)%>%
  mutate(Issuer_IndustryGrouping = ifelse(is.na(Issuer_IndustryGrouping.x),
                                          Issuer_IndustryGrouping.y,
                                          Issuer_IndustryGrouping.x))%>%
  select(-Issuer_IndustryGrouping.x, -Issuer_IndustryGrouping.y)




Industry_Mapping_bb <- data.frame("Issuer_IndustryGrouping_bb" = c("Banks",
                                                                   "Finance","Commercial Finance","Diversified Banks","Financial Services","Funds & Trusts",
                                                                   "Real Estate",
                                                                   "Life Insurance", "Property & Casualty Insurance",
                                                                   "Biotechnology", "Health Care Facilities & Services", "Managed Care", "Medical Equipment & Devices Manufacturing",
                                                                   "Pharmaceuticals",
                                                                   "Sovereigns", "Government Agencies",
                                                                   "Government Regional", "Supranationals", "Government Development Banks", "Winding Up Agencies", "Central Bank", "Government Local",
                                                                   "Transportation & Logistics",
                                                                   "Power Generation", "Utilities", "Pipeline",
                                                                   "Forest & Paper Products Manufacturing", "Containers & Packaging",
                                                                   "Airlines", "Railroad", "Travel & Lodging",
                                                                   "Internet Media", "Advertising & Marketing",
                                                                   "Consumer Services",
                                                                   "Exploration & Production",
                                                                   "Oil & Gas Services & Equipment",
                                                                   "Food & Beverage",
                                                                   "Semiconductors", "Software & Services", "Communications Equipment", "Wireline Telecommunications Services",
                                                                   "Waste & Environment Services & Equipment"
),

"sd_equivalent"              = c("Bank",
                                 "Finance", "Finance", "Finance", "Finance", "Finance",
                                 "Real Estate",
                                 "Insurance", "Insurance",
                                 "Health Care", "Health Care", "Health Care", "Health Care",
                                 "Pharmaceuticals",
                                 "Government", "Government",
                                 "Public Sector", "Public Sector", "Public Sector", "Public Sector", "Public Sector", "Public Sector",
                                 "Shipping",
                                 "Utilities", "Utilities", "Utilities",
                                 "Pulp, paper and forestry", "Pulp, paper and forestry",
                                 "Transportation", "Transportation", "Transportation",
                                 "Media", "Media",
                                 "Consumer Services",
                                 "Oil and gas E&P",
                                 "Oil and gas services",
                                 "Convenience Goods",
                                 "Telecom/IT", "Telecom/IT", "Telecom/IT", "Telecom/IT",
                                 "Industry"
)
)

CurrentInterestType_Mapping_bb <- data.frame("CurrentInterestType_bb" = c("VARIABLE",
                                                                          "FIXED",
                                                                          "FLOATING",
                                                                          "ZERO COUPON",
                                                                          "STEP CPN",
                                                                          "PAY-IN-KIND",
                                                                          "FLAT TRADING",
                                                                          "EXCHANGED"),
                                             
                                             "sd_equivalent"          = c("Adjust",
                                                                          "Fixed",
                                                                          "FRN",
                                                                          "Zero",
                                                                          "Step",
                                                                          "Pay-in-kind",
                                                                          "Flat trading",
                                                                          "Exchanged")
)




RiskClassRisk_mapping_bb <- data.frame("RiskClassRisk_bb" = c("SR UNSECURED",
                                                              "COVERED",
                                                              "SR SECURED",
                                                              "JR SUBORDINATED",
                                                              "SUBORDINATED",
                                                              "COMPANY GUARNT",
                                                              "PASS THRU CERTS",
                                                              "UNSECURED",
                                                              "SECURED",
                                                              "NOTES",
                                                              "UNSUBORDINATED",
                                                              "DEBENTURES",
                                                              "SENIOR NOTES",
                                                              "SR UNSUB",
                                                              "DEPOSIT NOTES",
                                                              "BANK GUARANTEED",
                                                              "SUB NOTES",
                                                              "SUB DEBENTURES",
                                                              "GOVT GUARANTEED",
                                                              "LOCAL GOVT GUARN",
                                                              "SR SUBORDINATED",
                                                              "SR SUB NOTES"),
                                       
                                       "sd_equivalent"    = c("Senior Unsecured",
                                                              "Covered Bonds",
                                                              "Senior Secured",
                                                              "Subordinated",
                                                              "Subordinated",
                                                              "Company Guaranteed",
                                                              "Pass-through",
                                                              "Unsecured",
                                                              "Secured",
                                                              "Exchange Traded Notes",
                                                              "Unsubordinated",
                                                              "Debentures",
                                                              "Notes",
                                                              "Unsubordinated",
                                                              "Notes",
                                                              "Unsecured",
                                                              "Subordinated",
                                                              "Subordinated",
                                                              "Government Guaranteed",
                                                              "Municipality Guaranteed",
                                                              "Subordinated",
                                                              "Subordinated"
                                       )
)

# Fikser småting
info <- info%>%
  left_join(Industry_Mapping_bb, by = c("BICS_LEVEL_2_INDUSTRY_GROUP_NAME" = "Issuer_IndustryGrouping_bb"))%>%
  mutate(Issuer_IndustryGrouping = ifelse(is.na(Issuer_IndustryGrouping), sd_equivalent, Issuer_IndustryGrouping))%>%
  select(-sd_equivalent)%>%
  left_join(CurrentInterestType_Mapping_bb, by = c("CurrentInterestType" = "CurrentInterestType_bb"))%>%
  mutate(CurrentInterestType = sd_equivalent)%>%
  select(-sd_equivalent)%>%
  mutate(RiskClassRisk_tmp = ifelse(RiskClassRisk %in% c("CERT OF DEPOSIT", "BONDS", "UNITS", "PFANDBRIEFS"), IssueType, RiskClassRisk),
         IssueType_tmp     = ifelse(RiskClassRisk %in% c("CERT OF DEPOSIT", "BONDS", "UNITS", "PFANDBRIEFS"), RiskClassRisk, IssueType),
         RiskClassRisk = RiskClassRisk_tmp,
         IssueType = IssueType_tmp)%>%
  select(-IssueType_tmp, -RiskClassRisk_tmp)%>%
  mutate(RiskClassRisk = toupper(RiskClassRisk),
         IssueType = ifelse(IssueType == "CERT OF DEPOSIT", "CDs", IssueType),
         IssueType = ifelse(BASEL_III_DESIGNATION == "Additional Tier 1", "Additional Tier 1", IssueType),
         IssueType = ifelse(BASEL_III_DESIGNATION == "Tier 1", "Tier 2", IssueType),
         IssueType = ifelse(BASEL_III_DESIGNATION == "Tier 2", "Tier 2", IssueType),
         IssueType = ifelse(BASEL_III_DESIGNATION == "Tier 3", "Tier 3", IssueType),
         IssueType = ifelse(IssueType %in% c("Tier 3", "Sr Non Preferred"), "Tier 3", IssueType),
         IssueType = ifelse(IssueType %in% c("CDs", "Additional Tier 1", "Tier 1", "Tier 2", "Tier 3"),
                            IssueType,
                            "Bonds"))%>%
  left_join(RiskClassRisk_mapping_bb, by = c("RiskClassRisk" = "RiskClassRisk_bb"))%>%
  mutate(RiskClassRisk = sd_equivalent)%>%
  select(-sd_equivalent)%>%
  mutate(RiskClassRisk = ifelse(RiskClassRisk %in% c("Additional Tier 1", "Tier 2", "Tier 3"),
                                IssueType,
                                RiskClassRisk))%>%
  ungroup()%>%
  mutate(RiskClassRisk = ifelse(IssueType == "Tier 3", "Tier 3", RiskClassRisk))%>%
  unique()



# REOPENINGS ---------------------------------------------------------------------------------------------------------------------

# Tickere for aktive papirer med reopenings
tickers_reopenings_norske_active <- bsrch("FI:norske_funged_reopened_active")
tickers_reopenings_utenlandske_active <- bsrch("FI:utenlandske_funged_reopened_active")


tickers_reopenings_active <- unique(c(tickers_reopenings_norske_active$id,
                                      tickers_reopenings_utenlandske_active$id)
)

# Hent data. Brukes til å sammenlikne utestående beløp med vår siste observasjon for utestående beløp
data_latest_reopenings <- bdp(tickers_reopenings_active,
                              c("TICKER",
                                "ID_ISIN",
                                "AMT_OUTSTANDING"))%>%
  filter(substr(ID_ISIN,1,2) != "NO")




data_latest_reopenings <- rownames_to_column(data_latest_reopenings)%>%rename(id = rowname)


outstanding_compare <- data_latest_reopenings%>%
  mutate(id_temp = substr(id,1,9))%>%
  left_join(tranche_bb_old%>%
              ungroup()%>%
              select(id, CurrentOutstandingAmount, Today)%>%
              mutate(id_temp = substr(id,1,9))%>%
              group_by(id)%>%
              filter(Today == max(Today)),
            by = "id_temp")%>%
  mutate(diff = AMT_OUTSTANDING - CurrentOutstandingAmount)%>%
  filter(diff != 0 | is.na(diff))


# Hent data for disse tickerne 
tickers_fetch_reopenings_new <- outstanding_compare$id.y

# Hvis det finnes papirer hvor det er diff,
if (length(tickers_fetch_reopenings_new) > 0) {
  
  reopenings_old <- reopenings
  
  
  
  reopenings_new <- lapply(tickers_fetch_reopenings_new, bds, "amount_outstanding_history")
  
  
  for(i in (1:length(tickers_fetch_reopenings_new))){
    if(length(reopenings_new[[i]]) == 1){
      reopenings_new[[i]] <- data.frame(reopenings_new[[i]][1])
    }else{
      
    }
  }
  
  names(reopenings_new) <- tickers_fetch_reopenings_new
  
  for(i in tickers_fetch_reopenings_new){
    if(nrow(reopenings_new[[i]]) > 0){
      names(reopenings_new[[i]]) <- c("CurrentOutstandingAmount", "Today")
    }else{
      reopenings_new[[i]] <- NULL
      
    }
    
  }
  
  
  
  tickers_fetch_reopenings_left <- names(reopenings_new)
  
  
  for(i in tickers_fetch_reopenings_left){
    reopenings_new[[i]] <- reopenings_new[[i]] %>%
      mutate(id = paste0(i)
      )
    
  }
  
  
  reopenings_new <- bind_rows(reopenings_new, .id = "id")
  
  reopenings <- bind_rows(reopenings_old%>%ungroup()%>%filter(!(id %in% reopenings_new$id)),
                          reopenings_new)
  
  
}else{
  
}



# COUPON HISTORY ------------------------------------------------------------------------------------------------------------------------------

# Henter data for flytrentepapirer hvor det har vært reset siden forrige observasjon

tickers_floating <- info%>%select(id, CurrentInterestType)%>%filter(CurrentInterestType == "FRN")
tickers_floating <- unique(tickers_floating$id)


coupon_data <- list()
for (i in tickers_floating) {
  coupon_data[[i]] <- info%>%filter(id == i)%>%select(FIRST_CPN_DT, MaturityDate, frequency)
  
}



coupon_schedules <- list()

for(i in tickers_floating){
  coupon_schedules[[i]] <- data.frame("RefixDate" = 
                                        seq.Date(coupon_data[[i]]$FIRST_CPN_DT, 
                                                 coupon_data[[i]]$MaturityDate,
                                                 coupon_data[[i]]$frequency),
                                      id = paste(i))
}

coupon_schedules <- bind_rows(coupon_schedules)

fetch_coupon_tickers <- coupon_schedules%>%
  filter(RefixDate > update_bb,
         RefixDate <= Sys.Date())%>%
  mutate(id = paste0(substr(id,1,9), " Corp"))


fetch_coupon_tickers <- unique(fetch_coupon_tickers$id)


if (length(fetch_coupon_tickers) > 0) {
  
  
  coupon_history_old <- coupon_history
  
  
  coupon_history_new <- lapply(fetch_coupon_tickers, bds, "floater_acc_schedule")
  
  
  names(coupon_history_new) <- fetch_coupon_tickers
  
  for(i in fetch_coupon_tickers){
    if(is.null(nrow(coupon_history_new[[i]][[1]])) == T){
      coupon_history_new[[i]] <- NULL
    }
    
  }
  
  for(i in (1:length(coupon_history_new))){
    coupon_history_new[[i]][[1]] <- coupon_history_new[[i]][[1]] %>%
      mutate(id = names(coupon_history_new)[[i]]
      )
    
  }
  
  for(i in (1:length(coupon_history_new))){
    coupon_history_new[[i]] <- data.frame(coupon_history_new[[i]][1])
    names(coupon_history_new[[i]]) <- c("Today", "CurrentCouponRate", "id")
    
  }
  
  
  coupon_history_new <- bind_rows(coupon_history_new, .id = "id")%>%
    filter(Today < Sys.Date())
  
  coupon_history <- bind_rows(coupon_history_old%>%
                                ungroup()%>%
                                filter(!(id %in% coupon_history_new$id)), 
                              
                              coupon_history_new)
}else{
  
}

update_bb <- Sys.Date()





save(coupon_history, reopenings, info_raw, update_bb, file = "data_raw_bb.rda")






# FERDIGSTILL DATASETTET tranche_bb ---------------------------------------------------------------------------------------------------------------------------------------

fx_crosses_bb <- info%>%ungroup()%>%select(Currency)%>%unique()

fx_crosses_bb <- unique(fx_crosses_bb$Currency)


fx_crosses_bb <- paste0(paste(fx_crosses_bb, collapse = "+"),".NOK.")

fx_rates <- read.csv(url(paste0("https://data.norges-bank.no/api/data/EXR/B.",fx_crosses_bb, "SP?format=csv&startPeriod=1914-01-01&endPeriod=", Sys.Date(), "&locale=no&bom=include")), sep = ";")


# Endre kolonnenavn og fiks format slik at valutakurs-dataen kan brukes
fx_rates <- fx_rates%>%
  select(TIME_PERIOD, BASE_CUR, OBS_VALUE, UNIT_MULT)%>%
  rename(Today = TIME_PERIOD, Currency = BASE_CUR, fx_rate = OBS_VALUE)%>%
  mutate(Today = as.Date(Today),
         fx_rate = as.numeric(str_replace(fx_rate, ",", "."))/10^UNIT_MULT)%>%
  select(-UNIT_MULT)

fx_rates <- data.frame("Today" = seq.Date(min(fx_rates$Today), max(fx_rates$Today), "day"))%>%
  bind_rows(fx_rates)%>%
  complete(Today, Currency)%>%
  group_by(Currency)%>%
  arrange(., Today)%>%
  fill(fx_rate, .direction = "downup")

info <- info%>%
  mutate(CurrentOutstandingAmount = ifelse(id %in% reopenings$id, NA, CurrentOutstandingAmount))


info <- info%>%
  select(ISIN, Issuer_IndustryGrouping, IssueDate, Issuer_Name, Currency, 
         IssueType, Currency, CurrentCouponRate, CurrentInterestType, 
         MaturityDate, IssuedAmount, RiskClassRisk, id, Issuer_Country)%>%
  filter(substr(ISIN,1,2) != "NO")%>%
  mutate(id = ifelse(str_detect(id, "Corp"),
                     id,
                     paste0(id, " Corp")))

tranche_bb <- bind_rows(coupon_history, reopenings, info)%>%
  left_join(info)%>%
  group_by(id)%>%
  arrange(., Today)%>%
  fill(everything(), .direction = "downup")%>%
  ungroup()%>%
  unique()%>%
  mutate(Today = as.Date(ifelse(is.na(Today), IssueDate, Today), origin = "1970-01-01"))


tranche_bb <- tranche_bb%>%
  ungroup()%>%
  group_by(id)%>%
  arrange(.,Today)%>%
  left_join(fx_rates)%>%
  mutate(fx_rate = first(fx_rate),
         CurrentOutstandingAmount = ifelse(id %in% reopenings$id, CurrentOutstandingAmount, IssuedAmount),
         CurrentOutstandingAmountNOK = ifelse(Currency != "NOK",
                                              CurrentOutstandingAmount * fx_rate,
                                              CurrentOutstandingAmount),
         IssuedAmount = ifelse(Today == first(Today), 
                               CurrentOutstandingAmount,
                               CurrentOutstandingAmount - lag(CurrentOutstandingAmount, 1)),
         IssuedAmountNOK = ifelse(Currency != "NOK",
                                  IssuedAmount * fx_rate,
                                  IssuedAmount),
         delta_rate = ifelse(Today == first(Today), 
                             CurrentCouponRate,
                             CurrentCouponRate - lag(CurrentCouponRate, 1)),
         MaturityDate = max(MaturityDate))%>%
  filter((IssuedAmount != 0 | delta_rate != 0),
         Today <= MaturityDate)%>%
  mutate(CurrentCouponRate = ifelse((CurrentCouponRate >= -10^(-13) & CurrentCouponRate < -10^(-15)),
                                    NA,
                                    CurrentCouponRate))%>%
  fill(CurrentCouponRate, .direction = "downup")


matured_bb <- tranche_bb%>%
  filter(MaturityDate <= Sys.Date())%>%
  group_by(id)%>%
  filter(Today == max(Today))%>%
  mutate(Today = MaturityDate,
         IssuedAmount = - CurrentOutstandingAmount,
         IssuedAmountNOK = - CurrentOutstandingAmountNOK,
         Today = MaturityDate,
         CurrentOutstandingAmount = 0,
         CurrentOutstandingAmountNOK = 0)




tranche_bb <- bind_rows(tranche_bb, matured_bb)

save(tranche_bb, file = "tranche_bb.rda")

















# Koble til server
dbhandle <- odbcDriverConnect('driver={SQL Server};server=wm-x-s-31;database = NBDataHub;trusted_connection=true')


# Hent data
tranche_sql <- sqlQuery(dbhandle,
                        
                        
                        paste0("
WITH

data AS(

SELECT [Today]
      ,[ISIN]
      ,[IssueDate]
      ,[MaturityDate]
      ,[CurrentOutstandingAmount]
      ,[Currency]
      ,[Issuer_CountryCode]
	    ,[CurrentCouponRate]
	    ,[CurrentMargin]
	    ,[Issuer_Name]
	    ,[CurrentInterestType]
	    ,[IssueType]
	    ,[EarlyRedeemedDate]
	    ,CurrentOutstandingAmount - LAG(CurrentOutstandingAmount) OVER (PARTITION BY ISIN ORDER BY Today) AS diff
	    ,CurrentCouponRate - LAG(CurrentCouponRate) OVER (PARTITION BY ISIN ORDER BY Today) AS diff_rate
	    ,ROW_NUMBER() OVER (PARTITION BY ISIN ORDER BY Today) AS rn
  FROM [NBDataHub].[NordicTrustee].[IssueHistory]

  WHERE Today >= '2016-01-01' AND CurrentOutstandingAmount IS NOT NULL AND Today >= IssueDate)

SELECT * FROM data
WHERE diff != 0 OR diff_rate != 0 OR rn = 1"))

# Hent informasjon per ISIN
issue_info <- sqlQuery(dbhandle,
                       
                       
                       paste0("
                       
SELECT [ISIN]
	    ,[RiskClassRisk]
	    ,[RiskClassType]
	    ,[Issuer_IndustryGrouping]
	    ,[Issuer_SectorCode]
	    ,[Issuer_OrganizationNumber]


  FROM [NBDataHub].[NordicTrustee].[Issue]"))

issue_info <- issue_info%>%
  group_by(Issuer_OrganizationNumber)%>%
  fill(RiskClassRisk, .direction = "downup")%>%
  fill(RiskClassType, .direction = "downup")%>%
  fill(Issuer_IndustryGrouping, .direction = "downup")

issue_info_sql_NA_ISIN <- issue_info%>%filter(is.na(RiskClassRisk))

issue_info_sql_NA_ISIN <- issue_info_sql_NA_ISIN$ISIN

# sql_info_NA_data <- bdp(paste0(issue_info_sql_NA_ISIN, " Corp"), c("COLLAT_TYP", "PAYMENT_RANK", "ID_ISIN"))

Mapping_SQL_NA <- sql_info_NA_data%>%
  mutate(RiskClassRisk_tmp = ifelse(COLLAT_TYP %in% c("CERT OF DEPOSIT", "BONDS", "UNITS", "PFANDBRIEFS"), PAYMENT_RANK, COLLAT_TYP),
         IssueType_tmp     = ifelse(COLLAT_TYP %in% c("CERT OF DEPOSIT", "BONDS", "UNITS", "PFANDBRIEFS"), COLLAT_TYP, PAYMENT_RANK),
         RiskClassRisk = RiskClassRisk_tmp,
         IssueType = IssueType_tmp,
         RiskClassRisk_tmp = toupper(RiskClassRisk_tmp))%>%
  left_join(RiskClassRisk_mapping_bb, by = c("RiskClassRisk_tmp" = "RiskClassRisk_bb"))%>%
  mutate(RiskClassRisk_tmp = sd_equivalent)%>%
  rename(ISIN = ID_ISIN)%>%
  select(ISIN, RiskClassRisk_tmp)%>%
  filter(is.na(RiskClassRisk_tmp) == F)
  
  

# Last ned spreader (se bort fra datoer vi allerede har i datasettet)
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

# Lag dataframe som mapper IssueType i regnearket mot IssueType i data som hentes fra SQL
mapping_IssueType <- data.frame(
  "IssueTypeNew" = c("Bonds", "CDs", "Convertibles", "Tier 2", "Linked Notes", 
                     "Warrants", "Tier 1", "Credit Linked Notes", "Exchangeable",
                     "Tier 3", "Claim, Interest", "Claim, Redemption"),
  
  "IssueType"    = c(0,1,2,3,4,6,7,8,9,11,12,13))

# Mapping av RiskClassRisk
mapping_RiskClassRisk <- data.frame(
  "RiskClassRiskNew" = c("Government Guaranteed", "Municipality Guaranteed", "Covered Bonds", "Junior Covered Bonds",
                         "Mortgage Bonds (Danish-RO)", "Senior Unsecured", "Structured Products", "Structured Products, 20",
                         "Exchange Traded Notes", "Tier 3", "Tier 2", "Additional Tier 1", "Government Guaranteed",
                         "Government Enterprise", "Municipality Guaranteed", "Super Senior Secured", "Senior Secured",
                         "Senior Unsecured", "Structured Products", "Subordinated", "Government Guaranteed",
                         "Municipality", "Government"),
  
  "RiskClassRisk"   = c(110,130,140,141,142,150,160,161,162,165,170,180,210,220,230,250,251,252,260,270,310,330,410)
)

# Det er noen rariteter i datasettet som hentes gjennom SQL. Fikser kupongrente.
tranche_sql <- tranche_sql%>%
  group_by(ISIN)%>%
  mutate(CurrentCouponRate = ifelse(CurrentCouponRate == 0 & CurrentMargin != 0, 
                                    lead(CurrentCouponRate),
                                    CurrentCouponRate))%>%
  select(-CurrentMargin)


# Mapping 
tranche_sql <- tranche_sql%>%
  left_join(issue_info)%>%
  left_join(mapping_IssueType)%>%
  mutate(IssueType = IssueTypeNew)%>%
  select(-IssueTypeNew)%>%
  left_join(mapping_RiskClassRisk)%>%
  mutate(RiskClassRisk = RiskClassRiskNew)%>%
  select(-RiskClassRiskNew)%>%
  left_join(mapping_Issuer_IndustryGrouping)%>%
  mutate(Issuer_IndustryGrouping = Issuer_IndustryGroupingNew)%>%
  select(-Issuer_IndustryGroupingNew)%>%
  left_join(country_codes)%>%
  select(-Issuer_CountryCode)%>%
  left_join(Mapping_SQL_NA, by = "ISIN")%>%
  mutate(RiskClassRisk = ifelse(is.na(RiskClassRisk), RiskClassRisk_tmp, RiskClassRisk))%>%
  select(-RiskClassRisk_tmp)


# Hent valutakurser fra NB. Utstedelser i valuta har ikke oppgitt beløp i kroner.
# Beregner verdier i kroner. Bruker kurs ved utstedelsesdato per ISIN, slik at utestående i kroner blir 0 ved forfall
fx_crosses_sd <- tranche_excel%>%ungroup()%>%select(Currency)%>%filter(Currency != "NOK")%>%unique()

fx_crosses_sql <- tranche_sql%>%select(Currency)%>%filter(Currency != "NOK")%>%unique()

fx_crosses_bb <- tranche_bb%>%select(Currency)%>%unique()


fx_crosses <- unique(c(fx_crosses_sql$Currency, fx_crosses_sd$Currency, fx_crosses_bb$Currency))

fx_crosses <- paste0(paste(fx_crosses, collapse = "+"),".NOK.")

fx_rates <- read.csv(url(paste0("https://data.norges-bank.no/api/data/EXR/B.",fx_crosses, "SP?format=csv&startPeriod=1914-01-01&endPeriod=", Sys.Date(), "&locale=no&bom=include")), sep = ";")


# Endre kolonnenavn og fiks format slik at valutakurs-dataen kan brukes
fx_rates <- fx_rates%>%
  select(TIME_PERIOD, BASE_CUR, OBS_VALUE, UNIT_MULT)%>%
  rename(Today = TIME_PERIOD, Currency = BASE_CUR, fx_rate = OBS_VALUE)%>%
  mutate(Today = as.Date(Today),
         fx_rate = as.numeric(str_replace(fx_rate, ",", "."))/10^UNIT_MULT)%>%
  select(-UNIT_MULT)


# Hent styringsrenten
policy_rate <- read.csv(url(paste0("https://data.norges-bank.no/api/data/IR/B.KPRA..?format=csv&startPeriod=1990-01-01&endPeriod=",Sys.Date(),"&locale=no&bom=include")), sep = ";")%>%
  filter(TENOR == "SD")%>%
  select(TIME_PERIOD, OBS_VALUE)%>%
  rename(Today = TIME_PERIOD,
         Value = OBS_VALUE)%>%
  mutate(Value = as.numeric(str_replace(Value, ",", ".")),
         Today = as.Date(Today))



# Slå sammen data som hentes gjennom SQL med data fra regnearket.
# Endre forfallsdato til EarlyRedeemedDate i tilfeller hvor EarlyRedeemedDate ikke er NA
# Beregn NOK-verdi for utstedelser i utenlandsk valuta
# Til slutt fjernes rader hvor det hverken har skjedd utvidelser eller tilbakebetaling eller kupongrenten endres
tranche_combined <- tranche_sql%>%
  ungroup()%>%
  mutate(Today = as.Date(Today),
         IssueDate = as.Date(IssueDate),
         MaturityDate = as.Date(ifelse(is.na(EarlyRedeemedDate), as.Date(MaturityDate), as.Date(EarlyRedeemedDate)), 
                                origin = "1970-01-01"))%>%
  bind_rows(tranche_excel)%>%
  group_by(ISIN)%>%
  arrange(.,Today)%>%
  mutate(Currency = last(Currency))%>%
  left_join(fx_rates)%>%
  mutate(fx_rate = first(fx_rate),
         CurrentOutstandingAmountNOK = ifelse(Currency == "NOK", CurrentOutstandingAmount, CurrentOutstandingAmount * fx_rate),
         IssuedAmount = ifelse(Today == first(Today), 
                               CurrentOutstandingAmount,
                               CurrentOutstandingAmount - lag(CurrentOutstandingAmount, 1)),
         IssuedAmountNOK = ifelse(Currency == "NOK", IssuedAmount, IssuedAmount * fx_rate),
         delta_rate = ifelse(Today == first(Today), 
                             CurrentCouponRate,
                             CurrentCouponRate - lag(CurrentCouponRate, 1)),
         MaturityDate = max(MaturityDate))%>%
  filter(!(IssuedAmount == 0 & delta_rate == 0),
         Today <= MaturityDate)



# Datasettet hentet gjennom SQL fanger ikke opp at papirene forfaller.
# Oppretter rad for å få med dette
matured <- tranche_combined%>%
  filter(MaturityDate >= "2016-01-01",
         MaturityDate <= Sys.Date())%>%
  group_by(ISIN)%>%
  filter(Today == max(Today))%>%
  mutate(IssuedAmount = -CurrentOutstandingAmount,
         IssuedAmountNOK = -CurrentOutstandingAmountNOK,
         Today = MaturityDate,
         CurrentOutstandingAmount = 0,
         CurrentOutstandingAmountNOK = 0)

matured_tranche_ISIN <- tranche_combined%>%
  filter(MaturityDate <= "2016-01-01")%>%
  group_by(ISIN)%>%
  filter(sum(IssuedAmount) != 0)

matured_tranche <- tranche_combined%>%
  ungroup()%>%
  filter(ISIN %in% matured_tranche_ISIN$ISIN)%>%
  group_by(ISIN)%>%
  filter(Today == max(Today))%>%
  mutate(IssuedAmount = -CurrentOutstandingAmount,
         IssuedAmountNOK = -CurrentOutstandingAmountNOK,
         Today = MaturityDate,
         CurrentOutstandingAmount = 0,
         CurrentOutstandingAmountNOK = 0)

# Legger til radene med forfall
# Opprett kolonnen TimeToMaturity, som er intervaller for tid til forfall.
# Til slutt, filterer ut observasjoner hvor utestående er småbeløp
data_complete <- bind_rows(tranche_combined, matured, matured_tranche)%>%
  mutate(IssuedAmount = ifelse(is.na(IssuedAmount), 0, IssuedAmount),
         IssuedAmountNOK = ifelse(is.na(IssuedAmountNOK), 0, IssuedAmountNOK),
         
         delta_rate        = ifelse(is.na(delta_rate), 0, delta_rate))%>%
  group_by(ISIN)%>%
  mutate(Issuer_Name         = last(Issuer_Name),
         CurrentInterestType = last(CurrentInterestType),
         Today               = as.Date(Today),
         IssueDate           = as.Date(IssueDate),
         MaturityDate        = as.Date(MaturityDate),
         TimeToMaturity = ifelse(MaturityDate - Today <= 365, "0-1", NA),
         TimeToMaturity = ifelse(MaturityDate - Today > 365 & MaturityDate - Today <= 2*365, "1-2", TimeToMaturity),
         TimeToMaturity = ifelse(MaturityDate - Today > 2*365 & MaturityDate - Today <= 5*365, "2-5", TimeToMaturity),
         TimeToMaturity = ifelse(MaturityDate - Today > 5*365 & MaturityDate - Today <= 10*365, "5-10", TimeToMaturity),
         TimeToMaturity = ifelse(MaturityDate - Today > 10*365 & MaturityDate - Today <= 15*365, "10-15", TimeToMaturity),
         TimeToMaturity = ifelse(MaturityDate - Today > 15*365,               "15+", TimeToMaturity),
         
         TimeToMaturity = factor(TimeToMaturity, levels = c("15+", "10-15", "5-10", "2-5", "1-2", "0-1")))%>%
  group_by(ISIN)%>%
  fill(.,CurrentOutstandingAmountNOK, .direction = "down")%>%
  fill(.,Currency, .direction = "downup")%>%
  fill(.,CurrentInterestType, .direction = "downup")%>%
  fill(.,IssueType, .direction = "downup")%>%
  fill(.,MaturityDate, .direction = "downup")%>%
  fill(.,CurrentCouponRate, .direction = "down")%>%
  fill(.,RiskClassRisk, .direction = "downup")%>%
  fill(.,Issuer_Name, .direction = "downup")%>%
  group_by(Issuer_Name)%>%
  fill(.,Issuer_Country, .direction = "downup")%>%
  fill(.,Issuer_IndustryGrouping, .direction  = "downup")%>%
  mutate(CurrentOutstandingAmount    = ifelse(abs(CurrentOutstandingAmount) < 10, 0, CurrentOutstandingAmount),
         CurrentOutstandingAmountNOK = ifelse(abs(CurrentOutstandingAmountNOK) < 10, 0, CurrentOutstandingAmountNOK))

names_industry <- data_complete%>%ungroup()%>%select(Issuer_Name, Issuer_IndustryGrouping)%>%unique()%>%filter(is.na(Issuer_IndustryGrouping) == F)

data_complete <- data_complete%>%left_join(names_industry, by = "Issuer_Name")%>%
  mutate(Issuer_IndustryGrouping = ifelse(is.na(Issuer_IndustryGrouping.x), Issuer_IndustryGrouping.y, Issuer_IndustryGrouping.x))%>%
  select(-Issuer_IndustryGrouping.y, -Issuer_IndustryGrouping.x)%>%
  group_by(tolower(Issuer_Name))%>%
  mutate(Issuer_Name = last(Issuer_Name))%>%
  fill(Issuer_IndustryGrouping, .direction = "downup")




tranche_daily <- bind_rows(data_complete%>%mutate(EarlyRedeemedDate = as.Date(EarlyRedeemedDate)), tranche_bb)%>%
  mutate(ISIN = ifelse(str_length(ISIN) == 12, ISIN, id))%>%
  ungroup()%>%unique()%>%
  mutate(ISIN_country_code = ifelse(str_length(ISIN) == 12, substr(ISIN,1,2), NA))%>%
  left_join(country_codes%>%rename(ISIN_Country = Issuer_Country), 
            by = c("ISIN_country_code" = "Issuer_CountryCode"))%>%
  select(-ISIN_country_code)%>%
  mutate(ISIN_Country = ifelse(is.na(ISIN_Country), "Ingen ISIN", ISIN_Country),
         Issuer_Country = ifelse(Issuer_Name == "Hawk Debtco Ltd", "United Kingdom", Issuer_Country))

# Lag dataframe med datoer på månedlig frekvens fra 1990 til i dag
dates_monthly <- data.frame("Today" = seq(as.Date("1990-01-01"), Sys.Date() + 31, by = "month") - 1)

# Lag dataframe med datoer fra 1990 til i dag, per ISIN
ISIN_dates <- dates_monthly%>%
  bind_rows(tranche_daily%>%select(ISIN)%>%
                ungroup()%>%unique())%>%
  complete(Today, ISIN)%>%
  filter(is.na(Today) == F,
         is.na(ISIN) == F)

# Finn end of month datoer per ISIN i data_complete. Disse datoene må filtreres ut fra ISIN-dates for at fill-operasjoner lenger ned skal fungere...
data_eom <- tranche_daily%>%
  select(Today, ISIN)%>%
  filter(Today %in% dates_monthly$Today)%>%
  mutate(Today_ISIN = paste0(Today, ISIN))

fx_rates_eom <- fx_rates%>%
  mutate(Today_eom = ceiling_date(Today, "month") - 1)%>%
  group_by(Today_eom, Currency)%>%
  filter(Today == max(Today))%>%
  mutate(Today = Today_eom)%>%
  ungroup()%>%
  select(-Today_eom)

# Lag dataframe med utestående per ISIN per mnd
outstanding_monthly <- ISIN_dates%>%
  ungroup()%>%
  filter(!(paste0(Today, ISIN) %in% data_eom$Today_ISIN))%>%
  bind_rows(tranche_daily)%>%
  arrange(Today)%>%
  group_by(ISIN)%>%
  fill(., Currency, .direction = "downup")%>%
  ungroup()%>%
  left_join(fx_rates_eom, by = c("Today", "Currency"))%>%
  group_by(ISIN)%>%
  fill(.,CurrentOutstandingAmountNOK, .direction = "down")%>%
  fill(.,CurrentOutstandingAmount, .direction = "down")%>%
  fill(.,Currency, .direction = "down")%>%
  fill(.,Issuer_Name, .direction = "down")%>%
  fill(.,Issuer_Country, .direction = "down")%>%
  fill(.,ISIN_Country, .direction = "downup")%>%
  fill(.,CurrentInterestType, .direction = "down")%>%
  fill(.,IssueType, .direction = "down")%>%
  fill(.,MaturityDate, .direction = "down")%>%
  fill(.,CurrentCouponRate, .direction = "down")%>%
  fill(.,RiskClassRisk, .direction = "down")%>%
  fill(.,Issuer_IndustryGrouping, .direction  = "down")%>%
  mutate(CurrentOutstandingAmountNOK_CurrentRate = ifelse(Currency != "NOK", 
                                                          CurrentOutstandingAmount * fx_rate.y,
                                                          CurrentOutstandingAmountNOK))%>%
  group_by(Issuer_Name)%>%
  fill(., Issuer_IndustryGrouping, .direction = "downup")%>%
  ungroup()%>%
  filter(as.Date(Today) %in% dates_monthly$Today,
         is.na(CurrentOutstandingAmountNOK) == F,
         CurrentOutstandingAmountNOK != 0)%>%
  mutate(TimeToMaturity = ifelse(MaturityDate - Today <= 365, "0-1", NA),
         TimeToMaturity = ifelse(MaturityDate - Today > 365 & MaturityDate - Today <= 2*365, "1-2", TimeToMaturity),
         TimeToMaturity = ifelse(MaturityDate - Today > 2*365 & MaturityDate - Today <= 5*365, "2-5", TimeToMaturity),
         TimeToMaturity = ifelse(MaturityDate - Today > 5*365 & MaturityDate - Today <= 10*365, "5-10", TimeToMaturity),
         TimeToMaturity = ifelse(MaturityDate - Today > 10*365 & MaturityDate - Today <= 15*365, "10-15", TimeToMaturity),
         TimeToMaturity = ifelse(MaturityDate - Today > 15*365,                                      "15+", TimeToMaturity),
         
         TimeToMaturity = factor(TimeToMaturity, levels = c("15+", "10-15", "5-10", "2-5", "1-2", "0-1")))%>%
  select(Today, ISIN, Issuer_Name, Currency, IssueType, CurrentInterestType, MaturityDate, 
         CurrentCouponRate, TimeToMaturity, CurrentOutstandingAmountNOK, CurrentOutstandingAmountNOK_CurrentRate, 
         RiskClassRisk, Issuer_IndustryGrouping, Issuer_Country, ISIN_Country, CurrentOutstandingAmount)


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



# Lag variabel som viser dato for forrige oppdatering av dataen  
update <- Sys.Date()

# Lagre oppdatert data i r-datafilen "sd_dashboard_data"
save(tranche_daily, outstanding_monthly, spreads, mapping_Issuer_IndustryGrouping, policy_rate, update, sql_info_NA_data, file = "obligasjonsdata_dashboard.rda")