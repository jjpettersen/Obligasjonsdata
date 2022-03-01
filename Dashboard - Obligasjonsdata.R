## Last inn pakker og datasett
library(pacman)

p_load(ggplot2, tidyverse, plotly, shiny, shinydashboard, lubridate, 
       scales, writexl, shinyWidgets, shinyjs, shinyBS, ggpubr)

setwd("F:/MB/MOA/Likviditet/Analyser/Dataprosjekt/Stamdata/JJP")

load("obligasjonsdata_dashboard.rda")


source("F:/MB/MOA/Likviditet/Figurproduksjon/R/nomafunctions/R/noma_tidy_plot.r")
source("F:/MB/MOA/Likviditet/Figurproduksjon/R/nomafunctions/R/noma_add_line.r")

nb_colors <- c("#225978" ,"#78A57D", "#cd8c41", "#965a96", "#dd222d" ,"#49b4df", "#c3b996", "#a4acb1", "#967396", "#007d82")


# Lager user interface = definerer hvordan appen skal se ut (layout, input, output)
ui <- fluidPage(
  
  useShinyjs(),

  
  titlePanel("Obligasjonsdata i R"),
  
  # Layout med sidebar = marg til venstre
  sidebarLayout(
    
    # Bestemmer hva margen med input skal inneholde
    sidebarPanel(width = 3,
                 
                 id    = "sidebar",
                 
                 ## Lager objekt i margen som lar brukeren bestemme tidsperiode for dataen
                 dateRangeInput(
                   inputId   = "dates_outstanding",
                   label     = "",
                   start     = as.Date("2016-01-01"),
                   end       = ceiling_date(Sys.Date(), "month") - 1,
                   language  = "no",
                   separator = "-",
                   weekstart = 1,
                   startview = "year"),
                 
                 
                 # Bestem datafrekvens
                 radioButtons(       
                   inputId  = "frequency",
                   label    = "Datafrekvens",
                   choices  = c("Månedlig"    = "month",
                                "Kvartalsvis"  = "quarter",
                                "Årlig"        = "year"),
                   selected = "month"),
                 
                 
                 
                 # Filtrer utstedere
                 pickerInput(       inputId  = "issuer_names",
                                    label    = "Utsteder",
                                    choices  = sort(unique(outstanding_monthly$Issuer_Name)), 
                                    options  = pickerOptions(`actions-box`          = TRUE,
                                                             `selected-text-format` = paste0("count > ", length(unique(outstanding_monthly$Issuer_Name))-1),
                                                             `count-selected-text`  = "Alle",
                                                             `live-search`          = TRUE,
                                                             selectAllText          = "Velg alle",
                                                             deselectAllText        = "Dropp alle"),
                                    multiple = TRUE,
                                    selected = unique(outstanding_monthly$Issuer_Name)),
                 
                 # Filtrer valutaer
                 pickerInput(       inputId  = "currencies",
                                    label    = "Valuta",
                                    choices  = sort(unique(outstanding_monthly$Currency)), 
                                    options  = pickerOptions(`actions-box`          = TRUE,
                                                             `selected-text-format` = paste0("count > ", length(unique(outstanding_monthly$Currency))-1),
                                                             `count-selected-text`  = "Alle",
                                                             selectAllText          = "Velg alle",
                                                             deselectAllText        = "Dropp alle"),
                                    multiple = TRUE,
                                    selected = unique(outstanding_monthly$Currency)),
                 
                 radioButtons("current_fx_rate_dummy",
                              "Valutakurs for utstedelser i valuta",
                              choices = c("Kurs ved utstedelse",  "Løpende kurs"),
                              selected = "Kurs ved utstedelse"),
                 
                 # Filtrer utstederland
                 pickerInput(       inputId  = "countries",
                                    label    = "Utstederland",
                                    choices  = sort(unique(outstanding_monthly$Issuer_Country)), 
                                    options  = pickerOptions(`actions-box`          = TRUE,
                                                             `selected-text-format` = paste0("count > ", length(unique(outstanding_monthly$Issuer_Country))-1),
                                                             `count-selected-text`  = "Alle",
                                                             selectAllText          = "Velg alle",
                                                             deselectAllText        = "Dropp alle"),
                                    multiple = TRUE,
                                    selected = unique(outstanding_monthly$Issuer_Country)),
                 
                 # Filtrer ISIN-land
                 pickerInput(       inputId  = "ISIN_countries",
                                    label    = "ISIN-marked",
                                    choices  = sort(unique(outstanding_monthly$ISIN_Country)), 
                                    options  = pickerOptions(`actions-box`          = TRUE,
                                                             `selected-text-format` = paste0("count > ", length(unique(outstanding_monthly$ISIN_Country))-1),
                                                             `count-selected-text`  = "Alle",
                                                             selectAllText          = "Velg alle",
                                                             deselectAllText        = "Dropp alle"),
                                    multiple = TRUE,
                                    selected = unique(outstanding_monthly$ISIN_Country)),
                 
                 
                 # Filtrer rentetyper
                 pickerInput(       inputId  = "coupon_types",
                                    label    = "Rentetype",
                                    choices  = sort(unique(outstanding_monthly$CurrentInterestType)), 
                                    options  = pickerOptions(`actions-box`          = TRUE,
                                                             `selected-text-format` = paste0("count > ", length(unique(outstanding_monthly$CurrentInterestType))-1),
                                                             `count-selected-text`  = "Alle",
                                                             selectAllText          = "Velg alle",
                                                             deselectAllText        = "Dropp alle"),
                                    multiple = TRUE,
                                    selected = unique(outstanding_monthly$CurrentInterestType)),
                 
                 # Filtrer type utstedelse
                 pickerInput(       inputId  = "issue_types",
                                    label    = "Type utstedelse",
                                    choices  = sort(unique(outstanding_monthly$IssueType)),
                                    options  = pickerOptions(`actions-box`          = TRUE,
                                                             `selected-text-format` = paste0("count > ", length(sort(unique(outstanding_monthly$IssueType)))-1),
                                                             `count-selected-text`  = "Alle",
                                                             selectAllText          = "Velg alle",
                                                             deselectAllText        = "Dropp alle"),
                                    multiple = TRUE,
                                    selected = unique(outstanding_monthly$IssueType)),
                 
                 # Filtrer risikoklasse
                 pickerInput(       inputId  = "risk_classes",
                                    label    = "Risikoklasse",
                                    choices  = sort(unique(outstanding_monthly$RiskClassRisk)),
                                    options  = pickerOptions(`actions-box`          = TRUE,
                                                             `selected-text-format` = paste0("count > ", length(sort(unique(outstanding_monthly$RiskClassRisk)))-1),
                                                             `count-selected-text`  = "Alle",
                                                             selectAllText          = "Velg alle",
                                                             deselectAllText        = "Dropp alle"),
                                    multiple = TRUE,
                                    selected = unique(outstanding_monthly$RiskClassRisk)),
                 
                 # Filtrer industru
                 pickerInput(       inputId  = "issuer_industries",
                                    label    = "Industri",
                                    choices  = sort(unique(outstanding_monthly$Issuer_IndustryGrouping)),
                                    options  = pickerOptions(`actions-box`          = TRUE,
                                                             `selected-text-format` = paste0("count > ", length(sort(unique(outstanding_monthly$Issuer_IndustryGrouping)))-1),
                                                             `count-selected-text`  = "Alle",
                                                             selectAllText          = "Velg alle",
                                                             deselectAllText        = "Dropp alle"),
                                    multiple = TRUE,
                                    selected = unique(outstanding_monthly$Issuer_IndustryGrouping)),
                 
                 # Filtrer tid til forfall
                 pickerInput(       inputId  = "time_to_maturity",
                                    label    = "Tid til forfall",
                                    choices  = paste0(sort(unique(outstanding_monthly$TimeToMaturity))),
                                    options  = pickerOptions(`actions-box`          = TRUE,
                                                             `selected-text-format` = paste0("count > ", length(sort(unique(outstanding_monthly$TimeToMaturity)))-1),
                                                             `count-selected-text`  = "Alle",
                                                             selectAllText          = "Velg alle",
                                                             deselectAllText        = "Dropp alle"),
                                    multiple = TRUE,
                                    selected = unique(outstanding_monthly$TimeToMaturity)),
                 
                 # Velg grupperingsvaribel (se utstedelser per risikoklasse f.eks)
                 pickerInput(       inputId        = "sort_var_1",
                                    label          = "Sorter etter (1)",
                                    choices        = c("Ingen"           = "Today",
                                                       "Valuta"          = "Currency",
                                                       "Ustederland"     = "Issuer_Country",
                                                       "Rentetype"       = "CurrentInterestType", 
                                                       "Utsteder"        = "Issuer_Name",
                                                       "Type utstedelse" = "IssueType",
                                                       "Tid til forfall" = "TimeToMaturity",
                                                       "Risikoklasse"    = "RiskClassRisk",
                                                       "Industri"        = "Issuer_IndustryGrouping"),
                                    selected = "Today",
                                    multiple = FALSE),
                 
                 # Velg grupperingsvaribel (se utstedelser per risikoklasse f.eks)
                 pickerInput(       inputId        = "sort_var_2",
                                    label          = "Sorter etter (2)",
                                    choices        = c("Ingen"           = "Today",
                                                       "Valuta"          = "Currency",
                                                       "Ustederland"     = "Issuer_Country",
                                                       "Rentetype"       = "CurrentInterestType", 
                                                       "Utsteder"        = "Issuer_Name",
                                                       "Type utstedelse" = "IssueType",
                                                       "Tid til forfall" = "TimeToMaturity",
                                                       "Risikoklasse"    = "RiskClassRisk",
                                                       "Industri"        = "Issuer_IndustryGrouping"),
                                    selected = "Today",
                                    multiple = FALSE),
                 
                 
                 # Bestem hvilken type graf du vil ha
                 selectInput(       inputId  = "chart_type",
                                    label    = "Figurtype",
                                    choices  = c("Linje"                    = "line",
                                                 "Stiplet linje"            = "dashed line", 
                                                 "Scatter"                  = "dot", 
                                                 "Stolpe"                   = "bar",
                                                 "Stablet stolpe"           = "stacked bar",
                                                 "Areal"                    = "stacked area",
                                                 "100 % areal"              = "stacked percent area"),
                                    selected = "stacked bar",
                                    multiple = F),
                 
                 # Skriv inn overskrift for grafen
                 textInput(         inputId  = "title",
                                    label    = "Tittel",
                                    value    = NULL),
                 
                 # Skriv inn under-overskrift
                 textInput(         inputId  =  "subtitle",
                                    label    =  "Undertittel",
                                    value    =  NULL),
                 
                 # Knapp som lar brukeren oppdatere datasettet
                 actionButton("update_data", "Oppdater data"),
                 
                 h5("Sist oppdatert:"),
                 
                 # Tekst som viser forrige dato for oppdatering
                 h5((paste0("Obligasjonsdata: ", format(updated_date_FI, "%d.%m.%Y")))),
                 h5((paste0("Kredittspreader: ", format(updated_date_spreads, "%d.%m.%Y"))))
                 
                 
    ),
    
    # Setter opp panelet i midten av appen
    mainPanel(
      
      # Vil ha tabs (faner)
      tabsetPanel(type = "tabs",
                  
                  # Lag en fane som heter "UtestÃ¥ende"
                  tabPanel("Utestående", 
                           
                           # Generer et plot med objektet "plot_outstanding", som er definert i server lenger ned
                           plotlyOutput("plot_outstanding", width = "75%"),
                           
                           # Knapp som lar brukeren laste ned dataen som vises i plottet
                           downloadButton("dl_outstanding_excel", "Last ned til Excel"),
                           
                           # Lar brukeren velge filnavn for nedlasting selv
                           textInput(inputId     = "filename_outstanding", 
                                     label       = "Filnavn", 
                                     placeholder = "")),
                  
                  # Lag en fane som heter "Utstedelser"
                  tabPanel("Utstedelser",
                           
                           # Lar brukeren velge brutto eller netto-utstedelser
                           radioButtons("net_issuance_dum",
                                        "",
                                        choices = c(
                                          "Bruttoutstedelser",
                                          "Nettoutstedelser"
                                        ),
                                        selected = "Bruttoutstedelser"),
                           
                           plotlyOutput("plot_issued", width = "75%"),
                           
                           
                           downloadButton("dl_issued_excel", "Last ned til Excel"),
                           
                           
                           textInput(inputId     = "filename_issued", 
                                     label       = "Filnavn", 
                                     placeholder = "")),
                  
                  tabPanel("Vekst 12-mnd",
                           
                           radioButtons("growth_contribution_dum",
                                        "",
                                        choices = c(
                                          "Individuell vekst (%)",
                                          "Bidrag samlet vekst (prosentenheter)"
                                        ),
                                        selected = "Bidrag samlet vekst (prosentenheter)"),
                           
                           
                           
                           
                           plotlyOutput("plot_growth", width = "75%"),
                           
                           
                           downloadButton("dl_growth_excel", "Last ned til Excel"),
                           
                           
                           textInput(inputId     = "filename_growth", 
                                     label       = "Filnavn", 
                                     placeholder = "")),
                  
                  # Lag en fane som heter "Daglige utstedelser"
                  tabPanel("Daglige utstedelser",
                           
                           radioButtons("net_issuance_daily_dum",
                                        "",
                                        choices = c(
                                          "Bruttoutstedelser",
                                          "Nettoutstedelser"
                                        ),
                                        selected = "Bruttoutstedelser"),
                           
                           # Lar brukeren velge brutto eller netto-utstedelser

                           
                           plotlyOutput("plot_issued_daily", width = "75%"),
                           
                           
                           downloadButton("dl_issued_daily_excel", "Last ned til Excel"),
                           
                           
                           textInput(inputId     = "filename_issued_daily", 
                                     label       = "Filnavn", 
                                     placeholder = "")),
                  
                  # Lag en fane med forfall
                  tabPanel("Forfall", 
                           
                           # Lar brukeren velge hvilken dato for forfallsprofil (man kan se hvordan det var tilbake i tid)
                           selectInput("date_maturing",
                                       "Velg dato",
                                       choices  = unique(outstanding_monthly$Today),
                                       selected = max(outstanding_monthly$Today)),
                           
                           # Velg tidsrom for forfall. Standard er 10y frem i tid.
                           dateRangeInput(
                             inputId   = "range_maturing",
                             label     = "Periode",
                             start     = ceiling_date(Sys.Date(), "month"),
                             end       = ceiling_date(Sys.Date() + 10 * 365, "year") - 1,
                             language  = "no",
                             separator = "-",
                             weekstart = 1,
                             startview = "year"),
                           
                           
                           
                           
                           plotlyOutput("plot_maturing", width = "75%"),
                           
                           downloadButton("dl_maturing_excel", "Last ned til Excel"),
                           
                           
                           textInput(inputId     = "filename_maturing", 
                                     label       = "Filnavn", 
                                     placeholder = "")
                           
                  ),
                  
                  # Lag fane med utestÃ¥ende
                  tabPanel("Renter utestående",
                           
                           # Checkbox som lager ovarlay med styringsrenten
                           checkboxInput("policy_rate_overlay_1",
                                         label = "Styringsrenten",
                                         value = F),
                           
                           plotlyOutput("plot_rates_outstanding", width = "75%"),
                           
                           downloadButton("dl_rates_outstanding_excel", "Last ned til Excel"),
                           
                           
                           textInput(inputId     = "filename_rates_outstanding", 
                                     label       = "Filnavn", 
                                     placeholder = "")
                           
                  ),
                  
                  # Lag fane med renter for nyutstedelser
                  tabPanel("Renter nyutstedelser",
                           
                           checkboxInput("policy_rate_overlay_2",
                                         label = "Styringsrenten",
                                         value = F),
                           
                           plotlyOutput("plot_rates_new_issues", width = "75%"),
                           
                           downloadButton("dl_rates_new_issues_excel", "Last ned til Excel"),
                           
                           
                           textInput(inputId     = "filename_rates_new_issues", 
                                     label       = "Filnavn", 
                                     placeholder = "")),
                  
                  # Lag fane med kredittspreader
                  tabPanel("Kredittspreader",
                           
                           # Overskrift
                           headerPanel("Tidsserier"),
                           
                           # Oppsettet i denne fanen er fluidrow
                           fluidRow(
                             column(3, # i en kolonne med bredde 3 skal vi ha et input-objekt med tidsperiode
                                    
                                    dateRangeInput(
                                      inputId   = "dates_spreads_time_series",
                                      label     = "Tidsperiode",
                                      start     = as.Date("2016-01-01"),
                                      end       = Sys.Date(),
                                      language  = "no",
                                      separator = "-",
                                      weekstart = 1,
                                      startview = "year")),
                             
                             column(4, # I en kolonne med bredde 4 skal vi ha et objekt som lar brukeren velge risikoklasse
                                    
                                    pickerInput(       inputId  = "RiskClass",
                                                       label    = "Risikoklasser",
                                                       choices  = sort(unique(spreads$RiskClass)),
                                                       options  = pickerOptions(`actions-box`          = TRUE,
                                                                                `selected-text-format` = paste0("count > ", length(sort(unique(spreads$RiskClass)))-1),
                                                                                `count-selected-text`  = "Alle",
                                                                                selectAllText          = "Velg alle",
                                                                                deselectAllText        = "Dropp alle"),
                                                       multiple = TRUE,
                                                       selected = c("Finance - Covered Bonds", "Finance - Senior Unsecured"))),
                             column(1, # Velg bucket for lopetid
                                    
                                    selectInput(
                                      inputId  = "tenor_spreads_time_series",
                                      label    = "Løpetid",
                                      choices  = sort(unique(spreads$Tenor)),
                                      selected = "5y",
                                      multiple = F))),
                           
                           plotlyOutput("plot_spreads_time_series", width = "75%"),
                           
                           downloadButton("dl_spreads_time_series_excel", "Last ned til Excel"),
                           
                           headerPanel(""),
                           headerPanel("Kurver"),
                           
                           
                           dateInput(
                             inputId   = "date_spreads_curve",
                             label     = "Per:",
                             min       = min(spreads$Today),
                             max       = max(spreads$Today),
                             value     = max(spreads$Today),
                             language  = "no",
                             weekstart = 1,
                             startview = "year"),
                           
                           # Checkbox som lar brukeren sammenlikne kurvene bak i tid
                           checkboxInput("curve_compare_dummy",
                                         "Sammenlikn med dato:",
                                         value = FALSE),
                           
                           # Dersom boksen over er huket av, vis et inputobjekt som lar brukeren velge dato man vil sammenlikne med
                           conditionalPanel("input.curve_compare_dummy==true",
                                            dateInput(
                                              inputId   = "date_compare_spreads_curve",
                                              label     = "",
                                              min       = min(spreads$Today),
                                              max       = max(spreads$Today),
                                              value     = max(spreads$Today) - 7,
                                              language  = "no",
                                              weekstart = 1,
                                              startview = "year")),
                           
                           
                           plotlyOutput("plot_spreads_curve", width = "75%"),
                           
                           downloadButton("dl_spreads_curve_excel", "Last ned til Excel")
                           
                           
                  ),
                  
                  # Fane med utvalgte figurer. Her kan brukeren kun velge dato-range
                  tabPanel("Utvalgte figurer",
                           
                           dateRangeInput(
                             inputId   = "dates_plots",
                             label     = "",
                             start     = as.Date("2016-01-01"),
                             end       = ceiling_date(Sys.Date(), "month") - 1,
                             language  = "no",
                             separator = "-",
                             weekstart = 1,
                             startview = "year"),
                           
                           
                           plotlyOutput("plot1", width = "75%"),
                           
                           plotlyOutput("plot2", width = "75%"),
                           
                           
                           plotlyOutput("plot3", width = "75%"),
                           
                           plotlyOutput("plot4", width = "75%")),
                  
                  
                  
                  id = "tabset"
                  
                  
      ),
      
      id = "main"
      
      
      
    ),
    
    
    
  ),
  
  # Velg font
  tags$head(tags$style(HTML('* {font-family: "Arial"};'))),

  
  tags$b("Nullstill App"),
  # Lag knapp som laster inn siden paa nytt
  tags$a(href="javascript:history.go(0)",
         popify(tags$i(class="fa fa-refresh fa-5x"),
                title     = "",
                content   = "",
                placement = "right"))
  
  
  
)



# Lager serveren. Den lager output (som defineres under) basert paa brukerens input

server <- function(input, output, session) {
  
  # Gjem margen til venstre dersom man er i fanene "kredittspreader" eller "utvalgte figurer"
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] %in% c("Utvalgte figurer", "Kredittspreader")){
      hideElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-8")
      addCssClass("main", "col-sm-12")
    }else{
      showElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-12")
      addCssClass("main", "col-sm-8")
    }
  })
  
  

  observeEvent(input$update_data, {
    showModal(modalDialog("Velg datasett. OBS: Trenger forhåndsdefinerte  søk i SRCH for Bloombergdata.",

      checkboxInput("update_FI",
                    "Volumer og renter",
                    value = F),
      checkboxInput("update_spreads",
                    "Kredittspreader",
                    value = F),
      
      footer = tagList(
        modalButton("Avbryt"),
        actionButton("ok", "Oppdater")
      )
      
      )
    )
    
    observeEvent(input$ok, {
      

      if(input$update_FI == T){
        showModal(modalDialog("Oppdaterer obligasjonsdata ...", footer = NULL))
        source("oppdater_obligasjonsdata.r")
        }
      
      
      
      if(input$update_spreads == T){
        showModal(modalDialog("Oppdaterer spreader ...", footer = NULL))
        source("oppdater_spreads.r")
      }
      
      showModal(modalDialog("Data oppdatert. Restart app for å laste inn data."))

      removeModal()
      
    })
    

    
  })
  
  # Lag datasettet som kan lastes ned i fanen "utestÃende"
  dl_input_outstanding <- reactive({
    
    data <- outstanding_monthly%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      mutate(Today_dum = ceiling_date(Today, input$frequency) - 1)%>%
      filter(Today == Today_dum)
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    data <- data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(Outstanding = ifelse(input$current_fx_rate_dummy == "Kurs ved utstedelse",
                                     sum(CurrentOutstandingAmountNOK, na.rm = T)/10^9,
                                     sum(CurrentOutstandingAmountNOK_CurrentRate, na.rm = T)/10^9))%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
    
    
    if(ncol(data) > 2  & input$sort_var_1 != "Today"){
      data <- data%>%
        spread(2, 3, fill = 0)
      
    }else{
      if(input$sort_var_1 == "Today" & input$sort_var_2 == "Today"){
      data <- data%>%
        ungroup()%>%
        select(-sort_var)}
    }
    
  })
  
  dl_input_growth <- reactive({
    
    data <- outstanding_monthly%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      mutate(Today_dum = ceiling_date(Today, "month") - 1)%>%
      filter(Today == Today_dum)
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    data <- data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(Outstanding = ifelse(input$current_fx_rate_dummy == "Kurs ved utstedelse",
                                     sum(CurrentOutstandingAmountNOK, na.rm = T)/10^9,
                                     sum(CurrentOutstandingAmountNOK_CurrentRate, na.rm = T)/10^9))%>%
      group_by_at("sort_var")%>%
      mutate(Outstanding_tmin12 = lag(Outstanding, 12),
             Outstanding_tmin12 = ifelse(is.na(Outstanding_tmin12), 0, Outstanding_tmin12),
             AbsGrowth = Outstanding - Outstanding_tmin12,
             Growth    = (Outstanding / Outstanding_tmin12 - 1) * 100)%>%
      group_by_at("Today")%>%
      mutate(AbsGrowthTot = sum(AbsGrowth),
             VolumeWeightedGrowth = (sum(Outstanding) / sum(Outstanding_tmin12) - 1) * 100,
             Weight = Outstanding_tmin12 / sum(Outstanding_tmin12),
             GrowthContribution = (AbsGrowth / AbsGrowthTot) * VolumeWeightedGrowth)
    
    if(input$sort_var_1 == "Today" & input$sort_var_2 == "Today"){
      data <- data%>%
        ungroup()%>%
        select(Today, Outstanding)%>%
        mutate(Growth = (Outstanding / lag(Outstanding, 12) - 1) * 100)%>%
        select(Today, Growth)
      }else{
          
    
    if(input$growth_contribution_dum == "Individuell vekst (%)"){
      data <- data%>%select(Today, sort_var, Growth)%>%
        group_by_at(c("Today", "sort_var"))%>%
        summarise(Growth = sum(Growth))
    }else{
      data <- data%>%select(Today, sort_var, GrowthContribution)%>%
        group_by_at(c("Today", "sort_var"))%>%
        summarise(GrowthContribution = sum(GrowthContribution))
    }
      }

    
    if(ncol(data) > 2){
      data <- data%>%
        spread(2, 3, fill = 0)
      
    }else{
      data <- data
    }
    
    data <- data%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
    
  })
  
  # Lag datasettet som kan lastes ned i fanen "utstedelser"
  dl_input_issued <- reactive({
    
    
    data <- tranche_daily%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      filter(Today <= Sys.Date())%>%
      mutate(Today = ceiling_date(Today, input$frequency) - 1)
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    
    
    data <- data%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])%>%
      group_by_at(c("Today", "sort_var"))%>%
      mutate(IssuedAmountNOK = ifelse(input$net_issuance_dum == "Bruttoutstedelser" & IssuedAmountNOK < 0 ,
                                      0,
                                      IssuedAmountNOK))%>%
      summarise(Issued = sum(IssuedAmountNOK, na.rm = T)/10^9)
    
    
      if(ncol(data) > 2  & input$sort_var_1 != "Today"){
        data <- data%>%
          spread(2, 3, fill = 0)

      }else{
        if(input$sort_var_1 == "Today" & input$sort_var_2 == "Today"){
          data <- data%>%
            ungroup()%>%
            select(-sort_var)}
      }
    
  })
  
  
  dl_input_issued_daily <- reactive({
    
    
    data <- tranche_daily%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      filter(Today <= Sys.Date())%>%
      mutate(IssuedAmountNOK = ifelse(input$net_issuance_daily_dum == "Bruttoutstedelser" & IssuedAmountNOK < 0 ,
                                      0,
                                      IssuedAmountNOK))
    
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    
    data <- data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(Issued = sum(IssuedAmountNOK, na.rm = T)/10^9)%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
    
    if(ncol(data) > 2  & input$sort_var_1 != "Today"){
      data <- data%>%
        spread(2, 3, fill = 0)

    }else{
      if(input$sort_var_1 == "Today" & input$sort_var_2 == "Today"){
        data <- data%>%
          ungroup()%>%
          select(-sort_var)}
    }
    
  })
  
  
  dl_input_maturing <- reactive({
    
    data <- outstanding_monthly%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes,
             Today                     ==  input$date_maturing)%>%
      mutate(Today = ceiling_date(Today, input$frequency) - 1,
             MaturityDate = ceiling_date(MaturityDate, input$frequency) - 1)%>%
      filter(MaturityDate >= input$range_maturing[1],
             MaturityDate <= input$range_maturing[2])
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    
    data <- data%>%
      group_by_at(c("MaturityDate", "sort_var"))%>%
      summarise(Maturing  = sum(CurrentOutstandingAmountNOK, na.rm = T)/10^9)
    
    if(ncol(data) > 2){
      data <- data%>%
        spread(2, 3, fill = 0)
      
    }else{
      data <- data
    }
    
  })
  
  
  dl_input_rates_outstanding <- reactive({
    
    data <- outstanding_monthly%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      mutate(Today_dum = ceiling_date(Today, input$frequency) - 1)%>%
      filter(Today == Today_dum)
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    
    data <- data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(AvgRate     = weighted.mean(CurrentCouponRate, CurrentOutstandingAmountNOK, na.rm = T))%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
    
    if(ncol(data) > 2){
      data <- data%>%
        spread(2, 3, fill = 0)
      
    }else{
      data <- data
    }
    
  })
  
  
  dl_input_rates_new_issues <- reactive({
    
    
    data <- tranche_daily%>%
      ungroup()%>%
      mutate(Today = ceiling_date(Today, "month") - 1)%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      group_by(ISIN)%>%
      filter(Today  ==  first(Today))%>%
      mutate(Today = ceiling_date(Today, input$frequency) - 1)
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    
    data <- data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(AvgRate     = weighted.mean(CurrentCouponRate, IssuedAmountNOK, na.rm = T))%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
    
    if(ncol(data) > 2  & input$sort_var_1 != "Today"){
      data <- data%>%
        spread(2, 3, fill = 0)
      
    }else{
      if(input$sort_var_1 == "Today" & input$sort_var_2 == "Today"){
        data <- data%>%
          ungroup()%>%
          select(-sort_var)}
    }
    
  })
  
  dl_input_spreads_time_series <- reactive({
    
    data <- spreads%>%
      ungroup%>%
      filter(Today >= input$dates_spreads_time_series[1],
             Today <= input$dates_spreads_time_series[2],
             RiskClass %in% input$RiskClass,
             Tenor == input$tenor_spreads_time_series
      )
    
    if(ncol(data) > 2){
      data <- data%>%
        spread(3, 4, fill = NA)
      
    }else{
      data <- data
    }
  })
  
  dl_input_spreads_curve <- reactive({
    
    data <- spreads%>%
      ungroup%>%
      filter(Today == input$date_spreads_curve | Today == input$date_compare_spreads_curve,
             RiskClass %in% input$RiskClass,
             !(Tenor %in% c(">10y", "10y", "9y", "8y")))
    
    if(input$curve_compare_dummy == F){
      data <- data%>%
        filter(Today == input$date_spreads_curve)
      
    }else{
      data <- data
    }
    
    if(ncol(data) > 2){
      data <- data%>%
        spread(2, 4, fill = NA)
      
    }else{
      data <- data
    }
    
    data <- data[, c(1, 2, 5, 9, 12, 3, 4, 6, 7, 8, 10, 11)]
    
  })
  
  # Velger hva som skal skje om brukeren trykker pÃ "last ned til excel".
  # Skriver datasettet til Excel med filnavn som bestemmes av brukeren
  output$dl_outstanding_excel <- downloadHandler(
    
    filename = function() {paste(input$filename_outstanding, ".xlsx", sep = "")},
    
    content  = function(file) {
      write_xlsx(dl_input_outstanding(), 
                 file)
    }
  )
  
  output$dl_growth_excel <- downloadHandler(
    
    filename = function() {paste(input$filename_growth, ".xlsx", sep = "")},
    
    content  = function(file) {
      write_xlsx(dl_input_growth(), 
                 file)
    }
  )
  
  output$dl_issued_excel <- downloadHandler(
    
    filename = function() {paste(input$filename_issued, ".xlsx", sep = "")},
    
    content  = function(file) {
      write_xlsx(dl_input_issued(), file)
    }
  )
  
  output$dl_issued_daily_excel <- downloadHandler(
    
    filename = function() {paste(input$filename_issued_daily, ".xlsx", sep = "")},
    
    content  = function(file) {
      write_xlsx(dl_input_issued_daily(), file)
    }
  )
  
  
  output$dl_maturing_excel <- downloadHandler(
    
    filename = function() {paste(input$filename_maturing, ".xlsx", sep = "")},
    
    content  = function(file) {
      write_xlsx(dl_input_maturing(), file)
    }
  )
  
  output$dl_rates_outstanding_excel <- downloadHandler(
    
    filename = function() {paste(input$filename_rates_outstanding, ".xlsx", sep = "")},
    
    content  = function(file) {
      write_xlsx(dl_input_rates_outstanding(), file)
    }
  )
  
  output$dl_rates_new_issues_excel <- downloadHandler(
    
    filename = function() {paste(input$filename_rates_new_issues, ".xlsx", sep = "")},
    
    content  = function(file) {
      write_xlsx(dl_input_rates_new_issues(), file)
    }
  )
  
  
  output$dl_spreads_time_series_excel <- downloadHandler(
    
    filename = function() {paste("spreads_time_series.xlsx")},
    
    content  = function(file) {
      write_xlsx(dl_input_spreads_time_series(), file)
    }
  )
  
  output$dl_spreads_curve_excel <- downloadHandler(
    
    filename = function() {paste("spreads_curve.xlsx")},
    
    content  = function(file) {
      write_xlsx(dl_input_spreads_curve(), file)
    }
  )
  
  
  
  # Lager datasettet som brukes i figuren med utestÃende volum
  
  
  data_outstanding <- reactive({
    data <- outstanding_monthly%>%
      ungroup()%>%
      filter(
        Issuer_Name             %in% input$issuer_names,
        Issuer_Country          %in% input$countries,
        ISIN_Country            %in% input$ISIN_countries,
        Currency                %in% input$currencies,
        IssueType               %in% input$issue_types,
        CurrentInterestType     %in% input$coupon_types,
        TimeToMaturity          %in% input$time_to_maturity,
        Issuer_IndustryGrouping %in% input$issuer_industries,
        RiskClassRisk           %in% input$risk_classes)%>%
      mutate(Today_dum = ceiling_date(Today, input$frequency) - 1)%>%
      filter(Today == Today_dum)%>%
      select(-Today_dum)
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{

          sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
          sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
        
          names(sort_1)[1] <- "sort_1"
          names(sort_2)[1] <- "sort_2"
          
          sort <- bind_cols(sort_1, sort_2)
            
      
        data <- bind_cols(data, sort)%>%
          mutate(sort_var = paste0(sort_1, " - ", sort_2))
        
      }}else{
        data <- data%>%
          mutate(sort_var = Today)
        
      }
    
    
    data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(Outstanding = ifelse(input$current_fx_rate_dummy == "Kurs ved utstedelse",
                                     sum(CurrentOutstandingAmountNOK, na.rm = T)/10^9,
                                     sum(CurrentOutstandingAmountNOK_CurrentRate, na.rm = T)/10^9))%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
    

    
  })
  
  data_growth <- reactive({
    data <- outstanding_monthly%>%
      ungroup()%>%
      filter(
        Issuer_Name             %in% input$issuer_names,
        Issuer_Country          %in% input$countries,
        ISIN_Country            %in% input$ISIN_countries,
        Currency                %in% input$currencies,
        IssueType               %in% input$issue_types,
        CurrentInterestType     %in% input$coupon_types,
        TimeToMaturity          %in% input$time_to_maturity,
        Issuer_IndustryGrouping %in% input$issuer_industries,
        RiskClassRisk           %in% input$risk_classes)%>%
      mutate(Today_dum = ceiling_date(Today, "month") - 1)%>%
      filter(Today == Today_dum)%>%
      select(-Today_dum)
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    
    
    data <- data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(Outstanding = ifelse(input$current_fx_rate_dummy == "Kurs ved utstedelse",
                                     sum(CurrentOutstandingAmountNOK, na.rm = T)/10^9,
                                     sum(CurrentOutstandingAmountNOK_CurrentRate, na.rm = T)/10^9))%>%
      group_by_at("sort_var")%>%
      mutate(Outstanding_tmin12 = lag(Outstanding, 12),
             Outstanding_tmin12 = ifelse(is.na(Outstanding_tmin12), 0, Outstanding_tmin12),
             AbsGrowth = Outstanding - Outstanding_tmin12,
             Growth    = (Outstanding / Outstanding_tmin12 - 1) * 100)%>%
      group_by_at("Today")%>%
      mutate(AbsGrowthTot = sum(AbsGrowth),
             VolumeWeightedGrowth = (sum(Outstanding) / sum(Outstanding_tmin12) - 1) * 100,
             Weight = Outstanding_tmin12 / sum(Outstanding_tmin12),
             GrowthContribution = (AbsGrowth / AbsGrowthTot) * VolumeWeightedGrowth)

    
    if(input$sort_var_1 == "Today" & input$sort_var_2 == "Today"){
      data <- data%>%
        ungroup()%>%
        mutate(Growth = (Outstanding / lag(Outstanding, 12) - 1) * 100,
               GrowthContribution = (Outstanding / lag(Outstanding, 12) - 1) * 100)
    }else
    {data}
    
    data <- data%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
      
  })
  
  # Bruk funksjonen noma_tidy_plot som lager figur basert pÃ data_outstanding
  output$plot_outstanding <- renderPlotly({
    
    noma_tidy_plot(
      data_outstanding(),
      "Today",
      "Outstanding",
      paste0("sort_var"),
      paste(input$chart_type),
      colors = nb_colors,
      plot_title     = input$title,
      plot_subtitle  = input$subtitle,
      legend_options = list(showlegend = ifelse(input$sort_var_1 == "Today" & input$sort_var_2 == "Today", F, T)),
      yaxis_title    = ifelse(input$chart_type == "stacked percent area", "","Milliarder kr."))
    
  })
  
  output$plot_growth <- renderPlotly({
    
    if(input$growth_contribution_dum == "Individuell vekst (%)"){
      
      noma_tidy_plot(
        data_growth(),
        "Today",
        "Growth",
        paste0("sort_var"),
        paste(input$chart_type),
        colors = nb_colors,
        plot_title     = input$title,
        plot_subtitle  = input$subtitle,
        legend_options = list(showlegend = ifelse(input$sort_var_1 == "Today" & input$sort_var_2 == "Today", F, T)),
        yaxis_title    = ifelse(input$chart_type == "stacked percent area", "Andel av vekst","%"))%>%
        layout(barmode = "relative")
      
    }else{
    
    noma_tidy_plot(
      data_growth(),
      "Today",
      "GrowthContribution",
      paste0("sort_var"),
      paste(input$chart_type),
      colors = nb_colors,
      plot_title     = input$title,
      plot_subtitle  = input$subtitle,
      legend_options = list(showlegend = ifelse(input$sort_var_1 == "Today" & input$sort_var_2 == "Today", F, T)),
      yaxis_title    = ifelse(input$chart_type == "stacked percent area", "Andel av vekst","%"))%>%
      layout(barmode = "relative")
      
      }
    
  })
  
  
  data_issued <- reactive({
    
    data <- tranche_daily%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      filter(Today <= Sys.Date())%>%
      mutate(Today = ceiling_date(Today, input$frequency) - 1)
      
      if(input$sort_var_1 != "Today"){
        
        if(input$sort_var_2 == "Today"){
          sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
          
          names(sort_1)[1] <- "sort_1"
          
          data <- bind_cols(data, sort_1)%>%
            mutate(sort_var = paste0(sort_1))}else{
              
              sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
              sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
              
              names(sort_1)[1] <- "sort_1"
              names(sort_2)[1] <- "sort_2"
              
              sort <- bind_cols(sort_1, sort_2)
              
              
              data <- bind_cols(data, sort)%>%
                mutate(sort_var = paste0(sort_1, " - ", sort_2))
              
            }}else{
              data <- data%>%
                mutate(sort_var = Today)
              
            }
    
    
    data%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])%>%
      group_by_at(c("Today", "sort_var"))%>%
      mutate(IssuedAmountNOK = ifelse(input$net_issuance_dum == "Bruttoutstedelser" & IssuedAmountNOK < 0 ,
                                      0,
                                      IssuedAmountNOK))%>%
      summarise(Issued = sum(IssuedAmountNOK, na.rm = T)/10^9)
    
  })
  
  data_issued_daily <- reactive({
    
    data <- tranche_daily%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      filter(Today <= Sys.Date())%>%
      mutate(IssuedAmountNOK = ifelse(input$net_issuance_daily_dum == "Bruttoutstedelser" & IssuedAmountNOK < 0 ,
                                      0,
                                      IssuedAmountNOK))
    
      
      if(input$sort_var_1 != "Today"){
        
        if(input$sort_var_2 == "Today"){
          sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
          
          names(sort_1)[1] <- "sort_1"
          
          data <- bind_cols(data, sort_1)%>%
            mutate(sort_var = paste0(sort_1))}else{
              
              sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
              sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
              
              names(sort_1)[1] <- "sort_1"
              names(sort_2)[1] <- "sort_2"
              
              sort <- bind_cols(sort_1, sort_2)
              
              
              data <- bind_cols(data, sort)%>%
                mutate(sort_var = paste0(sort_1, " - ", sort_2))
              
            }}else{
              data <- data%>%
                mutate(sort_var = Today)
              
            }
    
    data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(Issued = sum(IssuedAmountNOK, na.rm = T)/10^9)%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
    
  })
  
  
  output$plot_issued <- renderPlotly({
    
    noma_tidy_plot(
      data_issued(),
      "Today",
      "Issued",
      paste("sort_var"),
      paste(input$chart_type),
      colors = nb_colors,
      plot_title     = input$title,
      plot_subtitle  = input$subtitle,
      legend_options = list(showlegend = ifelse(input$sort_var_1 == "Today" & input$sort_var_2 == "Today", F, T)),
      yaxis_title    = ifelse(input$chart_type == "stacked percent area", "","Milliarder kr."))%>%
      layout(barmode = "relative")
    
  })
  
  output$plot_issued_daily <- renderPlotly({
    
    noma_tidy_plot(
      data_issued_daily(),
      "Today",
      "Issued",
      paste("sort_var"),
      paste(input$chart_type),
      color = nb_colors,
      plot_title     = input$title,
      plot_subtitle  = input$subtitle,
      legend_options = list(showlegend = ifelse(input$sort_var_1 == "Today" & input$sort_var_2 == "Today", F, T)),
      yaxis_title    = ifelse(input$chart_type == "stacked percent area", "","Milliarder kr."))%>%
      layout(barmode = "relative")
    
  })
  
  
  data_maturing <- reactive({
    
    data <- outstanding_monthly%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             CurrentInterestType     %in% input$coupon_types,
             IssueType               %in% input$issue_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes,
             Today                    ==  input$date_maturing)%>%
      mutate(MaturityDate = ceiling_date(MaturityDate, input$frequency) - 1)%>%
      filter(MaturityDate >= input$range_maturing[1],
             MaturityDate <= input$range_maturing[2])
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    
    data%>%
      group_by_at(c("MaturityDate", "sort_var"))%>%
      summarise(Maturing  = sum(CurrentOutstandingAmountNOK, na.rm = T)/10^9)
    
  })
  
  
  output$plot_maturing <- renderPlotly({
    
    noma_tidy_plot(
      data_maturing(),
      "MaturityDate",
      "Maturing",
      paste("sort_var"),
      paste(input$chart_type),
      colors = nb_colors,
      plot_title     = input$title,
      plot_subtitle  = input$subtitle,
      legend_options = list(showlegend = ifelse(input$sort_var_1 == "Today" & input$sort_var_2 == "Today", F, T)),
      yaxis_title    = ifelse(input$chart_type == "stacked percent area", "","Milliarder kr."))
    
  })
  
  
  
  
  data_rates_outstanding <- reactive({
    
    data <- outstanding_monthly%>%
      ungroup()%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      mutate(Today_dum = ceiling_date(Today, input$frequency) - 1)%>%
      filter(Today == Today_dum)%>%
      select(-Today_dum)
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    
    data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(AvgRate     = weighted.mean(CurrentCouponRate, CurrentOutstandingAmountNOK, na.rm = T))%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
    
  })
  
  
  data_rates_new_issues <- reactive({
    
    
    data <- tranche_daily%>%
      ungroup()%>%
      mutate(Today = ceiling_date(Today, "month") - 1)%>%
      filter(Issuer_Name             %in% input$issuer_names,
             Issuer_Country          %in% input$countries,
             ISIN_Country            %in% input$ISIN_countries,
             Currency                %in% input$currencies,
             IssueType               %in% input$issue_types,
             CurrentInterestType     %in% input$coupon_types,
             TimeToMaturity          %in% input$time_to_maturity,
             Issuer_IndustryGrouping %in% input$issuer_industries,
             RiskClassRisk           %in% input$risk_classes)%>%
      group_by(ISIN)%>%
      filter(Today                 ==  first(Today),
             IssuedAmountNOK        > 0)%>%
      mutate(Today = ceiling_date(Today, input$frequency) - 1)
    
    if(input$sort_var_1 != "Today"){
      
      if(input$sort_var_2 == "Today"){
        sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
        
        names(sort_1)[1] <- "sort_1"
        
        data <- bind_cols(data, sort_1)%>%
          mutate(sort_var = paste0(sort_1))}else{
            
            sort_1 <- data.frame(sort_1 = data[input$sort_var_1])
            sort_2 <- data.frame(sort_2 = data[input$sort_var_2])
            
            names(sort_1)[1] <- "sort_1"
            names(sort_2)[1] <- "sort_2"
            
            sort <- bind_cols(sort_1, sort_2)
            
            
            data <- bind_cols(data, sort)%>%
              mutate(sort_var = paste0(sort_1, " - ", sort_2))
            
          }}else{
            data <- data%>%
              mutate(sort_var = Today)
            
          }
    
    data%>%
      group_by_at(c("Today", "sort_var"))%>%
      summarise(AvgRate         = weighted.mean(CurrentCouponRate, IssuedAmountNOK, na.rm = T),
                IssuedAmountNOK = sum(IssuedAmountNOK, na.rm = T))%>%
      filter(Today >= input$dates_outstanding[1],
             Today <= input$dates_outstanding[2])
    
  })
  
  data_spreads_time_series <- reactive({
    
    spreads%>%
      ungroup%>%
      filter(Today >= input$dates_spreads_time_series[1],
             Today <= input$dates_spreads_time_series[2],
             RiskClass %in% input$RiskClass,
             Tenor == input$tenor_spreads_time_series
      )
  })
  
  data_spreads_curve <- reactive({
    
    dates <- ifelse(input$curve_compare_dummy == F, 
                    c(input$date_spreads_curve, input$date_compare_spreads_curve),
                    input$date_spreads_curve)
    
    data <- spreads%>%
      ungroup%>%
      filter(Today == input$date_spreads_curve | Today == input$date_compare_spreads_curve,
             RiskClass %in% input$RiskClass,
             !(Tenor %in% c(">10y", "10y", "9y", "8y")))%>%
      mutate(Tenor = factor(Tenor, levels = c("3m", "6m", "9m", "1y", "2y", "3y", "4y", "5y", "6y", "7y", "8y", "9y", "10y", ">10y")))%>%
      arrange(., Tenor)%>%
      arrange(., Today)%>%
      mutate(Today = as.character(Today))
    
    if(input$curve_compare_dummy == F){
      data <- data%>%
        filter(Today == input$date_spreads_curve)
      
    }else{
      data <- data
    }
    
  })
  
  
  output$plot_rates_outstanding <- renderPlotly({
    
    p <- noma_tidy_plot(
      data_rates_outstanding(),
      "Today",
      "AvgRate",
      paste("sort_var"),
      paste(input$chart_type),
      colors = nb_colors,
      plot_title     = input$title,
      plot_subtitle  = input$subtitle,
      legend_options = list(showlegend = ifelse(input$sort_var_1 == "Today" & input$sort_var_2 == "Today", F, T)),
      yaxis_title    = "%")
    
    # Om knappen for "Styringsrenten" er huket av, legg til en sort linje med styringsrenten med funksjonen noma_add_line
    if(input$policy_rate_overlay_1 == T){
      p <- noma_add_line(p, policy_rate, "Today", "Value", "Today", name = "Styringsrenten", color = "black")
      
    } else
    {
      p <- p
    }
    
  })
  
  
  output$plot_rates_new_issues <- renderPlotly({
    
    k <- data_rates_new_issues()
    
    if(input$chart_type == "dot"){
      p <- noma_tidy_plot(
        k,
        "Today",
        "AvgRate",
        paste("sort_var"),
        paste("dot"),
        marker_size    = "IssuedAmountNOK",
        sizename       = "IssuedAmountNOK",
        plot_title     = input$title,
        plot_subtitle  = input$subtitle,
        colors = nb_colors,
        legend_options = list(showlegend = ifelse(input$sort_var == "Today", F, T)),
        yaxis_title    = "%",
        custom_hover_text = paste("<b>",k[[input$sort_var]],"</b>",
                                  "<br>Today : ", format(k$Today, "%d.%m.%Y"), 
                                  "<br> AvgRate : ", round(k$AvgRate, 2),
                                  "<br> IssuedAmountNOK : ", round(k$IssuedAmountNOK / 10^9, 3)))
      
      
      
      
    } else{
      
      p <- noma_tidy_plot(
        k,
        "Today",
        "AvgRate",
        paste("sort_var"),
        paste(input$chart_type),
        plot_title     = input$title,
        plot_subtitle  = input$subtitle,
        colors = nb_colors,
        legend_options = list(showlegend = ifelse(input$sort_var == "Today", F, T)),
        yaxis_title    = "%")
    }
    
    if(input$policy_rate_overlay_2 == T){
      p <- noma_add_line(p, policy_rate, "Today", "Value", "Today", name = "Styringsrenten", color = "black")
    } else{
      p <- p
    }
    
    
    
  })
  
  
  output$plot_spreads_time_series <- renderPlotly({
    
    noma_tidy_plot(
      data_spreads_time_series(),
      "Today",
      "Spread",
      paste("RiskClass"),
      paste("line"),
      plot_title    = "Kredittspreader per risikoklasse",
      plot_subtitle = "Basispunkter",
      colors = nb_colors
      
    ) 
    
  })
  
  output$plot_spreads_curve <- renderPlotly({
    
    
    k <- data_spreads_curve()
    
    if(input$curve_compare_dummy == F){
      plot_ly(k, x = ~Tenor, y = ~Spread, color = ~RiskClass, linetype = ~Today, 
              type = "scatter", mode = "markers",
              text = ~RiskClass,
              hovertemplate = paste(
                "<b>%{text}</b><br>",
                k$Today,"<br>",
                "%{yaxis.title.text}: ",round(k$Spread, 2),"<br>",
                "%{xaxis.title.text}: %{x}<br>",
                "<extra></extra>")
      )%>%
        layout(
          title = list(text = paste0("Kredittspreader per løpetid",
                                     '<br>',
                                     '<sup>',
                                     "Basispunkter"),
                       xanchor = "left",
                       x = 0.05,
                       y = 0.95)
        )
      
    } else{
      
      plot_ly(k, x = ~Tenor, y = ~Spread, color = ~RiskClass, linetype = ~Today, linetypes = c("dot", "solid"), 
              type = "scatter", mode = "markers",
              text = ~RiskClass,
              hovertemplate = paste(
                "<b>%{text}</b><br>",
                k$Today,"<br>",
                "%{yaxis.title.text}: ",round(k$Spread, 2),"<br>",
                "%{xaxis.title.text}: %{x}<br>",
                "<extra></extra>")
      )%>%
        layout(
          title = list(text = paste0("Kredittspreader per løpetid",
                                     '<br>',
                                     '<sup>',
                                     "Basispunkter"),
                       xanchor = "left",
                       x = 0.05,
                       y = 0.95)
        )    }
    
    
    
  })
  
  # Plot 1 til plot 4 er figurene som vises i fanen "utvalgte figurer".
  plot1_data <- reactive({
    
    tranche_daily%>%
      ungroup()%>%
      filter(Issuer_IndustryGrouping %in% c("Bank", "Finance"),
             RiskClassRisk           %in% c("Senior Unsecured", "Covered Bonds"))%>%
      group_by_at(c("Today", "RiskClassRisk"))%>%
      mutate(Today           = ceiling_date(Today, "quarter") - 1,
             IssuedAmountNOK = ifelse(IssuedAmountNOK < 0,
                                      0,
                                      IssuedAmountNOK))%>%
      summarise(Issued = sum(IssuedAmountNOK, na.rm = T)/10^9)%>%
      filter(Today >= input$dates_plots[1],
             Today <= input$dates_plots[2])
    
    
  })
  
  
  plot2_data <- reactive({
    
    tranche_daily%>%
      ungroup()%>%
      filter(Issuer_IndustryGrouping %in% c("Bank", "Finance"),
             RiskClassRisk           %in% c("Senior Unsecured", "Covered Bonds"))%>%
      group_by_at(c("Today", "RiskClassRisk"))%>%
      mutate(Today = ceiling_date(Today, unit = "quarter") - 1)%>%
      summarise(Issued = sum(IssuedAmountNOK, na.rm = T)/10^9)%>%
      filter(Today >= input$dates_plots[1],
             Today <= input$dates_plots[2])
    
    
  })
  
  
  plot4_data <- reactive({
    
    tranche_daily%>%
      ungroup()%>%
      filter(Issuer_IndustryGrouping %in% c("Bank", "Finance"),
             RiskClassRisk           %in% c("Senior Unsecured", "Covered Bonds"),
             IssueType                == "Bonds",
             CurrentInterestType      != "Zero",
             IssuedAmountNOK > 0)%>%
      group_by_at("ISIN")%>%
      filter(Today == min(Today),
             Today >= input$dates_plots[1],
             Today <= input$dates_plots[2])
    
    
  })
  
  
  plot3_data <- reactive({
    
    issued <- tranche_daily%>%
      ungroup()%>%
      filter(RiskClassRisk %in% c("Covered Bonds", "Senior Unsecured"),
             Issuer_IndustryGrouping %in% c("Bank", "Finance"),
             IssuedAmountNOK > 0)%>%
      mutate(Year = ceiling_date(Today, "year") - 1)%>%
      group_by(Year, RiskClassRisk)%>%
      summarise(Value = sum(IssuedAmountNOK, na.rm = T)/10^9)%>%
      mutate(Type = "Issued")
    
    matured <- tranche_daily%>%
      ungroup()%>%
      filter(RiskClassRisk %in% c("Covered Bonds", "Senior Unsecured"),
             Issuer_IndustryGrouping %in% c("Bank", "Finance"),
             IssuedAmountNOK < 0)%>%
      mutate(Year = ceiling_date(Today, "year") - 1)%>%
      group_by(Year, RiskClassRisk)%>%
      summarise(Value = sum(IssuedAmountNOK, na.rm = T)/10^9)%>%
      mutate(Type = "Maturing")
    
    maturing <- tranche_daily%>%
      ungroup()%>%
      filter(RiskClassRisk %in% c("Covered Bonds", "Senior Unsecured"),
             Issuer_IndustryGrouping %in% c("Bank", "Finance"),
             MaturityDate > update)%>%
      mutate(Year = ceiling_date(MaturityDate, "year") - 1)%>%
      group_by(ISIN)%>%
      filter(Today == last(Today))%>%
      group_by(Year, RiskClassRisk)%>%
      summarise(Value = -sum(CurrentOutstandingAmountNOK, na.rm = T)/10^9)%>%
      mutate(Type = "Maturing")
    
    data <- bind_rows(issued, matured, maturing)%>%
      ungroup()%>%
      mutate(Type = paste0(RiskClassRisk, " ", Type))%>%
      filter(Year >= "2014-01-01",
             Year <= "2025-01-01")
    
    return(data)
    
    
    
  })
  
  
  
  output$plot1 <- renderPlotly({
    
    noma_tidy_plot(
      plot1_data(),
      "Today",
      "Issued",
      paste("RiskClassRisk"),
      paste("stacked bar"),
      colors = nb_colors,
      plot_title     = "Bruttoutstedelser av OMF og senior i bank og finans",
      plot_subtitle  = "Milliarder kroner")%>%
      layout(barmode = "relative")
    
  })
  
  output$plot2 <- renderPlotly({
    
    noma_tidy_plot(
      plot2_data(),
      "Today",
      "Issued",
      paste("RiskClassRisk"),
      paste("stacked bar"),
      colors = nb_colors,
      plot_title     = "Nettoutstedelser av OMF og senior i bank of finans",
      plot_subtitle  = "Milliarder kroner")%>%
      layout(barmode = "relative")
    
    
  })
  
  output$plot4 <- renderPlotly({
    
    k <- plot4_data()
    
    noma_tidy_plot(
      k,
      "Today",
      "CurrentCouponRate",
      paste("RiskClassRisk"),
      paste("dot"),
      marker_size    = "IssuedAmountNOK",
      sizename       = "IssuedAmountNOK",
      plot_title     = "Nyutstedelser av OMF og senior i bank of finans",
      plot_subtitle  = "Volumvektet kupongrente (%) og volum (milliarder kroner)",
      colors = nb_colors,
      custom_hover_text = paste("<b>",k$RiskClassRisk,"</b>",
                                "<br>Today : ", format(k$Today, "%d.%m.%Y"), 
                                "<br> CurrentCouponRate : ", round(k$CurrentCouponRate, 2),
                                "<br> IssuedAmountNOK : ", round(k$IssuedAmountNOK / 10^9, 3)))%>%
      noma_add_line(policy_rate, "Today", "Value", "Today", name = "Styringsrenten", color = "black")
    
  })
  
  
  output$plot3 <- renderPlotly({
    
    p <- noma_tidy_plot(
      plot3_data(),
      "Year",
      "Value",
      paste("Type"),
      paste("stacked bar"),
      colors = nb_colors,
      plot_title     = "Utstedelser og forfall av OMF og senior i bank og finans",
      plot_subtitle  = paste0("Milliarder kroner. Per", " ", format(update, "%d.%m.%Y")))%>%
      layout(barmode = "relative")
    
    
  })
  
  
  
  
  
}

# Lag en app basert pÃ ui og server slik de er definert i koden over.
shinyApp(ui = ui, server = server)



