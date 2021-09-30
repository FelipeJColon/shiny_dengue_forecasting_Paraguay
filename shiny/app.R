
source("./load_pkgs.R")

source("./preprocess.R")

thematic::thematic_shiny()

ui <- navbarPage(
  
  theme=bs_theme(version=4,
                 bootswatch = "minty",
                 heading_font = font_google("Roboto"),
                 base_font = font_google("Montserrat")),
  
  collapsible = FALSE,
  id="navBar",
  windowTitle = app_title,
  
  title = div(
    a(img(src="cmmid_logo2.png", height="45px"),
      href="https://cmmid.lshtm.ac.uk/"),
    span(app_title, style="line-height:50px"),
  ),
  
  tabPanel(title="Home",
           sidebarLayout(
             sidebarPanel(
               width=3,
               box(width = NULL,
                   status = "primary",
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   infoBoxOutput("total_cases", width = 12),
                   infoBoxOutput("province_max", width = 12),
                   infoBoxOutput("province_max_inc", width = 12)),
               
               br(),
               sliderInput("plot_date",
                           label = tags$em("Select a date to display dengue case data"),
                           min = as.Date(min_date,"%Y-%m-%d"),
                           max = as.Date(current_date,"%Y-%m-%d"),
                           value = as.Date(current_date), #%m-%
                           step=30,
                           timeFormat = "%b %y", 
                           animate=animationOptions(interval = 1700, 
                                                    loop = TRUE)),
               
               # br(),
               # p(textOutput("risk"), align = "left"),
               # p(textOutput("prob_outbreak"), align = "left")
             ),
             
             mainPanel(width=9,
                       tabPanel(title="",
                                h3("Welcome!"), 
                                p("This Shiny app has been developed to 
                                    visualise monthly probabilistic dengue 
                                   forecasts for Paraguay up to six months ahead 
                                   of time. "),
                                p("The aim of this Shiny app is to provide the ",
                                  tags$a(href="http://dgvs.mspbs.gov.py/page/#vista_boletines_dpto.html",
                                         "Ministerio de Salud Pública y Bienestar Social."),
                                  "with a support tool to aid decision-making
                                  and budgeting processes ahead of time."),
                                p("The map below allows you to visualise the
                                    spatiotemporal trends in historical dengue 
                                    case data for the period January 2012 to 
                                    August 2021 on a monthly basis. The desired
                                    date can be selected using the sliding bar on 
                                   the left-hand side box."),
                                leafletOutput(outputId="map", 
                                              width="100%",
                                              height=610),
                                br(), br()
                       )
                       
             )
           )
  ),
  tabPanel("Historical data",
           
           sidebarLayout(
             sidebarPanel(
               width=2,
               h5("Variables"),
               pickerInput("clima_select", 
                           choices = c("Cases",
                                       "Incidence",
                                       "Temperature",
                                       "Precipitation",
                                       "Specific humidity",
                                       "Wind speed",
                                       "ENSO index"), 
                           selected = c("Cases"),
                           multiple = FALSE),
               p("Select one variable from the menu"),   
               br(), 
               h5("Provinces"),
               pickerInput("province_select_clim", 
                           
                           
                           choices = as.character(
                             unique(obs_incidence$engname)), 
                           options = list(`actions-box` = TRUE),
                           selected = unique(obs_incidence$engname),
                           multiple = TRUE), 
               p("Select one or multiple provinces from the menu"),
               
               # uiOutput("text_slider"),  
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Temporal tends", 
                          h3("Historical data"),
                          p("On this tab you will be able to visualise the
                              temporal behaviour of the variables included in
                              the forecasting models. To do so, you may select 
                              a variable from the top menu on the right.
                              In addition, you may select a particular 
                              province or group of provinces from the second 
                              menu on the right."),
                          h5("Time series"),
                          p("This plot offers a visualisation of 
                              the time series data for a given variable. 
                              The different colours indicate the 
                              provinces selected. The", tags$em("X-axis"), 
                            "indicates the date and the", tags$em("Y-axis"),
                            "indicates the value of the variables investigated
                              in their corresponding units:"),
                          tags$li("Cases: number of individual cases reported"),
                          tags$li("Incidence: cases reported per 100 000 people"),
                          tags$li("Temperature: degrees Celsius"),
                          tags$li("Precipitation: mm per day"),
                          tags$li("Specific humidity: dimensionless"),
                          tags$li("Wind speed: metres per second"),
                          tags$li("ENSO index: degrees Celsius"),
                          apexchartOutput("clima_plot"),
                          br(), br()
                 ),
                 tabPanel("Seasonal trends", 
                          # br(),
                          h5("Seasonal trends"),
                          p("Here, you will be able to visualise 
                              the mean seasonal trends of the variable of
                              your choice. The different colours indicate 
                              the corresponding values for the province(s) 
                              selected."),
                          p("The", tags$em("X-axis"), 
                            "indicates the month of the year and the", 
                            tags$em("Y-axis"),
                            "indicates the value of the variables investigated
                                in their corresponding units:"),
                          tags$li("Cases: number of individual cases reported"),
                          tags$li("Incidence: cases reported per 100 000 people"),
                          tags$li("Temperature: degrees Celsius"),
                          tags$li("Precipitation: milimetres per day"),
                          tags$li("Specific humidity: dimensionless"),
                          tags$li("Wind speed: metres per second"),
                          tags$li("ENSO index: degrees Celsius"),
                          apexchartOutput("seas_plot"),
                          br(), br(), br(), br()
                 ),
                 tabPanel("Inter-annual trends", 
                          # br(),
                          h5("Inter-annual variability"),
                          p("Here, you will be able to visualise 
                              between-year variations on each of the variables
                              used in the model. Different colours indicate 
                              the different the provinces selected."),
                          p("The", tags$em("X-axis"), 
                            "indicates the years of the data, and the", 
                            tags$em("Y-axis"),
                            "indicates the value of the variables 
                                    investigated in their corresponding units:"),
                          tags$li("Cases: number of individual cases reported"),
                          tags$li("Incidence: cases reported per 100 000 people"),
                          tags$li("Temperature: degrees Celsius"),
                          tags$li("Precipitation: mm per day"),
                          tags$li("Specific humidity: dimensionless"),
                          tags$li("Wind speed: metres per second"),
                          tags$li("ENSO index: degrees Celsius"),
                          apexchartOutput("interann_plot"),
                          br(), br(), br(), br()
                 )
               )
             )
           )
  ),
  tabPanel("Forecasts",
           
           sidebarLayout(
             sidebarPanel(
               width=2,
               pickerInput("prov_select", 
                           h5("Province"),
                           choices = as.character(
                             unique(obs_incidence$engname)), 
                           options = list(`actions-box` = TRUE),
                           selected = "Central",
                           multiple = FALSE), 
               tags$p("Select a province from the menu"),
               br(), br(),
               pickerInput("forecast_select", 
                           h5("Forecast period"),
                           choices = as.character(
                             unique(incidence_db$initdate[
                               !is.na(incidence_db$initdate)
                             ])), 
                           options = list(`actions-box` = TRUE),
                           selected = "20210701",
                           multiple = FALSE), 
               tags$p("Select a forecast period from the menu"),
               br(), br(),
               shinyWidgets::prettySwitch("log", 
                                          label="Logarithmic scale",
                                          value = FALSE),
               # shinyWidgets::prettySwitch('button1',
               #                            label="Show values")
               ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Time series plots", 
                          tags$h4("Six-month dengue forecasts"), 
                          tags$p("This tab presents a visualisation of the forecasts
                     issued by our model. The plot below presents a time series 
                     of the predicted changes in dengue risk up to six months
                     ahead. You may select a given province and a forecast period
                     from the menu on the left. The first forecast date was February
                     2020. Forecasts have then been issued monthly."), 
                          
                          p("These forecasts represent the risk of dengue transmission",
                            tags$em('due to climate.'), "The plot is divided into two 
                         main sections. The
                         pre-forecast period comprises the historical data used
                         to train the model. The forecast period correspond to 
                         the period for which a forecast is issued. The solid 
                         blue lines indicate the historical (i.e., observed)
                         dengue incidence rate per 100 000 people up to one 
                         month before the first
                         forecast month. The gray solid lines indicate the 
                         posterior mean for each of 42 forecast ensemble members.
                         Gray dashed lines correspond to the upper and lower 
                         bounds 95% credible interval of the forecast. The red
                         line indicate 95th percentile of the distribution of 
                         the dengue data used here as an outbreak threshold. 
                         Outbreak periods are considered as those months where
                         the incidence rate is above the red line.
                         Notice that outbreak thresholds are province- and 
                         month-specific."),
                          tags$p("Notice that the local risk of infection observed
                         may significantly differ from the predictions, due to
                         the effects of interventions and other variables not 
                         explictly included in the model such as immunity or 
                         changes in diagnostic techniques."),
                          tags$p("We have included a button on the left-hand side 
                            menu to allow the visualisation of the plot on a
                            logarithmic scale. This may be useful when the
                            lines on the plot are so close to one another that
                            it is hard to distinguish the location of the 
                            outbreak threshold on the Y-axis."),
                          br(),
                          plotOutput("thr_plot",
                                     height="450px",
                                     width='100%'),
                          br(), br(), 
                          dataTableOutput("table_epid"),
                          br()
                 ),
                 tabPanel("Dengue incidence maps",
                          tags$h5("Spatial patterns of the dengue forecasts"),
                          p("This tab presents a visualisation of the spatial
                          patterns of the forecasts issued by our model. The 
                          maps below present the mean predicted dengue
                          incidence rate per 100 000 people up to six months 
                          ahead. You may select a given 
                          forecast period from the menu on the left. Notice
                            that the menu used to select a province is not 
                            active on this sub-tab."), 
                          
                          p("We remind you that these forecasts represent the
                          risk of dengue transmission due to climate. The plot
                          is divided into six maps, each of them corresponding
                          to a time lead of the forecast."),
                          
                          p("Notice that the local risk of infection observed
                          may significantly differ from the predictions, due to 
                          the effects of interventions and other variables not
                          explictly included in the model such as immunity or
                          changes in diagnostic techniques."),
                          
                          p("Notice that the
                            button for the logarithmic scale is not active
                            for this tab."),
                          
                          plotOutput("map_preds1",
                                     height="1000px",
                                     width='100%')
                 ),
                 tabPanel("Probability maps",
                          tags$h5("Probability of exceeding the outbreak threshold"),
                          p("This tab presents a visualisation of the posterior
                          probability of exceeding the outbreak threshold according
                          to our model up to six months ahead. You may select a given 
                          forecast period from the menu on the left."), 
                          
                          p("We remind you that these forecasts represent the
                          risk of dengue transmission due to climate. The plot
                          is divided into six maps, each of them corresponding
                          to a time lead of the forecast."),
                          
                          p("Notice that the
                            button for the logarithmic scale is not active
                            for this tab."),
                          
                          plotOutput("map_preds2",
                                     height="1000px",
                                     width='100%')
                 ),
                 tabPanel("Climate suitability",
                          tags$h5("Months suitable for dengue transmission"),
                          p("This tab presents a visualisation of the number
                          of months predicted to be suitable for dengue 
                          transmission. Suitability was defined as periods when
                          the predicted mean incidence rate was greater than
                          one case per 100 000 people following",
                            tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1003542",
                                   "(Colón-González et al., 2021)"), "and",
                            tags$a(href="https://www.thelancet.com/pdfs/journals/lanplh/PIIS2542-5196(21)00132-7.pdf",
                                   "(Colón-González et al., 2021b)")), 
                          
                          p("We remind you that these forecasts represent the
                          risk of dengue transmission due to climate and that 
                          the risk experience may be different to the one predicted
                          by the models."), 
                          
                          p("As before, the plot
                          is divided into six maps, each of them corresponding
                          to a time lead of the forecast."),
                          
                          p("Notice that the button for the logarithmic scale
                          is not active for this tab."),
                          
                          plotOutput("map_lts",
                                     height="1000px",
                                     width='100%')
                 )
               )
             )
           )
  ),
  
  tabPanel("Skill evaluation",
           
           # tabsetPanel(
           tabPanel(
             # tags$div(
             "",
             tags$h4("Skill of the system"),
             p("This tab presents a visual summary of the overall 
                      outbreak detection skill of the forecasting system
                      evaluated using historical data for the period 2012
                      to 2016."), 
             
             p("In the plot below, orange colours indicate
                      the proportion of correct predictions. Blue colours indicate
                      the proportion of incorrect predictions. The different 
                      bars indicate the time lead ranging between one to six
                      months ahead. As can be observed, the number of correct
                      predictions is considerably larger than that of the 
                      incorrect predictions."),
             
             plotOutput("hit_missed",
                        height="950px",
                        width='90%'),
             br(), br(), br(), br()
             # )
             
           )
  ),
  
  tabPanel("About this forecasting tool",
           # tabsetPanel(
           tabPanel("Summary",
                    tags$div(
                      tags$h4("Summary", align="center"),
                      p("The methodology used for the underlying forecasting
                              system is described in a previously published study", 
                        tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1003542",
                               "(Colón-González et al., 2021)"),
                        "and it is designed to be flexible and 
                              applicable to other locations. However, care should
                              be taken when working with a new case study. The
                              methodology should be co-designed with climate and 
                              health stakeholders in the region of interest."), 
                      p("The disease data and knowledge about the timing
                              of the start of the dengue season (e.g., January 
                              or later in the year), timing of intervention schedules, 
                              etc., should be discussed with public health 
                              experts in advance. A range of meteorological 
                              variables should be sourced from meteorology
                              stations or satellite products in collaboration 
                              with local meteorological services. The observed 
                              meteorological inputs should align with climate
                              forecast products and forecast lead-times should
                              be considered. If spatially resolved data are 
                              available, the framework will have to adapted to 
                              incorporate spatial information and account for 
                              spatial dependency structures."),
                      tags$br(),
                      tags$h4("Authors", align="center"),
                      uiOutput("text1"),
                      tags$h4("Contact", align="center"),
                      p("For all enquiries about this site please contact 
                            us at", tags$a(href='mailto:felipe.colon@lshtm.ac.uk',
                                           "Felipe.Colon@lshtm.ac.uk")),
                      tags$br(),tags$br(),
                      "To know more about of our work please visit: ",
                      tags$br(),
                      tags$a(href="http://cmmid.lshtm.ac.uk/",
                             "Centre for the Mathematical Modelling of
                        Infectious Diseases (CMMID)."),
                      tags$br(),
                      tags$a(href="https://www.lshtm.ac.uk/research/centres/centre-climate-change-and-planetary-health",
                             "Centre on Climate Change and Planetary Health
                        (CCCPH)."),
                      tags$br(),
                      tags$a(href="https://www.lshtm.ac.uk/",
                             "London School of Hygiene and Tropical Medicine
                        (LSHTM)."),
                      tags$br(),tags$br(),
                      tags$br(),
                      tags$h4("Disclaimer", align="center"),
                      "This app has been developed for research purposes only.",
                      "Data are provided without any warranty of any kind, either ",
                      "express or implied. The entire risk arising out of the ",
                      "use or performance of the app and associated data remains",
                      "with you.", 
                      "In no event shall the London School of Hygiene and ",
                      "Tropical Medicine (LSHTM), the authors, or anyone else",
                      "involved in the creation, production, or delivery of the",
                      "app be liable for any damages whatsoever including,",
                      "without limitation, damages for loss of business profits,",
                      "business interruption, loss of business information, or ",
                      "other pecuniary loss arising out of the use of or inability",
                      "to use this app even if LSHTM has been advised of the ",
                      "possibility of such damages.",
                      tags$br(),tags$br(),
                      tags$br(),tags$br(),
                      tags$br(),tags$br(),
                      tags$img(src = "cmmid_logo2.png", width = "210px",
                               height = "75px"), 
                      tags$img(src = "CCCPH_Logo_colour.png",
                               width = "250px", height = "75px"),
                      tags$img(src = "lshtm_logo.png", width = "190px",
                               height = "75px"),
                      tags$br(),tags$br(),
                      "London School of Hygiene and Tropical Medicine",
                      tags$br(),
                      "Keppel Street",
                      tags$br(),
                      "London",
                      tags$br(),
                      "WC1E 7HT",
                      tags$br(),
                      "+44 (0) 20 7636 8636",
                      tags$br(), tags$br()
                      # )
                    )
           )
  )
)


server <- function(input, output, session) {
  
  output$total_cases <- renderInfoBox({
    infoBox(
      title = paste("Dengue cases reported in Paraguay in", 
                    last_date, ":", sep=" "), 
      fill = FALSE,
      value =  format(as.numeric(last_month_cases), big.mark=","),
      icon = icon("user"),
      color = "black")
  })
  
  output$province_max <- renderInfoBox({
    infoBox(
      title = paste("The province with the largest number of cases was", 
                    prov_max_cases$engname, "with", sep=" "), 
      fill = FALSE,
      value =  format(as.numeric(max_cases), big.mark=","),
      icon = icon("atlas"),
      color = "black")
  })
  
  output$province_max_inc <- renderInfoBox({
    infoBox(
      title = paste("The province with the largest incidence rate was", 
                    prov_max_incidence$engname, "with", sep=" "), 
      fill = FALSE,
      value =  paste(format(as.numeric(round(max_incidence)), big.mark=","), 
                     "cases per 100 000 people"),
      icon = icon("head-side-virus"),
      color = "black")
  })
  
  output$map <- renderLeaflet({
    
    map <- myMap
    
    # add data to map
    mon_data  <- month(ymd(input$plot_date))
    year_data <- year(ymd(input$plot_date))
    
    datafiltered <- dplyr::filter(observed, 
                                  month==mon_data &
                                    year==year_data)
    map@data %<>% inner_join(datafiltered)
    
    
    bins <- unique(c(0, RoundTo(exp(seq(log1p(min(map$dengue_cases, na.rm=TRUE)), 
                                        log1p(max(map$dengue_cases, na.rm=TRUE)), 
                                        length = 10)), 5)))
    
    pal <- colorBin(colorRampPalette(c("#f1faee", "#a8dadc", "#457b9d", 
                                       "#1d3557", "#ee6c4d"))(10),
                    domain = map$dengue_cases, 
                    bins=bins)
    
    leaflet(map,
            options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>%
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addProviderTiles('CartoDB.Voyager') %>%
      addPolygons(
        fillColor = ~ pal(dengue_cases),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        weight = 3,
        label= ~ lapply(paste0("<b>", NAME_1, "</b><br>",
                               tsdatetime,"<br>", 
                               input$sel_data, ": ", 
                               round(dengue_cases,1) ), 
                        htmltools::HTML),
        labelOptions = labelOptions(style=list('font-size' = '14px'))) %>%
      leaflet::addLegend(
        "bottomright",
        pal = pal, values = ~dengue_cases,
        opacity = 0.75, title = input$sel_data
      )
  })
  
  # Tab 2
  
  area_reactive_clima <- reactive({
    
    if (input$clima_select=="Cases") { 
      obs_incidence$outcome = obs_incidence$dengue_cases
    } 
    if (input$clima_select=="Incidence") { 
      obs_incidence$outcome = obs_incidence$incidence
    } 
    if (input$clima_select=="Temperature") { 
      obs_incidence$outcome = obs_incidence$mean_temperature
    } 
    if (input$clima_select=="Precipitation") { 
      obs_incidence$outcome = obs_incidence$precipitation_amount_per_day
    } 
    if (input$clima_select=="ENSO index") { 
      obs_incidence$outcome = obs_incidence$nino34_anomaly
    } 
    if (input$clima_select=="Wind speed") { 
      obs_incidence$outcome = obs_incidence$wind_speed
    } 
    if (input$clima_select=="Specific humidity") { 
      obs_incidence$outcome = obs_incidence$specific_surface_humidity
    } 
    
    obs_incidence %>% 
      filter(engname %in% input$province_select_clim)
  })
  
  output$clima_plot <- renderApexchart({
    
    pal <- c("#1f77b4", "#aec7e8", "#ff7f0e",
             "#ffbb78", "#2ca02c", "#98df8a",
             "#d62728", "#ff9896", "#9467bd",
             "#c5b0d5", "#8c564b", "#c49c94",
             "#e377c2", "#f7b6d2", "#7f7f7f",
             "#c7c7c7", "#bcbd22", "#dbdb8d",
             "#17becf", "#9edae5")
    nval <- length(unique(area_reactive_clima()$areaid))
    cols <- pal[1:nval]
    
    apex(data = area_reactive_clima(), type = "line", 
         mapping = aes(x = tsdatetime, 
                       y = outcome, 
                       group = capitalize(areaid))) %>% 
      ax_colors(cols) %>%
      ax_yaxis(decimalsInFloat = 2) # number of decimals to keep
  })
  
  output$seas_plot <- renderApexchart({
    
    data_seas <- area_reactive_clima() %>%
      dplyr::mutate(month=month(tsdatetime)) %>%
      dplyr::group_by(areaid, month) %>%
      dplyr::summarise(outcome=mean(outcome))
    
    pal <- c("#1f77b4", "#aec7e8", "#ff7f0e",
             "#ffbb78", "#2ca02c", "#98df8a",
             "#d62728", "#ff9896", "#9467bd",
             "#c5b0d5", "#8c564b", "#c49c94",
             "#e377c2", "#f7b6d2", "#7f7f7f",
             "#c7c7c7", "#bcbd22", "#dbdb8d",
             "#17becf", "#9edae5")
    nval <- length(unique(area_reactive_clima()$areaid))
    cols <- pal[1:nval]
    
    apex(data = data_seas, type = "line", 
         mapping = aes(x = month, 
                       y = outcome, 
                       group = capitalize(areaid))) %>% 
      ax_colors(cols) %>%
      # ax_labs(x="Month", y=y_lab) %>%
      ax_yaxis(decimalsInFloat = 2) # number of decimals to keep
  })
  
  output$interann_plot <- renderApexchart({
    
    data_seas <- area_reactive_clima() %>%
      dplyr::mutate(year=year(tsdatetime)) %>%
      dplyr::group_by(areaid, year) %>%
      dplyr::summarise(outcome=mean(outcome))
    
    pal <- c("#1f77b4", "#aec7e8", "#ff7f0e",
             "#ffbb78", "#2ca02c", "#98df8a",
             "#d62728", "#ff9896", "#9467bd",
             "#c5b0d5", "#8c564b", "#c49c94",
             "#e377c2", "#f7b6d2", "#7f7f7f",
             "#c7c7c7", "#bcbd22", "#dbdb8d",
             "#17becf", "#9edae5")
    
    nval <- length(unique(area_reactive_clima()$areaid))
    cols <- pal[1:nval]
    
    apex(data = data_seas, type = "line", 
         mapping = aes(x = year, 
                       y = outcome, 
                       group = capitalize(areaid))) %>% 
      ax_colors(cols) %>%
      ax_yaxis(decimalsInFloat = 2) # number of decimals to keep
  })
  
  
  # Tab 3
  
  area_reactive_thr <- reactive({
    ob1 <- forecasts %>% 
      dplyr::filter(engname %in% input$prov_select,
                    initdate %in% input$forecast_select) %>%
      # dplyr::select(-dengue_cases) %>%
      # dplyr::mutate(epi95=(exp(epi.q95)/1e5)*population) %>%
      data.table()
    
    plot_date <- ymd(min(ob1$tsdatetime))
    
    ob2 <- observed %>%
      inner_join(myMap@data) %>%
      dplyr::filter(engname %in% input$prov_select,
                    tsdatetime < plot_date,
                    tsdatetime > plot_date %m-% months(6)) %>%
      data.table()
    
    full_join(ob1, ob2)
  })
  
  output$thr_plot <- renderPlot({ 
    
    y0 <- max(area_reactive_thr()$uuci, na.rm=TRUE)
    x1 <- max(area_reactive_thr()$tsdatetime) - months(7)
    x2 <- max(area_reactive_thr()$tsdatetime) - months(2)
    
    if(input$log){
      th1 <- thr_forecast_quant(area_reactive_thr()) +
        # scale_x_date(date_labels="%b.%y", date_breaks = "1 month") +
        scale_y_continuous("Log + 1", trans="log1p",
                           breaks = scales::trans_breaks(
                             'log1p', function(x) exp(x+1),
                             n = nticks),
                           labels = scales::trans_format(
                             'log1p', scales::math_format(
                               e^.x, function(x) round(x, 2)))) +
        annotate(geom="text", y=y0, x=x1,
                 label="Pre-forecast period", size=6) +
        annotate(geom="text", y=y0, x=x2, 
                 label="Forecast period", size=6) +
        theme(legend.text=element_text(size=14)) 
      
    } else {
      th1 <- thr_forecast_quant(area_reactive_thr()) +
        # scale_x_date(date_labels="%b.%y", date_breaks = "1 month") +
        annotate(geom="text", y=y0, x=x1,
                 label="Pre-forecast period", size=6) +
        annotate(geom="text", y=y0, x=x2, 
                 label="Forecast period", size=6) +
        theme(legend.text=element_text(size=14)) 
      
    }
    
    return(th1)
  })
  
  output$table_epid <- renderDataTable({
    
    ob1 <- forecasts %>% 
      dplyr::filter(engname %in% input$prov_select,
                    initdate %in% input$forecast_select) %>%
      # dplyr::select(-dengue_cases) %>%
      group_by(engname, initdate, tsdatetime) %>%
      dplyr::summarise(Mean.prediction=mean(preds),
                       Median.prediction=median(preds),
                       Lower.bound=min(llci),
                       Upper.bound=max(uuci),
                       Outbreak.threshold=mean(epi.q95)) %>%
      dplyr::rename(Province=engname,
                    Date=tsdatetime,
                    Issue.date=initdate) %>%
      dplyr::mutate(Forecast.month=month(Date)) %>%
      dplyr::select(Province, Issue.date, Forecast.month,
                    Mean.prediction, Median.prediction,
                    Lower.bound, Upper.bound, 
                    Outbreak.threshold) %>%
      data.table()
    
    ob2 <- probs %>%
      dplyr::filter(ID %in% input$prov_select,
                    initdate %in% input$forecast_select) %>%
      dplyr::select(ID, initdate, Date,
                    probq95) %>%
      dplyr::rename(Province=ID,
                    Issue.date=initdate,
                    Outbreak.probability=probq95) %>%
      dplyr::mutate(Province=as.character(Province),
                    Forecast.month=month(Date)) %>%
      dplyr::select(-Date) %>%
      data.table()
    
    inner_join(ob1, ob2)
  })
  
  
  area_reactive_maps <- reactive({
    
    preds_inc %>% 
      filter(initdate %in% input$forecast_select) %>%
      dplyr::mutate(lead=as.numeric(as.factor(tsdatetime)),
                    lead=factor(paste(lead, "months")),
                    lead=factor(lead, levels=rev(levels(lead))),
                    lead=recode(lead, "1 months"="1 month")) %>%
      group_by(initdate, tsdatetime, areaid, lead, Quantile_95) %>%
      dplyr::summarise(preds=mean(preds, na.rm=TRUE),
                       epi.q95=mean(epi.q95))
    
  })
  
  output$map_preds1 <- renderPlot({
    
    map <- myMap
    
    # brks <- unique(RoundTo(quantile(area_reactive_maps()$preds,
    #                                 probs=seq(0,1, by=0.2)), 5),
    #                digits=0)
    # 
    brks <- unique(c(0, RoundTo(exp(seq(log1p(min(area_reactive_maps()$preds, na.rm=TRUE)), 
                                        log1p(max(area_reactive_maps()$preds, na.rm=TRUE)), 
                                        length = 4)), 2)))
    
    brks[1] <- 0
    n       <- length(brks)
    brks[n] <- max(brks) + 2

    cols <- c("#f1faee", "#a8dadc", "#457b9d","#ee6c4d")
    
    mapdata <- area_reactive_maps() %>%
      inner_join(map@data) %>%
      dplyr::mutate(outcome_bins=cut(preds, breaks=brks,
                                     include.lowest=TRUE,
                                     dig.lab=5)) %>%
      dplyr::filter(!is.na(outcome_bins))
    
    mapdata$outcome_bins <- factor(mapdata$outcome_bins, ordered=TRUE)
    mapdata$outcome_bins <- factor(mapdata$outcome_bins,
                                   levels=levels(mapdata$outcome_bins),
                                   ordered=TRUE)
    levels(mapdata$outcome_bins) <- gsub(",", "-",
                                         levels(mapdata$outcome_bins))
    levels(mapdata$outcome_bins) <- gsub("[[]", "",
                                         levels(mapdata$outcome_bins))
    levels(mapdata$outcome_bins) <- gsub("[]]", "",
                                         levels(mapdata$outcome_bins))
    levels(mapdata$outcome_bins) <- gsub("[(]", "",
    levels(mapdata$outcome_bins))
    
    mapnew <- sf::read_sf(file.path("input_data",
                                    "paraguay_province.shp")) %>%
      full_join(mapdata) 
    
    # mapnew$X    <- st_coordinates(st_centroid(mapnew))[,1]
    # mapnew$Y    <- st_coordinates(st_centroid(mapnew))[,2]
    # mapnew$X[3] <- -54.75 # Correct centroid for Alto Parana
    # 
    # if(input$button1){
    #   
    #   ix <- mapnew$areaid %in% c("asuncion", "central")
    #   iy <- mapnew$areaid %in% c("alto parana")
    #   
    #   x_range <- abs(Reduce("-", range(mapnew$X)))
    #   y_range <- abs(Reduce("-", range(mapnew$Y)))
    #   
    #   mapnew$nudge_x <- 0
    #   mapnew$nudge_y <- 0
    #   mapnew$nudge_x[ix] <- -1 * 0.20 * x_range
    #   mapnew$nudge_y[ix] <- -1 * 0.05 * y_range
    #   mapnew$nudge_x[iy] <-  1 * 0.20 * x_range
    #   mapnew$nudge_y[iy] <- -1 * 0.10 * x_range
    #   
    #   
    #   map <-  ggplot() + 
    #     geom_sf(data=mapnew, mapping=aes(fill=outcome_bins),
    #             color="#cfcfcf", size=0.5) +
    #     theme_minimal() +
    #     scale_fill_manual(values=cols, drop=FALSE) +
    #     labs(fill = "") +
    #     theme_minimal() +
    #     theme(axis.text=element_text(size=12)) +
    #     theme(legend.text=element_text(size=16),
    #           legend.title=element_text(size=18)) +
    #     theme(strip.text.x=element_text(size=12)) +
    #     theme(legend.position="right") +
    #     # guides(fill = guide_legend(nrow =1)) +
    #     labs(title="Mean monthly dengue cases") +
    #     theme(plot.title = element_text(hjust = 0.5)) +
    #     theme(plot.title = element_text(size=20)) +
    #     theme(plot.caption = element_text(size=13, face="italic")) +
    #     geom_label_repel(data=mapnew, aes(x=X, y=Y, 
    #                                       label = round(outcome)),
    #                      size =5, min.segment.length = 0,
    #                      point.padding = NA,
    #                      colour="#5c6068", 
    #                      fontface = 'bold',
    #                      nudge_x = mapnew$nudge_x,
    #                      nudge_y = mapnew$nudge_y,
    #                      segment.color = "black") +
    #     guides(color = FALSE) +
    #     guides(fill = guide_legend(keywidth=3,
    #                                keyheight=3)) +
    #     xlab("") +
    #     ylab("") +
    #     xlim(-63,-53) +
    #     facet_wrap(~lead)
    #   
    #   map
    # } else {
      map <-  ggplot() + 
        geom_sf(data=mapnew, mapping=aes(fill=outcome_bins),
                color="#cfcfcf", size=0.5) +
        theme_minimal() +
        scale_fill_manual(values=cols, drop=FALSE) +
        # scale_fill_material('orange') +
        labs(fill = "") +
        theme_minimal() +
        theme(axis.text=element_text(size=12)) +
        theme(legend.text=element_text(size=16),
              legend.title=element_text(size=18)) +
        theme(strip.text.x=element_text(size=12)) +
        theme(legend.position="right") +
        guides(fill = guide_legend(nrow =1)) +
        labs(title="Mean monthly dengue cases") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.title = element_text(size=20)) +
        theme(plot.caption = element_text(size=13, face="italic")) +
        guides(fill = guide_legend(keywidth=3,
                                   keyheight=3)) +
        xlab("") +
        ylab("") +
        xlim(-63,-53) +
        facet_wrap(~lead)
      
      map
    # }
    
  })
  
  output$map_preds2 <- renderPlot({
    
    map <- myMap
    
    brks    <- seq(0, 1, by=0.25)
    brks[1] <- 0
    # n       <- length(brks)
    # brks[n] <- max(brks) + 5
    
    lvls <- c("[0,0.25]")
    
    cols <- c("#f1faee", "#a8dadc", "#457b9d","#ee6c4d")
    
    mapdata <- area_reactive_maps() %>%
      inner_join(map@data) %>%
      dplyr::mutate(outcome_bins=cut(Quantile_95, breaks=brks, 
                                     include.lowest=TRUE,
                                     dig.lab=5)) %>%
      # dplyr::filter(!is.na(outcome_bins))  %>%
      dplyr::mutate(lead=factor(lead, 
                                levels=c("1 month",
                                         "2 months",
                                         "3 months",
                                         "4 months",
                                         "5 months",
                                         "6 months")))
    
    # mapdata$outcome_bins <- factor(mapdata$outcome_bins, ordered=TRUE)
    mapdata$outcome_bins <- factor(mapdata$outcome_bins, 
                                   levels=levels(mapdata$outcome_bins),
                                   ordered=TRUE)
    levels(mapdata$outcome_bins) <- gsub(",", "-", 
                                         levels(mapdata$outcome_bins))
    levels(mapdata$outcome_bins) <- gsub("[[]", "", 
                                         levels(mapdata$outcome_bins))
    levels(mapdata$outcome_bins) <- gsub("[]]", "", 
                                         levels(mapdata$outcome_bins))
    levels(mapdata$outcome_bins) <- gsub("[(]", "", 
                                         levels(mapdata$outcome_bins))
    
    mapnew <- sf::read_sf(file.path("input_data",
                                    "paraguay_province.shp")) %>%
      full_join(mapdata) 
    
    # mapnew$X    <- st_coordinates(st_centroid(mapnew))[,1]
    # mapnew$Y    <- st_coordinates(st_centroid(mapnew))[,2]
    # mapnew$X[3] <- -54.75 # Correct centroid for Alto Parana
    # 
    # if(input$button1){
    #   
    #   ix <- mapnew$areaid %in% c("asuncion", "central")
    #   iy <- mapnew$areaid %in% c("alto parana")
    #   
    #   x_range <- abs(Reduce("-", range(mapnew$X)))
    #   y_range <- abs(Reduce("-", range(mapnew$Y)))
    #   
    #   mapnew$nudge_x <- 0
    #   mapnew$nudge_y <- 0
    #   mapnew$nudge_x[ix] <- -1 * 0.20 * x_range
    #   mapnew$nudge_y[ix] <- -1 * 0.05 * y_range
    #   mapnew$nudge_x[iy] <-  1 * 0.20 * x_range
    #   mapnew$nudge_y[iy] <- -1 * 0.10 * x_range
    #   
    #   
    #   map <-  ggplot() + 
    #     geom_sf(data=mapnew, mapping=aes(fill=outcome_bins),
    #             color="#cfcfcf", size=0.5) +
    #     theme_minimal() +
    #     scale_fill_manual(values=cols, drop=FALSE) +
    #     labs(fill = "") +
    #     theme_minimal() +
    #     theme(axis.text=element_text(size=12)) +
    #     theme(legend.text=element_text(size=16),
    #           legend.title=element_text(size=18)) +
    #     theme(strip.text.x=element_text(size=12)) +
    #     theme(legend.position="right") +
    #     # guides(fill = guide_legend(nrow =1)) +
    #     labs(title="Probability of exceeding the outbreak threshold") +
    #     theme(plot.title = element_text(hjust = 0.5)) +
    #     theme(plot.title = element_text(size=20)) +
    #     theme(plot.caption = element_text(size=13, face="italic")) +
    #     geom_label_repel(data=mapnew, aes(x=X, y=Y, 
    #                                       label = round(threshold, 2)),
    #                      size =5, min.segment.length = 0,
    #                      point.padding = NA,
    #                      colour="#5c6068", 
    #                      fontface = 'bold',
    #                      nudge_x = mapnew$nudge_x,
    #                      nudge_y = mapnew$nudge_y,
    #                      segment.color = "black") +
    #     guides(color = FALSE) +
    #     guides(fill = guide_legend(keywidth=3,
    #                                keyheight=3)) +
    #     xlab("") +
    #     ylab("") +
    #     xlim(-63,-53) +
    #     facet_wrap(~lead)
    #   
    #   map
    # } else {
      map <-  ggplot() + 
        geom_sf(data=mapnew, mapping=aes(fill=outcome_bins),
                color="#cfcfcf", size=0.5) +
        theme_minimal() +
        scale_fill_manual(values=cols, drop=FALSE) +
        labs(fill = "") +
        theme_minimal() +
        theme(axis.text=element_text(size=12)) +
        theme(legend.text=element_text(size=16),
              legend.title=element_text(size=18)) +
        theme(strip.text.x=element_text(size=12)) +
        theme(legend.position="right") +
        guides(fill = guide_legend(nrow =1)) +
        labs(title="Probability of exceeding the outbreak threshold") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(plot.title = element_text(size=20)) +
        theme(plot.caption = element_text(size=13, face="italic")) +
        guides(fill = guide_legend(keywidth=3,
                                   keyheight=3)) +
        xlab("") +
        ylab("") +
        xlim(-63,-53) +
        facet_wrap(~lead)
      
      map
    # }
    
  })
  
  output$map_lts <- renderPlot({
    
    map <- myMap
    
    data <- lts %>% 
      filter(initdate %in% input$forecast_select) %>%
      dplyr::mutate(lead=as.numeric(as.factor(tsdatetime)),
                    lead=factor(paste(lead, "months")),
                    lead=factor(lead, levels=rev(levels(lead))),
                    lead=recode(lead, "1 months"="1 month"),
                    suitability=ifelse(lts>0, "Suitable", "Not suitable"))
    
    brks    <- c(0, seq(3, 12, by=3))
    # brks[1] <- 0
    n       <- length(brks)
    # brks[n] <- max(brks) + 5
    
    cols <- c("#457b9d","#ee6c4d")
    
    mapdata <- data %>%
      inner_join(map@data) %>%
      dplyr::mutate(lead=factor(lead, 
                                levels=c("1 month",
                                         "2 months",
                                         "3 months",
                                         "4 months",
                                         "5 months",
                                         "6 months")))
    
    mapnew <- sf::read_sf(file.path("input_data",
                                    "paraguay_province.shp")) %>%
      full_join(mapdata) 
    
    # mapnew$X    <- st_coordinates(st_centroid(mapnew))[,1]
    # mapnew$Y    <- st_coordinates(st_centroid(mapnew))[,2]
    # mapnew$X[3] <- -54.75 # Correct centroid for Alto Parana
    # 
    map <-  ggplot() + 
      geom_sf(data=mapnew, mapping=aes(fill=suitability),
              color="#cfcfcf", size=0.5) +
      theme_minimal() +
      scale_fill_manual(values=cols, drop=FALSE) +
      labs(fill = "") +
      theme_minimal() +
      theme(axis.text=element_text(size=12)) +
      theme(legend.text=element_text(size=16),
            legend.title=element_text(size=18)) +
      theme(strip.text.x=element_text(size=12)) +
      theme(legend.position="right") +
      guides(fill = guide_legend(nrow =1)) +
      labs(title="Suitability for dengue transmission") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(size=20)) +
      theme(plot.caption = element_text(size=13, face="italic")) +
      guides(fill = guide_legend(keywidth=3,
                                 keyheight=3)) +
      xlab("") +
      ylab("") +
      xlim(-63,-53) +
      facet_wrap(~lead)

    map
    
  })
  
  
  # Tab 4 
  
  output$hit_missed <- renderPlot({
    
    colours <- c("hits"="#ff9f1c",
                 "correct_rejections" = "#ffbf69",
                 "false_alarms"="#91c8d9",
                 "missed"="#3d5a80")
    
    g1 <- ggplot(hitmiss, aes(fill=conditionf, 
                              y=value, 
                              x=factor(lead))) + 
      geom_bar(position="fill", stat="identity") +
      scale_fill_manual("", values=colours, 
                        labels=c("Hits",
                                 "Correct rejections",
                                 "False alarms",
                                 "Missed")) +
      
      facet_wrap(~NAME_1, ncol=4) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(size=20)) +
      theme(legend.text=element_text(size=18)) +
      theme(axis.text.x = element_text(size=13),
            axis.text.y = element_text(size=13),
            axis.title.x = element_text(size=20),
            axis.title.y = element_text(size=20)) +
      theme(strip.text.x = element_text(size = 14)) +
      guides(linetype = FALSE) +
      xlab("Time lead") +
      ylab("Proportion") +
      guides(fill = guide_legend(keywidth=3,
                                 keyheight=3,
                                 reverse = TRUE)) 
    g1
    
  })
  
  # Tab about
  output$text1 <- renderUI({
    HTML("<div class='py-5 team4'>
  <div class='container'>
    <div class='row justify-content-center mb-4'>
      <div class='col-md-7 text-center'>
      </div>
    </div>
    <div class='row'>
      <!-- column  -->
      <div class='col-lg-3 mb-4'>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
          <img src='felipe.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
            <h5 class='mt-4 font-weight-medium mb-0'>Dr Felipe J Colón-González</h5>
              <h6 class='subtitle mb-3'>Assistant Professor</h6>
              <p>London School of Hygiene & Tropical Medicine</p>
              <!-- Add icon library -->
              <ul class='list-inline'>
              <link rel='stylesheet' href='/path/to/folder/css/academicons.min.css'/>
              <link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/jpswalsh/academicons@1/css/academicons.min.css'>
                <li class='list-inline-item'><a href='mailto:felipe.colon@lshtm.ac.uk' class='text-decoration-none d-block px-1'><i class='fa fa-envelope'></i></a></li>
                <li class='list-inline-item'><a href='https://twitter.com/FJColon' class='text-decoration-none d-block px-1'><i class='fa fa-twitter'></i></a></li>
                <li class='list-inline-item'><a href='https://scholar.google.co.uk/citations?user=q94wpOEAAAAJ&hl=en' class='text-decoration-none d-block px-1'><i class='ai ai-google-scholar-square ai-1x'></i></a></li>
                <li class='list-inline-item'><a href='https://www.researchgate.net/profile/Felipe-J-Colon-Gonzalez' class='text-decoration-none d-block px-1'><i class='ai ai-researchgate-square ai-1x'></i></a></li>
                </ul>
            </div>
          </div>
        </div>
        <!-- Row -->
      </div>
      <!-- column  -->
      <div class='col-lg-3 mb-4'>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='leo.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>Dr Leonardo S Bastos</h5>
              <h6 class='subtitle mb-3'>Associate Researcher</h6>
              <p>Programa de Computação Científica da Fiocruz (Procc/Fiocruz)</p>
              <!-- Add icon library -->
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='mailto:leonardo.bastos@fiocruz.br' class='text-decoration-none d-block px-1'><i class='fa fa-envelope'></i></a></li>
                <li class='list-inline-item'><a href='https://twitter.com/leosbastos' class='text-decoration-none d-block px-1'><i class='fa fa-twitter'></i></a></li>
                <li class='list-inline-item'><a href='https://scholar.google.com/citations?user=A5VmZYMAAAAJ&hl=en' class='text-decoration-none d-block px-1'><i class='ai ai-google-scholar-square ai-1x'></i></a></li>
              </ul>
            </div>
          </div>
          </div>
        <!-- Row -->
      </div>
      <!-- column  -->
      <div class='col-lg-3 mb-4'>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='oliver.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>Dr Oliver Brady</h5>
              <h6 class='subtitle mb-3'>Associate Professor</h6>
              <p>London School of Hygiene & Tropical Medicine</p>
              <!-- Add icon library -->
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='mailto:oliver.brady@lshtm.ac.uk' class='text-decoration-none d-block px-1'><i class='fa fa-envelope'></i></a></li>
                <li class='list-inline-item'><a href='https://twitter.com/OliverBrady1' class='text-decoration-none d-block px-1'><i class='fa fa-twitter'></i></a></li>
                <li class='list-inline-item'><a href='https://scholar.google.com/citations?user=Z8yydV8AAAAJ&hl=en&oi=sra' class='text-decoration-none d-block px-1'><i class='ai ai-google-scholar-square ai-1x'></i></a></li>
              </ul>
            </div>
          </div>
          </div>
           <!-- Row -->
      </div>
      <!-- column  -->
      <div class='col-lg-3 mb-4'>
        <!-- Row -->
        <div class='row'>
          <div class='col-md-12'>
            <img src='rachel1.jpg' alt='wrapkit' class='img-fluid rounded-circle' />
          </div>
          <div class='col-md-12 text-center'>
            <div class='pt-2'>
              <h5 class='mt-4 font-weight-medium mb-0'>Dr Rachel Lowe</h5>
              <h6 class='subtitle mb-3'>Associate Professor</h6>
              <p>London School of Hygiene & Tropical Medicine</p>
              <!-- Add icon library -->
              <link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css'>
              <ul class='list-inline'>
                <li class='list-inline-item'><a href='mailto:rachel.lowe@lshtm.ac.uk' class='text-decoration-none d-block px-1'><i class='fa fa-envelope'></i></a></li>
              <li class='list-inline-item'><a href='https://twitter.com/drrachellowe' class='text-decoration-none d-block px-1'><i class='fa fa-twitter'></i></a></li>
                <li class='list-inline-item'><a href='https://scholar.google.es/citations?user=3Gis650AAAAJ&hl=en' class='text-decoration-none d-block px-1'><i class='ai ai-google-scholar-square ai-1x'></i></a></li>
                <li class='list-inline-item'><a href='https://www.researchgate.net/profile/Rachel_Lowe2' class='text-decoration-none d-block px-1'><i class='ai ai-researchgate-square ai-1x'></i></a></li>
                
              </ul>
            </div>
          </div>
        </div>
        <!-- Row -->
      </div>
    </div>
  </div>
</div>")
  })
  
}

shinyApp(ui, server)

