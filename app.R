
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(ggiraph)
library(DT)
library(shinyWidgets)
library(directlabels)
library(forecast)



trimestres <- c('Y_2019_Trimestre_4','Y_2020_Trimestre_1')

load('Datos_Global.Rda')



?tags

base_total <- Lista_salvar$data

otros <- Lista_salvar$otros

gg1 <- Lista_salvar$gg1

data_prop_depto <- Lista_salvar$Proporciones_2019
data_prop_depto_2020 <- Lista_salvar$Proporciones_2020


total_2019 <- formatC(19602, format = 'f', big.mark = ',', digits = 0)
total_2019_info <- formatC(16167, format = 'f', big.mark = ',', digits = 0)
total_deptos_riesgo <- formatC(9171, format = 'f', big.mark = ',', digits = 0)
total_deptos_riesgo_l_trim <- formatC(2262, format = 'f', big.mark = ',', digits = 0)
total_deptos_riesgo_l_trim_pred <- formatC(2518, format = 'f', big.mark = ',', digits = 0)
total_deptos_riesgo_2020 <- formatC(2260, format = 'f', big.mark = ',', digits = 0)



ui <- shinyUI(
  dashboardPage(dashboardHeader(title = span("Proyecto BA (Enciso-Merchan) - Universidad de los Andes",
                                             style = "color: white; font-size: 14px"),
                                titleWidth = 400),
                dashboardSidebar(
                  tags$head(tags$style(HTML('
                               /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #2D5FCC;
                                }
                              '))),
                
                  
                  selectInput(inputId = 'trim',
                              label= "Seleccione trimestre para predicción",
                              choices = trimestres,
                              selected = NA,
                              multiple = F),
                  selectInput(inputId = 'depto',
                              label= "Seleccione departamento de riesgo alto",
                              choices = sort(otros),
                              selected = NA,
                              multiple = F),
                  actionButton(inputId = "Clic", label = 'Calcule pronóstico')
                  
                ),
                
                
                
                dashboardBody(
                  tabsetPanel(type = 'tabs',
                              tabPanel('Resumen País',
                                       fluidRow(
                                         valueBoxOutput("resu1"),
                                         valueBoxOutput("resu2"),
                                         valueBoxOutput("resu3")
                                       ),
                                       fluidRow(
                                         valueBoxOutput("resu4"),
                                         valueBoxOutput("resu5"),
                                         valueBoxOutput("resu6")
                                       ),
                                       fluidRow(
                                           girafeOutput(outputId = 'Pais', width = '300%')
                                       ) 
                                       ),
                              tabPanel( 'Análisis Departamentos',
                                        fluidRow(
                                          box(
                                            width = 12,
                                            fluidRow(
                                              column(
                                                width = 7,
                                                girafeOutput(outputId = 'depto_plot', width = "100%")
                                              ),
                                              column(
                                                width = 4,
                                                style =  'padding-right:10px',
                                                offset = 0, style='padding:0px;',
                                                fluidRow(
                                                  column(
                                                    width = 12,
                                                    offset = 0, style='padding-top:100px;padding-bottom:70px; padding-left:0px',
                                                    DT::dataTableOutput(outputId = 'depto_data', width = "100%")
                                                  )
                                                ),
                                                fluidRow(
                                                  column(
                                                    width = 12,
                                                    offset = 0, style='padding:10px;',
                                                    valueBoxOutput("resu7", width = "70%")
                                                  )
                                                ),
                                                fluidRow(
                                                  column(
                                                    width = 12,
                                                    offset = 0, style='padding:10px;',
                                                    valueBoxOutput("resu8", width = "70%")
                                                  )
                                                )
                                                
                                              )
                                              
                                            )
                                          )
                                        )
                              )
                              
                  ),
                  tags$footer('**Se consideran departamentos de mayor riesgo aquellos que son prioridad segun el ICBF y Ministerio de Salud:
                                Norte de Santander, Risaralda, Vichada, Cesar, La Guajira, Choco, Guainia.',
                              tags$br(),
                              '**Se añaden los departamentos: Bogotá DC, Guaviare, Vaupes, Amazonas, Vichada, Valle del Cauca y Antioquia, 
                              los cuales, presentaron tasas de desnutrición mayor al promedio nacional en los últimos 4 años.',
                              tags$br(),
                              'Luis Miguel Enciso - Leonardo Merchan ',
                              tags$br(),
                              'MINE - MISO (Universidad de los Andes)')
                )
  )
  
)








server <- shinyServer(function(input, output, session)
{
  reactivo <- eventReactive(input$Clic,
                            {
                              kk <- input$trim
                              afir <- kk %in% names(base_total)
                              
                              otros_2 <- input$depto
                              
                              if(afir){
                                Lista_todos <- list()
                                for(deptos in otros_2)
                                {
                                  
                                  data_mun_1 <- base_total %>% 
                                    filter(NOMBRE_DPT == deptos) 
                                  
                                  data_mun_2 <- data_mun_1 %>% 
                                    select(MPIO,
                                           Y_2016_Trimestre_1:Y_2019_Trimestre_4, Grupo2) %>% 
                                    distinct() %>% 
                                    filter(Grupo2 != 'Sin datos suficientes') %>% 
                                    select(-Grupo2)
                                  
                                  
                                  data_modelo_mun <- apply(data_mun_2[,-c(1,2,3)], 2, sum)
                                  
                                  vector_mun <- as.numeric(data_modelo_mun)
                                  
                                  vector_mun[is.na(vector_mun)] <- 0
                                  ll <- length(vector_mun)
                                  
                                  vector_mun_2 <- vector_mun[-ll]
                                  
                                  
                                  
                                  ts_1 <- as.ts(vector_mun_2)
                                  
                                  modelo <- auto.arima(ts_1,
                                                       max.p = 5,
                                                       max.q = 5,
                                                       max.P = 5,
                                                       max.Q = 5,
                                                       max.d = 2)
                                  
                                  predicho <- forecast(modelo, 1)
                                  predicho_2 <- predicho$mean[1]
                                  
                                  acc_depto <- 1- abs((predicho_2 - vector_mun[ll])/predicho_2)
                                  
                                  data_prop <- data_mun_2 %>% 
                                    select(Y_2019_Trimestre_1:Y_2019_Trimestre_4)
                                  
                                  sumas <- apply(data_prop, 1, sum)
                                  
                                  sumas_2 <- sumas/sum(sumas)
                                  
                                  pred<- round(predicho_2*sumas_2,0)
                                  
                                  
                                  ###jj
                                  
                                  data_mun_3 <- data_mun_2 %>% 
                                    mutate(Prediccion = pred) %>% 
                                    select(MPIO, Prediccion)
                                  
                                  
                                  data_mapa <- data_mun_1 %>% 
                                    left_join(data_mun_3)
                                  
                                  data_mapa <- data_mapa %>% 
                                    select(long, lat, group, Prediccion, NOMBRE_MPI, Nueva,
                                           Y_2017_Trimestre_1:Y_2019_Trimestre_3) %>% 
                                    mutate( Trimestre_Anterior = Y_2019_Trimestre_3) %>% 
                                    mutate(Prediccion = as.numeric(Prediccion),
                                           Promedio_2017 = ceiling(Y_2017_Trimestre_4),
                                           Promedio_2018 = ceiling(Y_2018_Trimestre_4))
                                  
                                  
                                  Lista_salida <- list()
                                  
                                  pp <- ggplot(data_mapa, aes(x = long, y = lat, group = group, fill = Prediccion)) +
                                    geom_polygon(color = "black", size = 0.1) +
                                    # scale_fill_manual(values = c('#FF7504','#F5BC2C','white'))+
                                    coord_equal() +
                                    theme_void() +
                                    ggtitle("Distribución por cantidad de casos pronosticados") +
                                    theme(plot.title = element_text(hjust = 0.5, size = 35),
                                          legend.title = element_text(color = "black", size = 15),
                                          legend.text = element_text(color = "black", size = 12),
                                          legend.key.width = unit(2,"cm"),
                                          legend.key.height = unit(2,"cm")) 
                                  
                                  top <- data_mapa %>% 
                                    select(NOMBRE_MPI, Promedio_2017, Promedio_2018,
                                           Trimestre_Anterior,Prediccion) %>% 
                                    distinct() %>% 
                                    arrange(desc(Prediccion)) 
                                  
                                  names(top) <- c('Municipio','Cuarto trimestre 2017','Cuarto trimestre 2018',
                                                  'Trimestre anterior','Prediccion')
                                  
                                  top_2 <- top[1:5,]
                                  
                                  pp2 <- top_2 %>% 
                                    filter(!is.na(Municipio))
                                  
                                  acc_depto <- data.frame(ACC_Prediccion = paste(round(100*acc_depto, 2),'%',sep=''))
                                  
                                  Lista_salida[['acc']] <- acc_depto
                                  Lista_salida[['grafico']] <- pp
                                  Lista_salida[['tabla']] <- pp2
                                  
                                  
                                  Lista_todos[[deptos]] <- Lista_salida
                                  
                                  
                                }
                                
                              }else{
                                Lista_todos <- list()
                                
                                for(deptos in otros_2)
                                {
                                  
                                  data_mun_1 <- base_total %>% 
                                    filter(NOMBRE_DPT == deptos) 
                                  
                                  
                                  
                                  data_mun_2 <- data_mun_1 %>% 
                                    select(MPIO,
                                           Y_2016_Trimestre_1:Y_2019_Trimestre_4, Grupo2) %>% 
                                    distinct() %>% 
                                    filter(Grupo2 != 'Sin datos suficientes') %>% 
                                    select(-Grupo2)
                                  
                                  
                                  data_modelo_mun <- apply(data_mun_2[,-c(1,2,3)], 2, sum)
                                  
                                  vector_mun <- as.numeric(data_modelo_mun)
                                  
                                  vector_mun[is.na(vector_mun)] <- 0
                                  
                                  vector_mun_2 <- vector_mun
                                  
                                  
                                  ts_1 <- as.ts(vector_mun_2)
                                  
                                  modelo <- auto.arima(ts_1,
                                                       max.p = 5,
                                                       max.q = 5,
                                                       max.P = 5,
                                                       max.Q = 5,
                                                       max.d = 2)
                                  
                                  predicho <- forecast(modelo, 1)
                                  predicho_2 <- predicho$mean[1]
                                  
                                  acc_depto <- 'Nuevo dato'
                                  
                                  data_prop <- data_mun_2 %>% 
                                    select(Y_2019_Trimestre_1:Y_2019_Trimestre_4)
                                  
                                  sumas <- apply(data_prop, 1, sum)
                                  
                                  sumas_2 <- sumas/sum(sumas)
                                  
                                  pred<- round(predicho_2*sumas_2,0)
                                  
                                  
                                  data_mun_3 <- data_mun_2 %>% 
                                    mutate(Prediccion = pred) %>% 
                                    select(MPIO, Prediccion)
                                  
                                  
                                  data_mapa <- data_mun_1 %>% 
                                    left_join(data_mun_3)
                                  
                                  data_mapa <- data_mapa %>% 
                                    select(long, lat, group, Prediccion, NOMBRE_MPI, Nueva,
                                           Y_2017_Trimestre_1:Y_2019_Trimestre_4) %>% 
                                    mutate( Trimestre_Anterior = Y_2019_Trimestre_4) %>% 
                                    mutate(Prediccion = as.numeric(Prediccion),
                                           Promedio_2017 = ceiling(Y_2017_Trimestre_1),
                                           Promedio_2018 = ceiling(Y_2018_Trimestre_1),
                                           Promedio_2019 = ceiling(Y_2019_Trimestre_1))
                                  
                                  
                                  Lista_salida <- list()
                                  
                                  pp <- ggplot(data_mapa, aes(x = long, y = lat, group = group, fill = Prediccion)) +
                                    geom_polygon(color = "black", size = 0.1) +
                                    # scale_fill_manual(values = c('#FF7504','#F5BC2C','white'))+
                                    coord_equal() +
                                    theme_void() +
                                    ggtitle("Distribución por cantidad de casos pronosticados") +
                                    theme(plot.title = element_text(hjust = 0.5, size = 35),
                                          legend.title = element_text(color = "black", size = 15),
                                          legend.text = element_text(color = "black", size = 12),
                                          legend.key.width = unit(2,"cm"),
                                          legend.key.height = unit(2,"cm")) 
                                  
                                  top <- data_mapa %>% 
                                    select(NOMBRE_MPI, Promedio_2017, Promedio_2018, Promedio_2019,
                                           Trimestre_Anterior,Prediccion) %>% 
                                    distinct() %>% 
                                    arrange(desc(Prediccion)) 
                                  
                                  names(top) <- c('Municipio','Primer trimestre 2017','Primer trimestre 2018','Primer trimestre 2019',
                                                  'Trimestre Anterior','Prediccion')
                                  
                                  
                                  top_2 <- top[1:5,]
                                  
                                  pp2 <- top_2 %>% 
                                    filter(!is.na(Municipio))
                                  
                                  
                                  acc_depto <- data.frame(ACC_Prediccion = acc_depto)
                                  
                                  Lista_salida[['acc']] <- acc_depto
                                  Lista_salida[['grafico']] <- pp
                                  Lista_salida[['tabla']] <- pp2
                                  
                                  
                                  Lista_todos[[deptos]] <- Lista_salida
                                  
                                  
                                }
                              }
                              
                              
                              return(list(Lista = Lista_todos,
                                          afir = afir,
                                          depto = deptos))
                            })
  
  
  
  #### 2. BOGOTA
  
  
  output$depto_data <- DT::renderDataTable({
    req(reactivo())
    Lista <- reactivo()$Lista
    
    selTable <- Lista[[1]]$tabla
    
    DT::datatable(data = selTable,
                  escape=FALSE, 
                  options = list(sDom  = '<"top">lrt<"bottom">ip', 
                                 lengthChange = FALSE,
                                 dom = 'l',
                                 searchable = FALSE))
    
    
  })
  
  output$resu7 <- renderValueBox({
    req(reactivo())
    Lista <- reactivo()$Lista
    valueBox(value = tags$p(Lista[[1]]$acc, style = "font-size: 200%;"), 
             "Accuracy del modelo a nivel departamento.", 
             icon = icon("chart-pie"), color = "black")
  })
  
  output$depto_plot <- renderGirafe({
    req(reactivo())
    Lista <- reactivo()$Lista
    grafico <- Lista[[1]]$grafico
    girafe(ggobj = grafico, width_svg = 15, height_svg = 12.5)
    
  })
  
  output$Pais <- renderGirafe({
    girafe(ggobj = gg1, width_svg = 21, height_svg = 17.5)
  })
  
  
  output$resu1 <- renderValueBox({
    valueBox(value = tags$p(total_2019, style = "font-size: 200%;"), 
             "Casos totales registrados en el año 2019.", icon = icon("bell"), color = "navy")
  })
  
  output$resu2 <- renderValueBox({
    valueBox(value = tags$p(total_2019_info, style = "font-size: 150%;"), 
             "Casos totales registrados en el año 2019, 
             de municipios que tienen información en los últimos 4 años.", icon = icon("check-circle"), 
             color = "blue")
  })
  
  output$resu3 <- renderValueBox({
    valueBox(value = tags$p(total_deptos_riesgo, style = "font-size: 150%;"), 
             "**Casos totales registrados en los departamentos de mayor riesgo para el año 2019. (57% del total)", 
             icon = icon("chart-pie"), color = "light-blue")
  })
  
  output$resu4 <- renderValueBox({
    valueBox(value = tags$p(total_deptos_riesgo_l_trim, style = "font-size: 150%;"), 
             "**Casos registrados en departamentos de mayor riesgo 
             durante el último trimestre del 2019. (25% del total del año)", icon = icon("bell"), color = "aqua")
  })
  
  output$resu5 <- renderValueBox({
    valueBox(value = tags$p(total_deptos_riesgo_l_trim_pred, style = "font-size: 150%;"), 
             "**Predicción del último trimestre del año 2019 de 
             los departamentos en mayor riesgo riesgo.", icon = icon("bell"), 
             color = "teal")
  })
  
  output$resu6 <- renderValueBox({
    valueBox(value = tags$p(total_deptos_riesgo_2020, style = "font-size: 150%;"), 
             "**Predicción del primer trimestre del año 2020 en los 
             departamentos de mayor riesgo.", 
             icon = icon("chart-pie"), color = "olive")
  })
  
  
  output$resu8 <- renderValueBox({
    req(reactivo())
    afir <- reactivo()$afir
    nn <- reactivo()$depto
    
    
    if(afir){
      prop <- data_prop_depto %>% 
        filter(DEPTO == nn) %>% 
        select(prop_19) %>% 
        t() %>% 
        as.vector()
      valueBox(value = tags$p(paste(round(prop,4)*100, '%',sep=''), style = "font-size: 100%;"), 
               paste("Se recomienda destinar este porcentaje de los recursos públicos perteneciente a los departamentos de mayor riesgo
                     a", nn, 'para el periodo 2019-4'), 
               icon = icon("chart-pie"), color = "purple")
    }else{
      prop <- data_prop_depto_2020 %>% 
        filter(DEPTO == nn) %>% 
        select(prop_20) %>% 
        t() %>% 
        as.vector()
      valueBox(value = tags$p(paste(round(prop,4)*100, '%',sep=''), style = "font-size: 100%;"), 
               paste("Se recomienda destinar este porcentaje de los recursos públicos perteneciente a los departamentos de mayor riesgo
                     a", nn, 'para el periodo 2020-1'), 
               icon = icon("chart-pie"), color = "purple")
    }
    
  })
  
  
})




shinyApp(ui = ui, server = server)










