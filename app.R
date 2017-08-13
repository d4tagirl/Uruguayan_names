library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(stringi)
library(stringr)
library(purrr)
library(broom)
library(viridis)
library(splines)

raw <- read_csv("nombre_nacim_x_anio_sexo.csv", col_names = FALSE) %>%
  rename(year = X1,
         sex  = X2,
         name = X3,
         freq = X4) %>% 
  mutate(name = stri_trans_general(trimws(name), "latin-ascii"),
         name = stri_replace_all_regex(name, "[0-9-,?<+.'´]+", "")) %>% 
  filter(name != "",
         year != 2012) %>%
  group_by(name, year) %>%
  summarise(freq = sum(freq)) %>% 
  ungroup()

names_per_year <- raw %>%
  group_by(year) %>%
  summarize(year_total = sum(freq))

names_year_counts <- raw  %>%
  complete(name, year, fill = list(freq = NA)) %>% 
  group_by(name) %>% 
  mutate(name_total = sum(freq)) %>% 
  ungroup() %>% 
  left_join(names_per_year, by = "year") %>% 
  mutate(percent_year = freq / year_total,
         percent_name = freq / name_total) %>% 
  arrange(desc(name_total))

spline_predictions <- names_year_counts %>%
  filter(name_total > 300) %>% 
  nest(-name) %>%
  mutate(model = map(data, ~ glm(percent_name ~ ns(year, 4), ., family = "binomial"))) %>%
  unnest(map2(model, data, augment, type.predict = "response")) %>% 
  group_by(name) %>%
  mutate(average = mean(.fitted)) %>%
  top_n(1, .fitted) %>%
  ungroup() %>%
  mutate(ratio = .fitted / average)


build_df <- function(i) {
  names_year_counts %>%
    filter(name %in% unlist(i)) %>%
    complete(year, fill = list(freq = 0)) %>% 
    group_by(year) %>% 
    summarise(freq = sum(freq),
              name = paste0(name, collapse = "+")) 
}

mobile_regex <- "((iPhone)|(iPod)|(iPad)|(Android)|(BlackBerry))"

                  ######
                  # UI #
                  ######


ui <- navbarPage("Nombres en Montevideo",
                 tabPanel("Qué tan usado es tu nombre?",
                          sidebarLayout(
                            sidebarPanel(
                              # # App input
                              textInput("name", "Escribí (y borrá) los nombres que querés graficar:", 
                                        "Daniel Gerardo+Daniela"),
                              htmlOutput("text"),
                              hr(),
                              htmlOutput("text3")
                              ),
                            
                            # Show a plot
                            mainPanel(
                              htmlOutput("text2")
                              ,
                              
                              conditionalPanel(condition = "output.mobile == false",
                                               plotlyOutput("Plotly"))
                              ,
                              conditionalPanel(condition = "output.mobile != false",
                                               plotOutput("Plot"))
                              )
                              )
                            ),
                 
                 tabPanel("Los más usados",
                          sidebarLayout(
                            sidebarPanel(
                              # App input
                              sliderInput("rango0",
                                          "Rango de años:",
                                          min = 1940,
                                          max = 2011,
                                          value = c(1960, 1990),
                                          sep = ""
                              )),
                            
                            # Show a plot
                            mainPanel(
                              plotOutput("popular"))
                          )
                 )
                 ,
                 
                 tabPanel("Los que más crecieron \n y decrecieron",
                          sidebarLayout(
                            sidebarPanel(
                              # App input
                              sliderInput("rango1",
                                          "Rango de años:",
                                          min = 1940,
                                          max = 2011,
                                          value = c(1960, 1990),
                                          sep = ""
                            )),
                            
                            # Show a plot
                            mainPanel(
                              plotOutput("slopes"))
                          )
                 )
                 ,
                 
                 tabPanel("Los que crecieron excepcionalmente",
                          sidebarLayout(
                            sidebarPanel(
                              # App input
                              sliderInput("rango2",
                                          "Rango de años:",
                                          min = 1940,
                                          max = 2011,
                                          value = c(1960, 1990),
                                          sep = ""
                              )),
                            
                            # Show a plot
                            mainPanel(
                              plotOutput("pikes"))
                          )
                 )
                 
)

                ##########
                # SERVER #
                ##########

server <- function(input, output, session) {

  output$ag.mobile <- renderText(
    str_detect(session$request$HTTP_USER_AGENT, mobile_regex)
  )
  outputOptions(output, "ag.mobile", suspendWhenHidden = FALSE)
  
  output$mobile <- reactive(
    str_detect(session$request$HTTP_USER_AGENT, mobile_regex)
  )
  outputOptions(output, "mobile", suspendWhenHidden = FALSE)
  
  mobile <- reactive({
    str_detect(session$request$HTTP_USER_AGENT, mobile_regex)
  })

  selected_name <- reactive({
    
    valid_names <- stri_trans_general(trimws(toupper(input$name)), "latin-ascii") %>% 
      strsplit("[, :;\\-\\+]+") %>% 
      unlist
    valid_names <- valid_names[valid_names %in% unique(names_year_counts$name)]
    
    names_in_input <- 
      stri_trans_general(trimws(toupper(input$name)), "latin-ascii") %>% 
      strsplit("[, :;\\-]+") %>% 
      unlist %>% 
      map(., function(x) {
        each <- strsplit(x,"[+]+" ) %>% unlist
        each[each %in% valid_names]
        }) %>% 
      Filter(length, .)

    validate(need(
      valid_names %in% unique(names_year_counts$name),
      message = ""))
    
    names_in_input %>% 
      map(build_df) %>% 
      bind_rows()
      
    })
 

  name_not_found <- reactive({
    invalid_names <- stri_trans_general(trimws(toupper(input$name)), "latin-ascii") %>% 
      strsplit("[, :;\\-\\+]+") %>% 
      unlist
    invalid_names[!invalid_names %in% unique(names_year_counts$name)]
  })
  
  
  
  popular <- reactive({
    names_year_counts %>% 
      filter(year >= input$rango0[1],
             year <= input$rango0[2]) %>% 
      group_by(name) %>% 
      mutate(freq_range = sum(freq)) %>% 
      ungroup() %>% 
      distinct(name, freq_range) %>% 
      top_n(20)
  })
    
  output$popular <- renderPlot({
    popular() %>% 
    ggplot(aes(reorder(name, freq_range), freq_range, fill = reorder(name, freq_range))) +
    geom_col() +
    ggtitle("Nombres más usados en \n el rango de años") +
    xlab(NULL) +
    ylab(NULL) +
    coord_flip() +
    theme_minimal(base_size = 15) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_viridis(discrete = TRUE)
  }, height = 600)
  
  
  output$text <- renderText({
    text1 <- HTML("No hay registro de nadie llamado ")
    text2 <- HTML(paste0('<br/>', "ni "))
    display <- ""
    if (length(name_not_found()) == 1)
      {
      display <- HTML(paste0(text1, "<b>", name_not_found(), "</b>"))
    } else {
        if (length(name_not_found()) > 1) {
          display <- HTML(paste0(text1, "<b>", name_not_found()[1], "</b>"))
          for (i in 2:length(name_not_found())) {
            display <- HTML(paste0(display, text2, "<b>", name_not_found()[i], "</b>"))
          }
        }
    }
    display
    
    })


  output$text2 <- renderText({
    
    names_in_input <- 
      stri_trans_general(trimws(toupper(input$name)), "latin-ascii") %>% 
      strsplit("[, :;\\-]+") %>% 
      unlist %>% 
      map(., function(x) {
        strsplit(x,"[+]+" ) %>% unlist
      })
    
    names_in_input_unlist <- unlist(names_in_input)
    
    if (length(names_in_input_unlist) > 0) { 
      validate(need(
        names_in_input_unlist %in% unique(names_year_counts$name), 
        message = "No hay nadie con ese nombre, probá otro!"
      ))
      } else {
        validate(need(
          names_in_input_unlist %in% unique(names_year_counts$name), 
          message = "Ingresá un nombre!"))
        }
    
    HTML("<center><span style='font-size: 18px;'>Cantidad de nombres registrados <br /> por año</span></center>")
  })
  
  output$Plotly <- renderPlotly({         # reactive input
    
    ggplotly(selected_name() %>% 
               ggplot(aes(year, freq, colour = name,
                          text = paste('año: ', year,
                                       '<br /> cantidad : ', freq))) +
               geom_line(group = 1) +
               geom_point(size = 0.8) +
               scale_y_continuous(expand = c(0, 0)) +
               expand_limits(y = c(0, 1.1 * max(selected_name()$freq))) +
               theme_minimal() +
               xlab(" \n Pasá el mouse por arriba del gráfico para ver más información!") +
               theme(axis.title.x = element_text(size = 10, colour = "darkgrey"),
                     axis.title.y = element_blank(),
                     axis.line = element_line(colour = "grey"),
                     legend.title = element_blank(),
                     legend.position = 'bottom',
                     panel.grid.major = element_blank(), panel.border = element_blank(),
                     plot.title = element_text(vjust = 3)),
             tooltip = 'text')

  })
  outputOptions(output, "Plotly", suspendWhenHidden = FALSE)
  
  output$Plot <- renderPlot({
    selected_name() %>%
     ggplot(aes(year, freq, colour = name,
                text = paste('año: ', year,
                             '<br /> cantidad : ', freq))) +
     geom_line(aes(group = unlist(name))) +
     geom_point(size = 0.8) +
     scale_y_continuous(expand = c(0, 0)) +
     expand_limits(y = c(0, 1.1 * max(selected_name()$freq))) +
     theme_minimal(base_size = 15) +
     theme(axis.title.x = element_text(size = 10, colour = "darkgrey"),
           axis.title.y = element_blank(),
           axis.line = element_line(colour = "grey"),
           legend.title = element_blank(),
           legend.position = 'bottom',
           panel.grid.major = element_blank(), 
           panel.border = element_blank(),
           plot.title = element_text(vjust = 3))
  })
  outputOptions(output, "Plot", suspendWhenHidden = FALSE)
  
  
  slopes <- reactive({    
    
    validate(need(
      input$rango1[1] != input$rango1[2],
      message = "Tenés que elegir años distintos!"))    
    
    
    names_year_counts %>%
    filter(name_total > 800,
           year >= input$rango1[1],
           year <= input$rango1[2]) %>% 
    group_by(name) %>%
    nest(-name) %>%
    mutate(models = map(data, ~ lm(percent_name ~ year, .))) %>%
    unnest(map(models, tidy)) %>%
    filter(term == "year") %>%
    arrange(desc(estimate))
})
  
  output$slopes <- renderPlot({
    head(slopes(), 10) %>% bind_rows(tail(slopes(), 10)) %>%
      ggplot(aes(reorder(name, estimate), estimate, fill = reorder(name, estimate))) +
      geom_col(aes(fill = estimate > 0), show.legend = FALSE) +
      ggtitle("Nombres que más crecieron y decrecieron \n en el rango de años") +
      xlab(NULL) +
      ylab(NULL) +
      coord_flip() +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("red", "green")) +
      labs(caption = "los que más crecieron ---->")
      
}
, height = 600
)
  
  pikes <- reactive({
    spline_predictions %>%
      filter(year >= input$rango2[1],
             year <= input$rango2[2]) %>% 
      top_n(12, ratio) %>% 
      select(name) %>% 
      inner_join(names_year_counts) %>% 
      select(name, year, freq)

  })
  
  output$pikes <- renderPlot({
    
    validate(need(
      length(pikes()$name) != 0,
      message = "En este rango de años no hubo crecimiento excepcional de ningún nombre ¯\\_(ツ)_/¯"))    

    pikes() %>% 
      ggplot(aes(year, freq, color = name)) +
      geom_line() +
      facet_wrap( ~ reorder(name, desc(freq))) +
      theme_minimal(base_size = 15) +
      ggtitle("Nombres con crecimiento excepcional  \n en el rango de años") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5)
            ) 
    

  }
  , height = 600
  )
  
  output$text3 <- renderText({
    
    HTML("<span style='color:grey;'>Hecho con <3 por <a href='https://twitter.com/d4tagirl'>@d4tagirl</a>.<br />Datos: <a href='https://catalogodatos.gub.uy'>https://catalogodatos.gub.uy</a>")
  })
  
  
} 




            #######
            # Run #
            #######

shinyApp(ui = ui, server = server)
