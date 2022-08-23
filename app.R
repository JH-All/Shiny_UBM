### 23/08/22
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(leaflet)
tabela <- read_excel("data.xlsx")
tabela <- tibble(tabela)
UBM <- read_excel("coord_data.xlsx")
UBM$site <- as.factor(UBM$site)
UBM$lat <- as.numeric(UBM$lat)
UBM$long <- as.numeric(UBM$long)

# Seleção inicial
current_user_selection <- c("Pellona harroweri", "Ctenosciaena gracilicirrhus")

# Definindo infocard
my_infocard <- function(selected_species) {
  

  
  selected_image <- tabela %>% 
    filter(species == selected_species) %>% 
    pull(image)  
  
  selected_name <- tabela %>% 
    filter(species == selected_species) %>% 
    pull(name)  
  
  selected_about <- tabela %>% 
    filter(species == selected_species) %>% 
    pull(about) 
  
  
  # Parte da UI
  column(
    width = 4,
    div(
      class = "thumbnail text-center",
      
      # Tamanho imagens
      img(
        class = "img-rounded",
        style = "height: 200px;",
        src = selected_image
      ),
      
      div(
        class = "caption",
        h4(selected_species),
        p(selected_name)
      ),
      
      # Link para Fish Base
      actionButton(
        class = "btn-default",
        inputId = str_glue("ab_more_{selected_species %>%
                          tolower() %>% 
                          str_replace_all(' ', '_')}"),
        label = "More",
        onclick = str_glue("window.open('{selected_about}', '_blank')")
      ),
      # Botão de remover os infocards
      actionButton(
        class = "btn-default rm_btn",
        inputId = str_glue("ab_remove_{selected_species %>%
                           tolower() %>% 
                           str_replace_all(' ', '_')}"),
        label = "Remove"
      )
    )
  )
}


# PART 2 - UI 
ui <- navbarPage(
  title = "Ictiofauna de Ubatumirim",
  collapsible = TRUE,
  theme = shinytheme("darkly"),
  windowTitle = "B-Diversity",    
  tabPanel(
    title = "Infocard Peixes",
    tags$head(
      tags$script(
        HTML("$(document).on('click', '.rm_btn', function () {
                                Shiny.onInputChange('rm_btn_id', this.id);
                             });"
        )
      )
    ),
    
    # header 
    div(
      class = "container", 
      h1(class = "page-header", 
         "Species and local contribution to beta diversity: Baseline conditions for Ubatumirim Bay fish guilds", 
         tags$small("- Nome dos autores")),
      
      p(class = "lead", 
        "Resumo do Trabalho"),
      
      p("Notas: *referente as fotos"),
      
      hr()
    ),
    # sidebar panel
    div(
      class = "container",
      column(
        width = 4,
        wellPanel(
          div(
            shinyWidgets::pickerInput(
              inputId  = "pi_species_selector", 
              label    = "Select a species",
              choices  = tabela %>% pull(species),
              multiple = FALSE,
              selected = "Paralonchurus brasiliensis", 
              options = pickerOptions(
                actionsBox = FALSE,
                liveSearch = TRUE,
                size       = 10
              )
            )
          ),
          
          div(
            actionButton(
              class = "btn-primary",
              inputId = "ab_show_species", 
              label = "Add"
            )
          )
        )
      )
    ),
    column(
      width = 9,
      div(
        # render infocards dynamically (ui)
        uiOutput(outputId = "infocards")
      )
    )
  ),  
  tabPanel(title="Mapa da Regiao",
           leafletOutput("map1"))
)

# PART 3 - SERVER 
server <- function(input, output, session) {
  
  # Recebe a seleção do usuario ao clicarem o botao Add
  current_species <- eventReactive(
    eventExpr = input$ab_show_species, {
      input$pi_species_selector
    })
  
  
  # Salva essa seleção
  reactive_values <- reactiveValues()
  reactive_values$species_list <- current_user_selection
  # Adiciona a espécie quando relevante
  observeEvent(input$ab_show_species, {
    reactive_values$species_list <- 
      c(reactive_values$species_list, 
        current_species()) %>% 
      unique()
  })
  
  output$infocards <- renderUI({
    
    # render infocards 
    if(length(reactive_values$species_list) > 0) {
      reactive_values$species_list %>% 
        map(my_infocard) %>% 
        tagList()
    }
  })
  
  # Retira os infocards
  observeEvent(input$rm_btn_id, {
    reactive_values$species_list <- 
      reactive_values$species_list %>% 
      .[reactive_values$species_list %>% 
          toupper() != input$rm_btn_id %>% 
          str_remove("ab_remove_") %>% 
          toupper() %>% 
          str_replace("_", " ")]
  })
  
  # Mapa 
  output$map1 <- renderLeaflet({
    UBM %>% 
      leaflet()%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      addLayersControl(baseGroups = c("World Imagery")) %>% 
      addLabelOnlyMarkers(label = UBM$site, labelOptions = labelOptions(noHide=T,
                                                                        direction = "bottom",
                                                                        style=list( "color"="red","font-family"="serif", "font-style"="italic",
                                                                                    "border-color"="rgba(0,0,0,0,0.5","box-shadow" = "3px 3px rgba(0,0,0,0.25)"
                                                                        ))) %>% 
      setView(lat = -23.38199, lng = -44.92297, zoom = 12) %>%
      addMiniMap(
        toggleDisplay = TRUE,
        tiles = providers$Stamen.TonerLite, width=100, height=100,
        position="bottomleft", minimized=FALSE, zoomLevelOffset = -5 ) %>% 
      addCircleMarkers(color=ifelse(UBM$riqueza > 20, "red", "yellow"))
  })
  
}

shinyApp(ui = ui, server = server)