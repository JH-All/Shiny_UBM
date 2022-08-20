### REF 02/08/22
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(readxl)
tabela <- read_excel("ictio_ubm.xlsx")
tabela <- tibble(tabela)

# section 1.2 - set initial selection 
current_user_selection <- c("Pellona harroweri", "Ctenosciaena gracilicirrhus")

# section 1.3 - define custom infocard ----
my_infocard <- function(selected_species) {
  
  # selected_character == value from user pickerInput
  # e.g. "Mike Wheeler"
  
  selected_image <- tabela %>% 
    filter(species == selected_species) %>% 
    pull(image)  # get image url from dataset
  
  selected_name <- tabela %>% 
    filter(species == selected_species) %>% 
    pull(name)  # get actor's name from dataset
  
  selected_about <- tabela %>% 
    filter(species == selected_species) %>% 
    pull(about)   # get wikipedia's link from dataset
  
  
  # piece of UI to render dynamically
  column(
    width = 4,
    div(
      class = "thumbnail text-center",
      
      # picture
      img(
        class = "img-rounded",
        style = "height: 200px;",
        src = selected_image
      ),
      
      # main information
      div(
        class = "caption",
        h4(selected_species),
        p(selected_name)
      ),
      
      # link to wikipedia's page
      actionButton(
        class = "btn-default",
        inputId = str_glue("ab_more_{selected_species %>%
                          tolower() %>% 
                          str_replace_all(' ', '_')}"),
        label = "More",
        onclick = str_glue("window.open('{selected_about}', '_blank')")
      ),
      # remove button
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


# PART 2 - UI PART
# app backbone
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
    
    # section 2.1 - header 
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
    # section 2.2 - sidebar panel
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
  tabPanel(title="Mapa da Regiao")
)

# PART 3 - SERVER PART
server <- function(input, output, session) {
  
  # get user's selected character when 'Add' button is clicked
  current_species <- eventReactive(
    eventExpr = input$ab_show_species, {
      input$pi_species_selector
    })
  
  current_family <- eventReactive(
    eventExpr = input$ab_show_species, {
      input$pi_family_selector
    })
  
  # store user's selection
  reactive_values <- reactiveValues()
  reactive_values$species_list <- current_user_selection
  # add character when relevant
  observeEvent(input$ab_show_species, {
    reactive_values$species_list <- 
      c(reactive_values$species_list, 
        current_species()) %>% 
      unique()
  })
  
  output$infocards <- renderUI({
    
    # render infocards dynamically (server)
    if(length(reactive_values$species_list) > 0) {
      reactive_values$species_list %>% 
        map(my_infocard) %>% 
        tagList()
    }
  })
  
  # remove infocard
  observeEvent(input$rm_btn_id, {
    reactive_values$species_list <- 
      reactive_values$species_list %>% 
      .[reactive_values$species_list %>% 
          toupper() != input$rm_btn_id %>% 
          str_remove("ab_remove_") %>% 
          toupper() %>% 
          str_replace("_", " ")]
  })
  
}
# PART 4 - RUN APPLICATION
shinyApp(ui = ui, server = server)