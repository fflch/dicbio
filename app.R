library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
#library(data.table)
#library(collapsibleTree)
library(markdown)
library(stringr)

# Lê a base de dados do dicionário e corrige o erro da codificação UTF-8 
data <- read.csv("./data/DadosDoDicionario.csv", encoding = "UTF-8")
#colnames(data) <- c("ID","Headword", "FirstAttestationDate", "FirstAttestationExampleMD", "VariantSpellings", "Etymology", "WClass", "Credits")

# Lê o arquivo com as definições
definitions <- read.csv("./data/definitions.csv", encoding = "UTF-8")

# Lê o córpus etiquetado
TokTextDF <- read.csv2("./data/DataframePrincipal.csv", encoding = "UTF-8")

# Lê os metadados do córpus
CorpusMetadata <- read.csv2("./data/CorpusTextsMetadata.csv", encoding = "UTF-8")
colnames(CorpusMetadata) <- c("Filename", "Author", "Title", "DateOfPublication")

# Cria uma função que retorna os contextos de uma palavra consultada, com a palavra negritada,
# e formata para mostrar; os parâmetros são a palavra-entrada e o número da acepção
Contextos <- function(InputConsulta, SenseNumber){
  ContextosTexto <- as.list(TokTextDF$sentence[TokTextDF$lemma == InputConsulta & TokTextDF$sensenumber==SenseNumber])
  ContextosTextoFormatados <- NULL
  for(a in 1:length(ContextosTexto)){
    
    ContextosTextoFormatados[a] <- paste0(a, " - ", ContextosTexto[a], "<br>")
    
  }
  ContextosTextoFormatados <- as.list(ContextosTextoFormatados)
  return(ContextosTextoFormatados)
}

# Criar uma função que retorna as formas variantes da entrada

ui <- fluidPage(
  navbarPage(id="Dict", title=HTML("Dicionário Histórico de Termos da Biologia"), # Dá para fazer
                                                                                  # o título linkar 
                                                            # para a página inicial assim:
                                                        # title=tags$a(href="https://brunomaroneze.shinyapps.io/Dicio_Botanica/"
             
             windowTitle="Dicionário Histórico de Termos da Biologia", 
             collapsible = TRUE, inverse = FALSE, theme = shinytheme("readable"),
             tabPanel(id="Home","Home", fluid = TRUE,       
                      fluidRow(
                        column(width=4,offset=1, htmlOutput("ProjectIntro")),
                        column(width=6,offset=1, img(src = "VandelliTabXV.png", height = "500px"),
                               HTML("Tabela XV - Vandelli - <i>Diccionario de Termos Technicos de Historia Natural</i> (1788)")
                               ,tags$hr(),
                               HTML("<a href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/\"><img src=\"creativecommons.png\" height=\"31px\"></a>")
                               ,
                               HTML("<p style='font-size:10px'>O <b>Dicionário Histórico de Termos da Biologia</b>
                               está licenciado sob a <a href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/\">Licença Creative Commons Attribution-NonCommercial-ShareAlike 4.0
                               International</a></p>"),
                               tags$br(),
                               HTML("<p style='font-size:10px'>The <b>Historical Dictionary of Biology Terms</b>
                               is licenced under a <a href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/\">Creative Commons Attribution-NonCommercial-ShareAlike 4.0
                               International Licence</a></p>")
                               )  
                      )
             ),
             
             tabPanel(id="Portuguese Search","Consulta", fluid = TRUE,  
                      
                      sidebarLayout(sidebarPanel(width = 3,
                                                 selectInput(inputId = "headword",
                                                             list(label = "Selecione uma entrada "),
                                                             choices = c(sort(as.character(data$Headword))), 
                                                             selected = "antera",
                                                             multiple = FALSE,
                                                             size = 10,
                                                             selectize = FALSE)
                                                 ),
                                    mainPanel(width = 9,
                                                        htmlOutput("Entry"),
                                                        tags$hr(),
                                                        htmlOutput("Definition"),
                                                        tags$hr(),
                                                        htmlOutput("Etymo"),
                                                        tags$hr(),
                                                        htmlOutput("FirstAttestation"),
                                                        tags$hr(),
                                                        htmlOutput("HowToCite"),
                                                        tags$br()
                                                        
                                              )    
                                                  )
                                                  
                                      )
                                    )
                      
                                    )
                      

server <- function(input, output, session) {
  
# Descobri que não precisa desse updateSelectizeInput porque o input não vai ficar mudando
#  updateSelectizeInput(session, "headword", choices=c(sort(as.character(data$Headword))),
#                       selected="antera")
  
  
  EntryData <- reactive({
    
    EntryData <- as.list(data[data$Headword==input$headword,])
    EntryData
  })
  
  
  output$ProjectIntro <-renderText({
    includeMarkdown("ProjectIntro.Rmd")
  })
  
  output$Entry <- renderText({
    

    paste0("<font size='+2'><b>", input$headword, "</font></b><br>"
           , data$WClass[data$Headword==input$headword]
           )
    
    
  })
    
  output$Definition <- renderText({

    Definition <- NULL
    for(c in 1:sum(definitions$Headword == input$headword)){
      
      Definicao <- definitions$Definition[definitions$Headword == input$headword][c]
      Definition[c] <- paste0(c, ". ", Definicao,
                              "<br><details><summary>Exemplos (<u>clique para expandir</u>)</summary><span style='font-size:.8em;'>",
             paste(Contextos(input$headword, c), collapse = ""), "</span></details><br>")
    }
    
    Definition
    
  })
  
  
  output$Etymo <- renderText({
    
    paste("<b>Discussão histórico-etimológica:</b>",
          EntryData()$Etymology, "<p style='font-size: .7em;'><br>Autores(as) do verbete: ",
          EntryData()$Credits, "</p>")
  }) 
  
  
  output$FirstAttestation <- renderText({
    
    FirstAttestationDate <- EntryData()$FirstAttestationDate
    FirstAttestationExampleMD <- EntryData()$FirstAttestationExampleMD
    VariantSpellings <- EntryData()$VariantSpellings
    if (!is.na(VariantSpellings)){
    paste0("<font color='steelblue'>",input$headword ,"</font>, também grafado <font color='steelblue'>", VariantSpellings ,"</font>, é atestado em <b>",FirstAttestationDate,"</b>: ", FirstAttestationExampleMD)
    }else{
      paste0("<font color='steelblue'>",input$headword ,"</font> é atestado em <b>",FirstAttestationDate,"</b>: ", FirstAttestationExampleMD)
      
    }
  }) 

  output$HowToCite <- renderText({
    
    EntryAuthor <- EntryData()$Credits
    paste0("<p style='font-size: .7em;'><b>Como citar este verbete:</b><br>", EntryAuthor, ". ",
           str_to_title(input$headword), ". In: MARONEZE, Bruno (coord.) 
           <b>Dicionário Histórico de Termos da Biologia</b>. 2022. Disponível em: 
           https://dicionariodebiologia.shinyapps.io/Dicio_Biologia. 
           Acesso em: ",format(Sys.Date(), "%d %b. %Y"), ".</p>")

  }) 
#  output$NumDeContextos <- renderText({

    #  Criar trecho para incluir variantes gráficas
    
    # if(existe variante gráfica){
    
    #     paste0("O termo <b>", input$headword, "</b> também é grafado ", "Y...", "(ver contextos abaixo)")
    
    # } else {
    
    # entra o outro "if" dos contextos que já existe aí embaixo
    
    # }
    
    

#  })
  

  
}

shinyApp(ui = ui, server = server)
