# Esta é a tentativa de escrever um app Shiny que vai editar um dataframe e salvar em CSV

library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
#library(data.table)
#library(collapsibleTree)
library(markdown)


# Lê o córpus etiquetado do Vandelli
text_anndf <- read.csv2("./dadosVandelliUDTag.csv", encoding = "UTF-8")

# Cria uma função que retorna o número de palavras (tokens) em cada sentença
t <- as.data.frame(table(c(text_anndf$sentence_id))) # conta as frequências de cada sentença

ContPalavras <- function(x){
  
  y <- t$Freq[t$Var1==x]
  
  return(y)
  
}


ui <- fluidPage(
  
  navbarPage(id="Dict", title=HTML("Modernização ortográfica"), 
             
             windowTitle="Modernização ortográfica", 
             collapsible = TRUE, inverse = FALSE, theme = shinytheme("readable"),
             sidebarLayout(sidebarPanel(width = 3,
                                        selectInput(inputId = "sentence",
                                                    list(label = "Selecione uma sentença "),
                                                    choices = c(unique(text_anndf$sentence_id)), 
                                                    multiple = FALSE,
                                                    size = 10,
                                                    selectize = FALSE)
             ),
             mainPanel(width = 9,
                       
             
              tabPanel(id="Modernization","Edição", fluid = TRUE,  

# A ideia é mostrar uma sentença ou um grupo de sentenças, com espaços embaixo
# para que sejam digitadas as formas modernizadas
# Se a palavra for em latim, deve ter um checkbox "Termo em latim"
# A palavra digitada vai ser inserida no campo $orth do dataframe
# Se o checkbox "termo em latim" for escolhido, é inserido o valor "LATIM" no campo $orth
                      
                      mainPanel(htmlOutput("FraseOriginal"),
                                tags$hr(),
                                htmlOutput("FraseModerna"),
                                tags$hr(),
                                uiOutput("FormCorrecao")
                    )    
                      ))
                      
             ))
  
)


server <- function(input, output, session) {
  
  NumDePalavras <- reactive(ContPalavras(input$sentence))

  output$FraseOriginal <- renderText({
    paste0("Esta é a frase original do texto<br>",
           unique(text_anndf$sentence[text_anndf$sentence_id == input$sentence]))
  })
  
  output$FraseModerna <- renderText({
    
    "Digite a frase em ortografia modernizada"
   
  })
  
  output$FormCorrecao <- renderUI({
    
    palavras <- vector(mode = "list", length = NumDePalavras())
    
    for(n in 1:NumDePalavras()){
      
      palavras[n] <- as.character(textInput("palavra1", "palavra 1", value = "palavra 1"))
      
    }
    
    HTML(paste(palavras))
  })
 
  
  # Este algoritmo acima funciona para apresentar um número móvel de inputs de textos
  # Agora preciso mudar a função para ela retornar uma lista de palavras em vez de um número
  # text_anndf$token[text_anndf$sentence_id == 1]
  # Esta linha acima retorna a lista de palavras, mas precisa ver se ela funciona sozinha
  # ou se precisa estar dentro de um "reactive()"
  # Em seguida, preciso ver como faz para ignorar pontuação e mesmo assim manter o número
  # Daí preciso ver como faz para associar cada input de texto a uma célula do dataframe
  
}

shinyApp(ui = ui, server = server)

