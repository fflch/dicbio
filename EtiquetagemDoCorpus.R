#library(tidyverse)
#library(DT)
#library(data.table)
library(stringr)

library(XML)
library(xml2)
library(stylo)

# Lê os arquivos XML e junta todos num único:
corpusVandelli <- readChar("data/diciovandelli.xml", file.info("data/diciovandelli.xml")$size)
corpusVandelli <- gsub("^.+?\\n(.*)","\\1", corpusVandelli) #tira a primeira linha
corpusVandelli <- gsub("^.+?\\n(.*)","\\1", corpusVandelli) #tira a segunda linha

corpusSantucci <- readChar("data/anatomiasantucci.xml", file.info("data/anatomiasantucci.xml")$size)
corpusSantucci <- gsub("^.+?\\n(.*)","\\1", corpusSantucci) #tira a primeira linha
corpusSantucci <- gsub("^.+?\\n(.*)","\\1", corpusSantucci) #tira a segunda linha

corpusBrotero <- readChar("data/compendio1brotero.xml", file.info("data/compendio1brotero.xml")$size)
corpusBrotero <- gsub("^.+?\\n(.*)","\\1", corpusBrotero) #tira a primeira linha
corpusBrotero <- gsub("^.+?\\n(.*)","\\1", corpusBrotero) #tira a segunda linha

corpustotal <- paste("<corpus>", corpusVandelli, "\\n",
                     corpusSantucci, "\\n", corpusBrotero, "</corpus>")


# Lê a árvore XML do corpus total, extrai todos os termos e atributos
CorpusXML <- read_xml(corpustotal, encoding = "UTF-8", as_html = FALSE)
#corpusRoot <- xml_root(CorpusXML)

terms <- xml_find_all(CorpusXML, "//term")
tokenTerms <- xml_text(terms)
token_lemma <- xml_attr(terms, attr = "lemma")
token_orth <- xml_attr(terms, attr = "orth")
token_gram <- xml_attr(terms, attr = "msd")
token_senseNumber <- xml_attr(terms, attr = "senseNumber")

# Este loop é necessário para extrair todas as sentenças equivalentes
# No futuro, pretendo incluir aqui um código para negritar os termos
# em cada sentença

token_sentence <- NULL
author <- NULL
date <- NULL
currentPage <- NULL
previousPage <- NULL
pageNumber <- NULL
for(i in 1:length(terms)){
# Insere um caractere qualquer para depois ser transformado em <b></b>
  xml_text(terms[i]) <- paste0("[b]", xml_text(terms[i]), "[xb]")

# Extrai todas as sentenças e limpa as marcações
  token_sentence[i] <- as.character(xml_find_first(terms[i],
                                                   xpath = "./ancestor::s"))
  token_sentence[i] <- delete.markup(token_sentence[i], markup.type = "xml")

# Substitui as marcas inseridas pelas marcas de negrito
  token_sentence[i] <- gsub("\\[b\\]", "<b>", token_sentence[i])
  token_sentence[i] <- gsub("\\[xb\\]", "</b>", token_sentence[i])

# Retorna à forma anterior (necessário para não afetar o negrito nos outros termos)
  xml_text(terms[i]) <- gsub("\\[b\\]", "", xml_text(terms[i]))
  xml_text(terms[i]) <- gsub("\\[xb\\]", "", xml_text(terms[i]))

#Insere autor e data
  author[i] <- as.character(xml_find_first(terms[i],
                                       xpath = "string(./ancestor::text/@author)"))
  date[i] <- as.character(xml_find_first(terms[i],
                                       xpath = "string(./ancestor::text/@date)"))
  
# Insere números de páginas

# Primeiro, testa para ver se dentro da sentença do termo tem alguma quebra
  # de página
  if(as.character(xml_find_first(terms[i],
                      xpath = "string(./preceding-sibling::pb/@n)")) != ""){
 
  # Se tiver quebra de página, o número de página será a anterior mais a 
    # seguinte, com um tracinho
    
    currentPage[i] <- as.character(xml_find_first(terms[i],
                                     xpath = "string(./preceding-sibling::pb/@n)"))
    previousPage[i] <- str_extract(as.character(head(tail(xml_find_all(terms[i],
                                          xpath = "./preceding::pb/@n"), 2), n=1)),
                                   "(?<=\").+(?=\")")

    pageNumber[i] <- paste0(previousPage[i], "-", currentPage[i])

    
  }else if(as.character(xml_find_first(terms[i],
                               xpath = "string(./following-sibling::pb/@n)")) != ""){
    
    currentPage[i] <- as.character(xml_find_first(terms[i],
                                            xpath = "string(./following-sibling::pb/@n)"))
    previousPage[i] <- str_extract(as.character(head(tail(xml_find_all(terms[i],
                                      xpath = "./preceding::pb/@n"), 1), n=1)),
                                   "(?<=\").+(?=\")")
    pageNumber[i] <- paste0(previousPage[i], "-", currentPage[i])
    
  }else{ # Se não tiver quebra de página, o número da página será o anterior
    pageNumber[i] <- str_extract(as.character(tail(xml_find_all(terms[i],
                                                    xpath = "./preceding::pb/@n"), 1)),
                                 "(?<=\").+(?=\")")
  }

  token_sentence[i] <- paste0(token_sentence[i], " (", author[i],
                              ", ", date[i], ", p. ", pageNumber[i], ")")
}
rm(pageNumber, currentPage, previousPage)

# Cria um dataframe único
DataFrameTotalXML <- data.frame(
  token = tokenTerms,
  Headword = token_lemma,
  orth = token_orth,
  gram = token_gram,
  sensenumber = token_senseNumber,
  sentence = token_sentence
)

# Este loop preenche as lacunas "por defeito"
# Se o lema está vazio, por defeito será igual ao token
# Se o campo de atualização ortográfica está vazio, por defeito será
# igual ao lema
# Se o número da acepção está vazio, por defeito será 1
for(x in 1:length(DataFrameTotalXML$Headword)){
  if(is.na(DataFrameTotalXML$Headword[x])){
    DataFrameTotalXML$Headword[x] <- DataFrameTotalXML$token[x]
  }
  if(is.na(DataFrameTotalXML$orth[x])){
    DataFrameTotalXML$orth[x] <- DataFrameTotalXML$Headword[x]
  }
  if(is.na(DataFrameTotalXML$sensenumber[x])){
    DataFrameTotalXML$sensenumber[x] <- "1"
  }
}

# Preenche a coluna "variantes" com informações do token, Headword e gram

DataFrameTotalXML$variants <- ifelse(is.na(DataFrameTotalXML$gram)==FALSE,
                                     paste0(tolower(DataFrameTotalXML$token),
                                    " (", DataFrameTotalXML$gram, ")"
                                    #, " (", author, ", ", date, ")"
                                    ),
                                    paste0(tolower(DataFrameTotalXML$token)
                                    #       , " (", author, ", ", date, ")")
                                    # Tirar o comentário para incluir 
                                    # autor e data na variante
                                    )
)


# Salva o arquivo
write.csv(DataFrameTotalXML, file = "./data/DataframePrincipal.csv", fileEncoding = "UTF-8")

# Limpa a memória
rm(author, date, x, i, terms, CorpusXML, token_gram, token_lemma, token_orth,
   token_senseNumber, token_sentence, tokenTerms, corpusVandelli,
   corpusSantucci, corpusBrotero, corpustotal, DataFrameTotalXML)



#-------------------------------------------------------
# Este código abaixo tenta incluir a tag <s></s> em todas as sentenças
# do texto. Não funciona porque algumas das sentenças são aplicadas
# dentro de outras, como sentenças curtas ("L" por exemplo)

#Dataframeteste1 <- CriaDataframeDados("anatomiasantucci.xml")
#listasentencas <- unique(Dataframeteste1$sentence)

#for(c in 1:length(listasentencas)){
#  if(str_detect(listasentencas[c], regex("^\\<.+\\>$"))){
#    listasentencas[c] <- ""
#  } else if(listasentencas[c] == "<!"){
#    listasentencas[c] <- ""
#  }else if(str_detect(listasentencas[c], "DOCTYPE")){
#    listasentencas[c] <- ""
#  }else if(str_detect(listasentencas[c], regex("^\\<s>"))){
#    listasentencas[c] <- ""
#  }
#  listasentencas <- listasentencas[listasentencas != ""]
#}

#for(d in 1:length(listasentencas)){
#    listasentencas[d] <- str_replace(listasentencas[d], regex("^<.+>"), "")
  
#}

#for(e in 1:length(listasentencas)){
#  teste2 <- str_replace(teste2, listasentencas[e],
#                        paste0("<s>", listasentencas[e], "</s>"))
#}  

#arquivo <- file("arquivo.txt")
#writeLines(teste2, arquivo)
#-------------------------------------------------------