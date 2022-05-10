library(tidyverse)
library(DT)
library(data.table)
#library(koRpus.lang.pt)
library(stringr)


# TokTextDF$token[TokTextDF$lttr==0] <- "" # Elimina o caractere BOM

# Lê os metadados do córpus
CorpusMetadata <- read.csv2("../data/CorpusTextsMetadata.csv", encoding = "UTF-8") 
colnames(CorpusMetadata) <- c("Filename", "Author", "Title", "DateOfPublication")


## Scripts para anotação de POS usando o UDPipe
library(udpipe)

# m_port <- udpipe::udpipe_download_model(language = "portuguese-bosque") # faz download do modelo

m_port <- udpipe_load_model(file = "portuguese-bosque-ud-2.5-191206.udpipe") # carrega na memória

textVandelli <- readLines("../data/DiccionariodeVandelli.txt", encoding = "UTF-8") # lê o texto
textSantucci <- readLines("../data/AnatomiadeSantucci.txt", encoding = "UTF-8") # lê o texto

text_anndfVandelli <- udpipe::udpipe_annotate(m_port, x = textVandelli) %>%
  as.data.frame() # Cria um dataframe no formato CONLLU com as anotações

text_anndfSantucci <- udpipe::udpipe_annotate(m_port, x = textSantucci) %>%
  as.data.frame() # Cria um dataframe no formato CONLLU com as anotações

# Corrige a numeração da lista de sentenças
text_anndfVandelli$sentence_id[1] <- 1
for(x in 2:length(text_anndfVandelli$sentence_id)){
  if(text_anndfVandelli$sentence[x] == text_anndfVandelli$sentence[(x-1)]){
    text_anndfVandelli$sentence_id[x] <- text_anndfVandelli$sentence_id[(x-1)]
  } else {
    text_anndfVandelli$sentence_id[x] <- text_anndfVandelli$sentence_id[(x-1)] + 1
  }
}

text_anndfSantucci$sentence_id[1] <- 1
for(x in 2:length(text_anndfSantucci$sentence_id)){
  if(text_anndfSantucci$sentence[x] == text_anndfSantucci$sentence[(x-1)]){
    text_anndfSantucci$sentence_id[x] <- text_anndfSantucci$sentence_id[(x-1)]
  } else {
    text_anndfSantucci$sentence_id[x] <- text_anndfSantucci$sentence_id[(x-1)] + 1
  }
}

text_anndfVandelli$doc_id <- "DiccionariodeVandelli.txt"
text_anndfSantucci$doc_id <- "AnatomiadeSantucci.txt"

# Junta os dois e elimina as colunas desnecessárias

DataframeTotal <- rbind(text_anndfVandelli, text_anndfSantucci)
DataframeTotal$paragraph_id <- NULL
DataframeTotal$head_token_id <- NULL
DataframeTotal$dep_rel <- NULL
DataframeTotal$deps <- NULL
DataframeTotal$upos <- NULL
DataframeTotal$xpos <- NULL
DataframeTotal$feats <- NULL

rm(text_anndfSantucci, text_anndfVandelli) # Limpa a memória

# O arquivo de dados só precisa conter as palavras presentes no dicionário
# Assim, lematizamos o necessário e eliminamos o resto

DataframeTotal <- subset(DataframeTotal, lemma != "-") # Elimina as pontuações
DataframeTotal <- subset(DataframeTotal, lemma != "—")
DataframeTotal <- subset(DataframeTotal, lemma != "&")
DataframeTotal <- subset(DataframeTotal, lemma != "(")
DataframeTotal <- subset(DataframeTotal, lemma != ")")
DataframeTotal <- subset(DataframeTotal, lemma != ",")
DataframeTotal <- subset(DataframeTotal, lemma != ".")
DataframeTotal <- subset(DataframeTotal, lemma != "/")
DataframeTotal <- subset(DataframeTotal, lemma != ":")
DataframeTotal <- subset(DataframeTotal, lemma != ";")
DataframeTotal <- subset(DataframeTotal, lemma != "[")
DataframeTotal <- subset(DataframeTotal, lemma != "]")
DataframeTotal <- subset(DataframeTotal, lemma != "?")

# Lematização e correção ortográfica:
DataframeTotal$orth <- DataframeTotal$token # Copia tudo da coluna token para a coluna orth


DataframeTotal$orth[DataframeTotal$token == "DICCIONARIO"] <- "DICIONÁRIO" 
DataframeTotal$orth[DataframeTotal$token == "TECHNICOS"] <- "TÉCNICOS"

# Troca grupos de caracteres na coluna orth
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "nn", "n")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "aõ", "ão")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "oens", "ões")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "mm", "m")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "ll", "l")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "mm", "m")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "ff", "f")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "FF", "F")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "th", "t")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "ph", "f")
DataframeTotal$orth <- str_replace(DataframeTotal$orth, "cco", "co")


DataframeTotal$orth[DataframeTotal$token == "bifido"] <- "bífido"
DataframeTotal$orth[DataframeTotal$token == "Bifido"] <- "Bífido"
DataframeTotal$orth[DataframeTotal$token == "bifida"] <- "bífida"
DataframeTotal$orth[DataframeTotal$token == "Bifida"] <- "Bífida"
DataframeTotal$orth[DataframeTotal$token == "bifidos"] <- "bífidos"
DataframeTotal$orth[DataframeTotal$token == "Bifidos"] <- "Bífidos"
DataframeTotal$orth[DataframeTotal$token == "bifidas"] <- "bífidas"
DataframeTotal$orth[DataframeTotal$token == "Bifidas"] <- "Bífidas"
DataframeTotal$orth[DataframeTotal$token == "bractea"] <- "bráctea"
DataframeTotal$orth[DataframeTotal$token == "bracteas"] <- "brácteas"
DataframeTotal$orth[DataframeTotal$token == "Bractea"] <- "Bráctea"
DataframeTotal$orth[DataframeTotal$token == "Bracteas"] <- "Brácteas"
DataframeTotal$orth[DataframeTotal$token == "caliz"] <- "cálice"
DataframeTotal$orth[DataframeTotal$token == "Caliz"] <- "Cálice"
DataframeTotal$orth[DataframeTotal$token == "calizes"] <- "cálices"
DataframeTotal$orth[DataframeTotal$token == "Calizes"] <- "Cálices"
DataframeTotal$orth[DataframeTotal$token == "calices"] <- "cálices"
DataframeTotal$orth[DataframeTotal$token == "Calices"] <- "Cálices"
DataframeTotal$orth[DataframeTotal$token == "capreolo"] <- "capréolo"
DataframeTotal$orth[DataframeTotal$token == "Capreolo"] <- "Capréolo"
DataframeTotal$orth[DataframeTotal$token == "capreolos"] <- "capréolos"
DataframeTotal$orth[DataframeTotal$token == "Capreolo"] <- "Capréolos"
DataframeTotal$orth[DataframeTotal$token == "cartilagineo"] <- "cartilagíneo"
DataframeTotal$orth[DataframeTotal$token == "Cartilagineo"] <- "Cartilagíneo"
DataframeTotal$orth[DataframeTotal$token == "cartilaginea"] <- "cartilagínea"
DataframeTotal$orth[DataframeTotal$token == "Cartilaginea"] <- "Cartilagínea"
DataframeTotal$orth[DataframeTotal$token == "cartilagineas"] <- "cartilagíneas"
DataframeTotal$orth[DataframeTotal$token == "Cartilagineas"] <- "Cartilagíneas"
DataframeTotal$orth[DataframeTotal$token == "cotyledon"] <- "cotilédone"
DataframeTotal$orth[DataframeTotal$token == "Cotyledon"] <- "Cotilédone"
DataframeTotal$orth[DataframeTotal$token == "cotyledones"] <- "cotilédones"
DataframeTotal$orth[DataframeTotal$token == "Cotyledones"] <- "Cotilédones"
DataframeTotal$orth[DataframeTotal$token == "cuticula"] <- "cutícula"
DataframeTotal$orth[DataframeTotal$token == "Cuticula"] <- "Cutícula"
DataframeTotal$orth[DataframeTotal$token == "cuticulas"] <- "cutículas"
DataframeTotal$orth[DataframeTotal$token == "Cuticulas"] <- "Cutículas"
DataframeTotal$orth[DataframeTotal$token == "estipula"] <- "estípula"
DataframeTotal$orth[DataframeTotal$token == "Estipula"] <- "Estípula"
DataframeTotal$orth[DataframeTotal$token == "estipulas"] <- "estípulas"
DataframeTotal$orth[DataframeTotal$token == "Estipulas"] <- "Estípulas"
DataframeTotal$orth[DataframeTotal$token == "flosculo"] <- "flósculo"
DataframeTotal$orth[DataframeTotal$token == "Flosculo"] <- "Flósculo"
DataframeTotal$orth[DataframeTotal$token == "flosculos"] <- "flósculos"
DataframeTotal$orth[DataframeTotal$token == "Flosculos"] <- "Flósculos"
DataframeTotal$orth[DataframeTotal$token == "foliaceo"] <- "foliáceo"
DataframeTotal$orth[DataframeTotal$token == "Foliaceo"] <- "Foliáceo"
DataframeTotal$orth[DataframeTotal$token == "foliacea"] <- "foliácea"
DataframeTotal$orth[DataframeTotal$token == "Foliacea"] <- "Foliácea"
DataframeTotal$orth[DataframeTotal$token == "foliaceos"] <- "foliáceos"
DataframeTotal$orth[DataframeTotal$token == "Foliaceos"] <- "Foliáceos"
DataframeTotal$orth[DataframeTotal$token == "foliaceas"] <- "foliáceas"
DataframeTotal$orth[DataframeTotal$token == "Foliaceas"] <- "Foliáceas"
DataframeTotal$orth[DataframeTotal$orth == "fructificação"] <- "frutificação"
DataframeTotal$orth[DataframeTotal$orth == "fructificações"] <- "frutificações"
DataframeTotal$orth[DataframeTotal$orth == "Fructificação"] <- "Frutificação"
DataframeTotal$orth[DataframeTotal$orth == "Fructificações"] <- "Frutificações"
DataframeTotal$orth[DataframeTotal$token == "botanico"] <- "botânico"
DataframeTotal$orth[DataframeTotal$token == "Botanico"] <- "Botânico"
DataframeTotal$orth[DataframeTotal$token == "botanica"] <- "botânica"
DataframeTotal$orth[DataframeTotal$token == "Botanica"] <- "Botânica"
DataframeTotal$orth[DataframeTotal$token == "botanicos"] <- "botânicos"
DataframeTotal$orth[DataframeTotal$token == "Botanicos"] <- "Botânicos"
DataframeTotal$orth[DataframeTotal$token == "botanicas"] <- "botânicas"
DataframeTotal$orth[DataframeTotal$token == "Botanicas"] <- "Botânicas"
DataframeTotal$orth[DataframeTotal$token == "BOTANICO"] <- "BOTÂNICO"
DataframeTotal$orth[DataframeTotal$token == "BOTANICOS"] <- "BOTÂNICOS"
DataframeTotal$orth[DataframeTotal$token == "lacinia"] <- "lacínia"
DataframeTotal$orth[DataframeTotal$token == "Lacinia"] <- "Lacínia"
DataframeTotal$orth[DataframeTotal$token == "lacinias"] <- "lacínias"
DataframeTotal$orth[DataframeTotal$token == "Lacinias"] <- "Lacínias"
DataframeTotal$orth[DataframeTotal$token == "longitudinaes"] <- "longitudinais"
DataframeTotal$orth[DataframeTotal$token == "Longitudinaes"] <- "Longitudinais"
DataframeTotal$orth[DataframeTotal$token == "membranaceo"] <- "membranáceo"
DataframeTotal$orth[DataframeTotal$token == "Membranaceo"] <- "Membranáceo"
DataframeTotal$orth[DataframeTotal$token == "membranaceos"] <- "membranáceos"
DataframeTotal$orth[DataframeTotal$token == "Membranaceos"] <- "Membranáceos"
DataframeTotal$orth[DataframeTotal$token == "membranacea"] <- "membranácea"
DataframeTotal$orth[DataframeTotal$token == "Membranacea"] <- "Membranácea"
DataframeTotal$orth[DataframeTotal$token == "membranaceas"] <- "membranáceas"
DataframeTotal$orth[DataframeTotal$token == "Membranaceas"] <- "Membranáceas"
DataframeTotal$orth[DataframeTotal$token == "papilionaceo"] <- "papilionáceo"
DataframeTotal$orth[DataframeTotal$token == "Papilionaceo"] <- "Papilionáceo"
DataframeTotal$orth[DataframeTotal$token == "papilionacea"] <- "papilionácea"
DataframeTotal$orth[DataframeTotal$token == "Papilionacea"] <- "Papilionácea"
DataframeTotal$orth[DataframeTotal$token == "papilionaceos"] <- "papilionáceos"
DataframeTotal$orth[DataframeTotal$token == "Papilionaceos"] <- "Papilionáceos"
DataframeTotal$orth[DataframeTotal$token == "papilionaceas"] <- "papilionáceas"
DataframeTotal$orth[DataframeTotal$token == "Papilionaceas"] <- "Papilionáceas"
DataframeTotal$orth[DataframeTotal$token == "parasitico"] <- "parasítico"
DataframeTotal$orth[DataframeTotal$token == "Parasitico"] <- "Parasítico"
DataframeTotal$orth[DataframeTotal$token == "parasitica"] <- "parasítica"
DataframeTotal$orth[DataframeTotal$token == "Parasitica"] <- "Parasítica"
DataframeTotal$orth[DataframeTotal$token == "parasiticos"] <- "parasíticos"
DataframeTotal$orth[DataframeTotal$token == "Parasiticos"] <- "Parasíticos"
DataframeTotal$orth[DataframeTotal$token == "parasiticas"] <- "parasíticas"
DataframeTotal$orth[DataframeTotal$token == "Parasiticas"] <- "Parasíticas"
DataframeTotal$orth[DataframeTotal$token == "peciolo"] <- "pecíolo"
DataframeTotal$orth[DataframeTotal$token == "Peciolo"] <- "Pecíolo"
DataframeTotal$orth[DataframeTotal$token == "peciolos"] <- "pecíolos"
DataframeTotal$orth[DataframeTotal$token == "Peciolos"] <- "Pecíolos"
DataframeTotal$orth[DataframeTotal$token == "pedunculo"] <- "pedúnculo"
DataframeTotal$orth[DataframeTotal$token == "Pedunculo"] <- "Pedúnculo"
DataframeTotal$orth[DataframeTotal$token == "pedunculos"] <- "pedúnculos"
DataframeTotal$orth[DataframeTotal$token == "Pedunculos"] <- "Pedúnculos"
DataframeTotal$orth[DataframeTotal$token == "receptaculo"] <- "receptáculo"
DataframeTotal$orth[DataframeTotal$token == "Receptaculo"] <- "Receptáculo"
DataframeTotal$orth[DataframeTotal$token == "receptaculos"] <- "receptáculos"
DataframeTotal$orth[DataframeTotal$token == "Receptaculos"] <- "Receptáculos"
DataframeTotal$orth[DataframeTotal$token == "romboidaes"] <- "romboidais"
DataframeTotal$orth[DataframeTotal$token == "Romboidaes"] <- "Romboidais"
DataframeTotal$orth[DataframeTotal$token == "sexuaes"] <- "sexuais"
DataframeTotal$orth[DataframeTotal$token == "Sexuaes"] <- "Sexuais"
DataframeTotal$orth[DataframeTotal$token == "SEXUAES"] <- "SEXUAIS"
DataframeTotal$orth[DataframeTotal$token == "siliqua"] <- "síliqua"
DataframeTotal$orth[DataframeTotal$token == "Siliqua"] <- "Síliqua"
DataframeTotal$orth[DataframeTotal$token == "siliquas"] <- "síliquas"
DataframeTotal$orth[DataframeTotal$token == "Siliquas"] <- "Síliquas"
DataframeTotal$orth[DataframeTotal$token == "tunica"] <- "túnica"
DataframeTotal$orth[DataframeTotal$token == "Tunica"] <- "Túnica"
DataframeTotal$orth[DataframeTotal$token == "tunicas"] <- "túnicas"
DataframeTotal$orth[DataframeTotal$token == "Tunicas"] <- "Túnicas"


# Função para lematizar
lematizar <- function(lema, wclass){
  
for(n in 1:length(DataframeTotal$orth)){
  
  if(tolower(DataframeTotal$orth[n]) == tolower(lema)){ # O primeiro "if" lematiza palavras idênticas
    DataframeTotal$lemma[n] <- tolower(lema)
    
  } else
    
  if(tolower(DataframeTotal$orth[n]) == str_c(tolower(lema),"s")){ # O segundo "if" lematiza plurais

    DataframeTotal$lemma[n] <- tolower(lema)

  } else
    
  if(tolower(DataframeTotal$orth[n]) == str_c(str_sub(lema, end = -2L), "a") # Este lematiza adjetivos femininos
    & wclass == "adj") 
    
  {
    
    DataframeTotal$lemma[n] <- tolower(lema)
    
  } else
  
  if(tolower(DataframeTotal$orth[n]) == str_c(str_sub(lema, end = -2L), "as") # Este lematiza adjetivos femininos plurais
       & wclass == "adj") 
      
  {
      
    DataframeTotal$lemma[n] <- tolower(lema)
      
  } else
    
  if(str_detect(lema, "ão\\b")
     & tolower(DataframeTotal$orth[n]) == str_c(str_sub(lema, end = -3L), "ões") # Este lematiza substantivos em -ão
     & wclass == "subst") 
      
  {
      
    DataframeTotal$lemma[n] <- tolower(lema)
      
  } else
    
  if(str_detect(lema, "m\\b")
     & tolower(DataframeTotal$orth[n]) == str_c(str_sub(lema, end = -2L), "ns") # Este lematiza substantivos em -m
     & wclass == "subst") 
    
  {
    
    DataframeTotal$lemma[n] <- tolower(lema)
    
  } else
    
  if(str_detect(lema, "l\\b")
     & tolower(DataframeTotal$orth[n]) == str_c(str_sub(lema, end = -2L), "is") # Este lematiza adjetivos em -l
     & wclass == "adj") 
      
  {
      
    DataframeTotal$lemma[n] <- tolower(lema)
      
  } else
    
  if(str_detect(lema, "r\\b")
     & tolower(DataframeTotal$orth[n]) == str_c(lema, "es")) # Este lematiza subst e adj em -r 
      
  {
      
    DataframeTotal$lemma[n] <- tolower(lema)
      
  }
    
  
}
  
  return(DataframeTotal)
  
}
# Lematiza a partir da ortografia

DataframeTotal <- lematizar("antera", "subst")
DataframeTotal <- lematizar("bífido", "adj")
DataframeTotal <- lematizar("bráctea", "subst")
DataframeTotal <- lematizar("bulbo", "subst")
DataframeTotal <- lematizar("bulboso", "adj")
DataframeTotal <- lematizar("cálice", "subst")
DataframeTotal <- lematizar("capréolo", "subst")
DataframeTotal <- lematizar("cartilagíneo", "adj")
DataframeTotal <- lematizar("coarctado", "adj")
DataframeTotal <- lematizar("conivente", "subst")
DataframeTotal <- lematizar("cotilédone", "subst")
DataframeTotal <- lematizar("crena", "subst")
DataframeTotal <- lematizar("cutícula", "subst")
DataframeTotal <- lematizar("deflexo", "adj")
DataframeTotal <- lematizar("disco", "subst")
DataframeTotal <- lematizar("epiderme", "subst")
DataframeTotal <- lematizar("escamoso", "adj")
DataframeTotal <- lematizar("estame", "subst")
DataframeTotal <- lematizar("estigma", "subst")
DataframeTotal <- lematizar("estípula", "subst")
DataframeTotal <- lematizar("fibroso", "adj")
DataframeTotal <- lematizar("filamento", "subst")
DataframeTotal <- lematizar("flósculo", "subst")
DataframeTotal <- lematizar("foliáceo", "adj")
DataframeTotal <- lematizar("frutificação", "subst")
DataframeTotal <- lematizar("gema", "subst")
DataframeTotal <- lematizar("gomo", "subst")
DataframeTotal <- lematizar("jardim", "subst")
DataframeTotal <- lematizar("botânico", "adj")
DataframeTotal <- lematizar("lacínia", "subst")
DataframeTotal <- lematizar("longitudinal", "adj")
DataframeTotal <- lematizar("medular", "adj")
DataframeTotal <- lematizar("membranáceo", "adj")
DataframeTotal <- lematizar("oblongo", "adj")
DataframeTotal <- lematizar("papilionáceo", "adj")
DataframeTotal <- lematizar("parasítico", "adj")
DataframeTotal <- lematizar("pecíolo", "subst")
DataframeTotal <- lematizar("pedúnculo", "subst")
DataframeTotal <- lematizar("perene", "subst")
DataframeTotal <- lematizar("piloso", "adj")
DataframeTotal <- lematizar("pimpolho", "subst")
DataframeTotal <- lematizar("pistilo", "subst")
DataframeTotal <- lematizar("receptáculo", "subst")
DataframeTotal <- lematizar("repente", "subst")
DataframeTotal <- lematizar("romboidal", "adj")
DataframeTotal <- lematizar("sexual", "adj")
DataframeTotal <- lematizar("síliqua", "subst")
DataframeTotal <- lematizar("sucoso", "adj")
DataframeTotal <- lematizar("túnica", "subst")
DataframeTotal <- lematizar("vilo", "subst")


# A lematização de "hermafrodita" exige um cuidado diferente

DataframeTotal <- lematizar("hermafrodito", "adj")
DataframeTotal$lemma[DataframeTotal$lemma == "hermafrodito"] <- "hermafrodita"
DataframeTotal$lemma[DataframeTotal$lemma == "hermophrodito"] <- "hermafrodita" # Erro no córpus

# Junta a expressão "jardim botânico". Dá para adaptar esse código para uma função
# que junta quaisquer duas palavras

DataframeTotal <- subset(DataframeTotal, lemma != "") # Elimina tudo o que não tiver lema
                                                      # Por alguma razão, a nova versão do R
                                                      # exige que primeiro se faça isso antes de rodar
                                                      # o loop for embaixo

for(n in 1:length(DataframeTotal$doc_id)){
  if(DataframeTotal$lemma[n] == "jardim" &&
     DataframeTotal$lemma[n+1] == "botânico"){
    DataframeTotal$lemma[n] <- paste(DataframeTotal$lemma[n], DataframeTotal$lemma[n+1], sep = " ")
    DataframeTotal$token[n] <- paste(DataframeTotal$token[n], DataframeTotal$token[n+1], sep = " ")
    DataframeTotal$orth[n] <- paste(DataframeTotal$orth[n], DataframeTotal$orth[n+1], sep = " ")
    DataframeTotal$lemma[n+1] <- ""
  }
}

# Elimina tudo o que não for relevante para o dicionário
DadosdoDicionario <- read.csv2("../data/DadosDoDicionario.csv", encoding = "UTF-8") 
colnames(DadosdoDicionario) <- c("ID", "Headword", "FirstAttestationDate",
                                 "FirstAttestationExampleMD", "VariantSpellings", "Etymology",
                                 "WClass", "Credits")

DataframeTotal <- subset(DataframeTotal, lemma %in% DadosdoDicionario$Headword)

#Desambiguação de "gema"
DataframeTotal$sensenumber <- "1"
DataframeTotal$sensenumber[DataframeTotal$sentence_id=="851"] <- "2"
# Se mudar o número de sentenças, o número desta também mudará...


write.csv2(DataframeTotal, file = "../data/DataframePrincipal.csv", fileEncoding = "UTF-8")
