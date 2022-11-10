library(tidyverse)
library(DT)
library(data.table)
#library(koRpus.lang.pt)
library(stringr)


# TokTextDF$token[TokTextDF$lttr==0] <- "" # Elimina o caractere BOM

## Scripts para anotação de POS usando o UDPipe
library(udpipe)

# m_port <- udpipe::udpipe_download_model(language = "portuguese-bosque") # faz download do modelo

m_port <- udpipe_load_model(file = "./tests/portuguese-bosque-ud-2.5-191206.udpipe") # carrega na memória

# Trocar por um script automático que lê o arquivo CorpusTextsMetadata.csv

# Lê os metadados do córpus
CorpusMetadata <- read.csv2("./data/CorpusTextsMetadata.csv",
                            encoding = "UTF-8") 
colnames(CorpusMetadata) <- c("Filename", "Author", "Title",
                              "DateOfPublication")

# Cria uma função que retorna o dataframe de um arquvo txt

CriaDataframeDados <- function(NomeDoArquivo){
  
  CorpusText <- readLines(con = paste0("./data/", NomeDoArquivo),
                       encoding = "UTF-8")

  text_anndfCorpusText <- udpipe::udpipe_annotate(m_port, x = CorpusText) %>%
    as.data.frame() # Cria um dataframe no formato CONLLU com as anotações
  
  # Corrige a numeração da lista de sentenças
  text_anndfCorpusText$sentence_id[1] <- 1
  for(x in 2:length(text_anndfCorpusText$sentence_id)){
    if(text_anndfCorpusText$sentence[x] == text_anndfCorpusText$sentence[(x-1)]){
      text_anndfCorpusText$sentence_id[x] <- text_anndfCorpusText$sentence_id[(x-1)]
    } else {
      text_anndfCorpusText$sentence_id[x] <- text_anndfCorpusText$sentence_id[(x-1)] + 1
    }
  }
  
  text_anndfCorpusText$doc_id <- NomeDoArquivo
  
  return(text_anndfCorpusText)
}

# Junta todos num só
DataframeTotal <- lapply(CorpusMetadata$Filename,
                     CriaDataframeDados) %>% rbindlist()

# Elimina as colunas desnecessárias

DataframeTotal <- subset(DataframeTotal, upos!="PUNCT")
DataframeTotal$paragraph_id <- NULL
DataframeTotal$head_token_id <- NULL
DataframeTotal$dep_rel <- NULL
DataframeTotal$deps <- NULL
DataframeTotal$upos <- NULL
DataframeTotal$xpos <- NULL
DataframeTotal$feats <- NULL

# O arquivo de dados só precisa conter as palavras presentes no dicionário
# Assim, lematizamos o necessário e eliminamos o resto

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
DataframeTotal$orth[DataframeTotal$token == "rezina"] <- "resina"
DataframeTotal$orth[DataframeTotal$token == "Rezina"] <- "Resina"
DataframeTotal$orth[DataframeTotal$token == "rezinas"] <- "resinas"
DataframeTotal$orth[DataframeTotal$token == "Rezinas"] <- "Resinas"
DataframeTotal$orth[DataframeTotal$token == "rezinoso"] <- "resinoso"
DataframeTotal$orth[DataframeTotal$token == "Rezinoso"] <- "Resinoso"
DataframeTotal$orth[DataframeTotal$token == "rezinosos"] <- "resinosos"
DataframeTotal$orth[DataframeTotal$token == "Rezinosos"] <- "Resinosos"
DataframeTotal$orth[DataframeTotal$token == "rezinosa"] <- "resinosa"
DataframeTotal$orth[DataframeTotal$token == "Rezinosa"] <- "Resinosa"
DataframeTotal$orth[DataframeTotal$token == "rezinosas"] <- "resinosas"
DataframeTotal$orth[DataframeTotal$token == "Rezinosas"] <- "Resinosas"
DataframeTotal$orth[DataframeTotal$token == "hypogastrio"] <- "hipogástrio"
DataframeTotal$orth[DataframeTotal$token == "Hypogastrio"] <- "Hipogástrio"
DataframeTotal$orth[DataframeTotal$token == "hypogastrios"] <- "hipogástrios"
DataframeTotal$orth[DataframeTotal$token == "Hypogastrios"] <- "Hipogástrios"
DataframeTotal$orth[DataframeTotal$token == "epigastrio"] <- "epigástrio"
DataframeTotal$orth[DataframeTotal$token == "Epigastrio"] <- "Epigástrio"
DataframeTotal$orth[DataframeTotal$token == "epigastrios"] <- "epigástrios"
DataframeTotal$orth[DataframeTotal$token == "Epigastrios"] <- "Epigástrios"
DataframeTotal$orth[DataframeTotal$token == "glandula"] <- "glândula"
DataframeTotal$orth[DataframeTotal$token == "Glandula"] <- "Glândula"
DataframeTotal$orth[DataframeTotal$token == "glandulas"] <- "glândulas"
DataframeTotal$orth[DataframeTotal$token == "Glandulas"] <- "Glândulas"
DataframeTotal$orth[DataframeTotal$token == "isofago"] <- "esôfago"
DataframeTotal$orth[DataframeTotal$token == "Isofago"] <- "Esôfago"
DataframeTotal$orth[DataframeTotal$token == "isofagos"] <- "esôfagos"
DataframeTotal$orth[DataframeTotal$token == "Isofagos"] <- "Esôfagos"
DataframeTotal$orth[DataframeTotal$token == "isophago"] <- "esôfago"
DataframeTotal$orth[DataframeTotal$token == "Isophago"] <- "Esôfago"
DataframeTotal$orth[DataframeTotal$token == "isophagos"] <- "esôfagos"
DataframeTotal$orth[DataframeTotal$token == "Isophagos"] <- "Esôfagos"
DataframeTotal$orth[DataframeTotal$token == "esofago"] <- "esôfago"
DataframeTotal$orth[DataframeTotal$token == "Esofago"] <- "Esôfago"
DataframeTotal$orth[DataframeTotal$token == "esofagos"] <- "esôfagos"
DataframeTotal$orth[DataframeTotal$token == "Esofagos"] <- "Esôfagos"
DataframeTotal$orth[DataframeTotal$token == "esophago"] <- "esôfago"
DataframeTotal$orth[DataframeTotal$token == "Esophago"] <- "Esôfago"
DataframeTotal$orth[DataframeTotal$token == "esophagos"] <- "esôfagos"
DataframeTotal$orth[DataframeTotal$token == "Esophagos"] <- "Esôfagos"
DataframeTotal$orth[DataframeTotal$token == "myologia"] <- "miologia"
DataframeTotal$orth[DataframeTotal$token == "Myologia"] <- "Miologia"
DataframeTotal$orth[DataframeTotal$token == "myologias"] <- "miologias"
DataframeTotal$orth[DataframeTotal$token == "Myologias"] <- "Miologias"
DataframeTotal$orth[DataframeTotal$token == "MYOLOGIA"] <- "MIOLOGIA"
DataframeTotal$orth[DataframeTotal$token == "MYOLOGIAS"] <- "MIOLOGIAS"
DataframeTotal$orth[DataframeTotal$token == "maseter"] <- "masseter"
DataframeTotal$orth[DataframeTotal$token == "maseteres"] <- "masseteres"
DataframeTotal$orth[DataframeTotal$token == "maceter"] <- "masseter"
DataframeTotal$orth[DataframeTotal$token == "maceteres"] <- "masseteres"
DataframeTotal$orth[DataframeTotal$token == "Maceter"] <- "Masseter"
DataframeTotal$orth[DataframeTotal$token == "Maceteres"] <- "Masseteres"
DataframeTotal$orth[DataframeTotal$token == "orbita"] <- "órbita"
DataframeTotal$orth[DataframeTotal$token == "Orbita"] <- "Órbita"
DataframeTotal$orth[DataframeTotal$token == "orbitas"] <- "órbitas"
DataframeTotal$orth[DataframeTotal$token == "Orbitas"] <- "Órbitas"
DataframeTotal$orth[DataframeTotal$token == "occiput"] <- "ócciput"
DataframeTotal$orth[DataframeTotal$token == "occiputs"] <- "ócciputs"
DataframeTotal$orth[DataframeTotal$token == "Occiput"] <- "Ócciput"
DataframeTotal$orth[DataframeTotal$token == "Occiputs"] <- "Ócciputs"


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
DataframeTotal <- lematizar("verrucoso", "adj")
DataframeTotal <- lematizar("resina", "subst")
DataframeTotal <- lematizar("resinoso", "adj")
DataframeTotal <- lematizar("hipogástrio", "subst")
DataframeTotal <- lematizar("epigástrio", "subst")
DataframeTotal <- lematizar("diafragma", "subst")
DataframeTotal <- lematizar("glândula", "subst")
DataframeTotal <- lematizar("esôfago", "subst")
DataframeTotal <- lematizar("miologia", "subst")
DataframeTotal <- lematizar("masseter", "subst")
DataframeTotal <- lematizar("órbita", "subst")
DataframeTotal <- lematizar("ócciput", "subst")

# A lematização de "hermafrodita" exige um cuidado diferente

DataframeTotal <- lematizar("hermafrodito", "adj")
DataframeTotal$lemma[DataframeTotal$lemma == "hermafrodito"] <- "hermafrodita"
DataframeTotal$lemma[DataframeTotal$lemma == "hermophrodito"] <- "hermafrodita" # Erro no córpus

# Junta a expressão "jardim botânico"
# Funcionou com o txt_recode_ngram(), desde que eu preveja todas as formas variantes
# das três colunas token, orth, lemma
# O algoritmo embaixo funciona também, mas baseado só no lema

DataframeTotal$token <- txt_recode_ngram(DataframeTotal$token,
                                         c("jardim botânico", "jardim botanico", "jardins botanicos",
                                           "JARDINS BOTANICOS", "JARDIM BOTANICO",
                                           "Jardim Botanico", "Jardins Botanicos"), 2, sep = " ")
DataframeTotal$orth <- txt_recode_ngram(DataframeTotal$orth,
                                        c("jardim botânico", "jardins botânicos",
                                          "JARDINS BOTÂNICOS", "JARDIM BOTÂNICO",
                                          "Jardim Botânico", "Jardins Botânicos"), 2, sep = " ")
DataframeTotal$lemma <- txt_recode_ngram(DataframeTotal$lemma,
                                         "jardim botânico", 2, sep = " ")

# Junta a expressão "jardim botânico". Dá para adaptar esse código para uma função
# que junta quaisquer duas palavras

#DataframeTotal <- subset(DataframeTotal, lemma != "") # Elimina tudo o que não tiver lema
# Por alguma razão, a nova versão do R
# exige que primeiro se faça isso antes de rodar
# o loop for embaixo
#for(n in 1:length(DataframeTotal$doc_id)){
#  if(DataframeTotal$lemma[n] == "jardim" &&
#     DataframeTotal$lemma[n+1] == "botânico"){
#    DataframeTotal$lemma[n] <- paste(DataframeTotal$lemma[n], DataframeTotal$lemma[n+1], sep = " ")
#    DataframeTotal$token[n] <- paste(DataframeTotal$token[n], DataframeTotal$token[n+1], sep = " ")
#    DataframeTotal$orth[n] <- paste(DataframeTotal$orth[n], DataframeTotal$orth[n+1], sep = " ")
#    DataframeTotal$lemma[n+1] <- ""
#  }
#}



# Elimina tudo o que não for relevante para o dicionário

DadosdoDicionario <- read.csv("./data/DadosDoDicionario.csv", encoding = "UTF-8")
#colnames(DadosdoDicionario) <- c("ID", "Headword", "FirstAttestationDate",
#                                 "FirstAttestationExampleMD", "VariantSpellings", "Etymology",
#                                 "WClass", "Credits")

DataframeTotal <- subset(DataframeTotal, lemma %in% DadosdoDicionario$Headword)

# Insere os metadados nas sentenças
for(i in 1:length(DataframeTotal$sentence)){
  DataframeTotal$sentence[i] <- paste0(DataframeTotal$sentence[i], " (",
                                       CorpusMetadata$Author[CorpusMetadata$Filename == DataframeTotal$doc_id[i]],
                                       ", ", 
                                       CorpusMetadata$DateOfPublication[CorpusMetadata$Filename == DataframeTotal$doc_id[i]],
                                       ")")
}


# Desambiguações
DataframeTotal$sensenumber <- "1"

# "gema"
DataframeTotal$sensenumber[DataframeTotal$lemma=="gema" & 
                             DataframeTotal$sentence=="d. branco. b. gema. (VANDELLI, Domingos, 1788)"] <- "2"

# "bulbo"
DataframeTotal$sensenumber[DataframeTotal$token=="bulbo"
                           & DataframeTotal$doc_id=="AnatomiadeSantucci.txt"] <- "2"

# "disco"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Quando o meio, ou disco da folha se approxima, ou se une ao mesmo caule. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Quando as mesmas folhas lançaõ no disco inferior raizes como em algumas Algas. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Por margem da folha entendem-se todos os lados exteriores, naõ fallando do disco. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Sendo a margem cartilaginea, differente da substancia do disco, ou da superficie, Sedum. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="As veias da folha se contrahem de tal sorte, que comprimindo o disco, este sobresahe; isto he, eleva-se mais, que os mesmos lados da folha. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Sobresahindo de entre as rugas para parte do disco, ou da superficies da folha, de figura conica pela parte superior, e concava pela inferior. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="A folha, que tem varias excavaçoens, ou o disco entre as veias está abaixado. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Folha revestida e tuberculos algum tanto duros, espalhados pelo seu disco ou superficie. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Havendo sedas de disco, ou na superficie da folha, que sejaõ algum tanto duras, rijas, asperas, e quebradiças. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Estando o disco cheio de espinhos rijos, e picantes. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="A margem ou lado mais restricto obriga o disco da folha a ser concavo. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Sendo o disco superior da folha mais elevado, ou convexo. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="O disco da folha sobe, e desce para a margem formando assim varios angulos, e muitas pregas. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Subindo, e descendo o disco da folha convexamente até a margem. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="O disco da folha forma varias dobras obtusas, e alternadas. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Folha monstruosa, pois he quando a margem da folha sahe maior do que o disco admitte, de maneira que a margem he as ondas. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="A folha he mais funda no disco, que nos lados. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="A superficie interior ou disco da folha elevada longitudinalmente á mesma maneira de quilha. (VANDELLI, Domingos, 1788)"] <- "2"
DataframeTotal$sensenumber[DataframeTotal$lemma=="disco" & 
                             DataframeTotal$sentence=="Tendo dous angulos longitudinaes, prominentes, oppostos, e o disco mais convexo. (VANDELLI, Domingos, 1788)"] <- "2"

# Script para incluir negrito nas sentenças

for(i in 1:length(DataframeTotal$sentence)){
  
  DataframeTotal$sentence[i]<- sub(DataframeTotal$token[i],
                                   paste0("<b>",
                                          DataframeTotal$token[i], 
                                          "</b>"),
                                   DataframeTotal$sentence[i])
}

# Correção do negrito
DataframeTotal$sentence[DataframeTotal$sentence=="d. He a parte, que sustenta, une a anthera (131), ou he o pè da anthera, pelo qual esta se une á planta; ás vezes faltaõ totalmente os filamentos, ou saõ taõ pequenos, que apenas apparecem, e entaõ contaõ-se as <b>antheras</b>. (VANDELLI, Domingos, 1788)"
                        & DataframeTotal$token_id==22] <-
  "d. He a parte, que sustenta, une a anthera (131), ou he o pè da <b>anthera</b>, pelo qual esta se une á planta; ás vezes faltaõ totalmente os filamentos, ou saõ taõ pequenos, que apenas apparecem, e entaõ contaõ-se as antheras. (VANDELLI, Domingos, 1788)"

DataframeTotal$sentence[DataframeTotal$sentence=="Duas antheras em hum só filamento; Mercurialis; tres antheras em hum só filamento, Fumaria; cinco em tres <b>filamentos</b> (VANDELLI, Domingos, 1788)"
                        & DataframeTotal$token_id==11] <-
  "Duas antheras em hum só filamento; Mercurialis; tres <b>antheras</b> em hum só filamento, Fumaria; cinco em tres filamentos (VANDELLI, Domingos, 1788)"

DataframeTotal$sentence[DataframeTotal$sentence=="Duas antheras em hum só filamento; Mercurialis; tres antheras em hum só filamento, Fumaria; cinco em tres <b>filamentos</b> (VANDELLI, Domingos, 1788)"
                        & DataframeTotal$token_id==15] <-
  "Duas antheras em hum só filamento; Mercurialis; tres antheras em hum só <b>filamento</b>, Fumaria; cinco em tres filamentos (VANDELLI, Domingos, 1788)"

DataframeTotal$sentence[DataframeTotal$sentence=="Abertura da <b>anthera</b> pela qual sahe, ou se lança o pollen da sua cavidade, ou loculo, a qual está em hum lado da anthera no Leucojum; em muitas plantas tem as antheras esta abertura no apice Solanum; outras desde a (VANDELLI, Domingos, 1788)"
                        & DataframeTotal$token_id==31] <-
  "Abertura da anthera pela qual sahe, ou se lança o pollen da sua cavidade, ou loculo, a qual está em hum lado da <b>anthera</b> no Leucojum; em muitas plantas tem as antheras esta abertura no apice Solanum; outras desde a (VANDELLI, Domingos, 1788)"

write.csv2(DataframeTotal, file = "./data/DataframePrincipal.csv", fileEncoding = "UTF-8")

rm(i, lematizar, CriaDataframeDados, CorpusMetadata, DadosdoDicionario, DataframeTotal)