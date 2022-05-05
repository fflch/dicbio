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
lematizar <- function(palavra, lema){
  DataframeTotal$lemma[tolower(DataframeTotal$orth) == tolower(palavra)] <- tolower(lema)
  return(DataframeTotal)
}
# Lematiza a partir da ortografia

DataframeTotal <- lematizar("antera", "antera")
DataframeTotal <- lematizar("anteras", "antera")

DataframeTotal <- lematizar("bífido", "bífido")
DataframeTotal <- lematizar("bífidos", "bífido")
DataframeTotal <- lematizar("bífida", "bífido")
DataframeTotal <- lematizar("bífidas", "bífido")

DataframeTotal <- lematizar("bráctea", "bráctea")
DataframeTotal <- lematizar("brácteas", "bráctea")

DataframeTotal <- lematizar("bulbo", "bulbo")
DataframeTotal <- lematizar("bulbos", "bulbo")

DataframeTotal <- lematizar("bulboso", "bulboso")
DataframeTotal <- lematizar("bulbosa", "bulboso")
DataframeTotal <- lematizar("bulbosos", "bulboso")
DataframeTotal <- lematizar("bulbosas", "bulboso")

DataframeTotal <- lematizar("cálice", "cálice")
DataframeTotal <- lematizar("cálices", "cálice")

DataframeTotal <- lematizar("capréolo", "capréolo")
DataframeTotal <- lematizar("capréolos", "capréolo")

DataframeTotal <- lematizar("cartilagíneo", "cartilagíneo")
DataframeTotal <- lematizar("cartilagíneos", "cartilagíneo")
DataframeTotal <- lematizar("cartilagínea", "cartilagíneo")
DataframeTotal <- lematizar("cartilagíneas", "cartilagíneo")

DataframeTotal <- lematizar("coarctado", "coarctado")
DataframeTotal <- lematizar("coarctada", "coarctado")
DataframeTotal <- lematizar("coarctados", "coarctado")
DataframeTotal <- lematizar("coarctadas", "coarctado")

DataframeTotal <- lematizar("conivente", "conivente")
DataframeTotal <- lematizar("coniventes", "conivente")

DataframeTotal <- lematizar("cotilédone", "cotilédone")
DataframeTotal <- lematizar("cotilédones", "cotilédone")

DataframeTotal <- lematizar("crena", "crena")
DataframeTotal <- lematizar("crenas", "crena")

DataframeTotal <- lematizar("cutícula", "cutícula")
DataframeTotal <- lematizar("cutículas", "cutícula")

DataframeTotal <- lematizar("deflexo", "deflexo")
DataframeTotal <- lematizar("deflexos", "deflexo")
DataframeTotal <- lematizar("deflexa", "deflexo")
DataframeTotal <- lematizar("deflexas", "deflexo")

DataframeTotal <- lematizar("disco", "disco")
DataframeTotal <- lematizar("discos", "disco")

DataframeTotal <- lematizar("epiderme", "epiderme")
DataframeTotal <- lematizar("epidermes", "epiderme")

DataframeTotal <- lematizar("escamoso", "escamoso")
DataframeTotal <- lematizar("escamosos", "escamoso")
DataframeTotal <- lematizar("escamosa", "escamoso")
DataframeTotal <- lematizar("escamosas", "escamoso")

DataframeTotal <- lematizar("estame", "estame")
DataframeTotal <- lematizar("estames", "estame")

DataframeTotal <- lematizar("estigma", "estigma")
DataframeTotal <- lematizar("estigmas", "estigma")

DataframeTotal <- lematizar("estípula", "estípula")
DataframeTotal <- lematizar("estípulas", "estípula")

DataframeTotal <- lematizar("fibroso", "fibroso")
DataframeTotal <- lematizar("fibrosos", "fibroso")
DataframeTotal <- lematizar("fibrosa", "fibroso")
DataframeTotal <- lematizar("fibrosas", "fibroso")

DataframeTotal <- lematizar("filamento", "filamento")
DataframeTotal <- lematizar("filamentos", "filamento")

DataframeTotal <- lematizar("flósculo", "flósculo")
DataframeTotal <- lematizar("flósculos", "flósculo")

DataframeTotal <- lematizar("foliáceo", "foliáceo")
DataframeTotal <- lematizar("foliáceos", "foliáceo")
DataframeTotal <- lematizar("foliácea", "foliáceo")
DataframeTotal <- lematizar("foliáceas", "foliáceo")

DataframeTotal <- lematizar("frutificação", "frutificação")
DataframeTotal <- lematizar("frutificações", "frutificação")

DataframeTotal <- lematizar("gema", "gema")
DataframeTotal <- lematizar("gemas", "gema")

DataframeTotal <- lematizar("gomo", "gomo")
DataframeTotal <- lematizar("gomos", "gomo")

DataframeTotal <- lematizar("hermafrodito", "hermafrodita")
DataframeTotal <- lematizar("hermafroditos", "hermafrodita")
DataframeTotal <- lematizar("hermafrodita", "hermafrodita")
DataframeTotal <- lematizar("hermafroditas", "hermafrodita")

DataframeTotal <- lematizar("jardim", "jardim")
DataframeTotal <- lematizar("jardins", "jardim")

DataframeTotal <- lematizar("botânico", "botânico")
DataframeTotal <- lematizar("botânicos", "botânico")
DataframeTotal <- lematizar("botânica", "botânico")
DataframeTotal <- lematizar("botânicas", "botânico")

DataframeTotal <- lematizar("lacínia", "lacínia")
DataframeTotal <- lematizar("lacínias", "lacínia")

DataframeTotal <- lematizar("longitudinal", "longitudinal")
DataframeTotal <- lematizar("longitudinais", "longitudinal")

DataframeTotal <- lematizar("medular", "medular")
DataframeTotal <- lematizar("medulares", "medular")

DataframeTotal <- lematizar("membranáceo", "membranáceo")
DataframeTotal <- lematizar("membranáceos", "membranáceo")
DataframeTotal <- lematizar("membranácea", "membranáceo")
DataframeTotal <- lematizar("membranáceas", "membranáceo")

DataframeTotal <- lematizar("oblongo", "oblongo")
DataframeTotal <- lematizar("oblongos", "oblongo")
DataframeTotal <- lematizar("oblonga", "oblongo")
DataframeTotal <- lematizar("oblongas", "oblongo")

DataframeTotal <- lematizar("papilionáceo", "papilionáceo")
DataframeTotal <- lematizar("papilionáceos", "papilionáceo")
DataframeTotal <- lematizar("papilionácea", "papilionáceo")
DataframeTotal <- lematizar("papilionáceas", "papilionáceo")

DataframeTotal <- lematizar("parasítico", "parasítico")
DataframeTotal <- lematizar("parasítica", "parasítico")
DataframeTotal <- lematizar("parasíticos", "parasítico")
DataframeTotal <- lematizar("parasíticas", "parasítico")

DataframeTotal <- lematizar("pecíolo", "pecíolo")
DataframeTotal <- lematizar("pecíolos", "pecíolo")

DataframeTotal <- lematizar("pedúnculo", "pedúnculo")
DataframeTotal <- lematizar("pedúnculos", "pedúnculo")

DataframeTotal <- lematizar("perene", "perene")
DataframeTotal <- lematizar("perenes", "perene")

DataframeTotal <- lematizar("piloso", "piloso")
DataframeTotal <- lematizar("pilosos", "piloso")
DataframeTotal <- lematizar("pilosa", "piloso")
DataframeTotal <- lematizar("pilosas", "piloso")

DataframeTotal <- lematizar("pimpolho", "pimpolho")
DataframeTotal <- lematizar("pimpolhos", "pimpolho")

DataframeTotal <- lematizar("pistilo", "pistilo")
DataframeTotal <- lematizar("pistilos", "pistilo")

DataframeTotal <- lematizar("receptáculo", "receptáculo")
DataframeTotal <- lematizar("receptáculos", "receptáculo")

DataframeTotal <- lematizar("repente", "repente")
DataframeTotal <- lematizar("repentes", "repente")

DataframeTotal <- lematizar("romboidal", "romboidal")
DataframeTotal <- lematizar("romboidais", "romboidal")

DataframeTotal <- lematizar("sexual", "sexual")
DataframeTotal <- lematizar("sexuais", "sexual")

DataframeTotal <- lematizar("síliqua", "síliqua")
DataframeTotal <- lematizar("síliquas", "síliqua")

DataframeTotal <- lematizar("sucoso", "sucoso")
DataframeTotal <- lematizar("sucosos", "sucoso")
DataframeTotal <- lematizar("sucosa", "sucoso")
DataframeTotal <- lematizar("sucosas", "sucoso")

DataframeTotal <- lematizar("túnica", "túnica")
DataframeTotal <- lematizar("túnicas", "túnica")

DataframeTotal <- lematizar("vilo", "vilo")
DataframeTotal <- lematizar("vilos", "vilo")


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
colnames(DadosdoDicionario) <- c("ID", "Headword", "Definition", "FirstAttestationDate",
                                 "FirstAttestationExampleMD", "VariantSpellings", "Etymology",
                                 "WClass", "Credits")

DataframeTotal <- subset(DataframeTotal, lemma %in% DadosdoDicionario$Headword)

#Desambiguação de "gema"
DataframeTotal$sensenumber <- "1"
DataframeTotal$sensenumber[DataframeTotal$sentence_id=="851"] <- "2"
# Se mudar o número de sentenças, o número desta também mudará...


write.csv2(DataframeTotal, file = "../data/DataframePrincipal.csv", fileEncoding = "UTF-8")
