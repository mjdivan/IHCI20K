# Caso de Aplicación: Monitoreo de polaridad de noticias en La Pampa  (Argentina) entre [1-mar; 2-abr]
#Funciones de carga de datos

#Librerias
#DocTermMatrix and Stemming
library(tm)
library(SnowballC)
#Data Processing
library(dplyr)
library(tidyr)
library(slam) #matrices
library(doParallel) #procesa en paralelo

#LDA and Beta probabilities
library(topicmodels)
library(tidytext)
library(ldatuning) #encontrar la cantidad de tópicos adecuada
#Graphis
library(ggplot2)


#Carga de los datos retornando un corpus en memoria
#A volatile corpus is fully kept in memory and thus all changes only affect the corresponding R object
cargarNoticias=function(ruta, pEncoding="UTF-8",planguage="es",patron=NA){
  if(is.na(ruta)) return(NA);
  if(!is.character(ruta)) return(NA);
  
  cname=file.path(ruta);  
  dir(cname);
  
  if(is.na(patron))
    documentos= VCorpus(DirSource(cname,encoding=pEncoding),readerControl=list(language=planguage))
  else
    documentos= VCorpus(DirSource(cname,encoding=pEncoding,pattern = patron),readerControl=list(language=planguage))
  
  return(documentos);
};

noticiasEnDir=function(ruta, pEncoding="UTF-8",planguage="es",patron=NA){
  if(is.na(ruta)) return(NA);
  if(!is.character(ruta)) return(NA);
  
  resultado=data.frame(matrix(nrow=0,ncol=2));
  colnames(resultado)=c("ruta","archivo")
  
  if(!is.na(patron))
    files = list.files(path = ruta,pattern = patron)
  else
    files = list.files(path = ruta)
  
  for(idx in 1:length(files))
  {
    nrow1=c(ruta,files[idx])
    resultado[nrow(resultado)+1,]=nrow1
  }

  return(resultado);
};

preproBasicoCorpus=function(documentos){
  if(is.null(documentos)) return(NULL);
  
  #Convierte a minuscula el contenido
  documentos=tm_map(documentos, tolower)
  #Lo lleva a Texto plano
  documentos=tm_map(documentos, PlainTextDocument)
  #Quitar caracteres acentuados
  for (j in seq (documentos)) 
  {
    #Mantener acentos y ñ
    documentos [[j]]= gsub("\"", "", documentos [[j]])
    documentos [[j]]= gsub("“", "", documentos [[j]])   
    documentos [[j]]= gsub("'", "", documentos [[j]])    
    documentos [[j]]= gsub("‘", "", documentos [[j]])
    documentos [[j]]= gsub("«", "", documentos [[j]])
    documentos [[j]]= gsub("»", "", documentos [[j]]) 
    documentos [[j]]= gsub("…", "", documentos [[j]])
    documentos [[j]]=gsub("/", " ", documentos [[j]])
    documentos [[j]]= gsub ("@", " ", documentos [[j]])
    documentos [[j]]=gsub("\\|", " ", documentos [[j]])
    documentos [[j]]= gsub("\u2028", " ", documentos [[j]]) 
    documentos [[j]]= gsub("\u0093", " ", documentos [[j]])
    documentos [[j]]= gsub("\u0094", " ", documentos [[j]])
    
    documentos [[j]]=gsub("\\|", "", documentos [[j]])
    documentos [[j]]= gsub("\u2028", "", documentos [[j]])
    documentos [[j]]= gsub("«", "", documentos [[j]]) 
    documentos [[j]]= gsub("‘", "", documentos [[j]])   
    documentos [[j]]= gsub("“", "", documentos [[j]])
    documentos [[j]]= gsub("”", "", documentos [[j]])   
    documentos [[j]]= gsub("’", "", documentos [[j]])     
    documentos [[j]]= gsub("…", "", documentos [[j]])       
    documentos [[j]]= gsub("»", "", documentos [[j]])  
    documentos [[j]]= gsub("»", "", documentos [[j]])  
    documentos [[j]]= gsub("\u0093", "", documentos [[j]])  
    documentos [[j]]= gsub("\u0094", "", documentos [[j]])
    documentos [[j]]= gsub("¿", "", documentos [[j]])  
    documentos [[j]]= gsub("?", "", documentos [[j]])    
    documentos [[j]]= gsub("!", "", documentos [[j]])      
    documentos [[j]]= gsub("¡", "", documentos [[j]])        
    documentos [[j]]= gsub("–", "", documentos [[j]])    
    documentos [[j]]= gsub("·", "", documentos [[j]])     
    documentos [[j]]= gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", documentos [[j]])       
    documentos [[j]]= gsub("@\\w+", "", documentos [[j]])
    documentos [[j]]= gsub("[[:punct:]]", "", documentos [[j]])
    documentos [[j]]= gsub("[[:digit:]]", "", documentos [[j]])  
    documentos [[j]]= gsub("http\\w+", "", documentos [[j]])
    documentos [[j]]= gsub("[ \t]{2,}", "", documentos [[j]])
    documentos [[j]]= gsub("^\\s+|\\s+$", "", documentos [[j]])   
    
    #Palabras compuestas o localidades con nombres compuestos
    documentos[[j]] =gsub("buenos aires", "buenos_aires", documentos [[j]])
    documentos[[j]] =gsub("corea del sur", "corea_del_sur", documentos [[j]])
    documentos[[j]] =gsub("la pampa", "la_pampa", documentos [[j]])
    documentos[[j]] =gsub("santa rosa", "santa_rosa", documentos [[j]])
    documentos[[j]] =gsub("general pico", "general_pico", documentos [[j]])
    documentos[[j]] =gsub("la arena", "la_arena", documentos [[j]])
    documentos[[j]] =gsub("la reforma", "la_reforma", documentos [[j]])
    documentos[[j]] =gsub("diario textual", "diario_textual", documentos [[j]])  
    documentos[[j]] =gsub("oscar alpa", "oscar_alpa", documentos [[j]])  
    documentos[[j]] =gsub("gines gonzalez garcia", "gines_gonzalez_garcia", documentos [[j]])      
    documentos[[j]] =gsub("carla vizzotti", "carla_vizzotti", documentos [[j]])          
    documentos[[j]] =gsub("organizacion_mundial_de_la_salud", "oms", documentos [[j]])          
  }

    #Remover puntuación
  documentos=tm_map(documentos, removePunctuation)
  #Remover números
  documentos=tm_map(documentos, removeNumbers)  
  
  return(documentos);
}

removerStopWords=function(documentos,planguage="spanish")
{
  if(is.null(documentos)) return(NULL);
  documentos=tm_map (documentos, removeWords, stopwords (planguage))
  documentos = tm_map(documentos, PlainTextDocument)
  documentos=tm_map(documentos, stripWhitespace)
  
  return(documentos);  
}
#Retorna un data frame  con 4 columnas:  palabra, polaridad (-5 a 5), palabra original en inglés y stemm (en español)
cargarPolaridad=function()
{
  if(!("readr" %in% (loadedNamespaces()))) library(readr);
  if(!("tidyr" %in% (loadedNamespaces()))) library(tidyr);
  if(!("SnowballC" %in% (loadedNamespaces()))) library(SnowballC);
  
  dpolaridad=read_delim("./ieeelatam/aux/lexico_afinn.en.spanishv2.txt", ",", escape_double = FALSE, trim_ws = TRUE)
  #dpolaridad = polaridad %>% separate(1,c("palabra","tipo"))  
  dpolaridad$stem=SnowballC::wordStem(dpolaridad$palabra, language="spanish");
  
  return(dpolaridad)
}

cargarDAL_UBA=function(includeAllVersions=FALSE)
{
  if(is.na(includeAllVersions)) return(NULL);
  
  if(!("readr" %in% (loadedNamespaces()))) library(readr);
  if(!("tidyr" %in% (loadedNamespaces()))) library(tidyr);
  if(!("SnowballC" %in% (loadedNamespaces()))) library(SnowballC);
  
  dal_uba=read_delim("./ieeelatam/aux/DAL_CS_UBA.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  colnames(dal_uba)=c("palabraOriginal","media_agrado","media_activacion","media_imaginabilidad",
                      "stdev_agrado","stdev_activacion","stdev_imaginabilidad");
  dal_uba = dal_uba %>% separate(1,c("palabra","tipo"))  
  dal_uba$stem=SnowballC::wordStem(dal_uba$palabra, language="spanish");

  if(includeAllVersions==TRUE) return(dal_uba);
  
  listaUnica=dal_uba %>% select("palabra","media_agrado","media_activacion","media_imaginabilidad",
                                "stdev_agrado","stdev_activacion","stdev_imaginabilidad") %>% group_by(palabra) %>% 
                                summarise(magrado=mean(media_agrado,na.rm=TRUE), mactivacion=mean(media_activacion,na.rm=TRUE),
                                          mimag=mean(media_imaginabilidad,na.rm=TRUE),
                                          msdagrado=mean(stdev_agrado,na.rm=TRUE),  msdactiv=mean(stdev_activacion,na.rm=TRUE),
                                          msdimag=mean(stdev_imaginabilidad,na.rm=TRUE));
  listaUnica$stem=SnowballC::wordStem(listaUnica$palabra, language="spanish");
  return(listaUnica)
}

getPolaridad=function(DAL,termino,usaStem=FALSE)
{
  if(is.null(DAL) | is.null(termino)) {
    return(NA)
  }
  
  if(is.vector(termino))
  {
    if(length(termino)==0) 
    {
      return(NA)
    }
    termino=as.data.frame(termino,stringsAsFactors=FALSE)
  }
  
  if(is.data.frame(termino))
  {
    if(nrow(termino)==0) 
    {
      return(NA)
    }
    resultado=data.frame(matrix(nrow=0,ncol=1));
    colnames(resultado)=c("polaridad")
    
    for(idx in 1:nrow(termino))
    {
      if(is.character(termino[idx,1]))
      {
        palabrab=tolower(termino[idx,1])

        if(usaStem==TRUE)
        {
          terminoStem=SnowballC::wordStem(palabrab, language="spanish");
          busqueda=DAL%>% filter(stem %in% terminoStem) %>% summarise(media=mean(magrado)) %>%  select(media);
        } else
        {
          busqueda=DAL%>% filter(palabra %in% palabrab) %>% summarise(media=mean(magrado)) %>%  select(media);
        }
        
        if(is.null(busqueda) | nrow(busqueda)==0)
        {
          resultado[idx,]=c(NA)
        } else
        {
          resultado[idx,]=busqueda$media[1]
        }
        
      }else
      {
        resultado[idx,]=c(NA)
      }
    }
    
    return(resultado)
  }
  
  if(is.character(termino))
  {
    termino=tolower(termino)
    
    if(usaStem==TRUE)
    {
      terminoStem=SnowballC::wordStem(termino, language="spanish");
      busqueda=DAL%>% filter(stem %in% terminoStem) %>% summarise(media=mean(magrado)) %>%  select(media);
    } else
    {
      busqueda=DAL%>% filter(palabra %in% termino) %>% summarise(media=mean(magrado)) %>%  select(media);
    }
    
    if(is.null(busqueda) | nrow(busqueda)==0) return(NA)
    
    return(busqueda$media)
  }
  
  return(NA)
}

getFrecGralFromDTM=function(dtm)
{
  if(is.null(dtm)) return(NULL)
  
  freq =sort(colSums(as.matrix(dtm)), decreasing = TRUE)
  wf=data.frame(word=names(freq), freq=freq)
  
  return (wf)
}

getFrecGralTerm=function(wf,termino)
{
  if(is.null(wf)) return(NA)
  if(is.null(termino)) return(NA)
  
  if(is.vector(termino))
  {
    if(length(termino)==0) 
    {
      return(NA)
    }
    termino=as.data.frame(termino,stringsAsFactors=FALSE)
  }
  
  if(is.data.frame(termino))
  {
    if(nrow(termino)==0) 
    {
      return(NA)
    }
    resultado=data.frame(matrix(nrow=0,ncol=1));
    colnames(resultado)=c("frec")
    
    for(idx in 1:nrow(termino))
    {
      if(is.character(termino[idx,1]))
      {
       palabrab=tolower(termino[idx,1])
        
       busqueda= wf%>% filter(word %in% palabrab) %>% summarise(suma=sum(freq, na.rm = TRUE)) %>%  select(suma);

       if(is.null(busqueda) | nrow(busqueda)==0)
        {
          resultado[idx,]=c(NA)
        } else
        {
          resultado[idx,]=c(busqueda$suma)
        }
        
      }else
      {
        resultado[idx,]=c(NA)
      }
    }
    
    return(resultado)
  }
  
  if(is.character(termino))
  {
    termino=tolower(termino)
    
    busqueda=wf%>% filter(word %in% termino) %>% summarise(suma=sum(freq, na.rm = TRUE)) %>%  select(suma);
    
    if(is.null(busqueda) | nrow(busqueda)==0) return(NA)
    
    return(busqueda$suma)
  }
  
  return(NA)
}

archivoCuantificadores=function()
{
  if(!("readr" %in% (loadedNamespaces()))) library(readr);

  cuantificadores=read_csv("./ieeelatam/aux/cuantificadoresv2.txt", col_names = FALSE)

  return(cuantificadores);
}

archivoSustantivos=function()
{
  if(!("readr" %in% (loadedNamespaces()))) library(readr);
  
  cuantificadores=read_csv("./ieeelatam/aux/cuantificadoresv2.txt", col_names = FALSE)
  
  return(cuantificadores);
  
  return (sustantivos);
}


