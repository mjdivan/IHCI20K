source("./ieeelatam/00_funcionesComunes.R")

obtenerDTM=function(rutaAProcesar, patron,usarSTEMMING=FALSE){
  if(!is.character(rutaAProcesar)) return(NULL)
  if(!is.character(patron)) patron=NA;  
  
  if(is.na(patron) || is.null(patron))
    documentos=cargarNoticias(rutaAProcesar)#,patron="*_RF_*")
  else
    documentos=cargarNoticias(rutaAProcesar,patron)#,patron="*_RF_*")
  
  documentos=tm_map(documentos, PlainTextDocument)  
  documentos=tm_map(documentos, tolower)
  documentos=tm_map(documentos, PlainTextDocument)
  for (j in seq (documentos)) 
  {
    documentos [[j]]=gsub("/", " ", documentos [[j]])
    documentos [[j]]= gsub ("@", "", documentos [[j]])
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
    documentos [[j]]= gsub("\u2066", "", documentos [[j]])
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
  }
  documentos=tm_map(documentos, removePunctuation)
  documentos=tm_map(documentos, PlainTextDocument)  
  
  docterm =DocumentTermMatrix(documentos, control=list(stemming = usarSTEMMING, stopwords = TRUE, minWordLength = 3, 
                                                       removeNumbers = TRUE, removePunctuation = TRUE))  
  return(docterm)
}

estNroTopicosLDA=function(docterm,maxTopics=30,writeCSV=FALSE){
  if(!is.logical(writeCSV)) writeCSV=FALSE
  if(maxTopics<2) maxTopics=30

  #La media term frequency-inverse document frequency (tf-idf) sobre los documentos conteniendo
  #este término es utilizado para seleccionar el vocabulario. Esta medida permite omitir términos los cuales tienen baja
  #frecuencia así como aquellos con mucha ocurrenciia en muchos documentos. Solo se incluyen términos los cuales tengan
  #un valor de tf-idf de al menos (la mediana - la decima parte de la desviacion) el cual ees un poco menos a la mediana y asegura que los términos muy frecuenttes 
  #son omitidos 
  term_tfidf = tapply(docterm$v/row_sums(docterm)[docterm$i], docterm$j, mean) *
     log2(nDocs(docterm)/col_sums(docterm > 0))
  #summary(term_tfidf)
  docterm = docterm[, term_tfidf >= (median(term_tfidf)-(sd(term_tfidf)/10))]
  docterm = docterm[row_sums(docterm) > 0,]
  #summary(col_sums(docterm))
  
  #Cross-Validation
  topicos=c(2:maxTopics) #Varía desde 2 tópicos a maxTopics
  folds=5
  D=nrow(docterm)
  folding = sample(rep(seq_len(folds),ceiling(nrow(docterm)))[seq_len(nrow(docterm))])
  set.seed(0908)
  SEED = 20200730
  
  resultado=data.frame(matrix(nrow=0,ncol=4));
  #Perplexity is a measure of how well a probability model fits a new set of data
  colnames(resultado)=c("k","chain","alfa","perplejidad")
  
  for(j in 1:length(topicos))
  {
      k=topicos[j]
      for(i in 1:folds)
      {
        train_set=docterm[folding != i,]
        test_set=docterm[folding == i,]
        
        #fitted=LDA(train_set,k=k, method="Gibbs", control=list(burnin = burnin, iter = iter, keep = keep))
        fitted=LDA(train_set,k=k, method="VEM")
        resultado[nrow(resultado)+1,]=c(k, i, perplexity(fitted,newdata = test_set),as.double(lapply(list(fitted),slot ,"alpha")))
      }
  }

  #alfa is initialized as the mean value of the optimal alfa values in the cross-validation for the models fitted with VEM and alfa estimated
  #perplejidad minima da nrto dee topicos
  if(writeCSV==TRUE)
  {
    write.csv(resultado,file="estNroTopicosLDA.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
  }
  
  return(resultado)
}

calcularDecMarginalPerplejidad=function(ds)
{
  if(!is.data.frame(ds)) return(NULL)

  resultado=data.frame(matrix(nrow=nrow(ds),ncol=1));
  colnames(resultado)=c("diffperp")
  
  for(i in 1:nrow(ds))
  {
    currentChain=ds$chain[i]
    currentK=ds$k[i]
    perp=ds$perplejidad[i]
    
    nperplejidad=ds$perplejidad[ds$chain==currentChain & ds$k==(currentK-1)]
    if(length(nperplejidad)>0 & !is.na(nperplejidad[1]))
    {
      resultado$diffperp[i]=perp-nperplejidad[1]
    }
  }
  
  return(cbind(ds,resultado))
}

interpretarSimLDA=function(sim_lda){
  if(is.null(sim_lda)) return(NULL)
  if(!is.data.frame(sim_lda)) return(NULL);
  if(nrow(sim_lda)==0) return(NULL); 
  
  res=sim_lda;
  res$diffmedian=abs(median(sim_lda$perplejidad,na.rm=TRUE)-sim_lda$perplejidad)  
  minimo=min(res$diffmedian);
  
  return(res[res$diffmedian==minimo,])
}

aplicarLDA=function(docterm,nroTopicos,alfa=NA)
{
  if(is.na(nroTopicos) | nroTopicos<2) 
  {
    print("El número de tópicos debe ser igual o superior a 2")
    return(NULL)
  }
  #La media term frequency-inverse document frequency (tf-idf) sobre los documentos conteniendo
  #este término es utilizado para seleccionar el vocabulario. Esta medida permite omitir términos los cuales tienen baja
  #frecuencia así como aquellos con mucha ocurrenciia en muchos documentos. Solo se incluyen términos los cuales tengan
  #un valor de tf-idf de al menos (la mediana - la decima parte de la desviacion)  y asegura que los términos muy frecuenttes 
  #son omitidos 
  term_tfidf = tapply(docterm$v/row_sums(docterm)[docterm$i], docterm$j, mean) *
    log2(nDocs(docterm)/col_sums(docterm > 0))
  #summary(term_tfidf)
  docterm = docterm[, term_tfidf >= (median(term_tfidf)-(sd(term_tfidf)/10))]
  docterm = docterm[row_sums(docterm) > 0,] #Removemos los documentos con 0 en las filas
  
  require(topicmodels)
  
  if(is.na(alfa))
  {
    lda=LDA(docterm,k=nroTopicos,method="VEM")
  }else
  {
    lda=LDA(docterm,k=nroTopicos,method="VEM", control=list(alpha=alfa))
  }
  
  return(lda);
}  
  
datosTopicoAVis=function(lda,nroTopico=NULL,nroTerms=50,writeCSV=FALSE){
  if(is.null(lda)) return(NULL)
  if(nroTerms<2) nroTerms=50
  
  ap_topics=tidy(lda,matrix="beta")
  
  if(!is.null(nroTopico))
  {#Solo el topico indicado
    ap_top_terms = ap_topics %>%
      filter(topic %in% nroTopico) %>%
      group_by(topic) %>%
      top_n(nroTerms, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
  }else
  {
    ap_top_terms = ap_topics %>%
      group_by(topic) %>%
      top_n(nroTerms, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)    
  }
  
  if(writeCSV==TRUE)
  {
    write.csv(ap_top_terms,file="datosTopicosTermBeta.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
  }
  
  return(ap_top_terms);
}

polaridadPorTopicoDetallada=function(lda,dtm,nTerminos,usaStem=TRUE,writeCSV=FALSE)
{
  if(is.null(lda)) return(NULL)
  if(is.null(dtm)) return(NULL)
  if(!is.logical(writeCSV)) writeCSV=FALSE
  if(!is.logical(usaStem)) usaStem=FALSE
  if(!is.integer(nTerminos)) nTerminos=50
  
  wbt=datosTopicoAVis(lda,nroTerms=nTerminos)
  if(is.null(wbt) | dim(wbt)[1]==0) return(NULL)
  DAL=cargarDAL_UBA();
  if(is.null(DAL) | dim(DAL)[1]==0) return(NULL)
  
  wbt=cbind(wbt,getPolaridad(DAL,wbt$term,usaStem))
  wbt=cbind(wbt,getFrecGralTerm(getFrecGralFromDTM(dtm),wbt$term))
  
  return(wbt)
}

polaridadPorTopico=function(lda,dtm,usaStem=TRUE,writeCSV=FALSE)
{
  if(is.null(lda)) return(NULL)
  if(is.null(dtm)) return(NULL)
  if(!is.logical(writeCSV)) writeCSV=FALSE
  if(!is.logical(usaStem)) usaStem=FALSE
  
  wbt=datosTopicoAVis(lda)
  if(is.null(wbt) | dim(wbt)[1]==0) return(NULL)
  DAL=cargarDAL_UBA();
  if(is.null(DAL) | dim(DAL)[1]==0) return(NULL)
  
  wbt$polaridad=getPolaridad(DAL,wbt$term,usaStem)
  wbt=cbind(wbt,getFrecGralTerm(getFrecGralFromDTM(dtm),wbt$term))
  
  pos= wbt %>% filter(!is.na(polaridad) & !is.na(frec) & polaridad>2) %>% group_by(topic) %>% summarise(sumaPos=sum((polaridad-2)*frec)) %>% select(topic,sumaPos);
  neg= wbt %>% filter(!is.na(polaridad) & !is.na(frec) & polaridad<2) %>% group_by(topic) %>% summarise(sumaNeg=sum((2-polaridad)*frec)) %>% select(topic,sumaNeg);
  union=pos %>% inner_join(neg) %>% mutate(vPolTopico=sumaPos/sumaNeg)
  
  if(writeCSV==TRUE)
  {
    write.csv(union,file="polaridadPorTopico.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
  }
  
  return(union)
}

discriminandoTopicos=function(lda,top1,top2,nroPalDiferencia=10,writeCSV=FALSE)
{
  if(is.null(lda)) return(NULL)
  if(is.null(top1) | is.null(top2)) return(NULL)
  
  datos=datosTopicoAVis(lda,c(top1,top2),100000);
  
  beta_spread = datos %>%
    filter(topic>0.01) %>%
    mutate(topic = paste0("topic", ifelse(topic==top1,"1","2"))) %>%
    spread(topic, beta) %>%
    mutate(log_ratio21 = log2(topic2 / topic1));
  
  cmp21_pos= beta_spread %>% group_by(term) %>% summarise(log2ratio21=sum(log_ratio21)) %>% 
    arrange(desc(log2ratio21),term) %>% head(nroPalDiferencia);
  cmp21_neg= beta_spread %>% group_by(term) %>% summarise(log2ratio21=sum(log_ratio21)) %>% 
    arrange(log2ratio21,term) %>% head(nroPalDiferencia);
  cmp21 =cmp21_pos %>% rbind(cmp21_neg);
  
  cmp21$top1=top1;
  cmp21$top2=top2;
  
  if(writeCSV==TRUE)
  {
    filename=gsub(" ","",paste("diff_Top",topico1,"conTop",topico2,".csv"))
    write.csv(ap_top_terms,file=filename, append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
  }  
  
  return(cmp21)
}

discriminarAllTopicos=function(lda,nroPalDiferencia=10,writeCSV=FALSE)
{
  if(is.null(lda)) return(NULL)
  
  ldaresults=posterior(lda)
  topicos=dim(ldaresults$topics)[2]
  
  resultado=data.frame(matrix(nrow=0,ncol=4));
  #Perplexity is a measure of how well a probability model fits a new set of data
  colnames(resultado)=c("term","log2ratio21","top1","top2")
  
  for(t1 in 1:topicos)
  {
    if((t1+1)<=topicos)
    {
      desde=t1+1;
    
      for(t2 in desde:topicos)
      {
        parcial=discriminandoTopicos(lda,t1,t2,nroPalDiferencia);
        resultado=rbind(resultado,parcial)
      }
    }
  }
  
  if(writeCSV==TRUE)
  {
    write.csv(resultado,file="discriminarAllTopicosLDA.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
  }  
  
  return(resultado);
}

evolucionPolaridadPorTopico=function(rutas,pk,palfa,writeCSV)
{
  resultado=data.frame(matrix(nrow=0,ncol=5));
  #Perplexity is a measure of how well a probability model fits a new set of data
  colnames(resultado)=c("topic","sumaPos","sumaNeg","vPolTopico","ruta")
  
  for(idx in 1:length(rutas))
  {
    print(paste("Procesando DTM",rutas[idx],"..."))
    midtm=obtenerDTM(rutas[idx],NA)
    print(paste("Procesando LDA",rutas[idx],"..."))    
    milda=aplicarLDA(midtm,nroTopicos=pk,alfa=palfa)
    print(paste("Procesando Polaridad Por Tópico",rutas[idx],"..."))    
    polxtopico=polaridadPorTopico(milda,midtm)
    
    polxtopico$ruta=rutas[idx]
    resultado=rbind(resultado,polxtopico)
  }
  
  return(resultado)
}
  




