#Cálculo de métricas según el proyecto de medición y evaluación
source("./ieeelatam/00_funcionesComunes.R")

medirTerminos=function(rutaAProcesar,patron,topten=TRUE,writeCSV=FALSE){
  if(!is.logical(topten))
    topten=TRUE;#Si pasan cualquier cosa, se establece a TRUE por defecto
  if(!is.logical(writeCSV))
    writeCSV=FALSE;#Si TRUE, escribe un archivo denominado como la ruta para los positivos/negativos Top 50 y Top51+
 
  
  if(is.na(patron) || is.null(patron))
    documentos=cargarNoticias(rutaAProcesar)#,patron="*_RF_*")
  else
    documentos=cargarNoticias(rutaAProcesar,patron)#,patron="*_RF_*")
  
  documentos=preproBasicoCorpus(documentos)
  documentos=removerStopWords(documentos)
  
  docterm =DocumentTermMatrix(documentos)
  termdoc=TermDocumentMatrix(documentos)
  
  freq =sort (colSums(as.matrix(docterm)), decreasing = TRUE)
  wf=data.frame(word=names(freq), freq=freq)
  
  dal_uba = cargarDAL_UBA()
  
  wf$palabra=as.character(wf$word);
  wf$stem=SnowballC::wordStem(wf$palabra, language="spanish");
  
  #Cruce por palabra (no stemm)
  wf2=wf %>% left_join(dal_uba, by=c("word"="palabra"))
  
  resultado=data.frame(matrix(nrow=1,ncol=15));
  colnames(resultado)=c("qterm_pos","vterm_pos","qterm_neg","vterm_neg",
                        "qterm_act","vterm_act", "qterm_pas", "vterm_pas",
                        "qterm_fimag", "vterm_fimag","qterm_dimag","vterm_dimag",
                        "tasa_posneg", "tasa_actinac", "tasa_fdimag")
  
  if(topten==TRUE)
    positivos=head(wf2,50) %>% filter(!is.na(magrado) & magrado>2) %>%select(magrado,freq)
  else
    positivos=wf2[51:nrow(wf2),] %>% filter(!is.na(magrado) & magrado>2) %>%select(magrado,freq)
  
  if(dim(positivos)[1]>0)
  {
    resultado$vterm_pos[1]=as.double(sum((positivos$magrado-2)*positivos$freq))
    resultado$qterm_pos[1]=as.double(sum(positivos$freq))
  } else
  {
    resultado$vterm_pos[1]=0
    resultado$qterm_pos[1]=0
  }
  
  if(topten==TRUE)
    negativos=head(wf2,50) %>% filter(!is.na(magrado) & magrado<2) %>%select(magrado,freq)
  else
    negativos=wf2[51:nrow(wf2),] %>% filter(!is.na(magrado) & magrado<2) %>%select(magrado,freq)
  
  if(dim(negativos)[1]>0)
  {
    resultado$vterm_neg=as.double(sum((2-negativos$magrado)*negativos$freq))
    resultado$qterm_neg=as.double(sum(negativos$freq))
  } else
  {
    resultado$vterm_neg=0
    resultado$qterm_neg=0
  }
  
  if(topten==TRUE)
    activos=head(wf2,50) %>% filter(!is.na(mactivacion) & mactivacion>2) %>%select(mactivacion,freq)
  else
    activos=wf2[51:nrow(wf2),] %>% filter(!is.na(mactivacion) & mactivacion>2) %>%select(mactivacion,freq)
  
  if(dim(activos)[1]>0)
  {
    resultado$qterm_act[1]=as.double(sum(activos$freq))
    resultado$vterm_act[1]=as.double(sum(activos$freq*(activos$mactivacion-2)))
  }else
  {
    resultado$qterm_act[1]=0
    resultado$vterm_act[1]=0
  } 
  
  if(topten==TRUE)
    inactivos=head(wf2,50) %>% filter(!is.na(mactivacion) & mactivacion<2) %>%select(mactivacion,freq)
  else
    inactivos=wf2[51:nrow(wf2),] %>% filter(!is.na(mactivacion) & mactivacion<2) %>%select(mactivacion,freq)
  
  if(dim(inactivos)[1]>0)
  {
    resultado$qterm_pas[1]=as.double(sum(inactivos$freq))
    resultado$vterm_pas[1]=as.double(sum(inactivos$freq*(2-inactivos$mactivacion)))
  }else
  {
    resultado$qterm_pas[1]=0
    resultado$vterm_pas[1]=0
  }
  
  if(topten==TRUE)
    imag=head(wf2,50) %>% filter(!is.na(mimag) & mimag>2) %>%select(mimag,freq)
  else
    imag=wf2[51:nrow(wf2),] %>% filter(!is.na(mimag) & mimag>2) %>%select(mimag,freq)
  
  if(dim(imag)[1]>0)
  {
    resultado$qterm_fimag[1]=as.double(sum(imag$freq))
    resultado$vterm_fimag[1]=as.double(sum(imag$freq*(imag$mimag-2)))
  }else
  {
    resultado$qterm_fimag[1]=0
    resultado$vterm_fimag[1]=0
  }
  
  if(topten==TRUE)
    noimag=head(wf2,50) %>% filter(!is.na(mimag) & mimag<2) %>% select(mimag,freq)
  else
    noimag=wf2[51:nrow(wf2),] %>% filter(!is.na(mimag) & mimag<2) %>% select(mimag,freq)
  
  if(dim(noimag)[1]>0)
  {
    resultado$qterm_dimag[1]=as.double(sum(noimag$freq))
    resultado$vterm_dimag[1]=as.double(sum(noimag$freq*(2-noimag$mimag)))
  }else
  {
    resultado$qterm_dimag[1]=0
    resultado$vterm_dimag[1]=0
  }
  
  if(resultado$qterm_pos>0 & resultado$qterm_neg>0)
  {
    resultado$tasa_posneg[1]=(resultado$vterm_pos/resultado$qterm_pos)/(resultado$vterm_neg/resultado$qterm_neg);  
  } else
  {  
    resultado$tasa_posneg[1]=NA;
  }
  
  if(resultado$qterm_act>0 & resultado$qterm_pas>0)
  {
    resultado$tasa_actinac[1]=(resultado$vterm_act/resultado$qterm_act)/(resultado$vterm_pas/resultado$qterm_pas);  
  } else
  {  
    resultado$tasa_actinac[1]=NA;
  }
  
  if(resultado$qterm_fimag>0 & resultado$qterm_dimag>0)
  {
    resultado$tasa_fdimag[1]=(resultado$vterm_fimag/resultado$qterm_fimag)/(resultado$vterm_dimag/resultado$qterm_dimag);  
  } else
  {  
    resultado$tasa_fdimag[1]=NA;
  }
  
  if(writeCSV==TRUE)
  {
    root=gsub("[^A-z0-9]","",rutaAProcesar);
    root_f50=paste(root,"_first50.csv")
    root_a50=paste(root,"_after50.csv")
    
    f50=wf2[1:50,] %>% select(palabra,freq,magrado,mactivacion,mimag);
    a50=wf2[51:nrow(wf2),] %>% select(palabra,freq,magrado,mactivacion,mimag);
    
    write.csv(f50,file=root_f50, append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
    write.csv(a50,file=root_a50, append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")    
  }
  
  return(resultado)
}

medirTerminosLote=function(rutas,patron,writeCSV=FALSE){
  if(!is.vector(rutas)) return(NULL)
  if(!is.logical(writeCSV)) writeCSV=FALSE;
  
  resultado=data.frame(matrix(nrow=0,ncol=17));
  colnames(resultado)=c("ruta","tipo","qterm_pos","vterm_pos","qterm_neg","vterm_neg",
                        "qterm_act","vterm_act", "qterm_pas", "vterm_pas",
                        "qterm_fimag", "vterm_fimag","qterm_dimag","vterm_dimag",
                        "tasa_posneg", "tasa_actinac", "tasa_fdimag")
  
  mindir=data.frame(matrix(nrow=0,ncol=8));
  colnames(mindir)=c("ruta","tasaPosNeg","tasaActInt","tasaFiDi","posTop50","negTop50",
                     "actTop50","fiTop50")
  
  for(idx in 1:length(rutas))
  {
    r1=medirTerminos(rutas[idx],patron,TRUE,writeCSV)#TopTen
    r2=medirTerminos(rutas[idx],patron,FALSE,writeCSV)#Top11+
    
    nrow1=c(rutas[idx], "Top50",
                 r1$qterm_pos,r1$vterm_pos,r1$qterm_neg,r1$vterm_neg,
                 r1$qterm_act,r1$vterm_act, r1$qterm_pas, r1$vterm_pas,
                 r1$qterm_fimag, r1$vterm_fimag, r1$qterm_dimag,r1$vterm_dimag,
                 r1$tasa_posneg, r1$tasa_actinac, r1$tasa_fdimag)
    resultado[nrow(resultado)+1,]=nrow1;
    
    nrow2=c(rutas[idx], "Top51+",
            r2$qterm_pos,r2$vterm_pos,r2$qterm_neg,r2$vterm_neg,
            r2$qterm_act,r2$vterm_act, r2$qterm_pas, r2$vterm_pas,
            r2$qterm_fimag, r2$vterm_fimag, r2$qterm_dimag,r2$vterm_dimag,
            r2$tasa_posneg, r2$tasa_actinac, r2$tasa_fdimag)
    resultado[nrow(resultado)+1,]=nrow2;
    
    if(!is.na(r1$vterm_neg) & !is.na(r2$vterm_neg) & (r1$vterm_neg+r2$vterm_neg)!=0)
      tasaPosNeg=(r1$vterm_pos+r2$vterm_pos)/(r1$vterm_neg+r2$vterm_neg)
    else
      tasaPosNeg=NA;
    
    if(!is.na(r1$vterm_pas) & !is.na(r2$vterm_pas) & (r1$vterm_pas+r2$vterm_pas)!=0)
      tasaActIn=(r1$vterm_act+r2$vterm_act)/(r1$vterm_pas+r2$vterm_pas)
    else
      tasaActIn=NA;
    
    if(!is.na(r1$vterm_dimag) & !is.na(r2$vterm_dimag) & (r1$vterm_dimag+r2$vterm_dimag)!=0)
      tasaFiDi=(r1$vterm_fimag+r2$vterm_fimag)/(r1$vterm_dimag+r2$vterm_dimag)
    else
      tasaFiDi=NA;
    
    if(!is.na(r1$vterm_pos) & !is.na(r2$vterm_pos) & (r1$vterm_pos+r2$vterm_pos)!=0)
      posTop50=(r1$vterm_pos/(r1$vterm_pos+r2$vterm_pos))*100
    else
      posTop50=NA
    
    if(!is.na(r1$vterm_neg) & !is.na(r2$vterm_neg) & (r1$vterm_neg+r2$vterm_neg)!=0)
      negTop50=(r1$vterm_neg/(r1$vterm_neg+r2$vterm_neg))*100
    else
      negTop50=NA
    
    if(!is.na(r1$vterm_act) & !is.na(r2$vterm_act) & (r1$vterm_act+r2$vterm_act)!=0)
      actTop50=(r1$vterm_act/(r1$vterm_act+r2$vterm_act))*100
    else
      actTop50=NA
    
    if(!is.na(r1$vterm_fimag) & !is.na(r2$vterm_fimag) & (r1$vterm_fimag+r2$vterm_fimag)!=0)
      fiTop50=(r1$vterm_fimag/(r1$vterm_fimag+r2$vterm_fimag))*100
    else
      fiTop50=NA
    
    nrowMinDir=c(rutas[idx], tasaPosNeg, tasaActIn, tasaFiDi, posTop50, negTop50, actTop50, fiTop50)
    mindir[nrow(mindir)+1,]=nrowMinDir;    
  }
  
  if(writeCSV==TRUE)
  {
    write.csv(resultado,file="medirTerminosLote.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
    write.csv(mindir,file="medirTerminosLote_indir.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
  }
  
  return(resultado)
}


#Polaridad por noticia
medirNoticia=function(rutaAProcesar,archivo,dal_uba){
  if(is.null(dal_uba)) return(NULL)
  if(is.null(archivo) | is.na(archivo)) return(NULL)
  
  documentos=cargarNoticias(rutaAProcesar,patron=archivo)
  
  documentos=preproBasicoCorpus(documentos)
  documentos=removerStopWords(documentos)
  
  docterm =DocumentTermMatrix(documentos)
  termdoc=TermDocumentMatrix(documentos)
  
  freq =sort (colSums(as.matrix(docterm)), decreasing = TRUE)
  wf=data.frame(word=names(freq), freq=freq)
  
  wf$palabra=as.character(wf$word);
  wf$stem=SnowballC::wordStem(wf$palabra, language="spanish");
  
  #Cruce por palabra (no stemm)
  wf2=wf %>% left_join(dal_uba, by=c("word"="palabra"))
  
  resultado=data.frame(matrix(nrow=1,ncol=9));
  colnames(resultado)=c("ruta","archivo","qterm_pos","vterm_pos","qterm_neg","vterm_neg","tasaPosNot","tasaNegNot","vPolNot")
  
  resultado$ruta[1]=rutaAProcesar;
  resultado$archivo[1]=archivo;
  
  positivos=wf2 %>% filter(!is.na(magrado) & magrado>2) %>%select(magrado,freq)
  
  if(dim(positivos)[1]>0)
  {
    resultado$vterm_pos[1]=as.double(sum((positivos$magrado-2)*positivos$freq))
    resultado$qterm_pos[1]=as.double(sum(positivos$freq))
    
    if(resultado$qterm_pos[1]>0)
    {
      resultado$tasaPosNot[1] = resultado$vterm_pos[1] / resultado$qterm_pos[1];
    }else
      resultado$tasaPosNot[1] = NA;
  } else
  {
    resultado$vterm_pos[1]=0
    resultado$qterm_pos[1]=0
    resultado$tasaPosNot[1] = NA;
  }
  
  negativos=wf2 %>% filter(!is.na(magrado) & magrado<2) %>%select(magrado,freq)

  if(dim(negativos)[1]>0)
  {
    resultado$vterm_neg=as.double(sum((2-negativos$magrado)*negativos$freq))
    resultado$qterm_neg=as.double(sum(negativos$freq))
    
    if(resultado$qterm_neg[1]>0)
    { 
      resultado$tasaNegNot[1] = resultado$vterm_neg[1] / resultado$qterm_neg[1];
    }else
      resultado$tasaNegNot[1] = NA;    
  } else
  {
    resultado$vterm_neg[1]=0
    resultado$qterm_neg[1]=0
    resultado$tasaNegNot[1] = NA;
  }
  
  if(!is.na(resultado$vterm_neg[1]) & !is.na(resultado$vterm_pos[1]) &
     resultado$vterm_neg[1]>0 & resultado$vterm_pos[1]>0)
  {
    resultado$vPolNot[1]=(resultado$vterm_pos[1])/(resultado$vterm_neg[1]);
  } else
  {
    resultado$vPolNot[1]=NA;
  }
  
  return(resultado)
}

medirNoticias=function(rutas,writeCSV=FALSE){
  if(!is.vector(rutas)) return(NULL)
  if(!is.logical(writeCSV)) writeCSV=FALSE
  
  dal_uba = cargarDAL_UBA()

  resultado=data.frame(matrix(nrow=0,ncol=9));
  colnames(resultado)=c("ruta","archivo","qterm_pos","vterm_pos","qterm_neg","vterm_neg","tasaPosNot","tasaNegNot","vPolNot")

  for(idx in 1:length(rutas))
  {
      noticias=noticiasEnDir(rutas[idx])

      for(notidx in 1:nrow(noticias))
      {
        medida=medirNoticia(noticias$ruta[notidx],noticias$archivo[notidx],dal_uba)

        if(nrow(medida)>0)        
          resultado[nrow(resultado)+1,]=medida[1,]
      }
  }
  
  colnames(resultado)=c("ruta","archivo","qterm_pos","vterm_pos","qterm_neg","vterm_neg","tasaPosNot","tasaNegNot","vPolNot")
  mindir=data.frame(matrix(nrow=1,ncol=22));
  colnames(mindir)=c("minPosNot","maxPosNot","medPosNot","q1PosNot","q3PosNot","sdPosNot","cvPosNot",
                     "minNegNot","maxNegNot","medNegNot","q1NegNot","q3NegNot","sdNegNot","cvNegNot",
                     "minPolNot","maxPolNot","medPolNot","q1PolNot","q3PolNot","sdPolNot","cvPolNot","avgPolNot")
  
  res=quantile(resultado$vterm_pos, na.rm = TRUE)
  mindir$minPosNot[1]=res[1]
  mindir$q1PosNot[1]=res[2]
  mindir$medPosNot[1]=res[3]
  mindir$q3PosNot[1]=res[4]
  mindir$maxPosNot[1]=res[5]
  mindir$sdPosNot[1]=sd(resultado$vterm_pos, na.rm = TRUE)
  if(!is.na(mindir$sdPosNot[1]) & mindir$sdPosNot[1]!=0)
    mindir$cvPosNot[1]=mean(resultado$tasaPosNot, na.rm = TRUE)/mindir$sdPosNot[1]
  else
    mindir$cvPosNot[1]=NA
  
  res=quantile(resultado$vterm_neg, na.rm = TRUE)
  mindir$minNegNot[1]=res[1]
  mindir$q1NegNot[1]=res[2]
  mindir$medNegNot[1]=res[3]
  mindir$q3NegNot[1]=res[4]
  mindir$maxNegNot[1]=res[5]
  mindir$sdNegNot[1]=sd(resultado$vterm_neg, na.rm = TRUE)
  if(!is.na(mindir$sdNegNot[1]) & mindir$sdNegNot[1]!=0)
  {
    mindir$cvPosNot[1]=mean(resultado$tasaNegNot, na.rm = TRUE)/mindir$sdNegNot[1]
  }else
  {
    mindir$cvPosNot[1]=NA
  }
  
  res=quantile(resultado$vPolNot, na.rm = TRUE)
  mindir$minPolNot[1]=res[1]
  mindir$q1PolNot[1]=res[2]
  mindir$medPolNot[1]=res[3]
  mindir$q3PolNot[1]=res[4]
  mindir$maxPolNot[1]=res[5]
  mindir$sdPolNot[1]=sd(resultado$vPolNot, na.rm = TRUE)
  if(!is.na(mindir$sdPolNot[1]) & mindir$sdPolNot[1]!=0)
  {
    mindir$cvPolNot[1]=mean(resultado$vPolNot, na.rm = TRUE)/mindir$sdPolNot[1]
  }else
  {
    mindir$cvPolNot[1]=NA
  }
  mindir$avgPolNot[1]=mean(resultado$vPolNot, na.rm = TRUE)
  
  if(writeCSV==TRUE)
  {
    write.csv(resultado,file="medirNoticias.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
    write.csv(mindir,file="medirNoticias_indirectas.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
  }
 
  return(resultado)
}
