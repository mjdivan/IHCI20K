source("./ieeelatam/00_funcionesComunes.R")
source("./ieeelatam/01_funcionesMedicion.R")

medirMedios=function(rutas,writeCSV=FALSE){
  if(!is.vector(rutas)) return(NULL)
  if(!is.logical(writeCSV)) writeCSV=FALSE
  
  rdo=medirNoticias(rutas,TRUE)
  
  #Genera nueva columnas aprovechando la estructura con la que se guardan los datos YYYYMMDD_<ID>_<TituloNoticia>
  rdo$anio=substr(rdo$archivo,1,4)
  rdo$mes=substr(rdo$archivo,5,6)
  rdo$dia=substr(rdo$archivo,7,8)
  rdo2=rdo %>% separate(archivo,sep="_", into=c("year","medio","name"),remove=FALSE) %>% 
    select(ruta,archivo,qterm_pos,vterm_pos,qterm_neg,vterm_neg,tasaPosNot,tasaNegNot,vPolNot,anio,mes,dia,medio);
  
  medios=rdo2 %>% select(medio,vPolNot) %>% group_by(medio) %>% summarise(avgPolMedio=mean(vPolNot,na.rm=TRUE),
                                               medPolMedio=median(vPolNot,na.rm=TRUE),
                                               maxPolMedio=max(vPolNot,na.rm=TRUE),
                                               minPolMedio=min(vPolNot,na.rm=TRUE))  

  if(writeCSV==TRUE)
  {
    write.csv(rdo2,file="medirMedios.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
    write.csv(medios,file="medirMedios_indirectas.csv", append=FALSE, sep=";", dec=".", col.names=TRUE, fileEncoding = "UTF-8")
  }
  
  return(medios)
}