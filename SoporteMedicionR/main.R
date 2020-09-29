#setwd("/Users/mjdivan") #Actualizar el directorio de trabajo
source("./ieeelatam/00_funcionesComunes.R")
source("./ieeelatam/01_funcionesMedicion.R")
source("./ieeelatam/02_funcionesMedios.R")
source("./ieeelatam/03_funcionesLDA.R")


patron=NA;
ruta="./ieeelatam/data/marzo/semana1/"
rutas=c("./ieeelatam/data/marzo/semana1/","./ieeelatam/data/marzo/semana2/",
        "./ieeelatam/data/marzo/semana3/", "./ieeelatam/data/marzo/semana4/","./ieeelatam/data/abril/semana5/")
rutaAProcesar="./ieeelatam/data/todas/"

#############################Medir términos
usarSTEMMING=FALSE
res=medirTerminosLote(rutas,patron,writeCSV=TRUE)


############################Medir Noticias y Medios
medirNoticias(rutas,TRUE)
medirMedios(rutas,TRUE)
allnoticias=medirNoticias(rutas,FALSE)
allnoticias$anio=substr(allnoticias$archivo,1,4)
allnoticias$mes=substr(allnoticias$archivo,5,6)
allnoticias$dia=substr(allnoticias$archivo,7,8)
allnoticias=allnoticias %>% separate(archivo,sep="_", into=c("year","medio","name"),remove=FALSE) %>% 
  select(ruta,archivo,qterm_pos,vterm_pos,qterm_neg,vterm_neg,tasaPosNot,tasaNegNot,vPolNot,anio,mes,dia,medio);
allnoticias$semana[allnoticias$ruta=="./ieeelatam/data/marzo/semana1/"]="SEM-01"
allnoticias$semana[allnoticias$ruta=="./ieeelatam/data/marzo/semana2/"]="SEM-02"
allnoticias$semana[allnoticias$ruta=="./ieeelatam/data/marzo/semana3/"]="SEM-03"
allnoticias$semana[allnoticias$ruta=="./ieeelatam/data/marzo/semana4/"]="SEM-04"
allnoticias$semana[allnoticias$ruta=="./ieeelatam/data/abril/semana5/"]="SEM-05"

#marzo=allnoticias[as.integer(allnoticias$mes)==3,]
marzo$medio=as.factor(marzo$medio)
colores=topo.colors(5)#heat.colors(5)
colores=c("darkgreen","darkblue","firebrick","orange","purple")

  #Polaridad agrupado por semana y por medio
limitado=allnoticias[allnoticias$vPolNot<20 & !is.na(allnoticias$medio),]
ggplot(limitado, aes(x=semana, y=vPolNot, fill=medio)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Spectral")+
  labs(x=NULL, y="Polaridad")+
  theme(legend.position="top")

  #Polaridad de noticia por dia
  require(lubridate)
  allnoticias%>%mutate(fecha=dmy(paste(dia,mes,anio)))%>%group_by(fecha)%>%
    summarise(mediana=median(vPolNot, na.rm=TRUE))%>%select(fecha,mediana)%>%
    ggplot(aes(x=fecha, y=mediana, color=mediana))+
    geom_point(size=2)+geom_line()+
    scale_color_gradientn(colours = c("red","orange","darkgreen","cyan","purple"))+
    labs(x=NULL, y="Mediana de Polaridad por Noticia (medPolNot)")+
    theme(legend.position = "top")
#####################################################LDA
#Simulación para localizar la cantidad de tópicos
dtm=obtenerDTM(rutaAProcesar,NA)
#Corre la simulación para estimar
res2=estNroTopicosLDA(dtm,60,TRUE)
  #Carga resultado de una simulación ejecutada
  #res2=read.csv(file="estNroTopicosLDAv2.csv")

require(ggplot2)
#
ggplot(res2, aes(x = k, y = perplejidad)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_discrete(name="Número de tópicos candidatos", limits=seq(2,60,by=2))+
  ggtitle("Estimación de Tópicos con 5-fold cross-validation") +
  labs(x = "Número de tópicos candidatos", y = "Perplejidad al ajustar el modelo entrenado al conjunto de validación")

res5=res2;
res6=calcularDecMarginalPerplejidad(res5)
res4=res2;
res4$diffmedian=median(res2$perplejidad)-res2$perplejidad
ggplot() +
  #Diferencia respecto de la mediana
  geom_point(data=res4, aes(x = k, y = diffmedian), colour="firebrick", show.legend = TRUE) +
  geom_smooth(data=res4, aes(x = k, y = diffmedian), fill="red", colour="red", size=1) +
  #Difereencia marginal de perplejidad
  geom_point(data=res6, aes(x = k, y = diffperp), colour="darkblue", show.legend = TRUE) +
  geom_smooth(data=res6, aes(x = k, y = diffperp), fill="blue", colour="deepskyblue", size=1) +
  scale_x_discrete(name="Número de tópicos candidatos", limits=seq(8,64,by=8))+
  labs(x = "Número de tópicos candidatos", y = "Diferencias de Perplejidad")

#Parametros interpretados para usar en LDA
minimo=interpretarSimLDA(res2) # junto con contribucion marginal mínima
mean(res6$diffperp[res6$k==33])

res3=res2%>%group_by(k)%>%summarise(avgalfa=mean(alfa))%>%select(k,avgalfa)
ggplot(res3, aes(x = k, y = avgalfa)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_discrete(name="Número de tópicos candidatos", limits=seq(2,60,by=2))+
  ggtitle("Estimación de alfa con 5-fold cross-validation") +
  labs(x = "Número de tópicos candidatos", y = "alfa estimado")

#######################################Aplicar LDA
lda=aplicarLDA(dtm,nroTopicos=minimo$k,alfa=minimo$alfa[1])
ap_all_terms=datosTopicoAVis(lda,writeCSV = TRUE)
ap_top_terms=datosTopicoAVis(lda,nroTerms=10)

#tmresult=posterior(lda)
#attributes(tmresult)
#beta=tmresult$terms
#theta=tmresult$topics #Distribución de probabilidad de los topicos contenidos por documento
#terms(lda,10)#10 primeras palabras para  cada topico

#require(wordcloud)
#t1_term10=datosTopicoAVis(lda,2,10)
#require(wordcloud2)
#wordcloud2(t1_term10, shuffle = FALSE, size = 0.8)

#Gráfica de Top Terms
grapcmp=ap_top_terms %>%
       mutate(term = reorder_within(term, beta, topic)) %>%
       ggplot(aes(term, beta, fill = factor(topic))) +
       geom_col(show.legend = FALSE) +
       facet_wrap(~ topic, scales = "free") +
       coord_flip() +
       scale_x_reordered()
grapcmp


#EVOLUCION TOPICOS POR LDA
evolPol=evolucionPolaridadPorTopico(c(rutas,rutaAProcesar),pk=32,palfa=NA,FALSE)
evolPol$categoria[evolPol$ruta=="./ieeelatam/data/marzo/semana1/"]="S-01-MAR"
evolPol$categoria[evolPol$ruta=="./ieeelatam/data/marzo/semana2/"]="S-02-MAR"
evolPol$categoria[evolPol$ruta=="./ieeelatam/data/marzo/semana3/"]="S-03-MAR"
evolPol$categoria[evolPol$ruta=="./ieeelatam/data/marzo/semana4/"]="S-04-MAR"
evolPol$categoria[evolPol$ruta=="./ieeelatam/data/abril/semana5/"]="S-05-ABR"
evolPol$categoria[evolPol$ruta=="./ieeelatam/data/todas/"]="TODAS"
fcategorias=as.factor(evolPol$categoria)

require(ggplot2)
require(gridExtra)

#Rangos discretos para visualizar el índice nivel de polaridad
evolPol$rango=cut(evolPol$vPolTopico,breaks=c(-Inf,0.95,1.05,2,5,10,Inf),right=FALSE)
heatmap=ggplot(data=evolPol, aes(x=topic,y=categoria))+
  geom_tile(aes(fill=rango), colour="darkblue", size=0.2)+
  guides(fill=guide_legend(title="Nivel de Polaridad"))+
  scale_x_discrete(name="Tópicos",limits=seq(2,32,by=2))+
  ylab("Foco Temporal")+
  theme_grey(base_size=18)+ theme(legend.position ="top")+
  scale_fill_manual(values = c("#990000","#FFFF99","#99CCFF","#6699CC", "#336699",
                               "#003399", "#99CC99","#669966","#339933","#003300"))
heatmap

catTODAS=evolPol[evolPol$categoria=="TODAS",]
pbox=ggplot(catTODAS,aes(x=vPolTopico,y=""))+
  ylab("Todas las Noticias")+
  geom_violin(trim=FALSE, fill='orange', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal() +
  scale_x_discrete(name="Polaridad",limits=seq(-5,100,by=5))+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  stat_summary(fun=median, geom="point", size=2, color="darkred")+
  geom_text(x=median(catTODAS$vPolTopico, na.rm=TRUE)+1, y=1.5, color="darkred",
            label=paste("Mediana: ",round(median(catTODAS$vPolTopico, na.rm=TRUE),4)))+
  stat_summary(fun=mean, geom="point", shape=19, size=3, 
               color="darkgreen")+
  theme_grey(base_size=12)+
  geom_text(x=mean(catTODAS$vPolTopico, na.rm=TRUE)+1, y=1.47, color="darkgreen",
            label=paste("Media: ",round(mean(catTODAS$vPolTopico, na.rm=TRUE),4)))
pbox 

#Muestea Heatmap y  Boxplot en la misma fila como 2 columnas
#grid.arrange(heatmap,pbox,widths=c(0.6,0.4))
grid.arrange(heatmap)

#Polaridad por tópicos 50 terms
polpt=polaridadPorTopico(lda,dtm,writeCSV = TRUE)
polDetTop=polaridadPorTopicoDetallada(lda,dtm,nTerminos=10)

polDetTop %>% ggplot( aes(x=fTopicos,  y=(polaridad-2)*frec, fill=fTopicos)) +
  geom_flat_violin(scale = "count", trim = FALSE, width=2)+
  #geom_boxplot() +
  #scale_x_discrete(name="Tópicos",limits=seq(1,nTopicos,by=1))+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", position = position_nudge(4.9)) + 
  geom_dotplot(binaxis = "y", dotsize = 0.8, stackdir = "down", binwidth = 0.3, position = position_nudge(-0.025)) +  
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Polaridad por tópico") +
  xlab("Tópicos")

ggplot(polDetTop, aes(topic,term,fill=polaridad-2))+
  scale_x_discrete(name="Tópicos",limits=seq(1,nTopicos,by=1))+
  ylab("Polaridad")+
  scale_fill_gradient(low="red", high="blue", na.value = "white")+
  geom_tile()

ggplot(polDetTop, aes(topic, term)) +
  geom_raster(aes(fill = (polaridad-2)*frec)) +
  scale_fill_gradientn(colours = terrain.colors(10))

##Gráfica de términos que diferencian los tópicos
all=discriminarAllTopicos(lda,writeCSV=TRUE)
disc21=discriminandoTopicos(lda,3,4)
grapcmp21=ggplot(data=disc21, aes(x=log2ratio21, y=term, fill=log2ratio21)) +
  geom_bar(stat="identity")+
  theme_minimal()+
  xlab(paste("Log2 ratio de probabilidades beta entre los tópicos ",3," y ",4))+
  ylab("Palabras")+
  geom_text(aes(label=paste0("(",format(log2ratio21,digits=floor(log10(log2ratio21))+2),")")), vjust=1.0, 
            hjust= -.1, color="black", size=3.5)+
  scale_y_discrete(limits=disc21$term)
grapcmp21


SnowballC::wordStem(c("informamos","informen","informar"),language="spanish")
SnowballC::wordStem("informemos",language="spanish")