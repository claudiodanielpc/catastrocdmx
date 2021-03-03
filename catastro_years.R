#Cuadros valor bruto de la producción estatales
#Datos de los Censos Económicos 2019


##Borrar datos del entorno
rm(list=ls())

##Crear folders de almacenamiento
dir.create("catastro")


if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, kableExtra)


##Url general de los Censos económicos 2019

url<-"https://sig.cdmx.gob.mx/documents/"

#Crear consecutivo del 75 al 90  (números ocupados por la ADIP para identificar a la descarga)

lista<-(75:90)

##Descargar y extrar los datos para las 32 entidades federativas
for (i in seq_along(lista)) {  
  
  ##Descargar
  temp<-tempfile()
  download.file(paste0(url,lista[i],"/download"),
                mode="wb",
                destfile = temp)
  

  
  ##Extraer
  unzip(temp,
        exdir = "catastro")
  unlink(temp)
  
}




#Lectura y limpieza de datos====
archivos<-list.files(path = "catastro", 
                        pattern = "sig_cdmx_")


catastro<-purrr::map(archivos,
                    ~ read_csv(glue::glue("catastro/{.x}"), 
                               na = "*",col_types = cols(.default = "c"))%>%
                       select(fid,geo_shape,
                             colonia_predio,
                             uso_construccion,
                             anio_construccion,
                             alcaldia_cumplimiento)%>%
                      #Filtrar por sector
                      filter(uso_construccion=="Habitacional" |
                               uso_construccion=="Habitacional y comercial"))
                      

##Se transforma la lista a dataframe                      
catastro<-catastro%>%
map_df(as_tibble)%>%
  mutate(anio_construccion=as.numeric(anio_construccion))%>%
  mutate(anio_construccion=ifelse(anio_construccion==0,NA,
                                  anio_construccion))%>%
#Eliminar observaciones con NAs y año de construcción menor a 1900
filter(anio_construccion>=1900 )


##Datos para anotaciones en la gráfica
media<-format(round(mean(catastro$anio_construccion),0))
debajom<-catastro%>%filter(anio_construccion<1989)%>%nrow()
total<-nrow(catastro)
pct<-format(round(debajom/total*100,1))


##Gráfica CDMX
  catastro%>%
ggplot(., aes(x=anio_construccion, color=uso_construccion,
              fill=uso_construccion)) +
    geom_histogram(alpha=0.3,
                 position="identity")+
    #Línea de media
    geom_vline(aes(xintercept=mean(anio_construccion)),
               color="blue", linetype="dashed", size=1)+
    #anotación de totales
    annotate("text", 
             x = 1920, 
             y = 130000, 
             label = paste("Total: ", format(total,
                                             big.mark = ",")),
             fontface="bold")+
    #anotación de cuentas menores a 1989
    annotate("text",
             x = 1920, 
             y = 120000,
             label=paste("Cuentas por debajo de la media: ",
                              format(debajom,
                                     big.mark = ",")),
              fontface="bold") + 

    #anotación porcentaje del total
    annotate("text",
             x = 1920, 
             y = 110000,
             label=paste("Porcentaje: ",
                         pct,"%"),
             fontface="bold") + 
  
        
    #anotación de media
    annotate("text",
             x = 1990, 
             y = 135000,
             label=paste("Media: ",
                         media),
             fontface="bold") +
    
    scale_color_manual("Uso",values=c("#feb24c","#31a354"))+
    scale_fill_manual("Uso",values=c("#feb24c","#31a354"))+
    scale_y_continuous(labels=scales::comma)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
    theme_bw()+
  labs(
    title ="Cuentas catastrales de la CDMX. Año de construcción o última remodelación",
    subtitle = "Usos habitacional y habitacional-comercial",
    caption = "    
Fuente: Elaboración propia con datos del Gobierno de la Ciudad de México. 
Agencia Digital de Innovación Pública (ADIP). Sistema Abierto de Información Geográfica",
    x="Año de construcción o última remodelación",
    y="Número"
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text(size=20))
  
  
  ggsave("catastro/graf1.png", height=10, width=20, units='in', dpi=300)
  
  
  
  #Gráfica pór alcaldía
  catastro%>%
    ggplot(., aes(x=anio_construccion, color=uso_construccion,
                  fill=uso_construccion)) +
    geom_histogram(fill="white", alpha=0.5,
                   position="identity")+
    geom_vline(aes(xintercept=mean(anio_construccion)),
               color="blue", linetype="dashed", size=1)+
    scale_color_manual("Uso",values=c("#feb24c","#31a354"))+
    scale_fill_manual("Uso",values=c("#feb24c","#31a354"))+
    scale_y_continuous(labels=scales::comma)+ 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
    theme_bw()+
    labs(
      title ="Cuentas catastrales por alcaldía. Año de construcción o última remodelación",
      subtitle = "Usos habitacional y habitacional-comercial",
      caption = "    
Fuente: Elaboración propia con datos del Gobierno de la Ciudad de México. 
Agencia Digital de Innovación Pública (ADIP). Sistema Abierto de Información Geográfica",
      x="Año de construcción o última remodelación",
      y="Número"
    )+
    theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
          plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
          plot.caption = element_text(hjust = 0,size=12),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          text=element_text(size=20))+
    facet_wrap(~alcaldia_cumplimiento, scale = "free_y")
  
  
  ggsave("catastro/graf2.png", height=10, width=20, units='in', dpi=300)
  
  
  ##Tabla general
  tabla1<-catastro%>%
    group_by(alcaldia_cumplimiento)%>%
    tally()%>%
    arrange(desc(n))%>%
    mutate(pct=n/sum(n)*100)
  
  
  tabla1%>%
    bind_rows(tabla1%>%
                summarise(n=sum(n),
                          pct=sum(pct))%>%
                mutate(alcaldia_cumplimiento="Total"))%>%
    mutate(n=format(round(n,0),big.mark = ","),
           pct=format(round(pct,1)))%>%
    
    kable(caption='<h1 style="color:black;font-size:20px;"><b>Ciudad de México. Cuentas catastrales por alcaldía</b></h><br>
2010-2035<br>',
          format="html",
          align = "c",
          col.names = c("Alcaldía",
                        "Cuentas", "%"))%>%
    kable_styling(full_width = F)%>%
    row_spec(0, bold = F, color = "black", background = "#addd8e")%>%
    row_spec(1:16, bold = F, color = "black", background = "white")%>%
    row_spec(17, bold = T, color = "black", background = "grey")%>%
    footnote(general = "Elaboración propia con datos del Gobierno de la Ciudad de México. 
             Agencia Digital de Innovación Pública (ADIP). Sistema Abierto de Información Geográfica",
             general_title = "
Fuente: ")%>%
    as_image(file="catastro/tabla1.png")
  
  
  
  
  ##Tabla cuentas por debajo de la media
  tabla2<-catastro%>%
    filter(anio_construccion<1989)%>%
    group_by(alcaldia_cumplimiento)%>%
    tally()%>%
    arrange(desc(n))%>%
    mutate(pct=n/sum(n)*100)
  
  
  tabla2%>%
    bind_rows(tabla2%>%
                summarise(n=sum(n),
                          pct=sum(pct))%>%
                mutate(alcaldia_cumplimiento="Total"))%>%
    mutate(n=format(round(n,0),big.mark = ","),
           pct=format(round(pct,1)))%>%
    kable(caption='<h1 style="color:black;font-size:20px;"><b>Ciudad de México. Cuentas catastrales con <br>
    año de construcción o remodelación menor a 1989 </b></h><br>
2010-2035<br>',
          format="html",
          align = "c",
          col.names = c("Alcaldía",
                        "Cuentas", "%"))%>%
    kable_styling(full_width = F)%>%
    row_spec(0, bold = F, color = "black", background = "#addd8e")%>%
    row_spec(1:16, bold = F, color = "black", background = "white")%>%
    row_spec(17, bold = T, color = "black", background = "grey")%>%
    footnote(general = "Elaboración propia con datos del Gobierno de la Ciudad de México. 
             Agencia Digital de Innovación Pública (ADIP). Sistema Abierto de Información Geográfica",
             general_title = "
Fuente: ")%>%
    as_image(file="catastro/tabla2.png")
  
  
  
  
  
  ##Tabla cuentas por debajo de la media
  tabla3<-catastro%>%
    filter(anio_construccion<1989)%>%
    group_by(colonia_predio,
             alcaldia_cumplimiento)%>%
    tally()%>%
    ungroup()%>%
    arrange(desc(n))%>%
    mutate(pct=n/sum(n)*100)%>%
    ##Seleccionar los 30 primeros lugares
    slice(1:30)%>%
    mutate(colonia_predio=ifelse(colonia_predio=="",
                                 "Sin nombre",colonia_predio),
      ref=paste0(colonia_predio,", ",alcaldia_cumplimiento))
  
  
  tabla3%>%
    arrange(n)%>%
  mutate(ref=factor(ref, levels=ref))%>%
    ggplot(.,aes(ref,n))+
    geom_bar(stat="identity", fill="#addd8e", width=.8) +
    geom_text(aes(label=format(n,big.mark = ",")), vjust=0.5,hjust=0, 
              size=4,fontface="bold")+
    coord_flip() +
    xlab("") +
    theme_minimal()+
    labs(
      title = "Principales 30 colonias con cuentas catastrales
",
      subtitle = "Inicio de construcción o última remodelación menor a 1989",
      y = "Cuentas",
      x="",
      caption = "
Fuente: Elaboración propia con datos del Gobierno de la Ciudad de México. 
             Agencia Digital de Innovación Pública (ADIP). Sistema Abierto de Información Geográfica."
    )+
    theme(plot.title = element_text(hjust = 0, size=20,face="bold"),
          plot.subtitle = element_text(hjust = 0, size=12, face="italic"),
          plot.caption = element_text(hjust = 0,size=8),
          legend.position = "none",
          axis.text.x = element_blank())
  ggsave("catastro/graf3.png", height=10, width=20, units='in', dpi=300)
  