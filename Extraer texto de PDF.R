# Parte 1: Extraer texto de un PDF

#Paso 1: Librerías
#instalar pdftools en caso de que no se haya hecho. 
library(pdftools)

# Paso2: Ubicación del documento a trabajar.

# 2.1 descargo un pdf desde un link
pdf.file <- "https://scielo.conicyt.cl/pdf/rchilder/v45n3/0718-3437-rchilder-45-03-00571.pdf"

#2.2ubico el archivo en un directorio.
setwd("~/Documents/Base de Datos SFJ/PDF")

#2.3 descargo el archivo y le coloco un nombre.
download.file(pdf.file, destfile = "prueba1.pdf", mode = "wb")


# Paso 3: Manipulación del texto
#3.1 Para extraer el texto del documento completo
pdf.text <- pdftools::pdf_text("prueba1.pdf")

#3.2 en este caso, extraemos el texto de una página específica.
cat(pdf.text[[26]]) 

# 3.3.debiera generar tabla de contenidos en caso de que el PDF lo permita.
toc <- pdf_toc("prueba1.pdf")


#3.4 permite obtener información metadata
info <- pdf_info("prueba1.pdf")

#3.5 permite sacar las fuentes o tipos de letra (fonts) utilizadas 
fonts <- pdf_fonts("prueba1.pdf")

# 3.6 en este caso extraemos  una palabra en particular.
# Para ello, eliminamos los datos y convertimos a mínuscula todas las palabras.
pdf.text<-unlist(pdf.text)
pdf.text<-tolower(pdf.text)

#3.7 ahora ubicamos en qué página esta la palabra que queremos identificar. 
# En este caso es necesario instalar stingr si es que no lo está.
# Para ejemplificar ocupamos la palabra presidente.

library(stringr)
res<-data.frame(str_detect(pdf.text,"presidente"))
colnames(res)<-"Result"
res<-subset(res,res$Result==TRUE)
row.names(res)

#3.8 permiter revisar tablas insertas en  el PDF seleccionado.

txt <- pdf_text("https://scielo.conicyt.cl/pdf/rchilder/v45n3/0718-3437-rchilder-45-03-00571.pdf")

cat(txt[18])
cat(txt[19])

# Parte 2: MINERÍA DE DATOS CON PDF.

#En este caso se puede hacer comparación entre dos o más textos en formato PDF
# que respondan a un tema en común. Para con ello poder compararlos. 

#Paso 1: Librerías
#instalar pdftools en caso de que no se haya hecho. 
library(pdftools)
install.packages ("tm")
library (tm)
library(NLP)

# Paso2: Ubicación del documento a trabajar.


# Paso2: Ubicación del documento a trabajar.

# 2.1 descargo un pdf desde un link
pdf.file <- "https://scielo.conicyt.cl/pdf/rchilder/v45n3/0718-3437-rchilder-45-03-00571.pdf"

#2.2ubico el archivo en un directorio.
setwd("~/Documents/Base de Datos SFJ/PDF")

#2.3 descargo el archivo y le coloco un nombre.
download.file(pdf.file, destfile = "prueba1.pdf", mode = "wb")

# en este caso baje el archivo y lo gurade con dos nombres distintos en una 
# carpeta para poder hacer siguimiento si el código funciona.

#2.4 Crear un vector de nombres de archivos PDF utilizando la list.files función
#  Este vector contiene el nombre de los archivos a utilizar.
# El patternargumento dice que solo tome los archivos que terminan en "pdf".
# Este código  solo funciona si tiene su directorio de trabajo configurado 
# en la carpeta donde descargó los archivos PDF. 

files <- list.files(pattern = "pdf$")

# Paso 3: Manipulación del texto

install.packages("SnowballC")
library(SnowballC)

# 3.1 En este caso es importante poner el idioma sino será inefectiva la remoción
# de las stopwords.

library(tm)
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))


opinions.tdm <- tm_map(corp, removeWords, c("espinosa","barría","julio","diego","traverso", 
                                            "fig","figs","figura", "vol","volumen", "revista", "tomo"))


opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          removeWords  =TRUE,
                                          stopwords = TRUE,
                                          my_stopwords= TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          stripWhitespace= TRUE,
                                          bounds = list(global = c(2, Inf)))) 

#3.2 #inspecciona los terminos:

# 3.2.1 más comunes.
inspect(opinions.tdm)

#3.2.1 inspecciona los 10 terminos con 1 mención.
inspect(opinions.tdm[1:10,]) 

# Tres consideraciones:
#a) Existe la posibilidad de que aparezcan palabras cortadas, por ejemplo:cumpl 
# o ene l caso de este texto chilen en vez de chileno. Esto se debe a stemming = TRUE,
# el cual hace reducir la palabra a su raiz. Para ello, solo elimine del listado

#b) Existe la posibilidad que aparezcan palabras con comilas o puntos: “…un
# para lo cual, debe aplicar el siguiente código posteriormente:

corp <- tm_map (corp, removePunctuation, ucp = TRUE)

# se usa para eliminar palabras precedidas de comillas dobles y guiones que no
# se eliminaron.

# c) En el caso de  bounds = list(global = c(2, Inf)))) El número 2 equivale a decir 
#las palabras que están repetidas en los textos que se comparan. Por lo cual, 
# equivale a la cantidad de textos que se usan.

#Por lo cual, el código debiera quedar de esta manera:


corp <- tm_map (corp, removePunctuation, ucp = TRUE)
corp2 <- tm_map(corp, removeWords, c("diego","Julio","durante", "los", "las", "vez", "del", "que", "una","como", "tal", 
                                   "por","pero", "había", "fue", "ese", "esa","fig","figs","figura","tres", "sus",
                                   "vol","volumen", "revista","también", "sin", "tomo", "con", 
                                   "esta", "para", "por", "sagredo", "así"))



opinions.tdm <- TermDocumentMatrix(corp2, 
                                   control = 
                                     list(stopwords = TRUE,
                                          removeWords= TRUE,
                                          tolower = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(2, Inf)))) 

inspect(opinions.tdm)
inspect(opinions.tdm[1:10,]) 

# 3.3 Para encontrar palabras que aparecen al menos  n veces  (lowfreq) veces:

 findFreqTerms(opinions.tdm, lowfreq = 20, highfreq = Inf)

#3.4 Podemos guardar el resultado y usarlo para crear un subconjunto del TDM.
ft <- findFreqTerms (opinions.tdm, lowfreq =20, highfreq = Inf)
as.matrix (opinions.tdm [ft,]) 

#3.5 Ver los recuentos totales de esas palabras, podríamos guardar la matriz
#y aplicar la sum función en las filas:

ft.tdm <- as.matrix(opinions.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE)

# Paso 4: Visualización de los Resultados: Nube de Palabras.

library(wordcloud)
library(RColorBrewer)
library(ggplot2)


#version de prueba 
wordcloud(copr2, scale=c(1,0.5), min.freq=10, max.words=30,  colors=brewer.pal(8, "Dark2"))
wordcloud(corp2, scale=c(1,0.5), rot.per=0.25, colors=brewer.pal(8, "Dark2"))




delete.stopwords <- c("espinosa","barría","julio","diego","traverso", 
                  "fig","figs","figura", "vol","volumen", "revista", "tomo")

#Sitios web revisados
# https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

