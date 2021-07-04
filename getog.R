#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/3430b5a8-a516-42ca-a47d-2e1ce45925fb/download/produccin-de-pozos-de-gas-y-petrleo-2008.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/48585038-055a-4437-bb1d-4fe36073f453/download/produccin-de-pozos-de-gas-y-petrleo-2009.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/364ca28e-d069-4bd6-8771-925f0db152a8/download/produccin-de-pozos-de-gas-y-petrleo-2010.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/4817272c-7365-4bdd-b02d-75b118218b10/download/produccin-de-pozos-de-gas-y-petrleo-2011.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/0dce0e75-1556-47ee-8615-1955fbd54ade/download/produccin-de-pozos-de-gas-y-petrleo-2012.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/bc7ac8fe-2cec-4dab-acdd-322ea1ccc887/download/produccin-de-pozos-de-gas-y-petrleo-2013.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/cd9813a7-1e19-4f60-a02a-7903dd81aff7/download/produccin-de-pozos-de-gas-y-petrleo-2014.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/e375aa35-fd8d-41d6-aa0c-e6879ca567a1/download/produccin-de-pozos-de-gas-y-petrleo-2015.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/d8539ae8-0a71-4339-a16c-139b21bd2cd0/download/produccin-de-pozos-de-gas-y-petrleo-2016.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/df4857e1-7c3f-4980-b5b5-184fe78bfcf0/download/produccin-de-pozos-de-gas-y-petrleo-2017.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/333fd72a-9b83-4bc1-bc94-0f5940b52331/download/produccin-de-pozos-de-gas-y-petrleo-2018.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/8bc0d61c-0408-43d4-a7bc-7178fcb5d37e/download/produccin-de-pozos-de-gas-y-petrleo-2019.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/c4a4a6a0-e75a-4e12-ae5c-54d53a70348c/download/produccin-de-pozos-de-gas-y-petrleo-2020.csv',
#'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/465be754-a372-4c31-b855-81dc5fe3309f/download/produccin-de-pozos-de-gas-y-petrleo-2021.csv'))

getOGdata <- function(years=c(2006:2012),
                    #ind=TRUE,
                    dir=NULL,
                    asDataTable=TRUE,
                    cols=NULL) {
  if (!is.loaded("data.table")) library(data.table)
  if (!is.loaded("foreign")) library(foreign)
  
  ogDisponibles <- data.table(year=c(2006:2012),
                                linkDescarga=c('http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/4e1c55e5-1f1b-4fc8-aa37-2080d9795f29/download/produccin-de-pozos-de-gas-y-petrleo-2006.csv',
                                               'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/be663a63-f020-4e28-8f31-c5f81d47554d/download/produccin-de-pozos-de-gas-y-petrleo-2007.csv',
                                               'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/3430b5a8-a516-42ca-a47d-2e1ce45925fb/download/produccin-de-pozos-de-gas-y-petrleo-2008.csv',
                                               'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/48585038-055a-4437-bb1d-4fe36073f453/download/produccin-de-pozos-de-gas-y-petrleo-2009.csv',
                                               'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/364ca28e-d069-4bd6-8771-925f0db152a8/download/produccin-de-pozos-de-gas-y-petrleo-2010.csv',
                                               'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/4817272c-7365-4bdd-b02d-75b118218b10/download/produccin-de-pozos-de-gas-y-petrleo-2011.csv',
                                               'http://datos.minem.gob.ar/dataset/c846e79c-026c-4040-897f-1ad3543b407c/resource/0dce0e75-1556-47ee-8615-1955fbd54ade/download/produccin-de-pozos-de-gas-y-petrleo-2012.csv'))
                                               
  
  ogDisponibles <- ogDisponibles[year %in% years]
  
  ogDisponibles[,nickname:= paste0('year',substr(year,3,4))]
  
  if(is.null(dir)) stop('Hay que indicar un directorio donde desean guardarse los archivos')
  if(dir.exists(dir)) {
    archivos <-list.files(dir,
                          pattern = '*year*',ignore.case = TRUE)
  } 
    ogDescargadas <- tolower(regmatches(archivos,regexpr('(?i)year\\d{2}',archivos)))
    ogFaltantes <- ogDisponibles[!nickname %in% ogDescargadas]
    if(nrow(ogFaltantes)==0){
      cat("Todas los microdatos de la og ya están descargadas en el directorio\r")
    } else {
      
      for(i in 1:nrow(ogFaltantes)) {
        temp <- paste(dir,'/',ogFaltantes[i,'nickname'],'.csv',sep='')
        download.file(unlist(ogFaltantes[i,'linkDescarga']),temp)
        #unzip(temp,exdir = dir)
        
      }
      cat("Todas los microdatos de OG ya se terminaron de descargar en el directorio\r")
    }
    filesToRead <- list.files(path=dir,
                              pattern = paste0(ogDisponibles$nickname,'|',toupper(ogDisponibles$nickname),collapse = "|"))
    filesToRead <- filesToRead[grepl(pattern = '*year*',x = filesToRead,ignore.case = TRUE)]
    datos <- lapply(filesToRead,function(x){
      cat("Trabajando en el archivo ",x,"\r")
      if(grepl(pattern = 'csv',x = x)) {
        aux <- as.data.table(read.table(paste0(dir,'/',x), fill=T, stringsAsFactors=FALSE, header=T, sep=",", quote="\"", comment.char = "")[ ,c(2:16,22,24:26,28,30:37)])
        colnames(aux) <- toupper(colnames(aux))
      } else {
        aux <- data.table::fread(paste0(dir,'/',x))
      }
      
      if(!is.null(cols)) {
        aux <- aux[,colnames(aux) %in% cols, with=FALSE]
      }
      aux
      
    })
    datos <- rbindlist(datos, fill=TRUE)
    if(!asDataTable) {
      datos <- as.data.frame(datos)
    }
    return(datos)
  }
