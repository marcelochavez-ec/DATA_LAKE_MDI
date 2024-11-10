Leer_Drive<-function(link_drive,sep=",",dec=".") {
    require(data.table)
    id<-strsplit(link_drive,"id=")[[1]][2]
    return(fread(sprintf("https://drive.google.com/drive/folders/1QR0M4NvbSuQnmHnVKzgbhmPJHGFe4eTf?usp=sharing", id),
                 sep=sep,dec=dec,integer64 = "character"))
}

drive_find("https://drive.google.com/drive/folders/1QR0M4NvbSuQnmHnVKzgbhmPJHGFe4eTf?usp=sharing")

datos<-Leer_Drive("https://drive.google.com/open?id=1mhj0hSUuMefcB612NjtT75SWs71P7QBa",sep=";")
head(datos)