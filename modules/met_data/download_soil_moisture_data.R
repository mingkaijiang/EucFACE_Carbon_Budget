download_soil_moisture_data <- function() {
    s <- searchHIEv("FACE_R[1-6]_B1_SoilVars")
    downloadTOA5(hievSearch=s, maxnfiles=1000)
}