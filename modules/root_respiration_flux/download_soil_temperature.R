download_soil_temperature <- function() {
    s <- searchHIEv("FACE_R[1-6]_B1_SoilVars_")
    downloadTOA5(hievSearch=s, maxnfiles=10000)
}