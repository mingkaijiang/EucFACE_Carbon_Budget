download_wind_data <- function() {
    s <- searchHIEv("FACE_R[1-6]_T1_flux")
    downloadTOA5(hievSearch=s, maxnfiles=10000)
}