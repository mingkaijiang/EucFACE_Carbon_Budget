download_tair_below_canopy <- function() {
    s <- searchHIEv("FACE_R[1-6]_B1_AirVars_201[2-7]")
    downloadTOA5(hievSearch=s, maxnfiles=20000)
}