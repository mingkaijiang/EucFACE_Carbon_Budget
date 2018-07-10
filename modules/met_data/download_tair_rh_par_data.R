download_tair_rh_par_data <- function() {
    s <- searchHIEv("FACE_R[1-6]_T1_general")
    downloadTOA5(hievSearch=s, maxnfiles=10000)
}