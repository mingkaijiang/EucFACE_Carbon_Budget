#- Make the VOC-C flux
make_voc_flux <- function(){
    #### returns VOC flux (mg C m-2 d-1)
    ### Needed a model to do this
    
    ### download the data
    download_voc_data()
    
    ### read in the csv
    myDF <- read.csv(file.path(getToPath(), 
                               "FACE_P0044_RA_ISOPRENE_2013-15_RAW_V1.csv"))


    
}