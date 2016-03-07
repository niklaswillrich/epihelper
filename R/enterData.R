
enterData <- function(filename="quest.csv", path="data/") {

    file <- paste0(path, filename)

    # Check if directory exist if not creates one
    if(!dir.exists(path)) dir.create(path)
    # Check if file exists if not creates one
    if(!file.exists(file)) file.create(file=file)
    # Check if variable database exists and file size is bigger than 0
    if (!exists("database") & file.info(file)$size!=0) database <- read.csv(file=file)
    # Check if variable database exists otherwise creates one
    if (!exists("database")) database <- data.frame()

    for(i in 1:500) {
        #---------------------- Start the questionnaire ----------------------------
        id <- as.character(readline("id: "))
        pseudonym <- as.character(readline("pseudonym: "))
        age <- as.numeric(readline("age: "))
        sex <- readline("sex: ")

        #-------------- Appends database from one questionnaire -------------------
        database <- rbind(database,data.frame(id, pseudonym, age, sex))
        rm(id, pseudonym, age, sex)
        #-------------- Next questionnaire -------------------
        if(readline("Do you want to go on? (leave empty to proceed):")!="") break
    }

    write.csv(database, file=file, row.names = FALSE)

}
