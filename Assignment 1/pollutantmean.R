pollutantmean <- function(directory, pollutant, id = 1:332) {  
  data <- NA
  input_files <- paste(directory, "/", sprintf("%03d", id), ".csv", sep="")
  
  for (file in input_files) {
    data_frame = read.csv(file)
    data = c(data, data_frame[[pollutant]])
  }
  
  mean(data, na.rm = TRUE)
}
