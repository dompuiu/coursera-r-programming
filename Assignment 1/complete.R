complete <- function(directory, id = 1:332) {
  files_number = length(id)
  nobs <- rep(0, files_number)
  
  input_files <- paste(directory, "/", sprintf("%03d", id), ".csv", sep="")
  
  for (i in 1:files_number) {
    data_frame = read.csv(input_files[i])
    filtered_data_frame = data_frame[complete.cases(data_frame),]
    
    nobs[i] = nrow(filtered_data_frame)
  }
  
  data.frame(id, nobs)
}