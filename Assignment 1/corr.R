corr <- function(directory, threshold = 0) {
  data <- c()
  input_files <- paste(directory, "/", list.files(directory), sep="")
  
  for (file in input_files) {
    data_frame = read.csv(file)
    filtered_data_frame = data_frame[complete.cases(data_frame),]
    
    if (nrow(filtered_data_frame) > threshold) {
      correlation_value = cor(filtered_data_frame[["nitrate"]], filtered_data_frame[["sulfate"]])
      data = c(data, correlation_value)
    }
  }
  
  data
}