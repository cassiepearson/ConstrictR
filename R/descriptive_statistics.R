# Basic Descriptive Statistics
#
#------------------------------------------------------------------------------
# Description: Calculate the basic descriptive statistics of your dataframe.
# Statistics:  min, max, range, median, mean, variance, and standard deviation
#------------------------------------------------------------------------------
# Required Parameters: df: dataframe
# Optional Parameters: column: Change to false if your table is organized
#                              row-wise
#                      output: changes the output format, options:
#                              dataframe:   df
#                              matrix:      m
#                      file: Writes the output to descriptive_statistics.option:
#                            none: no output file
#                            csv:  csv file
#                            tsv:  tsv file
#                            txt:  two space separated txt file
#                      verbose: Change to true for printing returned results
#------------------------------------------------------------------------------

descriptive_statistics <- function(df, column=TRUE, output=df,
                                   file="none", verbose=FALSE) {

  # Matrix-ize input assuming a column or row organized table
  matrix_input <- as.matrix(df[,-1])

  if(verbose){
    message("Data converted to matrix. Calculating statistics...")
  }

  if(column) {
    # Column-wise input data, apply functions
    calculation_list <- list()
    calculation_list[[1]] <- apply(matrix_input,2,min)
    calculation_list[[2]] <- apply(matrix_input,2,max)
    calculation_list[[3]] <- apply(matrix_input,2,range)
    calculation_list[[4]] <- apply(matrix_input,2,median)
    calculation_list[[5]] <- apply(matrix_input,2,mean)
    calculation_list[[6]] <- apply(matrix_input,2,var)
    calculation_list[[7]] <- apply(matrix_input,2,sd)

    # Create output matrix
    matrix_output <- do.call(rbind, calculation_list)
  } else {
    # Row-wise input data, apply functions
    calculation_list <- list()
    calculation_list[[1]] <- apply(matrix_input,1,min)
    calculation_list[[2]] <- apply(matrix_input,1,max)
    calculation_list[[3]] <- apply(matrix_input,1,range)
    calculation_list[[4]] <- apply(matrix_input,1,median)
    calculation_list[[5]] <- apply(matrix_input,1,mean)
    calculation_list[[6]] <- apply(matrix_input,1,var)
    calculation_list[[7]] <- apply(matrix_input,1,sd)

    # Create output matrix
    matrix_output <- do.call(rbind, calculation_list)
  }

  if(verbose){
    message("Calculations complete. Creating output based on parameters...")
  }

  # Create output data structure of desired type
  switch(output,
    df = output_data <- as.data.frame(matrix_output),
    m = output_data <- matrix_output, # Already in matrix form
  )

  # Create output file
  switch(file,
    "none" = message("No data written to file."),
    "csv" = write.csv(matrix_output, file = "descriptive_statistics.csv",
                      na=""),
    "tsv" = write.table(matrix_output, file = "descriptive_statistics.tsv",
                        na="", sep="\t"),
    "txt" = write.table(matrix_output, file = "descriptive_statistics.txt",
                        na="", sep="  "),
    )

  if(verbose){
    message("Output data structure and file created (if not none). Exiting...")
  }

  if(verbose){
    print(output_data)
  }

  # Return in requested format
  return(output_data)

} # End of descriptive_statistics function

# End of descriptive_statistics.R
