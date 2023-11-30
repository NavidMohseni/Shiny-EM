load_file <- function(name, path, header) {
  ext <- tools::file_ext(name)
  if ("header" %in% header){
    header <- TRUE  
  }
  switch(ext,
         csv = vroom::vroom(path, delim = ",", col_names=header),
         tsv = vroom::vroom(path, delim = "\t", col_names=header),
         validate("Invalid file; Please upload a .csv or .tsv file")
  )
}
