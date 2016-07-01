library(RJSONIO)
library(RCurl)
library(matlab)

get_mapping_options <- function(){
  #returns a vector of endpoints under mapping/expand
  system("curl 'https://gdc-api.nci.nih.gov/cases/_mapping' > mapping_JSON.txt")
  endpoints <- fromJSON("mapping_JSON.txt")["expand"]
  #print(endpoints)
  nodes <- c()
  for (i in 1:length(endpoints$expand)){
    a <- unlist(as.vector(strsplit(endpoints$expand[i],"\\.")))
    nodes <- c(nodes,a[1])
  }
  nodes <- unique(nodes)
  return(nodes)
}

endpoints <- get_mapping_options()

#-----------------------------------------------------------------------------------------------
#-----------------------------------------QUERRY TOOLS-------------------------------------------
#------------------------------------------------------------------------------------------------

download_data <- function(ID,path="",flatten=FALSE){
  #Input: Comma seperated ID file
  #Output: Data files
  if(path == ""){
    system(paste("curl --remote-name --remote-header-name ","'https://gdc-api.nci.nih.gov/data/",ID,"'",sep=""))
  }
  else{
    system(paste("curl --remote-name --remote-header-name ","'https://gdc-api.nci.nih.gov/data/",ID,"' > ",path,"/",ID,"_data",sep=""))
  }
}

getCaseID <- function(file_id){
  #input: comma seperated file ID's (string)
  #returns: vector containing corresponding case ID's
  #output: un-formated text file containing case ID's
  
  file_id <- strsplit(file_id,",")
  file_id <- unlist(file_id)
  system("echo '' > CaseID.txt")
  for (id in file_id){
    system(paste("curl 'https://gdc-api.nci.nih.gov/cases?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22files.file_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%22",id,"%22+%5d%0d%0a+++%7d%0d%0a%7d&pretty=true&fields=case_id' | grep -r case_id >> CaseID.txt",sep=""))
    text = read.table("CaseID.txt", sep=" ")
  }
  return(c(unname(sapply(as.vector(text[length(text)]),as.character))))
}

get_file_id_from_file_name <- function(file_name){
  #input: comma seperated file names (string) (include zip extension)
  #returns: vector containing corresponding file ID's
  #output: un-formated text file containing file ID's
  
  file_name <- strsplit(file_name,",")
  file_name <- unlist(file_name)
  #print(file_name)
  system("echo '' > FileID.txt")
  for (id in file_name){
    system(paste("curl 'https://gdc-api.nci.nih.gov/files?filters=%7b%22op%22%3a+%22%3d%22%2c%0d%0a++++++%22content%22%3a+%7b%0d%0a++++++++++%22field%22%3a+%22file_name%22%2c%0d%0a++++++++++%22value%22%3a+%5b%22",id,"%22%5d%0d%0a++++++%7d%0d%0a%7d&pretty=true' | grep -r file_id >> FileID.txt",sep=""))
    text = read.table("FileID.txt", sep=" ")
  }
  #print(text)
  fileIDs <- ((c(unname(sapply(as.vector(text[length(text)-1]),as.character)))))
  fileIDs <- gsub(",","",fileIDs)
  return(fileIDs)
}

get_mapping_options <- function(){
  #returns a vector of endpoints under mapping/expand
  system("curl 'https://gdc-api.nci.nih.gov/cases/_mapping' > mapping_JSON.txt")
  endpoints <- fromJSON("mapping_JSON.txt")["expand"]
  #print(endpoints)
  nodes <- c()
  for (i in 1:length(endpoints$expand)){
    a <- unlist(as.vector(strsplit(endpoints$expand[i],"\\.")))
    nodes <- c(nodes,a[1])
  }
  nodes <- unique(nodes)
  return(nodes)
}

get_all_mapping_options <- function(){
  #returns a vector of endpoints under mapping/expand
  system("curl 'https://gdc-api.nci.nih.gov/cases/_mapping' > mapping_JSON.txt")
  endpoints <- fromJSON("mapping_JSON.txt")
  print((endpoints)[-1])
  for (row in endpoints){
  }
  nodes <- c()
  for (i in 1:length(endpoints$expand)){
    a <- unlist(as.vector(strsplit(endpoints$expand[i],"\\.")))
    nodes <- c(nodes,a[1])
  }
  nodes <- unique(nodes)
  return(nodes)
}

get_file_names_from_JSON<- function(file){
  #input: JSON file name
  #returns: vector containing file id's
  #output: file containing file names
  
  #Note: To get the json file, after filtering data from 
  #GDC website go to the files tab and press export table
  #to get the file
  
  rawjson<-fromJSON(file)
  flat_json <- unlist(rawjson)
  flat_json <- gsub("\r","",flat_json)
  flat_json <- gsub("\n","",flat_json)
  flat_json <- gsub("\t","",flat_json)
  list_json <- as.list(flat_json)
  file_names<-names(list_json)=="file_name"
  index <- grep("TRUE",as.character(file_names))
  final <- as.character(list_json[index])
  final <- gsub('([^\\]|^)"',"",final)
  #final <- noquote(final)
  #final <- gsub("\"","",final)
  write.table(final,file="file_names_from_json.txt",sep=",",row.names = F,col.names = F,quote= F)
  #print(a)
  return(final)
}

move_files <- function(extension,path){
  system(paste("mv *",extension," ",path,sep=""))
}

flatten_list <- function(some_list){
  flat_list <- unlist(some_list)
  flat_list <- gsub("\r","",flat_list)
  flat_list <- gsub("\n","",flat_list)
  flat_list <- gsub("\t","",flat_list)
}

newline_to_csv <- function(file){
  file <- flatten_list(as.list(scan(file=file, what="character")))
  file <- paste(file,collapse = ",")
  return(file)
}

vector_to_csv <- function(vec){
  write.table(vec,file="id_list.txt",sep=",",row.names = F,col.names = F,quote= F)
  newline_to_csv("id_list.txt")
}

download_data_from_json <- function(json){
  #in: json file
  #out: id_list.txt file containing a list of file ids
  names <- get_file_names_from_JSON(json)
  ids <- get_file_id_from_file_name(vector_to_csv(names))
  download_data(vector_to_csv(ids))
}


#-----------------------------------------------------------------------------------------------
#---------------------------------------GET GDC METADATA----------------------------------------
#-----------------------------------------------------------------------------------------------

#my_rot= use it as yes
get_GDC_metadata <- function(id_list, my_rot="yes", output="file",  order_rows=TRUE,  order_columns=TRUE, verbose=FALSE, debug=FALSE){
  
  # import list of ids
  my_ids <- flatten_list(as.list(scan(file=id_list, what="character")))
  metadata_matrix <- matrix()
  
  for (i in 1:length(my_ids)){
    
    #if(verbose==TRUE){print(paste("Processing sample (", i, ")"))}
    print(paste("Processing sample (", i, ")"))
    
    raw_metadata_vector <- vector()
    unique_metadata_vector <- vector
    
    if(debug==TRUE){print("Made it here (1)")}
    
    #----------------------------------------CHANGED PORTION-------------------------------------
    
    #endpoints <- get_mapping_options() this line is under the library commands inorder to perform it once
    for (endpoint in endpoints){
      raw_metadata_vector <- c(raw_metadata_vector,flatten_list((metadata_cases(my_id=my_ids[i], dict=endpoint))$data))
    }
    
    #--------------------------------------------------------------------------------------------
    
    if(debug==TRUE){print("Made it here (2)")}
    
    for (j in 1:length(unique(names(raw_metadata_vector)))){
      my_name <- unique(names(raw_metadata_vector))[j]
      #print(my_name)
      unique_name_value_pairs <- unique(  raw_metadata_vector[  which( names(raw_metadata_vector) == my_name )  ]  )
      name_list <- vector()
      #names(unique_name_value_pairs) <- rep(my_name, length(unique_name_value_pairs))
      for (k in 1:length(unique_name_value_pairs)){
        name_list <- c(name_list, (paste( my_name, ".", k ,sep="")))
      }
      names(unique_name_value_pairs) <- name_list
      unique_metadata_vector <- c(unique_metadata_vector, unique_name_value_pairs)
    }
    
    if(debug==TRUE){print("Made it here (3)")}
    
    if( i==1 ){ # on first sample, create the data matrix
      metadata_matrix <- matrix(unique_metadata_vector, ncol=1)
      rownames(metadata_matrix) <- names(unique_metadata_vector)
      colnames(metadata_matrix) <- my_ids[i]
      if(debug==TRUE){print("Made it here (3.1)")}
    }else{ # for all additional samples add on to the existing matrix
      if(debug==TRUE){print("Made it here (3.2)")}
      sample_metadata_matrix <- matrix(unique_metadata_vector, ncol=1)
      if(debug==TRUE){print("Made it here (3.3)")}
      rownames(sample_metadata_matrix) <- names(unique_metadata_vector)
      if(debug==TRUE){print("Made it here (3.4)")}
      colnames(sample_metadata_matrix) <- my_ids[i]
      if(debug==TRUE){
        print("Made it here (3.5)")
        matrix1 <<- metadata_matrix
        matrix2 <<- sample_metadata_matrix
      }
      if(debug==TRUE){print("Made it here (3.6)")}
      # place merge code here
      
      
      # Note - merging changes class of metadata_matrix from "matrix" to "data frame"; it's converted back below
      metadata_matrix <- combine_matrices_by_column(matrix1=metadata_matrix, matrix2=sample_metadata_matrix, func_order_rows=order_rows, func_order_columns=order_columns, func_debug=debug)
      if(debug==TRUE){
        print("Made it here (3.7)")
        print(paste("DATA_CLASS:", class(metadata_matrix)))
      }
    }
    
    
    
  }
  
  #ADDITION ------------------------------------------------------------------------------
  metadata_matrix <- metadata_matrix[-1,]
  
  # covert data product from data.frame back to matrix
  metadata_matrix <- as.matrix(metadata_matrix)
  
  if(debug==TRUE){print("Made it here (4)")}
  
  # rotate the matrix if that option is selected
  if( identical(my_rot, "yes")==TRUE ){
    metadata_matrix <- rot90(rot90(rot90(metadata_matrix)))
  }
  
  if(debug==TRUE){print("Made it here (5)")}
  
  # output the matrix as a flat file (default) or return as matrix
  # create name for the output file        
  if( identical(output, "file")==TRUE ){
    date_tag <- gsub(" ", "_", date())
    date_tag <- gsub("__", "_", date_tag)
    date_tag <- gsub(":", "-", date_tag)
    #output_name =paste(id_list, ".", date_tag, ".GDC_METADATA.txt", sep="")
    output_name = gsub(" ", "", paste(id_list,".GDC_METADATA.txt"))
    if(debug==TRUE){ print(paste("FILE_OUT: ", output_name)) }
    export_data(metadata_matrix, output_name)
  }else{
    return(metadata_matrix)
  }
  if(debug==TRUE){print("Made it here (6)")}
  
}
############################
############################
############################                                                                                           


############################
### ### ### SUBS ### ### ###
############################
metadata_cases <- function(before_id="https://gdc-api.nci.nih.gov/cases?filters=%7b%0d%0a+++%22op%22+%3a+%22%3d%22+%2c%0d%0a+++%22content%22+%3a+%7b%0d%0a+++++++%22field%22+%3a+%22files.file_id%22+%2c%0d%0a+++++++%22value%22+%3a+%5b+%22",
                           after_id="%22+%5d%0d%0a+++%7d%0d%0a%7d&pretty=true&fields=case_id&expand=",
                           my_id="07218202-2cd3-4db1-93e7-071879e36f27", 
                           dict="")
{ 
  my_call <- gsub(" ", "", paste(before_id, my_id, after_id, dict))
  my_call.json <- fromJSON(getURL(my_call))
  #print(my_call.json)
  return(my_call.json)
  #my_call.list <- flatten_list(my_call.json)
  #print(my_call.list)
}



export_data <- function(data_object, file_name){
  write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
}



flatten_list <- function(some_list){
  flat_list <- unlist(some_list)
  flat_list <- gsub("\r","",flat_list)
  flat_list <- gsub("\n","",flat_list)
  flat_list <- gsub("\t","",flat_list)
}



combine_matrices_by_column <- function(matrix1, matrix2, func_order_rows=TRUE, func_order_columns=TRUE, func_debug=debug){
  
  # perform the merge
  comb_matrix<- merge(data.frame(matrix1), data.frame(matrix2), by="row.names", all=TRUE,check.names = F)
  if(func_debug==TRUE){
    print("Made it here (3.6.1)")
    print(paste("MATRIX_1", dim(matrix1)))
    print(paste("MATRIX_2",dim(matrix2)))
    print(paste("MATRIX_C",dim(comb_matrix)))
    #print(colnames(matrix1))
    #print(colnames(matrix2))
    matrix4 <- comb_matrix
  }
  
  # undo garbage formatting that merge introduces
  rownames(comb_matrix) <- comb_matrix$Row.names
  comb_matrix$Row.names <- NULL
  
  #matrix4 <<- comb_matrix
  
  #MATRIX4 IS CREATED BUT NOT USED, COMB MATRIX IS FIXED (WRONG ONE)
  
  #if(func_debug==TRUE){print(paste("MATRIX DIM:", dim(comb_matrix)))}
  colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
  #if(func_debug==TRUE){print("Made it here (3.6.2)")}
  #print(c(colnames(matrix1), colnames(matrix2)))
  # order columns
  if( func_order_rows==TRUE){
    ordered_rownames <- order(rownames(comb_matrix))
    comb_matrix <- comb_matrix[ordered_rownames,]
  }
  #if(func_debug==TRUE){print("Made it here (3.6.3)")}
  
  # order rows
  if( func_order_columns==TRUE){
    ordered_colnames <- order(colnames(comb_matrix))
    comb_matrix <- comb_matrix[,ordered_colnames]
  }
  #print(colnames(comb_matrix))
  #if(func_debug==TRUE){print("Made it here (3.6.4)")}
  
  #if(func_debug==TRUE){ matrix5 <<- comb_matrix }
  matrix4 <- comb_matrix
  return(comb_matrix)
}



combine_matrices_by_row <- function(matrix1, matrix2, pseudo_fudge=10000, func_order_rows=TRUE, func_order_columns=TRUE, func_debug=debug){
  
  # perform the merge
  comb_matrix<- merge(matrix1, matrix2, by="col.names", all=TRUE)
  
  # undo garbage formatting that merge introduces
  rownames(comb_matrix) <- comb_matrix$Col.names
  comb_matrix$Col.names <- NULL
  colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
  
  # order columns
  if( func_order_rows==TRUE){
    ordered_rownames <- order(rownames(comb_matrix))
    comb_matrix <- comb_matrix[ordered_rownames,]
  }
  
  # order rows
  if( func_order_columns==TRUE){
    ordered_colnames <- order(colnames(comb_matrix))
    comb_matrix <- comb_matrix[,ordered_colnames]
  }
  
  return(comb_matrix)
}

#-----------------------------------------------------------------------------------------------
#------------------------------------------MERGE DATA-----------------------------------------
#-----------------------------------------------------------------------------------------------

GDC_raw_count_merge <- function( id_list="my_id_list", my_rot="no", pseudo_fudge=NA, order_rows=TRUE,  order_columns=TRUE, debug=FALSE, verbose=FALSE, remove_tag="")
  
{                       
  ### MAIN ###
  ###### load the neccessary packages
  if ( is.element("matlab", installed.packages()[,1]) == FALSE ){ install.packages("matlab") }    
  library(matlab)
  
  if(debug==TRUE){print("Made it here 1")}
  
  my_ids <- flatten_list(as.list(scan(file=id_list, what="character")))
  
  if(debug==TRUE){print("Made it here 2")}
  
  # read through the files and build out the data matrix
  for ( i in 1:length(my_ids) ){
    print(paste("Processing sample (", i, ")", "of", "[", length(my_ids),"]"))
    if( i==1 ){ # on first sample, create the data matrix
      if(verbose==TRUE){print(paste("Processing sample (", i, ")"))}
      my_data_matrix <- data.matrix(read.table(file=my_ids[i], row.names=1, header=FALSE, sep="\t", comment.char="", quote="", check.names=FALSE))
      colnames(my_data_matrix) <- my_ids[i]
    }else{ # for all additional samples add on to the existing matrix
      if(verbose==TRUE){print(paste("Processing sample (", i, ")"))}
      my_sample_matrix  <- data.matrix(read.table(file=my_ids[i], row.names=1, header=FALSE, sep="\t", comment.char="", quote="", check.names=FALSE))
      colnames(my_sample_matrix) <- my_ids[i]
      my_data_matrix <- combine_matrices_by_column2(my_data_matrix, my_sample_matrix)
    }
  }
  
  if(debug==TRUE){print("Made it here 4")}
  
  # remove tag from colnames
  colnames(my_data_matrix) <- gsub(remove_tag, "", colnames(my_data_matrix))
  
  # substitute for missing counts - NA by default
  if( is.na(pseudo_fudge)==TRUE ){}else{
    pseudo_count <- min(my_data_matrix, na.rm=TRUE)/pseudo_fudge # find the min real value; that num/pseudo_fudge = pseudo_count value
    comb_matrix[is.na(my_data_matrix)] <- pseudo_count # replace NA with pseudo_count
  }
  
  # remvove rows that contain summary statistics (These start with a "_")
  remove_index <- c()
  for (i in 1:length(rownames(my_data_matrix))){  if( length(grep(pattern='^_', x=rownames(my_data_matrix)[i])) > 0){ remove_index <- c(remove_index, i) } }
  my_data_matrix <- my_data_matrix[-remove_index,]
  
  # rotate the matrix if that option is selected
  if( identical(my_rot, "yes")==TRUE ){
    my_data_matrix <- rot90(rot90(rot90(my_data_matrix)))
  }
  
  if(debug==TRUE){print("Made it here 5")}
  
  # output the matrix as a flat file
  fileout_name <- gsub(" ", "", paste(id_list, ".merged_data.txt"))
  export_data(my_data_matrix, fileout_name)
  
}



### SUBS ###                    
import_data <- function(file_name){
  data.matrix(read.table(file_name, row.names=NA, header=TRUE, sep="\t", comment.char="", quote="", check.names=FALSE))
}



export_data <- function(data_object, file_name){
  write.table(data_object, file=file_name, sep="\t", col.names = NA, row.names = TRUE, quote = FALSE, eol="\n")
}


combine_matrices_by_column2 <- function(matrix1, matrix2, order_rows=TRUE, order_columns=TRUE){
  
  # perform the merge
  comb_matrix<- merge(matrix1, matrix2, by="row.names", all=TRUE)
  
  # undo garbage formatting that merge introduces
  rownames(comb_matrix) <- comb_matrix$Row.names
  comb_matrix$Row.names <- NULL
  colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
  
  # order columns
  if( order_rows==TRUE){
    ordered_rownames <- order(rownames(comb_matrix))
    comb_matrix <- comb_matrix[ordered_rownames,]
  }
  
  # order rows
  if( order_columns==TRUE){
   ordered_colnames <- order(colnames(comb_matrix))
   comb_matrix <- comb_matrix[,ordered_colnames]
  }
  
  return(comb_matrix)
}




combine_matrices_by_row <- function(matrix1, matrix2, pseudo_fudge=10000, order_rows=TRUE, order_columns=TRUE){
  
  # perform the merge
  comb_matrix<- merge(matrix1, matrix2, by="col.names", all=TRUE)
  
  # undo garbage formatting that merge introduces
  rownames(comb_matrix) <- comb_matrix$Col.names
  comb_matrix$Col.names <- NULL
  colnames(comb_matrix) <- c(colnames(matrix1), colnames(matrix2))
  
  # order columns
  if( order_rows==TRUE){
    ordered_rownames <- order(rownames(comb_matrix))
    comb_matrix <- comb_matrix[ordered_rownames,]
  }
  
  # order rows
  if( order_columns==TRUE){
    ordered_colnames <- order(colnames(comb_matrix))
    comb_matrix <- comb_matrix[,ordered_colnames]
  }
  
  return(comb_matrix)
}

#-----------------------------------------------------------------------------------------------
#--------------------------------------CALCULATE PCOA-------------------------------------------
#-----------------------------------------------------------------------------------------------

calculate_pco <- function(
  file_in,
  input_dir = "./",
  input_type = "file",
  output_PCoA_dir = "./",
  print_dist = 1,
  output_DIST_dir = "./",
  dist_method = "euclidean",
  headers = 1,
  debug=FALSE
)
  
{
  # load packages
  if ( is.element("matlab", installed.packages()[,1]) == FALSE ){ install.packages("matlab") }
  if ( is.element("ecodist", installed.packages()[,1]) == FALSE ){ install.packages("ecodist") }  
  library(matlab)      
  library(ecodist)
  #suppressPackageStartupMessages(library(Cairo))
  #suppressPackageStartupMessages(library(gplots))
  
  # define sub functions
  func_usage <- function() {
    writeLines("
               You supplied no arguments
               
               DESCRIPTION: (calculate_pco.r):
               This script will perform a PCoA analysis on the inputdata
               using the selected distance metric.  Output always produces a
               *.PCoA file that has the normalized eigenvalues (top n lines)
               and eigenvectors (bottom n x m matris, n lines) where n is the
               number of variables (e.g.subsystems), and m the number of
               samples. You can also choose to produce *.DIST files that contain
               the distance matrix used to generate the PCoA.
               
               USAGE: plot_pca(
               file_in = no default arg                               # (string)  input data file
               input_type = \"file\"                                   # (string) file or r_matrix
               input_dir = \"./\"                                       # (string)  directory(path) of input
               output_PCoA_dir = \"./\"                                 # (string)  directory(path) for output PCoA file
               print_dist = 0                                         # (boolean) print the DIST file (distance matrix)
               output_DIST_dir = \"./\"                                 # (string)  directory(path) for output DIST file 
               dist_method = \"bray-curtis\"                            # (string)  distance/dissimilarity metric,
               (choose from one of the following options)
               \"euclidean\" | \"maximum\"     | \"canberra\"    |
               \"binary\"    | \"minkowski\"   | \"bray-curtis\" |
               \"jacccard\"  | \"mahalanobis\" | \"sorensen\"    |
               \"difference\"| \"manhattan\"
               headers = 0                                            # (booealan) print headers in output PCoA file 
               )\n"
               )
    stop("calculate_pco stopped\n\n")
  }
  
  find_dist <- function(my_data, dist_method)
  {
    switch(dist_method,
           "euclidean" = dist(my_data, method = "euclidean"), 
           "maximum" = dist(my_data, method = "maximum"),
           "manhattan" = dist(my_data, method = "manhattan"),
           "canberra" = dist(my_data, method = "canberra"),
           "binary" = dist(my_data, method = "binary"),
           "minkowski" = dist(my_data, method = "minkowski"),
           
           #"bray-curtis" = distance(my_data, method = "bray-curtis"), # could not handle large data 1-12-12
           
           "bray-curtis" = bcdist(my_data), # 1-12-12
           #"bray-curtis" = vegdist(my_data, method="bray"), # 1-12-12
           #"bray-curtis" = designdist(my_data, method = "(A+B-2*J)/(A+B)") # 1-12-12
           
           "jaccard" = distance(my_data, method = "jaccard"),
           "mahalanobis" = distance(my_data, method = "mahalanobis"),
           "sorensen" = distance(my_data, method = "sorensen"),
           "difference" = distance(my_data, method = "difference")
           # unifrac
           # weighted_unifrac
           
           # distance methods with {stats}dist: dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
           #      euclidean maximum manhattan canberra binary minkowski
           
           # distance methods with {ecodist}distance: distance(x, method = "euclidean")
           #      euclidean bray-curtis manhattan mahalanobis jaccard "simple difference" sorensen
           
    )
  }
  
  
  # stop and give the usage if the proper number of arguments is not given
  if ( nargs() == 0 ){
    func_usage()
  }
  
  # load data
  
  #writeLines("FILE-IN")
  #writeLines(file_in)
  #input_data_path = gsub(" ", "", paste(input_dir, file_in))
  #writeLines("INPUT-DATA-PATH")
  #writeLines(input_data_path)
  #my_data <<- flipud(rot90(data.matrix(read.table(input_data_path, row.names=1, header=TRUE, sep="\t", comment.char="", quote="")))) # edited on 12-14-12, stop character conversions in column names
  
  if( debug==TRUE ){print("MADE IT HERE 1")}
  
  if ( identical(input_type, "file") ){
    if( debug==TRUE ){print("MADE IT HERE 2a")}
    input_data_path = gsub(" ", "", paste(input_dir, file_in))
    my_data <<- flipud(rot90(data.matrix(read.table(input_data_path, row.names=1, check.names=FALSE, header=TRUE, sep="\t", comment.char="", quote=""))))
  } else if ( identical(input_type, "r_matrix") ) {
    if( debug==TRUE ){print("MADE IT HERE 2.b")}
    my_data <<- flipud(rot90(file_in))
  } else {
    if( debug==TRUE ){print("MADE IT HERE 2.c")}
    stop("input_type value is not valid, must be file or r_matrix")
  }
  
  if( debug==TRUE ){print("MADE IT HERE 3")}
  
  
  num_data_rows = dim(my_data)[1] # substitute 0 for NA's if they exist in the data
  num_data_cols = dim(my_data)[2]
  for (row_num in (1:num_data_rows)){
    for (col_num in (1:num_data_cols)){
      #my_data[row_num, col_num] = as.integer(my_data[row_num, col_num]) # added 1-12-12 to fix "Error in vector("double", length) : vector size cannot be NA ...
      if (is.na(my_data[row_num, col_num])){
        my_data[row_num, col_num] <<- 0
      }
    }
  }
  
  if( debug==TRUE ){print("MADE IT HERE 4")}
  
  # a bit for naming outputs
  if( identical(input_type, "r_matrix") ){
    file_in.name <- deparse(substitute(file_in))
  } else {
    file_in.name <- file_in
  }
  
  # calculate distance matrix
  dist_matrix <<- find_dist(my_data, dist_method)
  DIST_file_out <- gsub(" ", "", paste(output_DIST_dir, file_in.name, ".", dist_method, ".DIST"))
  if (print_dist > 0) { write_file(file_name = DIST_file_out, data = data.matrix(dist_matrix)) }
  
  if( debug==TRUE ){print("MADE IT HERE 5")}
  
  # perform the pco
  my_pco <<- pco(dist_matrix)
  
  if( debug==TRUE ){print("MADE IT HERE 6")}
  
  # scale eigen values from 0 to 1, and label them
  eigen_values <<- my_pco$values
  scaled_eigen_values <<- (eigen_values/sum(eigen_values))
  for (i in (1:dim(as.matrix(scaled_eigen_values))[1])) {names(scaled_eigen_values)[i]<<-gsub(" ", "", paste("PCO", i))}
  scaled_eigen_values <<- data.matrix(scaled_eigen_values)
  #for (i in (1:dim(as.matrix(scaled_ev))[1])) dimnames(scaled_ev)[i]<<-gsub(" ", "", paste("PCO", i))
  
  if( debug==TRUE ){print("MADE IT HERE 7")}
  
  # label the eigen vectors
  eigen_vectors <<- data.matrix(my_pco$vectors) 
  dimnames(eigen_vectors)[[1]] <<- dimnames(my_data)[[1]]
  
  if( debug==TRUE ){print("MADE IT HERE 8")}
  
  # write eigen values and then eigen vectors to file_out
  PCoA_file_out = gsub(" ", "", paste(output_PCoA_dir, file_in.name, ".", dist_method, ".PCoA"))
  
  if ( headers == 1 ){
    write(file = PCoA_file_out, paste("# file_in    :", file_in.name,
                                      "\n# dist_method:", dist_method,
                                      "\n#________________________________",
                                      "\n# EIGEN VALUES (scaled 0 to 1) >",
                                      "\n#________________________________"),
          append=FALSE)
    write.table(scaled_eigen_values, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = TRUE, sep="\t", eol="\n")
  }else{
    write.table(scaled_eigen_values, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = FALSE, sep="\t", eol="\n")
  }
  
  if( debug==TRUE ){print("MADE IT HERE 9")}
  
  if ( headers == 1 ){
    write(file = PCoA_file_out, paste("#________________________________",
                                      "\n# EIGEN VECTORS >",
                                      "\n#________________________________"),
          append=TRUE)
  }
  
  if( debug==TRUE ){print("MADE IT HERE 10")}
  
  #write.table(eigen_vectors, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = TRUE, sep="\t")
  write.table(eigen_vectors, file=PCoA_file_out, col.names=FALSE, row.names=TRUE, append = TRUE, sep="\t", eol="\n")
  
  if( debug==TRUE ){print("MADE IT HERE 11")}
  
}


write_file <- function(file_name, data) {
  write.table(data, file=file_name, col.names=NA, row.names=TRUE, append = FALSE, sep="\t", quote = FALSE, eol="\n")
}


#-----------------------------------------------------------------------------------------------
#------------------------------------RENDER CALCULATED PCOA-------------------------------------
#-----------------------------------------------------------------------------------------------

# This script uses matR to generate 2 or 3 dimmensional pcoas

# table_in is the abundance array as tab text -- columns are samples(metagenomes) rows are taxa or functions
# color_table and pch_table are tab tables, with each row as a metagenome, each column as a metadata 
# grouping/coloring. These tables are used to define colors and point shapes for the plot
# It is assumed that the order of samples (left to right) in table_in is the same
# as the order (top to bottom) in color_table and pch_table

# basic operation is to produce a color-less pcoa of the input data

# user can also input a table to specify colors
# This table can contain colors (as hex or nominal) or can contain metadata
# This is a PCoA plotting functions that can handle a number of different scenarios
# It always requires a *.PCoA file (like that produce by AMETHST/plot_pco.r)
# It can handle metadata as a table - producing plots for all or selected metadata columns (metadata used to generate colors automatically)
# It can handle an amthst groups file as metadata (metadata used to generate colors automatically)
# It can handle a list of colors - using them to pain the points directly
# It can handle the case when there is no metadata - painting all of points the same
# users can also specify a pch table to control the shape of plotted icons (this feature may not be ready yet)
# CHANGE OVER v12 - you can select which samples (rows in the eigen vector matrix) with "plot_samples" variable

render_calcualted_pcoa <- function(
  PCoA_in="", # annotation abundance table (raw or normalized values)
  image_out="default",
  figure_main ="principal coordinates",
  components=c(1,2,3), # R formated string telling which coordinates to plot, and how many (2 or 3 coordinates)
  plot_samples="all", # "all" or a vector of samples (rows) to plot
  label_points=FALSE, # default is off
  metadata_table=NA, # matrix that contains colors or metadata that can be used to generate colors
  metadata_column_index=1, # column of the color matrix to color the pcoa (colors for the points in the matrix) -- rows = samples, columns = colorings
  amethst_groups=NA,        
  color_list=NA, # use explicit list of colors - trumps table if both are supplied
  pch_behavior="default", #  "default" use pch_default for all; "auto" automatically assign pch from table; "asis" use integer values in the column
  pch_default=16,
  pch_table="default",
  pch_column=1,
  pch_labels="default",
  image_width_in=22,
  image_height_in=17,
  image_res_dpi=300,
  width_legend = 0.2, # fraction of width used by legend
  width_figure = 0.8, # fraction of width used by figure
  title_cex = "default", # cex for the title of title of the figure, "default" for auto scaling
  legend_cex = "default", # cex for the legend, default for auto scaling
  figure_cex = 2, # cex for the figure
  figure_symbol_cex=2,
  vert_line="dotted", # "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"
  bar_cex = "default",
  bar_vert_adjust = 0,  
  use_all_metadata_columns=FALSE, # option to overide color_column -- if true, plots are generate for all of the metadata columns
  debug=FALSE
)

{
  
  if ( is.element("scatterplot3d", installed.packages()[,1]) == FALSE ){ install.packages("scatterplot3d") }
  if ( is.element("matR", installed.packages()[,1]) == FALSE ){
    install.packages("devtools")
    library(devtools)
    install_github(repo="MG-RAST/matR", dependencies=FALSE, ref="early-release")
    library(matR)
    dependencies()
  }  
  library(matR)
  library(scatterplot3d)
  
  argument_list <- is.na(c(metadata_table,amethst_groups,color_list)) # check that incompatible options were not selected
  argument_test = length(argument_list[argument_list==TRUE])
  
  if(debug==TRUE){print(paste("argument_test:", argument_test))}
  if(debug==TRUE){print(paste("length argument_test == TRUE:", length(argument_test==TRUE)))}
  
  #if ( 3 - length(subset(argument_test, argument_test==TRUE) ) > 1){
  if ( argument_test != 2){
    stop(
      paste(
        "\n\nOnly one of these can have a non NA value:\n",
        "     metadata_table: ", metadata_table,"\n",
        "     amethst_groups: ", amethst_groups, "\n",
        "     color_list    : ", color_list, "\n\n",
        sep="", collapse=""
      )
    )
  }
  
  
  ######################
  ######## MAIN ########
  ######################
  # load data - everything is sorted by id
  my_data <- load_pcoa_data(PCoA_in) # import PCoA data from *.PCoA file --- this is always done
  
  # load data - everything is sorted by id
  eigen_values <- my_data$eigen_values
  eigen_vectors <- my_data$eigen_vectors
  # get the sample names for ordering data, colors, and pch later
  
  sample_names <- rownames(eigen_vectors)
  
  sample_names <- gsub("\"", "", sample_names)
  
  #print(sample_names)
  #CHANGE ---------------------------------------------------------------------------
  #----------------------------------------------------------------------------------
  print(fileIDs)
  
  rownames(eigen_vectors) <- fileIDs
  sample_names <- rownames(eigen_vectors)
  
  #rownames(eigen_vectors) <- get_file_id_from_file_name(vector_to_csv(sample_names))
  #sample_names <- get_file_id_from_file_name(vector_to_csv(sample_names))
  #sample_names <- rownames(eigen_vectors)
  print(rownames(eigen_vectors))
  #print(sample_names)
  
  #print(get_file_id_from_file_name(vector_to_csv(sample_names)))
  #print(sample_names)
  # make sure everything is sorted by id
  if(debug==TRUE){sample_names.test1<<-sample_names}
  
  if(debug==TRUE){sample_names.test2<<-sample_names}
  ##eigen_vectors <- eigen_vectors[ order(sample_names), ]
  ##eigen_values <- eigen_values[ order(sample_names) ]
  ##sample_names <- sample_names[ order(sample_names) ]
  #eigen_vectors <- eigen_vectors[ order(eigen_values), ]
  #eigen_values <- eigen_values[ order(eigen_values) ]
  #sample_names <- sample_names[ order(eigen_values) ]
  
  if(debug==TRUE){
    eigen_vectors.test<<-eigen_vectors
    eigen_values.test<<-eigen_values  
  }
  
  #eigen_vectors <- eigen_vectors[ order(rownames(my_data$eigen_vectors)), ]
  #eigen_values <- eigen_values[ order(rownames(my_data$eigen_vectors)) ] # order will reflect id
  
  num_samples <- ncol(my_data$eigen_vectors)
  
  
  #if ( debug == TRUE ){ print(paste("num_samples: ", num_samples)) } 
  
  if(debug==TRUE){print("made it here 1")}
  
  # CHECK FOR LEVELS OF PCH AS FACTOR _ DEFINE TWO TYPES OF LEGENDS
  # load pch - handles table or integer(pch_default)
  
  
  
  
  
  # somwhere here logic for three pch options
  #  pch_behavior = c("default", "auto", "asis")
  
  
  if(debug==TRUE){print("ABOUT TO LOAD PCH")}
  pch_object <- load_pch(pch_behavior, pch_default, pch_table, pch_column, pch_labels, sample_names, num_samples, rownames(my_data$eigen_vectors), debug) 
  
  if(debug==TRUE){ pch_object.test <<- pch_object}
  #return(list( "plot_pch_values"=plot_pch_values, "pch.levels"=pch_levels, "pch.labels"=pch_labels) )
  
  plot_pch <- pch_object$plot_pch_vector
  pch_levels <- pch_object$pch_levels
  pch_labels <- pch_object$pch_labels
  
  if(debug==TRUE){
    print(paste("first_data_sample:", sample_names[1]))
    print(paste("last_data_sample :", sample_names[length(sample_names)]))
    print(paste("first_pch_sample :", names(plot_pch)[1]))
    print(paste("first_pch_sample :", names(plot_pch)[length(plot_pch)]))
    print(paste("first_pch_value  :", (plot_pch)[1]))
    print(paste("last_pch_value  :", (plot_pch)[length(plot_pch)]))
  }
  
  
  if(debug==TRUE){print("made it here 2")}
  
  #if(debug==TRUE){print(paste("main.pch_levels", pch_levels))}
  #if(debug==TRUE){print(paste("main.pch_labels", pch_labels))}
  
  
  
  if(debug==TRUE){print(paste("2.pch_levels", pch_levels))}
  if(debug==TRUE){print(paste("2.pch_labels", pch_labels))}
  
  
  #####################################################################################
  ########## PLOT WITH NO METADATA OR COLORS SPECIFIED (all point same color) #########
  #####################################################################################
  if ( argument_test==3 ){ # create names for the output files
    
    if(debug==TRUE){print("Rendering without metadata")}
    
    if ( identical(image_out, "default") ){
      image_out = paste( PCoA_in,".NO_COLOR.PCoA.png", sep="", collapse="" )
      figure_main = paste( PCoA_in, ".NO_COLOR.PCoA", sep="", collapse="" )
    }else{
      image_out = paste(image_out, ".png", sep="", collapse="")
      figure_main = paste( image_out,".PCoA", sep="", collapse="")
    }
    
    column_levels <- "data" # assign necessary defaults for plotting
    num_levels <- 1
    color_levels <- 1
    ncol.color_matrix <- 1
    pcoa_colors <- "black"   
    
    create_plot( # generate the plot
      PCoA_in,
      ncol.color_matrix,
      eigen_values, eigen_vectors, components, plot_samples,
      column_levels, num_levels, color_levels, pcoa_colors, plot_pch, pch_labels, pch_levels,
      image_out,figure_main,
      image_width_in, image_height_in, image_res_dpi,
      width_legend, width_figure,
      title_cex, legend_cex, figure_cex, figure_symbol_cex, bar_cex, bar_vert_adjust, label_points, vert_line, debug
    )
  }
  #####################################################################################
  #####################################################################################
  
  
  
  #####################################################################################
  ########### PLOT WITH AMETHST GROUPS (colors generated by load_metadata) ############
  #####################################################################################
  if ( identical( is.na(amethst_groups), FALSE ) ){ # create names for the output files
    
    if(debug==TRUE){print("Rendering with amethst_groups")}
    
    if ( identical(image_out, "default") ){
      image_out = paste( PCoA_in,".AMETHST_GROUPS.PCoA.png", sep="", collapse="" )
      figure_main = paste( PCoA_in, ".AMETHST_GROUPS.PCoA", sep="", collapse="" )
    }else{
      image_out = paste(image_out, ".png", sep="", collapse="")
      figure_main = paste( image_out,".PCoA", sep="", collapse="")
    }
    
    con_grp <- file(amethst_groups) # get metadata and generate colors from amethst groups file
    open(con_grp)
    line_count <- 1
    groups.list <- vector(mode="character")
    while ( length(my_line <- readLines(con_grp,n = 1, warn = FALSE)) > 0) {
      new_line <- my_line
      split_line <- unlist(strsplit(my_line, split=","))
      split_line.list <- rep(line_count, length(split_line))
      names(split_line.list) <- split_line
      groups.list <- c(groups.list, split_line.list)
      line_count <- line_count + 1
    }
    close(con_grp)
    if ( length(groups.list) != length(unique(names(groups.list))) ){
      stop("One or more groups have redundant entries - this is not allowed for coloring the PCoA")
    }
    metadata_column <- matrix(groups.list, ncol=1)
    
    suppressWarnings( numericCheck <- as.numeric(metadata_column) ) # check to see if metadata are numeric, and sort accordingly
    if( is.na(numericCheck[1])==FALSE ){
      column_name = colnames(metadata_column)[1]
      row_names = rownames(metadata_column)
      metadata_column <- matrix(numericCheck, ncol=1)
      colnames(metadata_column) <- column_name
      rownames(metadata_column) <- row_names
    }
    #sample_names
    
    metadata_column <- metadata_column[ sample_names,,drop=FALSE ] # order the metadata by sample 1d
    #metadata_column <- metadata_column[ order(rownames(metadata_column)),,drop=FALSE ] # order the metadata by value
    color_column <- create_colors(metadata_column, color_mode = "auto")
    
    column_levels <- levels(as.factor(as.matrix(metadata_column))) 
    num_levels <- length(column_levels)
    color_levels <- col.wheel(num_levels)
    ncol.color_matrix <- 1
    
    colnames(metadata_column) <- "amethst_metadata"
    column_levels <- column_levels[ order(column_levels) ] # NEW (order by levels values)
    color_levels <- color_levels[ order(column_levels) ] # NEW (order by levels values)
    
    pcoa_colors <- as.character(color_column[,1]) # convert colors to a list after they've been used to sort the eigen vectors
    
    create_plot( # generate the plot
      PCoA_in,
      ncol.color_matrix,
      eigen_values, eigen_vectors, components, plot_samples,
      column_levels, num_levels, color_levels, pcoa_colors, plot_pch, pch_labels, pch_levels,
      image_out,figure_main,
      image_width_in, image_height_in, image_res_dpi,
      width_legend, width_figure,
      title_cex, legend_cex, figure_cex, figure_symbol_cex, bar_cex, bar_vert_adjust, label_points, vert_line, debug
    )
    
  }
  #####################################################################################
  #####################################################################################
  
  if(debug==TRUE){print("made it here 3")}
  
  if(debug==TRUE){print(paste("3.pch_levels", pch_levels))}
  if(debug==TRUE){print(paste("3.pch_labels", pch_labels))}
  
  #####################################################################################
  ############ PLOT WITH LIST OF COLORS (colors generated by load_metadata) ###########
  #####################################################################################
  if ( identical( is.na(color_list), FALSE ) ){ # create names for the output files
    
    if(debug==TRUE){print("Rendering with color_list")}
    
    if ( identical(image_out, "default") ){
      image_out = paste( PCoA_in,".color_List.PCoA.png", sep="", collapse="" )
      figure_main = paste( PCoA_in, ".color_list.PCoA", sep="", collapse="" )
    }else{
      image_out = paste(image_out, ".png", sep="", collapse="")
      figure_main = paste( image_out,".PCoA", sep="", collapse="")
    }
    
    column_levels <- levels(as.factor(as.matrix(color_list))) # get colors directly from list of colors
    num_levels <- length(column_levels)
    color_levels <- col.wheel(num_levels)
    #color_levels <- col.wheel(num_levels)
    ncol.color_matrix <- 1
    pcoa_colors <- color_list
    
    create_plot( # generate the plot
      PCoA_in,
      ncol.color_matrix,
      eigen_values, eigen_vectors, components, plot_samples,
      column_levels, num_levels, color_levels, pcoa_colors, plot_pch, pch_labels, pch_levels,
      image_out,figure_main,
      image_width_in, image_height_in, image_res_dpi,
      width_legend, width_figure,
      title_cex, legend_cex, figure_cex, figure_symbol_cex, bar_cex, bar_vert_adjust, label_points, vert_line, debug
    )
  }
  #####################################################################################
  #####################################################################################
  
  
  if(debug==TRUE){print("made it here 4")}
  
  if(debug==TRUE){print(paste("4.pch_levels", pch_levels))}
  if(debug==TRUE){print(paste("bels", pch_labels))}
  
  #####################################################################################
  ########### PLOT WITH METADATA_TABLE (colors produced from color_matrix) ############
  ######## CAN HANDLE PLOTTING ALL OR A SINGLE SELECTED METADATA TABLE COLUMN #########
  #####################################################################################
  if ( identical( is.na(metadata_table), FALSE ) ){
    
    metadata_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
      read.table(
        file=metadata_table,row.names=1,header=TRUE,sep="\t",
        colClasses = "character", check.names=FALSE,
        comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
      )
    )   
    #metadata_matrix <- metadata_matrix[ order(rownames(metadata_matrix)),,drop=FALSE ]  # make sure that the metadata matrix is sorted (ROWWISE) by id
    
    
    
    
    #PROBLEM HERE!! SAMPLE_NAMES IS FILE NAMES, NOT FILE IDS -----------------------------------
    #---------------------------------------------------------------------------------------------
    
    #print(metadata_matrix["03aee74e-4e37-4a58-a720-c90e807d2f40",])
    #print(sample_names)
    
    metadata_matrix <- metadata_matrix[ sample_names,,drop=FALSE ]  # make sure that the metadata matrix is sorted (ROWWISE) by id
    if(debug==TRUE){print("made it here 5")}
    
    if ( use_all_metadata_columns==TRUE ){ # AUTOGENERATE PLOTS FOR ALL COLUMNS IN THE METADATA FILE - ONE PLOT PER METADATA COLUMN
      
      if(debug==TRUE){print("Rendering with metadata_matrix, all columns")}
      
      ncol.color_matrix <- ncol( metadata_matrix) # get then number of columns in the metadata data file = number of plots
      for (i in 1:ncol.color_matrix){ # loop to process through all columns
        
        if(debug==TRUE){print("made it here 6")}
        
        metadata_column <- metadata_matrix[ ,i,drop=FALSE ] # get column i from the metadata matrix
        if(debug==TRUE){ test1<<-metadata_column }
        
        if(debug==TRUE){print("made it here 7")}
        if(debug==TRUE){print(paste("7.pch_levels", pch_levels))}
        if(debug==TRUE){print(paste("7.pch_labels", pch_labels))}
        
        
        image_out = paste(PCoA_in,".", colnames(metadata_column), ".pcoa.png", sep="", collapse="") # generate name for plot file
        figure_main = paste( PCoA_in,".", colnames(metadata_column),".PCoA", sep="", collapse="") # generate title for the plot
        
        suppressWarnings( numericCheck <- as.numeric(metadata_column) ) # check to see if metadata are numeric, and sort accordingly
        if( is.na(numericCheck[1])==FALSE ){
          column_name = colnames(metadata_column)[1]
          row_names = rownames(metadata_column)
          metadata_column <- matrix(numericCheck, ncol=1)
          colnames(metadata_column) <- column_name
          rownames(metadata_column) <- row_names
        }
        
        if(debug==TRUE){print("made it here 8")}
        if(debug==TRUE){print(paste("8.pch_levels", pch_levels))}
        if(debug==TRUE){print(paste("8.pch_labels", pch_labels))}
        
        
        
        if(debug==TRUE){ test2<<-metadata_column }
        metadata_column <- metadata_column[ sample_names,,drop=FALSE ] # order the metadata by value
        #metadata_column <- metadata_column[ order(rownames(metadata_column)),,drop=FALSE ] # order the metadata by value
        if(debug==TRUE){ test3<<-metadata_column }
        
        color_column <- create_colors(metadata_column, color_mode = "auto") # set parameters for plotting
        ncol.color_matrix <- 1 
        column_factors <- as.factor(metadata_column) 
        column_levels <- levels(as.factor(metadata_column))
        num_levels <- length(column_levels)
        color_levels <- col.wheel(num_levels)
        
        rownames(eigen_vectors) <- gsub("\"", "", rownames(eigen_vectors)) # make sure that vectors are sorted identically to the colors
        
        #WORK IN PROGRESS ---------------------------------------------------------------
        #-----------------------------------------------------------------------------
        #print statements doesnt work
        print(rownames(color_column))
        print("aa")
        #print(eigen_vectors)
        #print("tygiuhoij")
        eigen_vectors <- eigen_vectors[ rownames(color_column), ]
        
        if(debug==TRUE){print("made it here 9")}
        if(debug==TRUE){print(paste("9.pch_levels", pch_levels))}
        if(debug==TRUE){print(paste("9.pch_labels", pch_labels))}
        
        
        #plot_pch <- plot_pch[ rownames(color_column) ]# make sure pch is sorted identically to colors
        
        pcoa_colors <- as.character(color_column[,1]) # convert colors to a list after they've been used to sort the eigen vectors
        
        if(debug==TRUE){
          test.color_column <<- color_column
          test.pcoa_colors <<- pcoa_colors
        }
        
        
        if(debug==TRUE){print("made it here 10")}
        if(debug==TRUE){print(paste("10.pch_levels", pch_levels))}
        if(debug==TRUE){print(paste("10.pch_labels", pch_labels))}
        
        
        
        create_plot( # generate the plot
          PCoA_in,
          ncol.color_matrix,
          eigen_values, eigen_vectors, components, plot_samples,
          column_levels, num_levels, color_levels, pcoa_colors, plot_pch, pch_labels, pch_levels,
          image_out,figure_main,
          image_width_in, image_height_in, image_res_dpi,
          width_legend, width_figure,
          title_cex, legend_cex, figure_cex, figure_symbol_cex, bar_cex, bar_vert_adjust, label_points, vert_line, debug
        )        
      }
      
      
    }else if ( use_all_metadata_columns==FALSE ){ # ONLY CREATE A PLOT FOR THE SELECTED COLUMN IN THE METADATA FILE
      
      if(debug==TRUE){print("Rendering with metadata_matrix, single column")}
      
      metadata_column <- metadata_matrix[ ,metadata_column_index, drop=FALSE ] # get column i from the metadata matrix
      
      # name single extracted column correctly
      if(debug==TRUE){test.metadata_matrix <<- metadata_matrix}
      
      metadata_column_names <- colnames(metadata_matrix)
      if(debug==TRUE){print("LINE 430")}
      
      
      if(debug==TRUE){print(paste("metadata_column_index:", metadata_column_index))}
      if( is.integer(metadata_column_index)==TRUE){
        column_name <- colnames(metadata_matrix)[metadata_column_index]
        if(debug==TRUE){print(paste("column_name  (is int):", column_name))}  
      }else{
        column_name <- metadata_column_index
        if(debug==TRUE){print(paste("column_name (not int):", column_name))}
      }
      
      if(debug==TRUE){ test.metadata_column <<- metadata_column }
      
      if ( identical(image_out, "default") ){
        image_out = paste(PCoA_in,".", column_name, ".pcoa.png", sep="", collapse="") # generate name for plot file
        figure_main = paste( PCoA_in,".", column_name,".PCoA", sep="", collapse="") # generate title for the plot
        #image_out = paste(PCoA_in,".", colnames(metadata_column), ".pcoa.png", sep="", collapse="") # generate name for plot file
        #figure_main = paste( PCoA_in,".", colnames(metadata_column),".PCoA", sep="", collapse="") # generate title for the plot
      }else{
        image_out = paste(image_out, ".png", sep="", collapse="")
        figure_main = paste( image_out,".PCoA", sep="", collapse="")
      }
      
      suppressWarnings( numericCheck <- as.numeric(metadata_column) ) # check to see if metadata are numeric, and sort accordingly
      if( is.na(numericCheck[1])==FALSE ){
        #column_name = colnames(metadata_column)[1]
        row_names = rownames(metadata_column)
        metadata_column <- matrix(numericCheck, ncol=1)
        colnames(metadata_column) <- column_name
        rownames(metadata_column) <- row_names
      }
      
      if(debug==TRUE){ test2<<-metadata_column }
      
      #metadata_column <- metadata_column[ order(metadata_column),,drop=FALSE ] # order the metadata by value
      metadata_column <- metadata_column[ sample_names,,drop=FALSE ] # order the metadata by value
      if(debug==TRUE){ test3<<-metadata_column }
      
      color_column <- create_colors(metadata_column, color_mode = "auto") # set parameters for plotting
      ncol.color_matrix <- 1 
      column_factors <- as.factor(metadata_column) 
      column_levels <- levels(as.factor(metadata_column))
      num_levels <- length(column_levels)
      color_levels <- col.wheel(num_levels)
      rownames(eigen_vectors) <- gsub("\"", "", rownames(eigen_vectors)) # make sure that vectors are sorted identically to the colors
      eigen_vectors <- eigen_vectors[ rownames(color_column), ]        
      pcoa_colors <- as.character(color_column[,1]) # convert colors to a list after they've been used to sort the eigen vectors
      create_plot( # generate the plot
        PCoA_in,
        ncol.color_matrix,
        eigen_values, eigen_vectors, components, plot_samples,
        column_levels, num_levels, color_levels, pcoa_colors, plot_pch, pch_labels, pch_levels,
        image_out,figure_main,
        image_width_in, image_height_in, image_res_dpi,
        width_legend, width_figure,
        title_cex, legend_cex, figure_cex, figure_symbol_cex, bar_cex, bar_vert_adjust, label_points, vert_line, debug
      )
      
    }else{
      stop(paste("invalid value for use_all_metadata_columns(", use_all_metadata_columns,") was specified, please try again", sep="", collapse=""))
    }
  }
  
}
#####################################################################################

######################
###### END MAIN ######
######################

######################
######## SUBS ########
######################

#######################
######## SUB(1): Function to import the data from a pre-calculated PCoA
######################
load_pcoa_data <- function(PCoA_in){
  
  #print("loading PCoA")
  
  con_1 <- file(PCoA_in)
  con_2 <- file(PCoA_in)
  # read through the first time to get the number of samples
  open(con_1);
  num_values <- 0
  data_type = "NA"
  while ( length(my_line <- readLines(con_1,n = 1, warn = FALSE)) > 0) {
    if ( length( grep("PCO", my_line) ) == 1  ){
      num_values <- num_values + 1
    }
  }
  close(con_1)
  # create object for values
  eigen_values <- matrix("", num_values, 1)
  dimnames(eigen_values)[[1]] <- 1:num_values
  eigen_vectors <- matrix("", num_values, num_values)
  dimnames(eigen_vectors)[[1]] <- 1:num_values
  # read through a second time to populate the R objects
  value_index <- 1
  vector_index <- 1
  open(con_2)
  current.line <- 1
  data_type = "NA"
  while ( length(my_line <- readLines(con_2,n = 1, warn = FALSE)) > 0) {
    if ( length( grep("#", my_line) ) == 1  ){
      if ( length( grep("EIGEN VALUES", my_line) ) == 1  ){
        data_type="eigen_values"
      } else if ( length( grep("EIGEN VECTORS", my_line) ) == 1 ){
        data_type="eigen_vectors"
      }
    }else{
      split_line <- noquote(strsplit(my_line, split="\t"))
      if ( identical(data_type, "eigen_values")==TRUE ){
        dimnames(eigen_values)[[1]][value_index] <- noquote(split_line[[1]][1])
        eigen_values[value_index,1] <- noquote(split_line[[1]][2])       
        value_index <- value_index + 1
      }
      if ( identical(data_type, "eigen_vectors")==TRUE ){
        dimnames(eigen_vectors)[[1]][vector_index] <- noquote(split_line[[1]][1])
        for (i in 2:(num_values+1)){
          eigen_vectors[vector_index, (i-1)] <- as.numeric(noquote(split_line[[1]][i]))
        }
        vector_index <- vector_index + 1
      }
    }
  }
  close(con_2)
  # finish labeling of data objects
  dimnames(eigen_values)[[2]] <- "EigenValues"
  dimnames(eigen_vectors)[[2]] <- dimnames(eigen_values)[[1]]
  class(eigen_values) <- "numeric"
  class(eigen_vectors) <- "numeric"
  # write imported data to global objects
  #eigen_values <<- eigen_values
  #eigen_vectors <<- eigen_vectors
  return(list(eigen_values=eigen_values, eigen_vectors=eigen_vectors))
  
}

######################
# SUB(3): Function to import the pch information for the points # load pch matrix if one is specified
######################
load_pch <- function(pch_behavior, pch_default, pch_table, pch_column, pch_labels, sample_names, num_samples, my_names, debug){
  
  
  #pch_behavior=c("default", "auto", "asis")
  #my_names <- gsub("\"", "", my_names)
  
  if(debug==TRUE){print(rep("LOADING PCH",50))}
  
  if(debug==TRUE){print(paste("class(my_names): ", class(my_names), sep=""))}
  
  if(debug==TRUE){print(paste("(preloop) PCH_BEHAVIOR: ", pch_behavior))}
  
  if( identical(pch_behavior,"default") ){
    
    if(debug==TRUE){print(paste("(in loop) PCH_BEHAVIOR: ", pch_behavior))}
    
    my_names <- gsub("\"", "", my_names)
    pch_matrix <- data.matrix(matrix(rep(pch_default, num_samples), ncol=1))
    #pch_matrix <- pch_matrix[order(rownames(pch_matrix)),]
    #pch_matrix <- pch_matrix[ order(sample_names), ]
    plot_pch <- pch_matrix[ , 1, drop=FALSE]
    plot_pch_vector <- as.vector(plot_pch)
    pch_labels <- levels(as.factor(plot_pch_vector))
    #names(plot_pch_vector) <- my_names
    pch_levels <- pch_labels
    pch_labels <- pch_labels
    if(debug==TRUE){
      print(paste("plot_pch_vector: ", class(plot_pch_vector)))
      print(plot_pch_vector)
      plot_pch_vector.test <<- plot_pch_vector
    }
    
  }else if ( identical(pch_behavior,"asis") ){
    
    if(debug==TRUE){print(paste("(in loop) PCH_BEHAVIOR: ", pch_behavior))}
    
    pch_matrix <- data.matrix(read.table(pch_table, row.names=1, header=TRUE, sep="\t", comment.char="", quote="", check.names=FALSE))
    
    #my_names <- gsub("\"", "", my_names)
    
    
    #pch_matrix <- pch_matrix[ order(rownames(pch_matrix)), ]
    if(debug==TRUE){pch_matrix.test1<<-pch_matrix}
    pch_matrix <- pch_matrix[ sample_names, ]
    if(debug==TRUE){pch_matrix.test2<<-pch_matrix}
    plot_pch <- pch_matrix[ , pch_column, drop=FALSE ]
    #plot_pch <- pch_matrix[ order(pch_column), drop=FALSE]
    plot_pch_vector <- as.vector(plot_pch)
    names(plot_pch_vector) <- sample_names
    pch_levels <- levels(as.factor(plot_pch_vector))
    if(debug==TRUE){pch_levels.test<<-pch_levels}
    #pch_labels <- pch_labels
    if(debug==TRUE){pch_labels.test<<-pch_labels}
    if(debug==TRUE){plot_pch_vector.test<<-plot_pch_vector}
    if(debug==TRUE){print(paste("ASIS.pch_labels", pch_labels))}
    if(debug==TRUE){print(paste("ASIS.pch_levels", pch_levels))}
    
    if( length(pch_levels)!=length(pch_labels) ){ stop("you have (", length(pch_labels), ") labels in pch_labels for (", length(pch_levels),") unique factor levels") }
    
    if(debug==TRUE){
      print(paste("plot_pch_vector class: ", class(plot_pch_vector)))
      print(paste("plot_pch_vector: ",plot_pch_vector))
      plot_pch_vector.test <<- plot_pch_vector
    }
    
  }else if( identical(pch_behavior, "auto") ){
    
    if(debug==TRUE){print(paste("(in loop) PCH_BEHAVIOR: ", pch_behavior))}
    
    # pch_matrix <- data.matrix(read.table(pch_table, row.names=1, header=TRUE, sep="\t", comment.char="", quote="", check.names=FALSE))
    
    pch_matrix <- as.matrix( # Load the metadata table (same if you use one or all columns)
      read.table(
        file=pch_table,row.names=1,header=TRUE,sep="\t",
        colClasses = "character", check.names=FALSE,
        comment.char = "",quote="",fill=TRUE,blank.lines.skip=FALSE
      )
    )   
    
    if(debug==TRUE){ pch_matrix.test <<- pch_matrix }
    pch_temp <- create_pch(pch_matrix, pch_column, sample_names, debug)
    plot_pch_vector <- as.vector(pch_temp$my_pch)
    pch_levels <- pch_temp$pch_levels
    pch_labels <- names(pch_levels)  
    
  }else{
    stop(paste("( ",pch_behavior, " )", "is an invalid pch_behavior option value - try \"default\", \"asis\", or \"auto\""))
  }
  
  if( length(plot_pch_vector) != num_samples ){
    stop(paste("The number of samples in pch column ( ", length(plot_pch), " ) does not match number of samples ( ", num_samples, " )"))
  }
  
  return(list( "plot_pch_vector"=plot_pch_vector, "pch_levels"=pch_levels, "pch_labels"=pch_labels) )
}
######################
######################


######################
# SUB(4): Sub to provide scaling for title and legened cex
######################
calculate_cex <- function(my_labels, my_pin, my_mai, reduce_by=0.30, debug){
  
  # get figure width and height from pin
  my_width <- my_pin[1]
  my_height <- my_pin[2]
  
  # get margine from mai
  my_margin_bottom <- my_mai[1]
  my_margin_left <- my_mai[2]
  my_margin_top <- my_mai[3]
  my_margin_right <- my_mai[4]
  
  #if(debug==TRUE){
  #  print(paste("my_pin: ", my_pin, sep=""))
  #  print(paste("my_mai: ", my_mai, sep=""))
  #}
  
  # find the longest label (in inches), and figure out the maximum amount of length scaling that is possible
  label_width_max <- 0
  for (i in 1:length(my_labels)){  
    label_width <- strwidth(my_labels[i],'inches')
    if ( label_width > label_width_max){ label_width_max<-label_width  }
  }
  label_width_scale_max <- ( my_width - ( my_margin_right + my_margin_left ) )/label_width_max
  ## if(debug==TRUE){ 
  ##                 cat(paste("\n", "my_width: ", my_width, "\n", 
  ##                           "label_width_max: ", label_width_max, "\n",
  ##                           "label_width_scale_max: ", label_width_scale_max, "\n",
  ##                           sep=""))  
  ##                 }
  
  
  # find the number of labels, and figure out the maximum height scaling that is possible
  label_height_max <- 0
  for (i in 1:length(my_labels)){  
    label_height <- strheight(my_labels[i],'inches')
    if ( label_height > label_height_max){ label_height_max<-label_height  }
  }
  adjusted.label_height_max <- ( label_height_max + label_height_max*0.4 ) # fudge factor for vertical space between legend entries
  label_height_scale_max <- ( my_height - ( my_margin_top + my_margin_bottom ) ) / ( adjusted.label_height_max*length(my_labels) )
  ## if(debug==TRUE){ 
  ##                 cat(paste("\n", "my_height: ", my_height, "\n", 
  ##                           "label_height_max: ", label_height_max, "\n", 
  ##                           "length(my_labels): ", length(my_labels), "\n",
  ##                           "label_height_scale_max: ", label_height_scale_max, "\n",
  ##                           sep="" )) 
  ##                 }
  
  # max possible scale is the smaller of the two 
  scale_max <- min(label_width_scale_max, label_height_scale_max)
  # adjust by buffer
  #scale_max <- scale_max*(100-buffer/100) 
  adjusted_scale_max <- ( scale_max * (1-reduce_by) )
  #if(debug==TRUE){ print(cat("\n", "adjusted_scale_max: ", adjusted_scale_max, "\n", sep=""))  }
  return(adjusted_scale_max)
  
}

######################
######################

######################
# SUB(5): Fetch par values of the current frame - use to scale cex
######################
par_fetch <- function(){
  my_pin<-par('pin')
  my_mai<-par('mai')
  my_mar<-par('mar')
  return(list("my_pin"=my_pin, "my_mai"=my_mai, "my_mar"=my_mar))    
}
######################
######################





######################
# SUB(6): Workhorse function that creates the plot
######################
create_plot <- function(
  PCoA_in,
  ncol.color_matrix,
  eigen_values, eigen_vectors, components, plot_samples,
  column_levels, num_levels, color_levels, pcoa_colors, plot_pch, pch_labels, pch_levels,
  image_out,figure_main,
  image_width_in, image_height_in, image_res_dpi,
  width_legend, width_figure,
  title_cex, legend_cex, figure_cex, figure_symbol_cex, bar_cex, bar_vert_adjust, label_points, vert_line, debug
){
  
  if(debug==TRUE){print("creating figure")}
  
  png( # initialize the png 
    filename = image_out,
    width = image_width_in,
    height = image_height_in,
    res = image_res_dpi,
    units = 'in'
  )
  
  # LAYOUT CREATION HAS TO BE DICTATED BY PCH TO A DEGREE _ NUM LEVELS (1 or more)
  # Determine num levels for pch
  num_pch <- length(levels(as.factor(plot_pch)))
  # CREATE THE LAYOUT
  if ( num_pch > 1 ){
    my_layout <- layout( matrix(c(1,1,2,3,4,3,5,5), 4, 2, byrow=TRUE ), widths=c(0.5,0.5), heights=c(0.1,0.8,0.3,0.1) )
  }else{
    my_layout <- layout(  matrix(c(1,1,2,3,4,4), 3, 2, byrow=TRUE ), widths=c(width_legend,width_figure), heights=c(0.1,0.8,0.1) )
    # requires an extra plot.new() to skip over pch legend (frame 4 or none )
  }
  # my_layout <- layout(  matrix(c(1,1,2,3,4,3,5,5), 4, 2, byrow=TRUE ), widths=c(width_legend,width_figure), heights=c(0.1,0.4,0.8,0.4,0.1) ) # for auto pch legend
  layout.show(my_layout)
  
  # PLOT THE TITLE (layout frame 1)
  par( mai = c(0,0,0,0) )
  par( oma = c(0,0,0,0) )
  plot.new()
  if ( identical(title_cex, "default") ){ # automatically scale cex for the legend
    if(debug==TRUE){print("autoscaling the title cex")}
    title_par <- par_fetch()
    title_cex <- calculate_cex(figure_main, title_par$my_pin, title_par$my_mai, reduce_by=0.10)
  }
  text(x=0.5, y=0.5, figure_main, cex=title_cex)
  
  # PLOT THE LEGEND (layout frame 2)
  plot.new()
  if ( identical(legend_cex, "default") ){ # automatically scale cex for the legend
    if(debug==TRUE){print("autoscaling the legend cex")}
    legend_par <- par_fetch()
    legend_cex <- calculate_cex(column_levels, legend_par$my_pin, legend_par$my_mai, reduce_by=0.40)
  }
  legend( x="center", y="center", legend=column_levels, pch=15, col=color_levels, cex=legend_cex)
  
  # PLOT THE PCoA FIGURE (layout frame 3)
  # set par options (Most of the code in this section is copied/adapted from Dan Braithwaite's pco plotting in matR)
  
  #par(op)
  par <- list ()
  #par$mar <- par()['mar']
  #par$oma <- par()['oma']
  #par$mar <- c(4,4,4,4)
  #par$mar <- par(op)['mar']
  #par$oma <- par(op)['oma']
  #par$oma <- c(1,1,1,1)
  #par$mai <- c(1,1,1,1)
  par$main <- ""#figure_main
  #par$labels <- if (length (names (x)) != 0) names (x) else samples (x)
  if ( label_points==TRUE ){
    #par$labels <-  rownames(eigen_vectors)
    if ( identical(plot_samples, "all") ){
      par$labels <-  rownames(eigen_vectors)
    }else{
      par$labels <- plot_samples
    }
  } else {
    par$labels <- NA
  }
  
  
  
  
  
  #if (length (groups (x)) != 0) par$labels <- paste (par$labels, " (", groups (x), ")", sep = "")
  par [c ("xlab", "ylab", if (length (components) == 3) "zlab" else NULL)] <- paste ("PC", components, ", R^2 = ", format (eigen_values [components], dig = 3), sep = "")
  #col <- if (length (groups (x)) != 0) groups (x) else factor (rep (1, length (samples (x))))
  #levels (col) <- colors() [sample (length (colors()), nlevels (col))]
  #g <- as.character (col)
  #par$pch <- 19
  par$cex <- figure_cex
  #par$oma <- c(1,1,1,1)
  #par$mai <- c(1,1,1,1)
  # main plot paramters - create the 2d or 3d plot
  i <- eigen_vectors [ ,components [1]]
  j <- eigen_vectors [ ,components [2]]
  k <- if (length (components) == 3) eigen_vectors [ ,components [3]] else NULL
  
  # only use selected samples if that option is selected
  if ( identical(plot_samples, "all") ){}else{
    names(i)<-rownames(eigen_vectors)
    names(j)<-rownames(eigen_vectors)
    names(k)<-rownames(eigen_vectors)
    i <- i[ plot_samples ]
    j <- j[ plot_samples ]
    k <- if (length (components) == 3) k[ plot_samples ] else NULL
  }
  if(debug==TRUE){ii<<-i; jj<<-j}
  
  if (is.null (k)) {
    #par$col <- col
    
    par$cex <- figure_cex
    
    #if( debug==TRUE ){ print(paste("class "
    #par$col <- pcoa_colors ####<--------------
    if ( identical(plot_samples, "all") ){
      par$col <- pcoa_colors
    }else{
      names(pcoa_colors) <- rownames(eigen_vectors)
      par$col <- pcoa_colors[ plot_samples ]
    }
    if( debug==TRUE ){ print(paste("length par$col:", length(par$col))) }
    if( debug==TRUE ){ print(paste("par$col:", par$col)) }
    
    #if(debug==TRUE){print(paste("func_pch: ",plot_pch, sep="")}
    #par$pch <- plot_pch
    if ( identical(plot_samples, "all") ){
      par$pch <- plot_pch
    }else{
      names(plot_pch) <- rownames(eigen_vectors)
      par$pch <- plot_pch[ plot_samples ]
    }
    if( debug==TRUE ){ print(paste("length par$pch:", length(par$pch))) }
    if( debug==TRUE ){ print(paste("par$pch:", par$pch)) }
    
    #par$cex.symbols <- figure_symbol_cex
    #par <- resolveMerge (list (...), par)
    xcall (plot, x = i, y = j, with = par, without = "labels")
    xcall (points, x = i, y = j, with = par, without = "labels")
    grid ()
  } else {
    # parameter "color" has to be specially handled.
    # "points" above wants "col", scatterplot3d wants "color", and we
    # want the user not to worry about it...
    # par$color <- col
    #par$cex <- figure_cex
    
    
    #par$color <- pcoa_colors
    if ( identical(plot_samples, "all") ){
      par$color <- pcoa_colors
    }else{
      names(pcoa_colors) <- rownames(eigen_vectors)
      par$color <- pcoa_colors[ plot_samples ]
    }
    if( debug==TRUE ){ print(paste("length par$color:", length(par$color))) }
    if( debug==TRUE ){ print(paste("par$color:", par$color)) }
    
    #if(debug==TRUE){print(paste("func_pch: ",plot_pch, sep="")}
    
    #par$pch <- plot_pch
    if ( identical(plot_samples, "all") ){
      par$pch <- plot_pch
    }else{
      names(plot_pch) <- rownames(eigen_vectors)
      par$pch <- plot_pch[ plot_samples ]
    }
    if( debug==TRUE ){ print(paste("length par$pch:", length(par$pch))) }
    if( debug==TRUE ){ print(paste("par$pch:", par$pch)) }
    
    par$cex.symbols <- figure_symbol_cex
    par$type <- "h"
    par$lty.hplot <- vert_line
    par$axis <- TRUE
    par$box <- FALSE
    #par <- resolveMerge (list (...), par)
    reqPack ("scatterplot3d")
    xys <- xcall (scatterplot3d, x = i, y = j, z = k, with = par,
                  without = c ("cex", "labels")) $ xyz.convert (i, j, k)
    #without = c ("labels")) $ xyz.convert (i, j, k)
    i <- xys$x ; j <- xys$y
  }
  text (x = i, y = j, labels = par$labels, pos = 4, cex = par$cex)
  #invisible (P)
  #})
  
  # PCH LEGEND (4 or doesn't exist) ############ <-
  
  if (num_pch>1){
    #par( mai = c(0,0,0,0) )
    #par( oma = c(0,0,0,0) )
    plot.new()
    par_legend_par <- par_fetch()
    par_legend_cex <- calculate_cex(column_levels, par_legend_par$my_pin, par_legend_par$my_mai, reduce_by=0.40)
    #my_pch_levels <<- as.integer(levels(as.factor(plot_pch)))
    
    
    if(debug==TRUE){print("made it here 11")}
    if(debug==TRUE){print(paste("11.pch_levels", pch_levels))}
    if(debug==TRUE){print(paste("11.pch_labels", pch_labels))}
    
    #if( identical(pch_behavior, "default") ){
    #  pch_legend_text <- rep("pch",num_pch)
    #}else{
    #pch_legend_text<-pch_labels[ order(pch_labels) ]
    #  pch_legend_text <- pch_labels
    #  if ( length(pch_legend_text)!=num_pch ){
    #    stop(paste("length(pch_legend_text) (", length(pch_legend_text), ") and num of unique pch entries (", num_pch,") is not the same."))
    #  }
    #}
    
    
    #pch_legend_pch <- as.integer(levels(as.factor(plot_pch)))
    #ordered_pch_legend_pch <- pch_legend_pch[ order(pch_legend_pch) ]
    pch_levels <- as(pch_levels, "numeric")
    legend( x="center", y="center", legend=pch_labels, pch=pch_levels, cex=par_legend_cex, pt.cex=par_legend_cex)
    #legend( x="center", y="center", legend=pch_legend_text, pch=ordered_pch_legend_pch, cex=par_legend_cex, pt.cex=par_legend_cex)
    #legend( x="center", legend="TEST", cex=par_legend_cex, pt.cex=par_legend_cex)
  }
  
  # PLOT THE COLOR BAR (frame 4 or 5)
  #par( mar = c(2,2,2,2) )
  #par( oma = c(1,1,1,1) )
  bar_x <- 1:num_levels
  bar_y <- 1
  bar_z <- matrix(1:num_levels, ncol=1)
  image(x=bar_x,y=bar_y,z=bar_z,col=color_levels,axes=FALSE,xlab="",ylab="")
  loc <- par("usr")
  if( identical(bar_cex,"default") ){
    bar_texts <- paste(column_levels[1], column_levels[num_levels])
    bar_par <- par_fetch()
    bar_cex <- calculate_cex(bar_texts, bar_par$my_pin, bar_par$my_mai, reduce_by=0.10)
  }
  text(loc[1], (loc[1]+bar_vert_adjust), column_levels[1], pos = 4, xpd = T, cex=bar_cex, adj=c(0,0))
  #1 3
  
  text(loc[2], (loc[1]+bar_vert_adjust), column_levels[num_levels], pos = 2, xpd = T, cex=bar_cex, adj=c(0,0))
  
  #text(loc[2], (loc[1]+bar_vert_adjust), paste(column_levels[num_levels],":1",sep=""), pos = 2, xpd = T, cex=bar_cex, adj=c(0,0))
  #text(loc[2], (loc[2]+bar_vert_adjust), paste(column_levels[num_levels],":2",sep=""), pos = 2, xpd = T, cex=bar_cex, adj=c(0,0))
  #text(loc[2], (loc[3]+bar_vert_adjust), paste(column_levels[num_levels],":3",sep=""), pos = 2, xpd = T, cex=bar_cex, adj=c(0,0))
  #text(loc[2], (loc[4]+bar_vert_adjust), paste(column_levels[num_levels],":4",sep=""), pos = 2, xpd = T, cex=bar_cex, adj=c(0,0))
  
  #text(loc[1], loc[2], column_levels[1], pos = 4, xpd = T, cex=bar_cex, adj=c(0,0))
  #text(loc[2], loc[2], column_levels[num_levels], pos = 2, xpd = T, cex=bar_cex, adj=c(0,1))
  
  graphics.off()
}
######################
######################


######################
# SUB(7): Handle partially formatted metadata to produce colors for a single column in a metadata table
######################
## column_color <- function( color_matrix, my_color_mode="auto", my_column ){
##   ncol.color_matrix <<- ncol(color_matrix)
##   plot_colors.matrix <<- create_colors(color_matrix, color_mode=my_color_mode)
##   column_factors <<- as.factor(color_matrix[,my_column])
##   column_levels <<- levels(as.factor(color_matrix[,my_column]))
##   num_levels <<- length(column_levels)
##   color_levels <<- col.wheel(num_levels)
##   pcoa_colors <<- plot_colors.matrix[,my_column]
## }
######################
######################


######################
# SUB(8): Create optimal contrast color selection using a color wheel
# adapted from https://stat.ethz.ch/pipermail/r-help/2002-May/022037.html 
######################
col.wheel <- function(num_col, my_cex=0.75) {
  cols <- rainbow(num_col)
  col_names <- vector(mode="list", length=num_col)
  for (i in 1:num_col){
    col_names[i] <- getColorTable(cols[i])
  }
  cols
}
######################
######################


######################
# SUB(9): The inverse function to col2rgb()
# adapted from https://stat.ethz.ch/pipermail/r-help/2002-May/022037.html
######################
rgb2col <- function(rgb) {
  rgb <- as.integer(rgb)
  class(rgb) <- "hexmode"
  rgb <- as.character(rgb)
  rgb <- matrix(rgb, nrow=3)
  paste("#", apply(rgb, MARGIN=2, FUN=paste, collapse=""), sep="")
}
######################
######################


######################
# SUB(10): Convert all colors into format "#rrggbb"
# adapted from https://stat.ethz.ch/pipermail/r-help/2002-May/022037.html
######################
getColorTable <- function(col) {
  rgb <- col2rgb(col);
  col <- rgb2col(rgb);
  sort(unique(col))
}
######################
######################


######################
# SUB(11): Automtically generate colors from metadata with identical text or values
######################
create_colors <- function(metadata_column, color_mode = "auto"){ # function to     
  my_data.color <- data.frame(metadata_column)
  #ids <- rownames(metadata_column)
  #color_categories <- colnames(metadata_column)
  #for ( i in 1:dim(metadata_matrix)[2] ){
  column_factors <- as.factor(metadata_column[,1])
  column_levels <- levels(as.factor(metadata_column[,1]))
  num_levels <- length(column_levels)
  color_levels <- col.wheel(num_levels)
  levels(column_factors) <- color_levels
  my_data.color[,1]<-as.character(column_factors)
  #}
  
  
  return(my_data.color)
}
######################
######################


######################
# SUB(12): Automtically generate pch from metadata with identical text or values
######################
create_pch <- function(metadata_table, metadata_column, sample_names, debug){ # function to     
  #return(list("my_pch"=my_pch, "pch_levels"=pch_levels, "pch_levels_text"=pch_levels_text))
  
  
  #pch_matrix <- data.matrix(metadata_table)
  if(debug==TRUE){metadata_table.test <<- metadata_table}
  
  plot_pch <- metadata_table[ sample_names, metadata_column, drop=FALSE ]
  plot_pch_vector <- as.vector(plot_pch)
  
  if(debug==TRUE){plot_pch_vector.test <<- plot_pch_vector}
  
  names(plot_pch_vector) <- sample_names
  pch_labels <- levels(as.factor(plot_pch_vector))
  
  num_labels <- length(pch_labels)
  
  if( num_labels>25 ){ stop("too many pch levels - must be 25 or less") }
  
  pch_levels <- 1:num_labels
  names(pch_levels) <- pch_labels
  if(debug==TRUE){ pch_levels.test <<- pch_levels }
  
  my_pch <- integer()
  for (i in 1:nrow(plot_pch)){
    my_pch <- c(my_pch, pch_levels[ as.character( plot_pch[i,metadata_column] ) ])
    if(debug==TRUE){ print(paste("my_pch: ", my_pch)) }
  }
  
  if(debug==TRUE){ my_pch.test <<- my_pch; pch_levels.test <<- pch_levels}
  
  return(list("my_pch"=my_pch, "pch_levels"=pch_levels))
}
