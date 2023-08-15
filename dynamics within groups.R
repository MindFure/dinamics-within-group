source("C:/Users/works/Documents/type_colomns_data.R")
source("C:/Users/works/Documents/standardize_data.R")
source("C:/Users/works/Documents/conduct_tests.R")
source("C:/Users/works/Documents/format_table_with_na_split.R")
source("C:/Users/works/Documents/check_arguments.R")
source("C:/Users/works/Documents/validate_list_its.R")

library(DescTools)
library(IDPmisc)
library(rlist)
library(flextable)
library(officer)
library(purrr)
library(magrittr)

dynamics <- function(data,PATH,FILENAME,comp.flag=T,list_its,time.points.title=NULL){

  check_arguments(data, PATH, FILENAME, comp.flag, list_its,time.points.title)
  
  validate_list_its(list_its)
  
  #-----------------------------------------------------------------------------
  
  # Проверка на одинаковость длинны списков
  lengths <- sapply(list_its, function(x) length(x$name.title))
  all_lengths_equal <- length(unique(lengths)) == 1
  
  # Проверяем, что значения идентичны  
  titles_identical <- identical(list_its[[1]]$name.title, list_its[[2]]$name.title)
  
  if(all_lengths_equal && titles_identical) {
    
     unique_col_titles <- list_its[[1]]$name.title
     
  }else if (all_lengths_equal && !titles_identical){
  
    all_titles <- unlist(lapply(list_its, function(x) x$name.title))
    unique_col_titles <- unique(all_titles)
  
  }else if(length(unique(lengths)) < length(lengths)){ 
    
  all_titles <- unlist(lapply(list_its, function(x) x$name.title))
  unique_col_titles <- unique(all_titles) 
  
  } else if(length(unique(lengths)) == length(lengths)){
    
  #собиравем name.title со всех списков и делаем их уникальными
  all_titles <- unlist(lapply(list_its, function(x) x$name.title))  
  unique_col_titles <- unique(all_titles)
  
  }
  #----------------------------------------------------------------------------- 
  # Создаем пустой список для хранения результатов
  result_list <- list()
  
  # Создаем пустой список для хранения промежуточных результатов
  temp_results <- list()
  
  # Устанавливаем начальное значение индекса элемента
  item_index <- 1
  
  if(!is.null(time.points.title)){
  new_colnames <- setNames(time.points.title, seq_along(time.points.title))
  }
  
  for(item in list_its){
    
    id_groups <- item$id_groups
    comb_list <- combn(item$id.bin,2,simplify = FALSE)
    comb_list_all <- item$id.bin
    current_index <- item_index
    new_order <- as.character(item$point.time)
    
  for(g in id_groups){
    
    comb_test <- unlist(comb_list)
    print(paste("22",comb_test))
    
    df <- as.matrix(data[data[[item$id.group]] %in% g,])

    if(!is.null(time.points.title)){
    df_order <- data.frame(id.bin = item$id.bin, point.time = item$point.time)
    df_order <- df_order[order(df_order$point.time), ]
    df <- df[, df_order$id.bin]
    colnames(df) <- df_order$point.time
    colnames(df) <- new_colnames[colnames(df)]
    
    df_full <- data.frame(matrix(" ", nrow = nrow(df), ncol = length(time.points.title)))
    colnames(df_full) <- time.points.title
    intersect_cols <- intersect(colnames(df_full), colnames(df)) 
    intersect_cols <- as.character(intersect_cols)
    df_full[,intersect_cols] <- df[,intersect_cols]
    colnames(df_full) <- time.points.title
    df <- as.matrix(df_full)
    
    }
    if(is.null(time.points.title)){
      df_cols <- unique(unlist(item$id.bin))
      df <- df[,df_cols]
      colnames(df) <- item$name.title
      
    df_full <- data.frame(matrix(" ", nrow = nrow(df), ncol = length(unique_col_titles)))
    colnames(df_full) <- unique_col_titles

    intersect_cols <- intersect(colnames(df_full), colnames(df))
    intersect_cols <- as.character(intersect_cols)
    df_full[,intersect_cols] <- df[,intersect_cols]
    colnames(df_full) <- unique_col_titles
    df <- as.matrix(df_full)
    }
    
    col_type_list <- rep(item$type, length(comb_list_all))

    col_type_list <- unlist(col_type_list)
    print(col_type_list)

    common.list <- lapply(1:length(comb_list_all), function(x) list(id = comb_list_all[[x]], type = col_type_list[[x]]))
    print(paste("1111",common.list))



    if(comp.flag==T){
    sr <- cmp.group.all(id.group     = item$id.group,
                  group.labels = id_groups,
                  columns.list = common.list,
                  data         = data,
                  save.file.cmp.2groups    = F,
                  PATH         = path.dir.out,
                  FILENAME     = "FILE",
                  result = NULL)
    rr <- as.matrix(sr[,5])
    rr <- as.vector(rr)

    }
    #
    # #---------------------------------------------------------------------------
    df <- standardize_data(df)
    # #---------------------------------------------------------------------------

    if(comp.flag==T){

      empty_cols <- which(df == " ", arr.ind = TRUE)
      insert_idxs <- unique(empty_cols[,2])
      if(length(insert_idxs)!=0){
      max_idx <- length(rr) + length(insert_idxs)
      new_rr <- character(max_idx)
      rr_idx <- 1
      i <- 1
       for (i in 1:max_idx) {
        if(i %in% insert_idxs) {
          new_rr[i] <- "-"
        } else {
          if(rr_idx <= length(rr)) {
            new_rr[i] <- rr[rr_idx]
            rr_idx <- rr_idx + 1
          }
        }
       }
      if(is.null(time.points.title)){
      rr <- new_rr
      } else{
      #browser()
      rr_order <- new_rr
      idx <- rep(NA, length(rr_order))
      idx[item$point.time] <- 1:length(item$point.time)
      rr_order_new <- rr_order[idx]
      rr_order_new[!is.na(idx)] <- rr_order[item$point.time]
      rr_order_new <- ifelse(is.na(rr_order_new), "-", rr_order_new)
      rr <- rr_order_new 
      }
      }
    }
    
    #---------------------------------------------------------------------------
    results1 <- type_columns_data(df, col_type_list)
    df <- results1$df
    med <- results1$column_stats
    #---------------------------------------------------------------------------
    data_df = as.matrix(data[data[[item$id.group]] %in% g,])
    data_df_naomit = data_df[,unique(unlist(comb_list))]
    df.matrix <- ifelse(is.matrix(data_df_naomit),na.count.string(data_df_naomit[,1]),na.count.string(data_df_naomit[,1]))

    result <- data.frame(matrix(NA,ncol=length(med)+2,nrow=1))
    result[1,] <- c(paste0(g,"\nN=",df.matrix),med,"")
    
    if(comp.flag==T){
    if(g==tail(id_groups, n=1)){
      last_row <- nrow(result)+1
      result <- rbind(result, rep(NA, ncol(result))) 
      result[last_row, 1] <- "Сравнение:"
      rr <- c(rr, rep(NA, ncol(result) - length(rr)-1))
    for(i in 1:length(rr)){
     result[last_row, i+1] <- rr[i] 
     }
    }
    }
    list_z <- unlist(lapply(ncol(df), combn, 2, simplify=FALSE), recursive=FALSE) 
    
    #---------------------------------------------------------------------------
    #browser()
    result <- conduct_tests(data_df,df, col_type_list, med,result,list_z)
    #---------------------------------------------------------------------------
    #browser()
    #result[1, length(med)+2] <- toString(result[,length(med)+2]) 
    #result[1, length(med)+3] <- toString(result[,length(med)+3])
    result[1, length(med)+2] <- paste(result[,length(med)+2], collapse="\n") 
    result[1, length(med)+3] <- paste(result[,length(med)+3], collapse="\n")
    if(g==tail(id_groups, n=1)){
    col1_idx <- ncol(result) - 1
    col2_idx <- ncol(result)
    row_idx <- 2
    result[row_idx, col1_idx] <- NA
    result[row_idx, col2_idx] <- NA
    }
    result <- result[complete.cases(result[ ,1]),]
    last.res <- tail(result,n=1)
    #result <- na.omit(result)
    
    result_list[[g]] <- result[complete.cases(result[ ,1]),]
  }
      

  result_new <- do.call(rbind,result_list)
  hh <- as.numeric(unique(unlist(comb_list)))
  names(result_new) <- c("Группа",unique_col_titles,"Величина эффекта","p-уровень,\nкоррекция")
  
  temp_results[[current_index]] <- result_new
  item_index <- item_index + 1  
  current_index <- current_index + 1
  }
  result_new1 <-do.call(rbind.data.frame, lapply(temp_results, function(x) unname(rbind(names(x), as.matrix(x)))))
  names(result_new1) <- result_new1[1,]
  result_new1 <- result_new1[-1,]
  
  na.list <- list("na", NA, NA, NA, NA)
  while(length(na.list)!=ncol(result_new1)){
    na.list <- list.append(na.list,NA)
  }
  #------------------------------------------------------------------------------------
  col.names = names(result_new1)
  s <- as.data.frame(na.list)
  names(s) <- col.names
  result_new1 <- rbind(s,result_new1)
  
  for(i in 1:nrow(result_new1)){
    if(result_new1[i,1]=="Группа"){
      result_new1[i, ] <- na.list
    }
  }
  col_name <- sapply(comb_list, function(x) {
    col_index <- x[1]  
    col_name <- colnames(data)[col_index]
    return(col_name)
  })
  #browser()
  col.title <- sapply(list_its, get_new_group)
  result_new1[result_new1$'Группа' == "na", "Группа"] <- col.title
  path.data = paste0(PATH, FILENAME,".csv")
  write.table(result_new1,file=path.data,sep=";",dec=",",na="",col.names = T,row.names = F)
  
  #-----------------------------------------------------------------------------
   
  result_new1 <- format_table_with_na_split(result_new1)
  #-----------------------------------------------------------------------------
  
  return(result_new1)
}