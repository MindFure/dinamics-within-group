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

dynamics <- function(data,PATH,FILENAME,comp.flag=T,indicators.description,time.points.title=NULL){

  check_arguments(data, PATH, FILENAME, comp.flag, indicators.description,time.points.title)
  
  validate_indicators.description(indicators.description)
  
  #-----------------------------------------------------------------------------
  
  # Проверка на одинаковость длинны списков
  lengths <- sapply(indicators.description, function(x) length(x$time.point.names))
  all_lengths_equal <- length(unique(lengths)) == 1
  
  # Проверяем, что значения идентичны  
  titles_identical <- identical(indicators.description[[1]]$time.point.names, indicators.description[[2]]$time.point.names)
  
  if(all_lengths_equal && titles_identical) {
    
     unique_col_titles <- indicators.description[[1]]$time.point.names
     
  }else if (all_lengths_equal && !titles_identical){
  
    all_titles <- unlist(lapply(indicators.description, function(x) x$time.point.names))
    unique_col_titles <- unique(all_titles)
  
  }else if(length(unique(lengths)) < length(lengths)){ 
    
  all_titles <- unlist(lapply(indicators.description, function(x) x$time.point.names))
  unique_col_titles <- unique(all_titles) 
  
  } else if(length(unique(lengths)) == length(lengths)){
    
  #собиравем time.point.names со всех списков и делаем их уникальными
  all_titles <- unlist(lapply(indicators.description, function(x) x$time.point.names))  
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
  
  for(item in indicators.description){
    
    group.labels <- item$group.labels
    comb_list <- combn(item$columns.id,2,simplify = FALSE)
    comb_list_all <- item$columns.id
    current_index <- item_index
    new_order <- as.character(item$time.point.indices)
    
  for(g in group.labels){
    
    comb_test <- unlist(comb_list)
    print(paste("22",comb_test))
    
    df <- as.matrix(data[data[[item$group.col.id]] %in% g,])

    if(!is.null(time.points.title)){
    df_order <- data.frame(columns.id = item$columns.id, time.point.indices = item$time.point.indices)
    df_order <- df_order[order(df_order$time.point.indices), ]
    df <- df[, df_order$columns.id]
    colnames(df) <- df_order$time.point.indices
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
      df_cols <- unique(unlist(item$columns.id))
      df <- df[,df_cols]
      colnames(df) <- item$time.point.names
      
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
    sr <- cmp.group.all(group.col.id     = item$group.col.id,
                  group.labels = group.labels,
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
      idx[item$time.point.indices] <- 1:length(item$time.point.indices)
      rr_order_new <- rr_order[idx]
      rr_order_new[!is.na(idx)] <- rr_order[item$time.point.indices]
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
    data_df = as.matrix(data[data[[item$group.col.id]] %in% g,])
    data_df_naomit = data_df[,unique(unlist(comb_list))]
    df.matrix <- ifelse(is.matrix(data_df_naomit),na.count.string(data_df_naomit[,1]),na.count.string(data_df_naomit[,1]))

    result <- data.frame(matrix(NA,ncol=length(med)+2,nrow=1))
    result[1,] <- c(paste0(g,"\nN=",df.matrix),med,"")
    
    if(comp.flag==T){
    if(g==tail(group.labels, n=1)){
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
    if(g==tail(group.labels, n=1)){
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
  col.title <- sapply(indicators.description, get_new_group)
  result_new1[result_new1$'Группа' == "na", "Группа"] <- col.title
  path.data = paste0(PATH, FILENAME,".csv")
  write.table(result_new1,file=path.data,sep=";",dec=",",na="",col.names = T,row.names = F)
  
  #-----------------------------------------------------------------------------
   
  result_new1 <- format_table_with_na_split(result_new1)
  #-----------------------------------------------------------------------------
  
  return(result_new1)
}