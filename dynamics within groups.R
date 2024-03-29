source("C:/Users/works/Documents/type_colomns_data.R")
source("C:/Users/works/Documents/standardize_data.R")
source("C:/Users/works/Documents/conduct_tests.R")
source("C:/Users/works/Documents/format_table_with_na_split.R")
source("C:/Users/works/Documents/check_arguments.R")
source("C:/Users/works/Documents/validate_list_its.R")

# ��������� ����������� ����������
library(DescTools)
library(IDPmisc)
library(rlist)
library(flextable)
library(officer)
library(purrr)
library(magrittr)

# ���������� ������� dynamics
dynamics <- function(data,PATH,FILENAME,comp.flag=T,indicators.description,time.points.title=NULL){

  # ��������� ��������� ������� � ������� check_arguments
  check_arguments(data, PATH, FILENAME, comp.flag, indicators.description,time.points.title)
  
  # ���������� �������� �����������
  validate_indicators.description(indicators.description)
  
  #-----------------------------------------------------------------------------
  
  # ������� ������ � ������� ������� ��������� ����� ��� ������� ����������
  lengths <- sapply(indicators.description, function(x) length(x$time.point.names))
  # ���������, ��� ��� ����� ����� ����� �����
  all_lengths_equal <- length(unique(lengths)) == 1
  
  # ���������, ��� �������� ��������� ����� � ������ � ������ �������� ����������� ���������  
  titles_identical <- identical(indicators.description[[1]]$time.point.names, indicators.description[[2]]$time.point.names)
  
  # ���� ����� ��������� ����� ��������� � �� �������� ���������, �� �������� ��������� ����� �� ������� ��������
  if(all_lengths_equal && titles_identical) {
    
     unique_col_titles <- indicators.description[[1]]$time.point.names
     
  }else if (all_lengths_equal && !titles_identical){# ���� ����� ��������� ����� ���������, �� �� �������� ������, �� ���������� ��� �������� ��������� �����
  
    all_titles <- unlist(lapply(indicators.description, function(x) x$time.point.names))
    unique_col_titles <- unique(all_titles)
  
  }else if(length(unique(lengths)) < length(lengths)){ # ���� ���������� ���������� ���� ������ ������ ���������� �����������, ������, � ������ ����������� ������ ���������� ��������� �����
    
  all_titles <- unlist(lapply(indicators.description, function(x) x$time.point.names))
  unique_col_titles <- unique(all_titles) 
  
  } else if(length(unique(lengths)) == length(lengths)){ # ���� ��� ����� ������, �� �������� ��� ��������� ����� � ������ �� �����������
    
  #��������� time.point.names �� ���� ������� � ������ �� �����������
  all_titles <- unlist(lapply(indicators.description, function(x) x$time.point.names))  
  unique_col_titles <- unique(all_titles)
  
  }
  #----------------------------------------------------------------------------- 
  # ������� ������ ������ ��� �������� �����������
  result_list <- list()
  
  # ������� ������ ������ ��� �������� ������������� �����������
  temp_results <- list()
  
  # ������������� ��������� �������� ������� ��������
  item_index <- 1
  
  if(!is.null(time.points.title)){
  #������� �������� ����� �� ������ time.points.title ���� �� �� �������� ������
  new_colnames <- setNames(time.points.title, seq_along(time.points.title))
  }
  
  # ������� ��������� � ������ indicators.description:
  for(item in indicators.description){
    
    # ���������� ���������� �� item:
    #---------------------------------------------------------------------------
    # �������� ������ ����� ����� ������
    group.labels <- item$group.labels
    # ��� ���������� �������� ������ (���� ��������)
    comb_list <- combn(item$columns.id,2,simplify = FALSE)
    # ������ �������� ������
    comb_list_all <- item$columns.id
    # ������� ������ ��������, ������� ����� �������������� � ���������� 
    current_index <- item_index
    new_order <- as.character(item$time.point.indices)
    #---------------------------------------------------------------------------
  # ���� �� ������ ����� ������
  for(g in group.labels){
    
    comb_test <- unlist(comb_list)
    print(paste("22",comb_test))
    
    # ������������ ������� ������ df �� ������ ��������� ������ g
    df <- as.matrix(data[data[[item$group.col.id]] %in% g,])

    if(!is.null(time.points.title)){
    # �������� ���������� df_order � ��������� �������� � ���������� �������
    df_order <- data.frame(columns.id = item$columns.id, time.point.indices = item$time.point.indices)
    # ���������� df_order �� ��������� ������ ��� ����������� ������� ��������
    df_order <- df_order[order(df_order$time.point.indices), ]
    # �������� ����� ������� df � ���������������� ���������
    df <- df[, df_order$columns.id]
    # ������ ���� �������� �� ������� ��������� �����
    colnames(df) <- df_order$time.point.indices
    # ������ ���� �������� �� ����� ����� �� time.points.title
    colnames(df) <- new_colnames[colnames(df)]
    # �������� ����� ������� df_full � ���������
    df_full <- data.frame(matrix(" ", nrow = nrow(df), ncol = length(time.points.title)))
    # ���������� ���� �������� df_full �� ������ time.points.title
    colnames(df_full) <- time.points.title
    # ���������� ����������� ���� �������� ��� ������������ ������
    intersect_cols <- intersect(colnames(df_full), colnames(df)) 
    intersect_cols <- as.character(intersect_cols)
    # ���������� ��������������� ����� � df_full ������� �� df
    df_full[,intersect_cols] <- df[,intersect_cols]
    # ���������� ���� �������� df_full �� ������ time.points.title
    colnames(df_full) <- time.points.title
    # �������������� df_full ������� � �������
    df <- as.matrix(df_full)
    
    }
    if(is.null(time.points.title)){
    # �������� ������ ���������� �������� �������� �� item$columns.id
    df_cols <- unique(unlist(item$columns.id))
    # ����� ������ ��� �������� � ������� df, ������� ������� � df_cols
    df <- df[,df_cols]
    # ���������� ���� �������� df ����� ��������� ����� �� item$time.point.names
    colnames(df) <- item$time.point.names
    # �������� ����� ������� df_full � ������� ��������  
    df_full <- data.frame(matrix(" ", nrow = nrow(df), ncol = length(unique_col_titles)))
    # ���������� ���� �������� df_full ����� �� unique_col_titles
    colnames(df_full) <- unique_col_titles
    # ���������� ����������� ���� �������� ��� ������������ ������
    intersect_cols <- intersect(colnames(df_full), colnames(df))
    intersect_cols <- as.character(intersect_cols)
    # ���������� ��������������� ����� � df_full ������� �� df
    df_full[,intersect_cols] <- df[,intersect_cols]
    # ���������� ���� �������� df_full ����� �� unique_col_titles
    colnames(df_full) <- unique_col_titles
    # �������������� df_full ������� � ������� ��� ���������� ���������
    df <- as.matrix(df_full)
    }
    # �������� ������� col_type_list, � ������� ��� ������� (item$type) ����������� ��� ������ ����������
    col_type_list <- rep(item$type, length(comb_list_all))
    # �������������� ������� col_type_list � ������� ������
    col_type_list <- unlist(col_type_list)
    print(col_type_list)
    # �������� ������ common.list, ������� �������� ���� �������� � �� ����
    common.list <- lapply(1:length(comb_list_all), function(x) list(id = comb_list_all[[x]], type = col_type_list[[x]]))
    print(paste("1111",common.list))


    # ���� comp.flag ����� TRUE, �� ����������� ��������� ����� � ������� ������� cmp.group.all
    if(comp.flag==T){
    sr <- cmp.group.all(group.col.id     = item$group.col.id,
                  group.labels = group.labels,
                  columns.list = common.list,
                  data         = data,
                  save.file.cmp.2groups    = F,
                  PATH         = path.dir.out,
                  FILENAME     = "FILE",
                  result = NULL)
    # ���������� ������� ����������� �� sr � �������������� ��� � ������
    rr <- as.matrix(sr[,5])
    rr <- as.vector(rr)

    }
    #
    # #---------------------------------------------------------------------------
    # �������������� ������ � ������� df � ������� ������� standardize_data
    df <- standardize_data(df)
    # #---------------------------------------------------------------------------

    if(comp.flag==T){
      # ���������� �������� ������ ����� � ������� df
      empty_cols <- which(df == " ", arr.ind = TRUE)
      # ���������� ���������� �������� �������� � ������� ��������
      insert_idxs <- unique(empty_cols[,2])
      # ��������, ���� �� ������ ������, � ���� ��, �� ���������� ������� rr
      if(length(insert_idxs)!=0){
      max_idx <- length(rr) + length(insert_idxs)
      new_rr <- character(max_idx)
      rr_idx <- 1
      i <- 1
      # ������ �� �������� ��������� � ������� new_rr
       for (i in 1:max_idx) {
        if(i %in% insert_idxs) {
          new_rr[i] <- "-"
        } else {
          # ��������, �� ��������� �� ���������� ��������� � ������� rr
          if(rr_idx <= length(rr)) {
            new_rr[i] <- rr[rr_idx]
            rr_idx <- rr_idx + 1
          }
        }
       }
      # ���������� ������� rr � ������ ��������� �����
      if(is.null(time.points.title)){
      rr <- new_rr
      } else{
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
    # ������� ��������� ������ �� col_type_list
    group_summary_results <- type_columns_data(df, col_type_list)
    # ��������� df ������� �� group_summary_results
    df <- group_summary_results$df
    # ��������� ���������� �� �������� �� group_summary_results
    column_stats <- group_summary_results$column_stats
    #---------------------------------------------------------------------------
    # ������� ������� ������ (data_df) ��� ������� ������ (g) �� �������� ������ (data)
    data_df = as.matrix(data[data[[item$group.col.id]] %in% g,])
    # ��������� ������ ������ ��� ��� ��������, ������� ������������ � ����������� comb_list
    data_df_naomit = data_df[,unique(unlist(comb_list))]
    # ������� ������� df.matrix, � ������� ��� ������ ������ ������������ ���������� ������������� ��������
    # � ��������������� �������� �� data_df_naomit
    df.matrix <- ifelse(is.matrix(data_df_naomit),na.count.string(data_df_naomit[,1]),na.count.string(data_df_naomit[,1]))
    # ������� ����� ������ (result) ��� �������� ����������� ������� ������� ������ (g)
    # ���������� �������� � result ������������ ����������� �������������� ������ (column_stats) ���� 2
    result <- data.frame(matrix(NA,ncol=length(column_stats)+2,nrow=1))
    # ��������� ������ ������ ������ ������ (result) ����������� ������, ������������ ������� ������ (g),
    # ����������� ����� � data_df (N=...) � ��������������� ��������� (column_stats)
    result[1,] <- c(paste0(g,"\nN=",df.matrix),column_stats,"")
    
    if(comp.flag==T){
    # ���������, �������� �� ������� ������ (g) ��������� � ������ group.labels
    if(g==tail(group.labels, n=1)){
      # ��������� ����� ��������� ������ � ���������� � ��������� ��� ���� ������ � NA
      last_row <- nrow(result)+1
      result <- rbind(result, rep(NA, ncol(result))) 
      # ��������� ������ ������ � ��������� ������ ��� "���������:"
      result[last_row, 1] <- "���������:"
      # ��������� � ������� rr NA, ����� ������� ��� ����� �� �����, ��� � ������ �����������
      rr <- c(rr, rep(NA, ncol(result) - length(rr)-1))
      # ��������� ���������� ������ � ��������� ������ ������������ ��������� (rr)
    for(i in 1:length(rr)){
     result[last_row, i+1] <- rr[i] 
     }
    }
    }
    # ���������� ��� ��������� ���������� �������� ��� ������� �������� ��������
    pairwise_combinations <- unlist(lapply(ncol(df), combn, 2, simplify=FALSE), recursive=FALSE) 
    
    #---------------------------------------------------------------------------
    # ������� ���������� � ����� � ����������� �� ���� ������
    result <- conduct_tests(data_df,df, col_type_list, column_stats,result,pairwise_combinations)
    #---------------------------------------------------------------------------
    # ��������� ������ � ������ ������ ��� ����������� ����������� (������ �������)
    result[1, length(column_stats)+2] <- paste(result[,length(column_stats)+2], collapse="\n")
    # ��������� ������ � ������ ������ ��� ����������� ����������� (������ �������)
    result[1, length(column_stats)+3] <- paste(result[,length(column_stats)+3], collapse="\n")
    # ���������, ���� ������� ������ (g) - ��������� � ������ group.labels
    if(g==tail(group.labels, n=1)){
    # �������� ������� �������� ��� ��������� ���� �������� � ����������
    col1_idx <- ncol(result) - 1
    col2_idx <- ncol(result)
    # ������ ������, ���� ����� ��������� NA ��� ���������
    row_idx <- 2
    # ��������� ������ ������� ������� ��� ���� ������ ��������� NA
    result[row_idx, col1_idx] <- NA
    # ��������� ������ �������� ������� ��� ���� ������ ��������� NA
    result[row_idx, col2_idx] <- NA
    }
    # ��������� ������ ������ � ������� ������� � ������ ������� (�� NA)
    result <- result[complete.cases(result[ ,1]),]
    # ��������� ��������� ������ � ���������� ��� last.res
    last.res <- tail(result,n=1)
    # ��������� ��������� ��������� ��� ������� ������ � ������ �����������
    result_list[[g]] <- result[complete.cases(result[ ,1]),]
  }
      
  # ���������� ���������� ��� ������ ������ � ���� ������� �������
  result_new <- do.call(rbind,result_list)
  # �������� ���������� �������� �������� �� comb_list � ����������� �� � �������� ������
  hh <- as.numeric(unique(unlist(comb_list)))
  # ������� ����� ��� �������� ������� result_new, ������� �������� �����, ��������� �����, �������� ������� � p-������� ���������
  names(result_new) <- c("������",unique_col_titles,"�������� �������","p-�������,\n���������")
  # ��������� ���������� ���������� ��� �������� ������ ���������� �� ��������� ������ temp_results
  temp_results[[current_index]] <- result_new
  # ����������� ������ �������� � ������� ������ �� 1
  item_index <- item_index + 1  
  current_index <- current_index + 1
  }
  # ������� ������ ������ �� final_results_table, ��� ��� ��� �������������� ������ ��� ��������� �������� ��������
  final_results_table <-do.call(rbind.data.frame, lapply(temp_results, function(x) unname(rbind(names(x), as.matrix(x)))))
  names(final_results_table) <- final_results_table[1,]
  final_results_table <- final_results_table[-1,]
  # ������� ������ na.list ��� ���������� ����� � NA
  na.list <- list("na", NA, NA, NA, NA)
  # ���� ����� ������ na.list �� ������ ������ ���������� �������� � final_results_table, ���������� ��������� NA
  while(length(na.list)!=ncol(final_results_table)){
    na.list <- list.append(na.list,NA)
  }
  #------------------------------------------------------------------------------------
  # �������� ����� �������� �� final_results_table
  col.names = names(final_results_table)
  # ������� DataFrame � �������� NA �������� �� ������ ������ na.list
  s <- as.data.frame(na.list)
  # ������ ����� �������� DataFrame s �� ������ ���� �������� final_results_table
  names(s) <- col.names
  # ���������� DataFrame s � final_results_table, ����� �������� ������ � NA � ������
  final_results_table <- rbind(s,final_results_table)
  # �������� �� ������ ������ ������� final_results_table
  for(i in 1:nrow(final_results_table)){
    # ���� �������� � ������ ������� ������ ����� "������"
    if(final_results_table[i,1]=="������"){
      # �������� ��� ������ �� �������� �� ������ na.list
      final_results_table[i, ] <- na.list
    }
  }
  # �������� ����� ������� �� ������ comb_list
  col_name <- sapply(comb_list, function(x) {
    col_index <- x[1]  
    col_name <- colnames(data)[col_index]
    return(col_name)
  })
  # �������� ��������� ����� �� indicators.description
  col.title <- sapply(indicators.description, get_new_group)
  # �������� "na" �������� � ������� "������" �� ��������� �����
  final_results_table[final_results_table$'������' == "na", "������"] <- col.title
  # ������� ���� � �����, � ������� ����� ���������� final_results_table � ������� CSV
  path.data = paste0(PATH, FILENAME,".csv")
  # ���������� final_results_table � CSV ����
  write.table(final_results_table,file=path.data,sep=";",dec=",",na="",col.names = T,row.names = F)
  
  #-----------------------------------------------------------------------------
  # ������������ ��������� ������� � word
  final_results_table <- format_table_with_na_split(final_results_table)
  #-----------------------------------------------------------------------------
  
  return(final_results_table)
}