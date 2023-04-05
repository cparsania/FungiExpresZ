

#' Filter FPKM matrix by mapping rate
#'
#' @param x a character string denoting a file to mapping rate. 
#' @param mapping_threshold threshold value, default 0.7. 
#'
#' @return a tibble. 
#' @export
#' @keywords internal
.filter_samples_by_mapping_rate <- function(x, mapping_threshold = 0.7){
        
        # read data
        mapping_rate_data <- readr::read_csv(x)
        
        # select required columns. 
        mapping_rate_data <- mapping_rate_data %>% dplyr::select(study, sample, mapping_rate)
        
        # filter
        ret <- mapping_rate_data %>% dplyr::filter(mapping_rate >= mapping_threshold)
        
        return(ret)
}




#' Prepare FPKM matrix from stringTie output
#' @description This is the helper function to prepare the FPKM data to include in the FungiExpresZ. 
#' @param data_dir a character string denoting a valid directory path in which stringTie output and a file of mapping rate is stored. See details. 
#' @param frac a double, default 0.01, denoting the value to be added to the FPKM prior to log2. 
#' @details `data_dir` must contains two type of files -1) .xls file (must be suffixed with `_expression_values.xls`) and 
#' 2) .csv file (must be suffixed with `_mapping_rate.csv`). Each .xls file is a sample wise default output of stringTie program. FPKM values will be derived from the 9th column of this file. 
#' .csv file contains mapping rate of samples from all .xls files. It requires atleast 3 columns with the column names `study` (denotes the SRA study id), `sample` (denotes the SRA id),and `mapping_rate` (denotes
#' mapping rate where 1 is eqal to 100% mapping).  
#' 
#' 
#' The return object will be the tibble of log2(FPKM + frac) values. Each column will be the SRA sample and each row will be the gene. The rows containing 0 in all the samples and columns containing  
#' 0 in all the rows will be discarded. 
#' 
#' Column names (in most cases SRA id) will be derived from the file names. It is requirement that .xls files named in the format of `<SRAID>_expression_values.xls`. 
#' 
#' @return a tibble containing sample wise log2 (FPKM + frac) values.
#' @export
#' @import TidyWrappers
#'
prepare_fungiexpresz_fpkm_matrix_rds_files_from_stringTie_output <- function(data_dir , 
                                                                             frac = 0.01){
        
        
        excel_file_dirs <- data_dir 
        
        expression_values_excel_files <-  fs::dir_ls(excel_file_dirs,recurse = T, regexp = "*_expression_values.xls")
        
        # check availability of the expression value files 
        if(length(expression_values_excel_files) == 0 ){
                stop(cli::format_error(message = "No file with the extension *_expression_values.xls is found in the data directory -> {data_dir}."))
        }
        
        
        # check availability of the mapping file 
        mapping_rate_csv_file <- fs::dir_ls(excel_file_dirs,recurse = F, regexp = "*_mapping_rate.csv")
        if(length(mapping_rate_csv_file) == 0 ){
                stop(cli::format_error(message = "The file *_mapping_rates.csv not found in the data directory -> {data_dir}."))
        }
        
        
        # derive sra id from file names. 
        
        cli::cli_alert_info(text = "Substring after first '_' will be considered as an SRA id for sample. It is mandatory to have file names in this format '<SRAID>_other_text.xls'.")
        
        sample_sra_id <- expression_values_excel_files %>% fs::path_file() %>% 
                stringr::str_replace_all(pattern = "\\_.*",replacement = "")
        
        if(any(duplicated(sample_sra_id))){
                duplicated_id <- sample_sra_id[which(duplicated(sample_sra_id))]
                stop(cli::format_error("id{?s} cannot be duplicated -> {.strong {duplicated_id}}."))
        }
        
        names(expression_values_excel_files) <- sample_sra_id
        
        # get mapping rate 
        
        samples_to_keep <- .filter_samples_by_mapping_rate(x = mapping_rate_csv_file) %>% dplyr::pull(sample)
        samples_to_keep <- samples_to_keep %>% unique()
        
        expression_values_excel_files <- expression_values_excel_files[samples_to_keep]
        expression_values_excel_files <- expression_values_excel_files[!is.na(expression_values_excel_files)]
        
        fpkm_data <- purrr::map(expression_values_excel_files , ~ plyranges::read_gff(..1) %>% 
                                        dplyr::filter(type == "transcript") %>% 
                                        tibble::as_tibble() %>% 
                                        readr::type_convert() %>% 
                                        dplyr::select(gene_id, FPKM) %>% 
                                        # take mean of FPKM if same transcript have multiple FPKM   
                                        dplyr::group_by(gene_id) %>% 
                                        dplyr::summarise(FPKM = mean(FPKM)))
        
        
        # final wide FPKM format data 
        fpkm_data <- tibble::enframe(fpkm_data) %>% 
                tidyr::unnest(cols = c(value)) %>%
                tidyr::pivot_wider(id_cols = "gene_id", names_from = 'name', values_from = "FPKM")
        
        ## columns having all 0 
        column_all_zero <- fpkm_data %>% TidyWrappers::tbl_get_vars_zero_all()
        
        if(length(column_all_zero) > 0 ){
                cli::cli_alert_warning(" Sample{?s} with 0 in all rows will be discarded. Th{?is/ese} {?is/are} {.strong {column_all_zero}}.")
        }
        
        ## remove columns having all zero 
        fpkm_data_non_zero_cols <- fpkm_data %>% TidyWrappers::tbl_remove_vars_zero_all()
        
        ## extract rows having all numeric are zero (apply to numeric cols only)
        rows_all_zero <- fpkm_data_non_zero_cols %>% TidyWrappers::tbl_keep_rows_zero_all()
        if(nrow(rows_all_zero) >0 ){
                cli::cli_alert_warning(" Row{?s} with 0 in all columns will be discarded. Th{?is/ese} {?is/are} {rows_all_zero %>% pull(1)}.")
        }
        
        ## remove rows having all numeric are zero (apply to numeric cols only)
        fpkm_data_non_zero_cols <- fpkm_data_non_zero_cols %>% TidyWrappers::tbl_remove_rows_zero_all()
        
        ## add fraction to all numeric values 
        fpkm_data_long <- fpkm_data_non_zero_cols %>% TidyWrappers::tbl_convert_log2( frac = frac)
        
        return(fpkm_data_long)
        
}


#' Prepare SRA sample annotations from fungal_sra_run_info_table
#' 
#' @param sra_id a character vector denoting valid SRA id. 
#' @param fungal_sra_run_info_object_path a character string denoting path to the `fungal_sra_run_info_table.rds` object
#' @details this is an internal function. It is called from the [FungiExpresZ::prepare_annotations_for_fungiexpres_data_mtrix()].
#' @return a tibble. 
#' @export
#' @keywords internal 
.get_annotations_from_fungal_sra_run_info_table <- function(sra_id , fungal_sra_run_info_object_path){
        
        #fungal_sra_run_info_object_path <- "../../fe_new_data_by_Raimen/fungal_sra_run_info_table.rds"
        fungal_sra_run_info <- readr::read_rds(fungal_sra_run_info_object_path)        
        
        required_cols <- c("organism_2",
                           "strain",
                           "genotype",
                           "center_name",
                           "insert_size",
                           "library_name",
                           "library_layout",
                           "load_date",
                           "collection_date",
                           "run",
                           "bio_project",
                           "bio_sample",
                           "sample_name",
                           "sra_sample",
                           "sra_study")
        
        
        sample_info_from_fungal_sra_run_table <- tibble::tibble(run = sra_id) %>% 
                dplyr::left_join(fungal_sra_run_info) %>% 
                dplyr::select(required_cols) 
        
        return(sample_info_from_fungal_sra_run_table)
        
} 

#' Prepare SRA sample annotations from  SRAdb
#' @description It use three tables - `sra`, `experiment` and `sample` to get necessary information for an SRA id. 
#' @param sra_id a character vector denoting valid SRA id. 
#' @param path_sra_db_sqlite_file a character string denoting path to the `SRAmetadb.sqlite` object.  
#' @details this is an internal function. It is called from the [FungiExpresZ::prepare_annotations_for_fungiexpres_data_mtrix()].
#' @return a tibble. 
#' @export
#' @keywords internal
.get_annotations_from_sradb <- function(sra_id, path_sra_db_sqlite_file) {
        
        #path_sra_db_sqlite_file = "/Users/chiragparsania/Documents/Projects/8_FungiExpresZ_iMac_version/fe_other_data/SRAdb_sql_lite/SRAmetadb.sqlite"
        sqlite_file <- path_sra_db_sqlite_file
        
        # make database connection 
        con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
        
        
        # get annotations from 'sra' table
        required_cols_from_sra_table <- c("run_accession",
                                          "updated_date",
                                          "library_strategy",
                                          "library_selection",
                                          "library_layout",
                                          "library_construction_protocol",
                                          "description",
                                          "study_title",
                                          "study_abstract",
                                          "study_description",
                                          "sample_accession",
                                          "submission_center",
                                          "submission_lab",
                                          "submission_date")
        
        
        annot_from_sra_tbl <- dplyr::tbl(con, "sra") %>% 
                dplyr::select(required_cols_from_sra_table) %>% 
                dplyr::filter(run_accession %in%  !!sra_id ) %>%
                dplyr::collect()
        
        
        # get annotations from `sample` table
        
        required_cols_from_sample_table <- c("sample_accession" ,"taxon_id","scientific_name")
        
        ## Table sample does not contain column run_accession but it has sample_accession. So, we will use sample_accession from previous table to fatch required data 
        
        annot_from_sample_tbl <- dplyr::tbl(con, "sample") %>%
                dplyr::select(required_cols_from_sample_table) %>%
                dplyr::filter(sample_accession %in% !! sra_id ) %>%
                dplyr::collect()
        
        
        # Table : experiment 
        
        required_cols_from_experiment_table <- c("sample_accession" ,"instrument_model")
        
        ## Like table sample , table experiment also does not contain column run_accession. So we will use column sample_accession from previous table to fatch the data 
        
        annot_from_experiment_tbl <- dplyr::tbl(con, "experiment") %>%
                dplyr::select(required_cols_from_experiment_table) %>%
                dplyr::filter(sample_accession %in% !!sra_id ) %>%
                dplyr::collect() %>% unique() %>% 
                tidyr::nest(instrument_model) %>% 
                dplyr::mutate(instrument_model =  purrr::map_chr(data , ~.x %>% dplyr::pull(1) %>% 
                                                                         paste0(. , collapse = ","))) %>% dplyr::select(1,3)
        
        
        final_sra_db_annotations <- 
                tibble:::tibble(run_accession = sra_id) %>% 
                dplyr::left_join(annot_from_sra_tbl , by = "run_accession") %>%
                dplyr::left_join(annot_from_sample_tbl , by = "sample_accession") %>%
                dplyr::left_join(annot_from_experiment_tbl , by = "sample_accession")
        
        return(final_sra_db_annotations)
}



#' Prepare SRA sample annotations required for FungiExpresZ. 
#'
#' @param data_matrix a FPKM matrix for which SRA annotations to be obtained. Column names of will be considered as SRA id. 
#' @param fungiexpresz_species a character string denoting a species name for the `data_matrix`. Species must present in the FungiExpresZ GO annotations. 
#' @param fungiexpresz_ref_annotation a character denoting a reference annotation for the species. Reference annotation must present in the FungiExpresZ GO annotations. 
#' @param fungal_sra_run_info_object_path a character string denoting path to the `fungal_sra_run_info_table.rds` object.
#' @param path_sra_db_sqlite_file  a character string denoting path to the `SRAmetadb.sqlite` object. 
#'
#' @return a tibble of SRA annotation which can be append to existing `sample_info` object of FungiExpresZ.
#' @export
#' @keywords internal
#' 
prepare_annotations_for_fungiexpres_data_matrix <- function(data_matrix, 
                                                           fungiexpresz_species, 
                                                           fungiexpresz_ref_annotation, 
                                                           fungal_sra_run_info_object_path , 
                                                           path_sra_db_sqlite_file){
        
        
        # get annotations from fungal sra run info table 
        annot1 <- .get_annotations_from_fungal_sra_run_info_table(sra_id = data_matrix %>% colnames() %>% .[-1], 
                                                                  fungal_sra_run_info_object_path = fungal_sra_run_info_object_path)
        
        
        # get annotations from SRAdb sqlite object
        annot2 <- .get_annotations_from_sradb(sra_id = data_matrix %>% colnames() %>% .[-1], path_sra_db_sqlite_file = path_sra_db_sqlite_file)
        
        # combine annotations  in one 
        
        sra_sample_annotation <- annot2 %>% dplyr::left_join(annot1,by = c("run_accession" = "run")) 
        
        required_cols_order <-  c("taxon_id",
                                  "scientific_name",
                                  "strain",
                                  "study_title",
                                  "study_abstract",
                                  "genotype",
                                  "library_layout.y",
                                  "library_construction_protocol",
                                  "library_name",
                                  "library_selection",
                                  "library_strategy",
                                  "instrument_model",
                                  "insert_size",
                                  "submission_lab",
                                  "submission_center",
                                  "updated_date",
                                  "center_name",
                                  "run_accession",
                                  "sample_accession",
                                  "bio_project",
                                  "bio_sample")
        
        sra_sample_annotation <- sra_sample_annotation %>% dplyr::select(required_cols_order)
        
        
        # add species name and reference annotation 
        
        sra_sample_annotation_final <- sra_sample_annotation %>% 
                dplyr::mutate(species = fungiexpresz_species,
                              reference_annot = fungiexpresz_ref_annotation)  %>%
                dplyr::select(-run_accession  , dplyr::everything()) %>%
                dplyr::select(species ,reference_annot , dplyr::everything())
        
        return(sra_sample_annotation_final)
        
}




#' Get updated annotations objects (sample_info & ref_annot_to_expr_rds) of FungiExpreZ. 
#'
#' @param new_sample_info a tibble of SRA annotations associated with `new_data_matrix`. This object can be generated using the function [FungiExpresZ::prepare_annotations_for_fungiexpres_data_matrix()].
#' @details This function returns a list of two objects - 
#' 1) updated_ref_annot_to_expr_rds: This is the updated version of existing FungiExpresZ object -> `ref_annot_to_expr_rds`.
#' 2) updated_sample_info: This is the updated version of existing FungiExpresZ object -> `sample_info`.
#' Both these objects will be added R/sysdata.rda.
#' 
#' Internally it uses FungiExpresZ:::sample_info and FungiExpresZ:::ref_annot_to_expr_rds to get the access to existing annotaions. 
#' @return a list of two -> updated_ref_annot_to_expr_rds, updated_sample_info. 
#' @export
#'
get_updated_existing_annotations <- function(new_sample_info = sample_info){
        
        # new_sample_info must be from a single species 
        
        if(length(new_sample_info$species %>% unique()) > 1){
                stop("new_sample_info$species must be of a single species.")
        }
        
        # update sample info
        existing_sample_info <- FungiExpresZ:::sample_info
        
        updated_sample_info <-  dplyr::bind_rows(new_sample_info, existing_sample_info) %>% dplyr::distinct()
        
        
        # update reference annotation to expr map
        new_data_matrix_rds_file_name = new_sample_info$species[1] %>% stringr::str_replace_all(" ","_") %>% stringr::str_c("_expr_mat.rds")
        
        new_annot_to_expr_map <- tibble::tibble(reference_annotation =  new_sample_info$reference_annot[1],
                                                expression_mat_data_file = new_data_matrix_rds_file_name)
        existing_reference_annotation_to_expr_map <- FungiExpresZ:::ref_annot_to_expr_rds
        updated_ref_annot_to_expr_rds <-  dplyr::bind_rows(existing_reference_annotation_to_expr_map, new_annot_to_expr_map) %>% dplyr::distinct()
        
        return(list(updated_ref_annot_to_expr_rds = updated_ref_annot_to_expr_rds, updated_sample_info = updated_sample_info))
        
        
}

