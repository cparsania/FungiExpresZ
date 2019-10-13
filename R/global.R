##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## get_expression_mats_dir_path ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#' Get absolute path for the expression matrix .rds files. 
#' It returns the character string indicating to absolute directory path of expression matrix .rds files.
#' @return A character vector of a string.
#' 
#' @keywords internal
get_expression_mats_dir_path <- function(){
        
        #expr_mat_rds_file_name <- "a_nidulans_expr_mat.rds"
        #if(file.exists("./.httr-oauth")){ ## load data from drop box
        
        if(FALSE){ ## Dropbox download explicitly turned off
                local_drop_dir <- "./dropbox_to_local"
                ### dowload data from dropbox to local 
                get_data_dropbox_to_local()
                return(local_drop_dir)
        } else{ ## use subset data 
                local_sample_dir = system.file( "app", "expression_mats_rds_files_new" , package = "FungiExpresZ")
                return(local_sample_dir)
        }
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## shinyInput ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#' create shiny inputs
#'
#' \code{shinyInput} function creates a character vector of length \code{len} for a given shiny input under the argument \code{FUN}
#' 
#' @param FUN A shiny UI function for which vector of defined length need to be created
#' @param len An integer denoting length of return vector
#' @param id A character string denoting id of UI element
#' @param ns An object of class NS
#' @param ... Other parameters to be pass to argument \code{FUN}
#'
#' @return A character vector of elements \code{FUN} of length \code{len}.
#' @references \link{https://stackoverflow.com/questions/45739303/r-shiny-handle-action-buttons-in-data-table}
#'
#' @keywords internal
shinyInput <- function(FUN, len, id,ns,...) {
        
        inputs <- character(len)
        for (i in seq_len(len)) {
                inputs[i] <- as.character(FUN(paste0(ns(id), i), ...))
        }
        inputs
}



##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## get_gg_colors ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#' Get character vector of  ggplot colors in default order
#' 
#' Given a character or numeric vactor \code{x} , the function returns the named vector of default ggplot colors of length \code{length(x)}. The names will be elements of \code{x} and values will be unique color associated to each element of \code{x}.  
#'
#' @param x internal
#' 
#' 
#' @return internal
#' @keywords  internal
#' @examples 
#' \dontrun{
#' x <- letters[1:5]
#' gg_col = get_gg_colors(x)
#' 
#' ## visualize colors 
#' pie(seq_len(length(x)) / seq_len(length(x)) ,labels = x , col=gg_col) 
#' 
#' y <- LETTERS[ 1:5]
#' 
#' visualize colors 
#' get_gg_colors(y)
#' pie(seq_len(length(y)) / seq_len(length(y)) ,labels = y , col=gg_col)
#' } 
get_gg_colors <- function(x){
        colrs <- scales::hue_pal()(length(x))
        rlang::set_names(colrs , x)
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## generate_random_strings ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#taken from: https://stackoverflow.com/questions/42734547/generating-random-strings

#' Generates n random string of length 10
#'
#' 
#' 
#' @param n An integer denoting number of unique strings to be generated.
#'
#' @return A character vector of length \code{n} containing unique string   
#' @keywords internal
#'
generate_random_strings <- function(n = 5000) {
        
        a <- do.call(paste0, replicate(10, sample(LETTERS, n, TRUE), FALSE))
        paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##  reorder_clusters ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#' reorder_clusters
#'
#' 
#' @param mat : numeric matrix. nrow(mat) must be equal to the length of row_clusters and ncol(mat) must be equal to the column_clusters. 
#' @param row_clusters : character vector, where each element indicates the row cluster name of mat
#'
#' @return : list 
#' @keywords internal
#'
reorder_clusters  <- function(mat,row_clusters ){
        
        # mat <- matrix(rnorm(1:1000) , nrow = 26)
        # rownames(mat) <- sample(letters , 26)
        # row_clusters = sample(1 , 26 , replace = T)
        
        row_clusters <- forcats::as_factor(as.character(row_clusters))
        
        dd <- mat %>% 
                as.data.frame() %>% 
                tibble::rownames_to_column() %>% 
                tibble::as_tibble() %>%
                dplyr::mutate(clust =  as.character(row_clusters)) %>% 
                dplyr::group_by(clust)
        
        dd2 <- dd %>% 
                dplyr::summarise_if(is.numeric , sum)
        
        col_names <- dd2 %>% 
                dplyr::select_if(is.numeric) %>% 
                names() %>% 
                rlang::syms()
        
        new_ord <- dd2 %>%
                dplyr::group_by(clust)%>% 
                dplyr::mutate(ord = which.max(c(!!!col_names))) %>% 
                dplyr::mutate(maxim = pmax(!!!col_names)) %>% 
                dplyr::arrange(ord , -maxim) %>% 
                dplyr::pull(clust) %>% 
                as.character()
        
        tbl_ord <- dd %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(clust = forcats::fct_recode(dd$clust , !!!setNames( new_ord ,dd2$clust) )) %>% 
                dplyr::mutate(clust =  as.character(clust)) %>%
                dplyr::arrange((clust)) 
        
        ret_clust <- tbl_ord %>% 
                dplyr::pull(clust) %>% 
                as.character()
        ret_mat <- tbl_ord %>% 
                as.data.frame() %>% 
                tibble::column_to_rownames("rowname") %>% 
                dplyr::select(-clust) %>% as.matrix()
        
        return(list(clust = ret_clust , mat = ret_mat))
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## lm_eqn ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' lm_eqn
#' 
#' regression eqn
#' 
#' @param df internal
#' @param x  internal
#' @param y  internal
#'
#' @return internal
#' @keywords internal
#'
lm_eqn <- function(df, x, y) {
        
        m <- lm(y ~ x, df)
        eq <- substitute(
                italic(y) == a + b %.% italic(x) * "," ~ ~ italic(r)^2 ~ "=" ~ r2,
                list(
                        a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)
                )
        )
        as.character(as.expression(eq))
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### get_density ----
## taken from http://slowkow.com/notes/ggplot2-color-by-density/
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#' get_density
#'
#' calculate 2D density 
#' 
#' @param x internal
#' @param y internal
#' @param n internal
#' 
#' @keywords internal
#'  
#' @return internal
#' 
#'
get_density <- function(x, y, n = 200) {
        
        dens <- tryCatch({
                dens <- MASS::kde2d(x = x, y = y, n = n)
                ix <- base::findInterval(x, dens$x)
                iy <- base::findInterval(y, dens$y)
                ii <- base::cbind(ix, iy)  
                return(dens$z[ii])
        }, error = function(e){
                return(NULL)
        })
        if(is.null(dens)){
                dens <- rep(0 , length(x))
        }
        return(dens)
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## decorate_ggplot ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#' Decorate GG plot
#'
#' @param gplot an object of class ggplot2
#' @param x_labs_size internal
#' @param y_labs_size internal
#' @param x_tick_size internal
#' @param y_tick_size internal
#' @param x_tick_angle internal
#' @param title_labs_size internal
#' @param plot_title internal
#' @param axis_x_title internal
#' @param axis_y_title internal
#' @param color_legend_title internal
#' @param fill_legend_title internal
#' @param legend_text_size internal
#' @param legened_title_size internal
#' @param legend_key_height internal
#' @param strip_text_size internal
#' @param legend_key_width internal
#' @param legend_spacing_x internal
#' @param legend_spacing_y internal
#' @param guide_legend internal
#' @param ... other parameters passed to \code{ggplot2::theme}
#' @keywords internal
#' @return internal
#'
decorate_ggplot <- function(gplot,
                            x_labs_size,
                            y_labs_size,
                            x_tick_size,
                            y_tick_size,
                            x_tick_angle = 90,
                            title_labs_size,
                            plot_title= "",
                            axis_x_title = "",
                            axis_y_title = "",
                            color_legend_title = "",
                            fill_legend_title = "",
                            legend_text_size = 15,
                            legened_title_size = 15,
                            legend_key_height = 10, 
                            strip_text_size = 15,
                            legend_key_width = 10, 
                            legend_spacing_x = 0.2,
                            legend_spacing_y = 0.2,
                            guide_legend = TRUE,
                            ...) {
        
        gplot_decorated <- gplot +
                ggplot2::labs(title  = plot_title , x = axis_x_title , y = axis_y_title , color = color_legend_title , 
                              fill=fill_legend_title) + 
                
                ggplot2::theme(
                        plot.title = element_text(size = title_labs_size),
                        
                        ## axis 
                        axis.title.x = element_text(size = x_labs_size),
                        axis.title.y = element_text(size = y_labs_size),
                        axis.text.x = element_text(size = x_tick_size , angle = x_tick_angle),
                        axis.text.y = element_text(size = y_tick_size),
                        
                        
                        # strip 
                        strip.text = element_text(size = strip_text_size), 
                        
                        ## legend 
                        legend.text = element_text(size = legend_text_size),
                        legend.title = element_text(size = legened_title_size),
                        legend.key.height = unit(x = legend_key_height, units = "mm"),
                        legend.key.width  = unit(x = legend_key_width, units = "mm"),
                        legend.spacing.x = unit(legend_spacing_x, 'cm'),
                        legend.spacing.y = unit(legend_spacing_y, 'cm'),
                        ...
                )
        
        return(gplot_decorated)
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## check_var_types ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 
#' validate vars
#' 
#' @param x  : tibble of vars to check
#' @param req_data_type : req data type of each col in x. Must have same lenght to ncol(x)
#' @keywords internal
#' @return internal
#' 
#'
check_var_types <- function(x, req_data_type) {
        
        # x = mpg
        # req_data_type = c("character","character","numeric","integer","integer","character","character","integer","integer","character","character")
        stopifnot(base::ncol(x) == base::length(req_data_type))
        vars_req_data <- tibble::tibble(vars = colnames(mpg), req_dt = req_data_type)
        vars_given_dt <- dplyr::summarise_all(x, class) %>%
                tidyr::gather(vars, orig_dt)
        out <- dplyr::left_join(vars_given_dt, vars_req_data) %>%
                dplyr::filter(orig_dt != req_dt)
        
        return(out)
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## get_test_data ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#' get random sample of gene expression matrix 
#' @param n_samp An integer denoting number of SRA samples to sample.
#'
#' @return internal
#' @keywords internal
#'
get_test_data <- function(n_samp = 5) {
        
        #library(tidyverse)
        load("app/r_data_objects/an_ca_sra_data_log2_fpkm3.RData")
        an_data <- an_ca_sra_data_log2_fpkm3 %>% 
                dplyr::filter(org == "c_albicans")
        rand_samp <- sample(unique(an_data$sample_name), n_samp)
        dd <- an_ca_sra_data_log2_fpkm3 %>% 
                dplyr::filter(sample_name %in% rand_samp) %>% dplyr::select(-org)
        return(dd)
}



##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## my_filter ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# 


#' create column filters 
#'
#' @param col internal
#' @param op internal
#' @param value internal
#' @param env internal
#'
#' @return internal
#' @references  https://github.com/r-lib/rlang/issues/116
#' @keywords internal
my_filter <- function(col, op, value, env = parent.frame()) {
        
        cond <- call(op, sym(col), value)
        cond <- rlang::new_quosure(cond, env)
        return(cond)
        # dplyr::filter(data, !!cond)
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## tibble_to_row_clusters ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@

## datafrome to clusters. 

## problem : given the tibble return me row clusters.
## tibble requirement  : first column must be char type having non repetative elements. 
## function must have parameter to normalise the matrix. 
## function must validate each column type. 
## function must check whether normalisation can be performed or not. 

#' Given a tibble of gene expression matrix, perform row wise clustering
#'
#' @param x internal
#' @param row_ident internal
#' @param cols_to_use internal
#' @param use_z_score_for_clustering internal
#' @param output_cols internal
#' @param num_of_top_var_rows internal
#' @param nclust internal
#' @keywords internal
#' @return internal
#' 
#'
tibble_to_row_clusters <- function(x, 
                                   row_ident = 1, 
                                   cols_to_use , 
                                   use_z_score_for_clustering= TRUE, 
                                   output_cols = "zscore" , 
                                   num_of_top_var_rows = -1 ,
                                   nclust = 4){
        
        # 
        # dat <- get_test_data()
        # dat <- dat %>% tidyr::spread(sample_name , log2fpkm)
        # x <- dat %>% slice(sample(1:100, 100))
        # cols_strng <- colnames(x)[2:5]
        # use_z_score_for_clustering = FALSE
        # num_of_top_var_rows = -1
        # cols  = rlang::parse_exprs(cols_strng)
        # row_ident = 1
        # row_ident_col_name <- quo(colnames(x)[row_ident])
        # nclust = 1
        # cols_to_use <-  dat %>% dplyr::select(-1) %>% colnames()
        
        ## check arguments types
        # x : data.frame
        # use_z_score_for_clustering : logical of lenght 1 
        # num_of_top_var_rows : numeric of length 1
        # row_ident : numeric of length 1
        # nclust : numeric of length 1
        # output_type : character of length 1 
        
        stopifnot(base::is.data.frame(x) , 
                  all(length(row_ident)==1 , is.numeric(row_ident)),  
                  all(length(use_z_score_for_clustering)==1 , base::is.logical(use_z_score_for_clustering)),
                  all(length(num_of_top_var_rows)==1 , is.numeric(num_of_top_var_rows)), 
                  all(length(nclust)==1 , is.numeric(nclust))
        )
        
        ## column of row_ident must be of type char and non repeatative
        row_ident_col_name <- rlang::parse_expr(colnames(x)[row_ident])
        if(any(!dplyr::pull(x ,row_ident) %>% 
               is.character()  ,  
               base::duplicated(pull(x, row_ident)) ) ){
                stop(paste("column", as.character(row_ident_col_name), 
                           "must be of type character.")) ## column 1 must be of type character. 
        }
        
        ## cols_to_use must be of type numeric 
        cols_strng <- cols_to_use
        cols  = dplyr::enquos(cols_strng)
        if(!(purrr::map_lgl(x %>% dplyr::select(!!!cols) , is.numeric) %>% all)) { 
                stop(paste0(cols_strng , collapse = " ") , " all must be of type numeric.") ## other than column 1, all must be of type numeric 
        }
        
        ## get sd and zscore from function tibble_to_rowwise_sd_zs
        zscore_colname_suffix  <- paste(generate_random_strings(1) , "_zscore",sep = "") #list(...)[["std_dev_colname"]]
        std_dev_colname <-  paste(generate_random_strings(1) , "_std_dev" ,sep = "") #list(...)[["zscore_colname_suffix"]]
        
        
        dd <- tibble_to_rowwise_sd_zs(x = x , 
                                      cols_to_use = cols_strng ,
                                      std_dev_colname = !!std_dev_colname , 
                                      zscore_colname_suffix = zscore_colname_suffix) 
        
        # NA will be replaced by 0. Mostly NAs generate from z-score calculation. For e.g if any 
        ## observation has identical value across all the sample, z-score for that observation will be 0.  
        
        dd <- dd %>% mutate_if(is.numeric , function(ii) {
                ii[is.nan(ii)] <- 0
                ii[is.na(ii)] <- 0
                return(ii)
        }) 
        
        ## use zscore for clustering
        if(use_z_score_for_clustering){
                for_clust  <- dd %>% 
                        dplyr::select(1,std_dev_colname,dplyr::contains(zscore_colname_suffix))
                cat("using zscore for clustering\n\n")
        }else{
                for_clust <- dd %>% 
                        dplyr::select(-dplyr::contains(zscore_colname_suffix))
                cat("using raw value for clustering\n\n")
        }
        
        ## subset top variable genes 
        if(num_of_top_var_rows > 0){
                for_clust <- for_clust %>% 
                        dplyr::arrange(dplyr::desc(!!as.symbol(std_dev_colname))) %>% 
                        dplyr::slice(1:num_of_top_var_rows) 
        }else if(num_of_top_var_rows == -1 ){
                for_clust <- for_clust 
        }
        
        ## prepare kmeans input. 
        kmeans_input <- for_clust %>% 
                dplyr::select(-std_dev_colname) %>% ## remove stddev col 
                as.data.frame() %>% 
                tibble::column_to_rownames(colnames(for_clust)[row_ident]) 
        
        ## perform kmeans
        set.seed(1234)
        km_out <- stats::kmeans(kmeans_input %>% as.matrix() ,centers = nclust)
        
        ## reorder clusters 
        if(TRUE){
                rr <- reorder_clusters(kmeans_input , km_out$cluster)
                clust_reordered <- set_names( rr$clust, rownames(rr$mat))
        }
        
        
        ## add clusters. Rows will be matched by row identity column 
        dd_c <- dd %>% 
                dplyr::right_join(tibble::tibble(ident = names(clust_reordered), clust = clust_reordered) , 
                                  by = setNames("ident" , as.character(row_ident_col_name))) %>% 
                dplyr::arrange(clust)
        
        ## separate  1) raw val columns, 2) zscore cols, 3) std dev col
        
        zscore_dat <- dd_c %>% 
                dplyr::select(1 , dplyr::contains(zscore_colname_suffix)) %>% 
                dplyr::rename_all(function(i){str_replace(i , zscore_colname_suffix , "")}) 
        
        raw_val_dat <- dd_c %>% dplyr::select(1, !!!cols) 
        
        std_dev_dat <- dd_c %>% dplyr::select(1, std_dev_colname) %>% 
                rename_if(is.numeric, function(i) {str_replace(i, std_dev_colname, "std_dev")}) 
        
        cluster_info <- dd_c %>% dplyr::select(1, clust) 
        
        return_data <- list(raw_value = raw_val_dat , zscore = zscore_dat , std_dev = std_dev_dat, 
                            clusters  = cluster_info)
        
        
        return(return_data)
        
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## tibble_to_rowwise_sd_zs ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#' given the tibble and column indices add columns of  row wise zscore and row wise standard deviation
#'
#' @param cols_to_use 
#' @param std_dev_colname 
#' @param zscore_colname_suffix 
#' @param x 
#'
#' @return internal
#' @keywords internal
#'
tibble_to_rowwise_sd_zs <-function(x ,cols_to_use ,
                                   std_dev_colname =  "sd" , 
                                   zscore_colname_suffix = "_zscore") {
        
        
        ## test vars 
        # dat <- dat %>% tidyr::spread(sample_name , log2fpkm)
        # x <- dat
        # cols_to_use <- dat %>%dplyr::select(-1) %>%colnames()
        # std_dev_colname <- "sd"
        # zscore_colname_suffix = "_zscore"
        # 
        #x = x , cols_to_use = cols_strng ,std_dev_colname = std_dev_colname , zscore_colname_suffix = zscore_colname_suffix
        
        cols <- dplyr::enquos(cols_to_use)
        std_dev_colname <- dplyr::enquo(std_dev_colname)
        
        #cols <- c("SRR3384897","SRR4454155" ,"SRR4454585","SRR4456864","SRR5882561")
        #cols <- parse_exprs(cols)
        
        ## cols must be numeric
        if (!(purrr::map_lgl(x %>% dplyr::select(!!!cols) , is.numeric) %>% all)) {
                stop(paste0(
                        as.character(cols) ,
                        "all must be of type numeric." ,
                        collapse = " "
                )) ## other than column 1, all must be of type numeric
        }
        
        ## given column names cannot be `n` or `sd`
        if (any(cols %in% c("n", std_dev_colname))) {
                stop("column names cannot be `n` or " , std_dev_colname)
        }
        
        xx <- x %>% 
                dplyr::ungroup() 
        
        ## get zscore 
        dd_z <- xx %>% 
                dplyr::select(1,!!!cols) %>% 
                tidyr::gather("key" , "value",c(-1)) %>% 
                dplyr::group_by(!!as.symbol(colnames(xx)[1])) %>% 
                dplyr::mutate(zscore = base::scale(value)) %>% 
                dplyr::select(-value) %>% 
                tidyr::spread(key, zscore) %>% ##spread by default arrange the column alphabatically. 
                dplyr::select(1,!!!cols) %>% ## This is necessary to return the same column order as input. 
                dplyr::rename_if(is.numeric, function(i){
                        paste(i , zscore_colname_suffix , sep = "")
                }) %>% 
                dplyr::ungroup() 
        
        ## Add std dev column. Std dev will be calculated from orig data and selected columns. NOT from zscore and all columns 
        dd_sd_z <- xx %>% 
                dplyr::select(1,!!!cols) %>% 
                tidyr::nest(-1) %>%
                dplyr::mutate(!!std_dev_colname := purrr::map(.data$data, ~(sd(.)))) %>%  
                tidyr::unnest() %>%
                dplyr::left_join(dd_z , colnames(xx)[1]) %>% 
                dplyr::select(-!!std_dev_colname, dplyr::everything())
        
        
        return(dd_sd_z)
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## get_ca_gene_names_mapping ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#' Map Candida albicans genes between orf_XXXXX and C1_XXXXX
#' 
#' Get tibble of Candida albicans gene names map (orfXXX to C1_XXXX)
#'
#' @keywords  internal
#'
get_ca_gene_names_mapping <- function() {
        
        id_mapping_file <- "app/annotations/ca_gene_name_map.txt"
        id_map <-
                readr::read_delim(id_mapping_file ,
                                  delim = "\t" ,
                                  col_names = T)
        return(id_map)
        
}


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## perform_go_enrichmet----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#' Perform GO enrichment using clusterProfiler
#' 
#' It uses one of the two functions \code{clusterProfiler::enricher} or \code{clusterProfiler::compareCluster}
#'
#' @param genome internal
#' @param query_genes internal
#' @param p_adjust_method internal
#' @param ontology internal
#' @param min_gs_size internal
#' @param max_gs_size internal
#' @param pval_cutoff internal
#' @param qval_cutoff internal
#'
#' @keywords internal
#' @return internal
#' 
#'
perform_go_enrichmet <- function(genome, query_genes , 
                                 pval_cutoff = 1 , 
                                 qval_cutoff = 1,
                                 p_adjust_method = "BH" , 
                                 ontology = "Biological Process",
                                 min_gs_size = 10 , 
                                 max_gs_size = 500){
        
        # ah_data_summary2 <- read_rds("../../9_bioseqc/fungiexpresz/app/annotations/fungi_db_orgdb_derieved_go_data.rds")
        # p_adjust_method <-"BH" # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")[3]
        # min_gs_size <- 10
        # max_gs_size <- 500
        # #genome =   "FungiDB-39_AastaciAPO3" ,
        # genome =  "FungiDB-42_AnidulansFGSCA4"
        # ontology = "Biological Process"
        # sample_data <- read_delim("app/test_dataset/an_fang_data/An_Pol2_list.txt" , delim = "\t") %>% pull(Gene)
        # query_genes = sample_data
        
        
        ## load GO data when needed
        # if(!exists("ah_data_summary2")) {
        #   #load("app/annotations/fungi_db_orgdb_derieved_go_data.RData")
        #   load("./annotations/fungi_db_orgdb_derieved_go_data.RData")
        # }
        # 
        
        ## get query metadata
        query_meta_data <- ah_data_summary2 %>% 
                dplyr::filter(genome == !!genome) %>% 
                dplyr::select(orgdb_cols) %>% 
                tidyr::unnest() %>% 
                dplyr::filter(ONTOLOGY == !!ontology)
        
        #print(query_meta_data)
        
        ## TERM to GENE table 
        term_to_gene <- query_meta_data  %>% 
                dplyr::select(c("GO_ID", "GID")) %>% 
                tidyr::drop_na() 
        
        ## TERM to NAME table 
        term_to_name <- query_meta_data %>%
                dplyr::select(c("GO_ID", "GO_TERM_NAME")) %>% 
                tidyr::drop_na() 
        
        ## perform enrichment
        enr <- tryCatch({
                if(is.list(query_genes)){
                        enr <- clusterProfiler::compareCluster(geneClusters = query_genes ,
                                                               fun = "enricher" ,
                                                               TERM2GENE = term_to_gene , 
                                                               TERM2NAME = term_to_name , 
                                                               pvalueCutoff = pval_cutoff, 
                                                               pAdjustMethod = p_adjust_method,
                                                               minGSSize = min_gs_size, 
                                                               maxGSSize = max_gs_size, 
                                                               qvalueCutoff = qval_cutoff
                        )
                } else {
                        enr <- clusterProfiler::enricher(gene = query_genes, 
                                                         pvalueCutoff = pval_cutoff, 
                                                         pAdjustMethod = p_adjust_method, 
                                                         minGSSize = min_gs_size, 
                                                         maxGSSize = max_gs_size, 
                                                         qvalueCutoff = qval_cutoff, 
                                                         TERM2GENE = term_to_gene,
                                                         TERM2NAME = term_to_name)
                }
                ## if enr is NULL, generate error 
                if(is.null(enr)){
                        err <- rlang::error_cnd(.subclass = NULL, 
                                                message = 
                                                        paste("\nNo terms enriched.\n 
                                      Expected input gene IDs:  \n",
                                                              paste0(ifelse(query_meta_data$GID %>% length() > 5, 
                                                                            paste0(query_meta_data$GID[1:5] , collapse = ", ") , 
                                                                            paste0(query_meta_data$GID , collapse = ", ") ) , collapse = ", "),"\n",
                                                              "Select correct species or change input id."))
                        cnd_signal(err)
                }else{
                        enr
                }
        }, error = function(e){ ## compareCluster does not return NULL instead directly throws error. Following code will replace default error. 
                err <- rlang::error_cnd(.subclass = NULL, 
                                        message = 
                                                paste("\nNo terms enriched.\n 
                                    Expected input gene IDs:  \n",
                                                      paste0(query_meta_data$GID[1:5] , collapse = ", "),"\n",
                                                      "Select correct species or change input id."))
                return(err) ## this will be used to show popup if enrichment failed
        })
        return(enr)
}

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## text to tobble ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Given a string separated by specified deliminator, the function returns a tibble. 
#'
#' 
#' @param text internal
#' @param sep internal
#'
#' @return internal
#' @keywords internal
#'
text_to_tibble <- function(text , sep = "\t"){
        
        # text <- "gene_name\tSRR180589\tSRR180587\tSRR180586\nC1_00180W_A\t9.52\t10.69\t10.4\nC1_00490C_A\t9.67\t9.04\t8.4\nC1_01060W_A\t8.58\t8.92\t8.52\nC1_01350C_A\t8.02\t8.93\t8.75\nC1_01370C_A\t10.19\t11.68\t11.48\nC1_01480C_A\t8.96\t10.1\t9.85\nC1_01640W_A\t7.73\t8.98\t8.65\nC1_01690C_A\t6.79\t7.73\t7.56\nC1_02330C_A\t9.26\t10.52\t10.38\nC1_02460W_A\t9.54\t10.68\t10.24\nC1_02700C_A\t10.28\t11.28\t9.15\nC1_03010W_A\t9.65\t11.37\t11.39\nC1_03020C_A\t9.41\t10.57\t10.55\nC1_03030W_A\t9.46\t10.75\t10.57\nC1_03090W_A\t9.13\t10.3\t10.12\nC1_03110W_A\t8.66\t9.9\t9.85\nC1_03190C_A\t7.75\t7.92\t8.15"
        
        out <- tryCatch({
                
                dd <- text %>% 
                        stringr::str_split("\n")  %>% ## split by new line will give each row 
                        base::unlist() %>% 
                        stringr::str_split(sep , simplify = T) %>% 
                        tibble::as_tibble() %>% 
                        rlang::set_names(.[1,]) %>% ## use first row as tibble header 
                        dplyr::slice(-1) %>%## remove first row
                        readr::type_convert() ## automatic conversion of data type
                return(dd)
        }, error = function(e){
                print(paste0("Error in text_to_tibble : " , e , collapse = "\n"))
                return(NULL)
        })
        
        return(out)
}



#' Convert text into clean document using several functions from the package \code{tm}
#' 
#' 
#' @param text internal
#' @param remove_numbers internal
#' @param remove_stop_words internal
#' @param remove_specific_words internal
#' @param remove_punctuation internal
#' @param text_stemming internal
#' @param specific_words internal
#' @return internal
#' @keywords internal
#' 
#' 
#'
text_to_clean_document <- function(text, remove_numbers = TRUE , remove_stop_words = TRUE , 
                                   remove_specific_words = TRUE , remove_punctuation = TRUE , 
                                   text_stemming = FALSE , specific_words = NULL){
        
        ## text to docs
        docs <- tm::VCorpus(tm::VectorSource(text))
        
        ## remove species char 
        toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
        
        docs <- tm::tm_map(docs, toSpace, "/")
        docs <- tm::tm_map(docs, toSpace, "@")
        docs <- tm::tm_map(docs, toSpace, "\\|")
        
        
        # Convert the text to lower case
        docs <- tm::tm_map(docs, content_transformer(tolower))
        
        # Remove numbers
        if(remove_numbers){
                docs <- tm::tm_map(docs, removeNumbers) 
        }
        
        ## remove stop words 
        if(remove_stop_words){
                docs <- tm::tm_map(docs, removeWords, c(stopwords("SMART") ,stopwords("english"))) 
        }
        
        # remove specific words
        if(remove_specific_words){
                
                docs <- tm::tm_map(docs, removeWords, specific_words)
                
        }
        # Remove punctuations
        if(remove_punctuation){
                docs <- tm::tm_map(docs, removePunctuation)  
        }
        
        # Eliminate extra white spaces
        docs <- tm::tm_map(docs, stripWhitespace)
        
        if(text_stemming) {
                # Text stemming
                docs <- tm::tm_map(docs, stemDocument)  
        }
        return(docs)
}



#' get github repo version devel bedge 
#'
#' @param pkg github repo url
#' @param col bedge color
#' @param add_github_logo TRUE if github logo to be shown on badge 
#' @return url
#' @keywords internal
#' @importFrom badger badge_devel
badge_devel_url <- function(pkg ="cparsania/fungiexpresz" ,col = "green", add_github_logo =FALSE){
        
        url <- badger::badge_devel(pkg = pkg, color = col) %>% 
                stringr::str_replace_all(pattern = "\\)|\\(|^\\W+" , replacement = "") %>% 
                stringr::str_split(pattern = "\\]" , simplify = T) %>% 
                .[1] 
        
        if(add_github_logo){
                url <- url  %>% paste0("?logo=github" , collapse = "")
        }
                return(url)
}

#' get custom badge url
#'
#' @param x text to show on left panel of badge
#' @param y text to show on right panel of badge
#' @param col badge color
#' @param add_github_logo TRUE if github logo to be shown on badge
#' @return url
#' @importFrom badger badge_custom
#' @keywords internal
badge_custom_url <- function(x = "current", y = NULL , col = "red" , add_github_logo =FALSE){
        url <- badger::badge_custom(x ,y , color = col)  %>% 
                stringr::str_replace_all(pattern = "^\\W+|\\)" , replacement = "")  
        
        if(add_github_logo){
                url <- url  %>% paste0("?logo=github" , collapse = "")
        }
                return(url)
}




 # get_gg_themes <- function(theme_name){
 # 
 #   availabel_themes <- c("theme_grey", "theme_gray", "theme_bw","theme_linedraw" , "theme_light" ,"theme_dark" ,"theme_minimal" ,"theme_classic", 
 #                         "theme_void","theme_test")  
 #   theme_name = "dsfsdaf"
 #   
 #   required_theme <- switch(theme_name,  
 #          "theme_grey"="theme_grey()",
 #          "theme_gray"="theme_gray()",
 #          "theme_bw"="theme_bw()",
 #          "theme_linedraw"="theme_linedraw()",
 #          "theme_light"="theme_light()",
 #          "theme_dark"="theme_dark()",
 #          "theme_minimal"="theme_minimal()",
 #          "theme_classic"="theme_classic()",
 #          "theme_void"="theme_void()",
 #          "theme_test"="theme_test()"
 #          )
 #   
 #   theme_default = "theme_bw()"
 #   if(is.null(required_theme)) required_theme = theme_default 
 # 
 #   return(required_theme)
 # }
 


