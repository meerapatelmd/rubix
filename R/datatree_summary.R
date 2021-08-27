#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[purrr]{map}},\code{\link[purrr]{set_names}}
#'  \code{\link[dplyr]{tidyeval-compat}},\code{\link[dplyr]{group_by_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{bind}}
#'  \code{\link[tidyr]{unite}}
#'  \code{\link[data.tree]{as.Node}},\code{\link[data.tree]{as.Node.data.frame}},\code{\link[data.tree]{as.Node.dendrogram}},\code{\link[data.tree]{as.Node.list}},\code{\link[data.tree]{as.Node.phylo}},\code{\link[data.tree]{as.Node.rpart}}
#' @rdname classified_summary
#' @export 
#' @importFrom purrr map set_names
#' @importFrom dplyr as_label group_by_at vars all_of summarise ungroup distinct bind_rows
#' @importFrom tidyr unite
#' @importFrom data.tree as.Node



classified_summary <- 
        function(data, 
                 ...) {
                
                
                grouper_colnames <- 
                        enquos(...) %>%
                        purrr::map(dplyr::as_label) %>% 
                        unlist()
  
                output <- 
                        vector(mode = "list",
                               length = length(grouper_colnames)) %>%
                        purrr::set_names(grouper_colnames)
                
                for (i in 1:length(grouper_colnames)) {
                        
                        this_group_by <- 
                                grouper_colnames[1:i]
                        
                        output[[i]] <-
                        data %>%
                                dplyr::group_by_at(dplyr::vars(dplyr::all_of(this_group_by))) %>% 
                                dplyr::summarise(group_count = n(), .groups = "drop") %>% 
                                dplyr::ungroup() %>% 
                                dplyr::distinct() %>% 
                                tidyr::unite(col = pathString, 
                                             dplyr::all_of(this_group_by), 
                                             sep = "/")
                        
                        
                        
                        
                }
                
                output <- 
                        output %>% 
                        dplyr::bind_rows()
                
                
                # groupers <- 
                #         dplyr::enquos(...)
                # 
                # summarized_data <- 
                # data %>% 
                #         dplyr::group_by(!!!groupers) %>% 
                #         dplyr::summarise(group_count = n(), 
                #                          .groups = "drop") %>% 
                #         dplyr::ungroup() %>% 
                #         tidyr::unite(col = "pathString",
                #                      !!!groupers, 
                #                      sep = "/")

                

                        data.tree::as.Node(summarized_data,
                                           na.rm = FALSE)
                
                
                
        }