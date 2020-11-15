#' @title 
#' Bring a Column to Position 1 
#' 
#' @inheritParams wrapper_args
#' 
#' @example inst/examples/position.R
#' @seealso 
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#'  
#' @rdname to_position_1
#' @export 
#' @importFrom dplyr select everything


to_position_1 <- 
        function(data,
                 col) {
                
                
                data %>%
                        dplyr::select({{ col }},
                                      dplyr::everything())
                
                
        }


#' @title 
#' Bring a Column to the Last Position
#' 
#' @inheritParams wrapper_args
#' 
#' @example inst/examples/position.R
#' @seealso 
#'  \code{\link[dplyr]{select}},
#'  \code{\link[dplyr]{bind_cols}},
#'  \code{\link[dplyr]{reexports}},
#'  
#' @rdname to_last_position
#' @export 
#' @importFrom dplyr select everything bind_cols

to_last_position <- 
        function(data,
                 col) {
                
                
                
                dplyr::bind_cols(
                        data %>%
                                dplyr::select(-{{ col }}),
                        data %>%
                                dplyr::select({{ col }})
                )
                                    
        }

#' @title 
#' Bring a Column to the Last Position
#' 
#' @inheritParams wrapper_args
#' 
#' @example inst/examples/position.R
#' @seealso 
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{bind}}
#' @rdname to_position_n
#' @export 
#' @importFrom dplyr select select_at all_of bind_cols

to_position_n <- 
        function(data,
                 col,
                 n) {
                
                
                if (!(n %in% 1:ncol(data))) {
                        
                        stop("`n` must be in range of 1:ncol(data)")
                }
                
                
                if (n == 1) {
                        
                        to_position_1(
                                data = data,
                                col = {{ col }}
                        )
                        
                        
                } else if (n == ncol(data)) {
                        
                        to_last_position(data = data,
                                         col = {{ col }})
                        
                }
                
                
                input <- 
                        data %>%
                        dplyr::select(-{{ col }})
                
                
                
                # Index
                part_a <- 1:(n-1)
                part_c <- n:ncol(input)
                
                
                output_a <- 
                        input %>%
                        dplyr::select_at(vars(dplyr::all_of(part_a))) 
                
                output_b <- 
                        data %>%
                        dplyr::select({{ col }})
                
                
                output_c <- 
                        input %>%
                        dplyr::select_at(vars(dplyr::all_of(part_c)))
                
                
                
                
                dplyr::bind_cols(
                        output_a,
                        output_b,
                        output_c
                )
                
        }
