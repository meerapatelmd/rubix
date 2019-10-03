
credit_card_df_ls$SPEND %>%
        mutate(date = ymd(date), cc_id = as.integer(cc_id), balance = as.numeric(balance)) %>%
        acast(date ~ cc_id, value.var = "balance", fill = NA) %>%
        mutate_at(vars(1:9), ~list(ifelse(is.na(.), )))

impute_by_carry_forward <-
        function(dataframe, ...) {
                
        }