# thanks here to: https://stackoverflow.com/questions/45579287/r-assign-class-using-mutate
add.formula <- function(x) {class(x) <- c(class(x), "formula"); x}

augment_df_with_rules <- function(right_way_in, in_df, method = "classification") {
  # were' going to insert a few stats cols after the input data so
  # the start of the trees will be at this column:
  trg_col <- LETTERS[ncol(in_df)] # TODO
  mod_col <- LETTERS[ncol(in_df)+1] # TODO
  x <- LETTERS[ncol(in_df)+4]
  
  result_df <- in_df %>%
    as_tibble() %>%
    bind_cols(right_way_in)
  
  if(method == "classification") {
    result_df <- result_df %>%
      mutate(tree_best = glue::glue("=INDEX({x}[ROW_NUM]:SN[ROW_NUM], MODE(MATCH({x}[ROW_NUM]:SN[ROW_NUM], {x}[ROW_NUM]:SN[ROW_NUM], 0 )))"), .before = tree_1) %>% # TODO make generic, not just row [ROW_NUM]
      mutate(tree_confidence = glue::glue("=COUNTIF({x}[ROW_NUM]:SN[ROW_NUM], E[ROW_NUM]) / COUNTA({x}[ROW_NUM]:SN[ROW_NUM])"), .before = tree_1) %>%
      mutate(tree_match = glue::glue("={mod_col}[ROW_NUM]={trg_col}[ROW_NUM]"), .before = tree_1)
  } else {
    result_df <- result_df %>%
      mutate(tree_best = glue::glue("=AVERAGE({x}[ROW_NUM]:SN[ROW_NUM])"), .before = tree_1) %>% # TODO make generic, not just row [ROW_NUM]
      mutate(tree_confidence = glue::glue("=1-SQRT(VAR({x}[ROW_NUM]:SN[ROW_NUM], E[ROW_NUM])) / AVERAGE({x}[ROW_NUM]:SN[ROW_NUM])"), .before = tree_1) %>%
      mutate(tree_match = glue::glue("=({mod_col}[ROW_NUM]-{trg_col}[ROW_NUM])/{trg_col}[ROW_NUM]"), .before = tree_1)
  }
  
  result_df <- result_df %>%
    mutate(row_num = as.character(row_number()+1)) %>% # +1 because in excel there's a title in row 1
    mutate_at(vars(starts_with("tree")), list(~ str_replace_all(., "\\[ROW_NUM\\]", row_num))) %>%
    mutate_at(vars(starts_with("tree")), add.formula) %>%
    select(-row_num)
  
  return(result_df)
}


# function to process the format returned by tidypredict_sql into excel
# sql_to_excel ----
sql_to_excel <- function(trees_df, input_df, n_sf = 1, squishit = F) {
  replacements <- names(input_df) %>%
    tolower() %>%
    enframe(name = NULL, value = "word") %>%
    mutate(col_letter = paste0(LETTERS[row_number()], "[ROW_NUM]"))
  
  wrong_way_clas_a <- trees_df %>%
    mutate(instruction = str_replace_all(instruction, "AND", ",")) %>%
    mutate(instruction = str_replace_all(instruction, "CASE\nWHEN", "=IF(AND")) %>%
    mutate(n_parts = str_count(instruction, "\n"), .before = instruction) %>%
    mutate(instruction = str_replace_all(instruction, "WHEN", ", IF(AND")) %>%
    mutate(instruction = str_replace_all(instruction, "THEN", ", ")) %>%
    mutate(instruction = str_replace_all(instruction, "END", ", 'SHOULDNTHAPPEN'")) %>%
    #  mutate(instruction = str_replace_all(instruction, "\n", ", ")) %>%
    mutate(end = str_pad(string = "", width = n_parts, side = "right", pad = ")"), .after = n_parts) %>%
    mutate(output = paste0(instruction, end)) %>%
    mutate(output = str_replace_all(output, "'", '"')) %>%
    mutate(output = str_squish(output))
  
  # limit to a certain number of significant figures
  if(exists("n_sf")) {
    wrong_way_clas_a <- wrong_way_clas_a  %>%
      mutate(output = str_replace_all(output, "\\d+\\.\\d+", function(x) as.character(round(as.numeric(x), n_sf))))
  }
  
  if(squishit) {
    wrong_way_clas_a <- wrong_way_clas_a  %>%
      mutate(output = str_replace_all(output, " ", ""))
  }
  
  
  library(tidytext)
  wrong_way_clas <- wrong_way_clas_a %>%
    mutate(rule_n = row_number()) %>%
    # unpack so we can get the variables
    unnest_tokens(word, output, token = "regex", pattern = "`") %>%
    select(rule_n, word) %>%
    # do a lookup find&replace using left_join
    left_join(replacements) %>%
    mutate(new_word = coalesce(col_letter, word)) %>%
    # repack into whole functions
    group_by(rule_n) %>%
    summarise(output = paste0(new_word, collapse = '')) %>%
    select(output) %>%
    mutate(tree_number = row_number(), .before = output)
  
  #| transpose (flip) the array so that
  #| the equations are in columns rather than rows
  right_way_clas <- wrong_way_clas %>%
    mutate(tree_number = paste0("tree_", tree_number)) %>%
    #  head(20) %>%
    gather(key = var_name, value = value, 2:ncol(wrong_way_clas)) %>% 
    spread(key = names(wrong_way_clas)[1],value = 'value') %>%
    select(-var_name)
  
  return(right_way_clas)
} 
#usage:
# theRes <- sql_to_excel(trees_df_clas, iris)
# theRes["tree_1"]
