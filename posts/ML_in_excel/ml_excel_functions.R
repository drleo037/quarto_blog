# thanks here to: https://stackoverflow.com/questions/45579287/r-assign-class-using-mutate
add.formula <- function(x) {class(x) <- c(class(x), "formula"); x}

get_excel_letter <- function(x) {
  paste0(LETTERS[((x-1) %/% 26)], LETTERS[1+((x-1) %% 26)])
}

augment_df_with_rules <- function(right_way_in, in_df, method = "classification") {
  # were' going to insert a few stats cols after the input data so
  # the start of the trees will be at this column:
  nstats <- 3 # I'm adding 3 extra columns (best, confidence and match)
  trg_col <- get_excel_letter(ncol(in_df))
  mod_col <- get_excel_letter(ncol(in_df)+1)
  first_model_col <- get_excel_letter(ncol(in_df)+nstats+1)
  last_model_col  <- get_excel_letter(ncol(right_way_in)+ncol(in_df)+nstats)

  result_df <- in_df %>%
    as_tibble() %>%
    bind_cols(right_way_in)
  
  if(method == "classification") {
    result_df <- result_df %>%
      mutate(tree_best = glue::glue("=INDEX({first_model_col}[ROW_NUM]:{last_model_col}[ROW_NUM], MODE(MATCH({first_model_col}[ROW_NUM]:{last_model_col}[ROW_NUM], {first_model_col}[ROW_NUM]:{last_model_col}[ROW_NUM], 0 )))"), .before = tree_1) %>% # TODO make generic, not just row [ROW_NUM]
      mutate(tree_confidence = glue::glue("=COUNTIF({first_model_col}[ROW_NUM]:{last_model_col}[ROW_NUM], E[ROW_NUM]) / COUNTA({first_model_col}[ROW_NUM]:{last_model_col}[ROW_NUM])"), .before = tree_1) %>%
      mutate(tree_match = glue::glue("={mod_col}[ROW_NUM]={trg_col}[ROW_NUM]"), .before = tree_1)
  } else {
    result_df <- result_df %>%
      mutate(tree_best = glue::glue("=AVERAGE({first_model_col}[ROW_NUM]:{last_model_col}[ROW_NUM])"), .before = tree_1) %>% # TODO make generic, not just row [ROW_NUM]
      mutate(tree_confidence = glue::glue("=1-SQRT(VAR({first_model_col}[ROW_NUM]:{last_model_col}[ROW_NUM], E[ROW_NUM])) / AVERAGE({first_model_col}[ROW_NUM]:{last_model_col}[ROW_NUM])"), .before = tree_1) %>%
      mutate(tree_match = glue::glue("=({mod_col}[ROW_NUM]-{trg_col}[ROW_NUM])/{trg_col}[ROW_NUM]"), .before = tree_1)
  }
  
  result_df <- result_df %>%
    mutate(row_num = as.character(row_number()+1)) %>% # +1 because in excel there's a title in row 1
    mutate_at(vars(starts_with("tree")), list(~ str_replace_all(., "\\[ROW_NUM\\]", row_num))) %>%
    mutate_at(vars(starts_with("tree")), add.formula) %>%
    select(-row_num)
  
  return(result_df)
}

# function to process the format returned by tidypredict_sql into
# a format that excel can parse
# sql_to_excel ----
sql_to_excel <- function(trees_df, input_df, n_sf = 1, squishit = F) {
  
  equations_in_cols_a <- trees_df %>%
    # adapt
    # the kinds of things you find output from tidypredict_sql
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
    equations_in_cols_a <- equations_in_cols_a  %>%
      mutate(output = str_replace_all(output, "\\d+\\.\\d+", function(x) as.character(round(as.numeric(x), n_sf))))
  }
  
  # remove any unnecessary spaces that are only really there to aid the human eye
  if(squishit) {
    equations_in_cols_a <- equations_in_cols_a  %>%
      mutate(output = str_replace_all(output, " ", ""))
  }
  
  
  library(tidytext)
  # we are going to swap references to the column names in R
  # with column names in excel (e.g. Sepal.Width becomes A[ROW_NUM])
  # we're adding [ROW_NUM] because later we're going to have many rows of calcs
  # substitute A[ROW_NUM], with A[1], A[2] etc.
  # this dataframe has two columns,  the R column name and the A/B/C etc for excel
  # we'll subsitute the R ones with th excel cols references later
  replacements <- names(input_df) %>%
    tolower() %>%
    enframe(name = NULL, value = "word") %>%
    mutate(col_letter = paste0(LETTERS[row_number()], "[ROW_NUM]"))

  # ultimately we'll be having one equation per column
  # for now there's one equation per row
  # we're going to want to labels them rule_1...rule_n
  # (these will be the col titles) in excel)
  # AND ...
  # we want to substitute the original variable names in the equations
  # with the correspondinh excel column references
  equations_in_cols <- equations_in_cols_a %>%
    # make a reference to the column names...
    mutate(tree_number = row_number()) %>%
    # unpack the (wide) equation into (long) parts so we can get at the variables
    unnest_tokens(word, output, token = "regex", pattern = "`") %>%
    # we only need a couple of the columns going forwards
    select(tree_number, word) %>%
    # do a lookup find&replace using left_join
    left_join(replacements) %>%
    # then coalesce, as this will sub-in col_letter is it's defined, and word if not
    mutate(new_word = coalesce(col_letter, word)) %>%
    # now we can repack the (long) parts of the equations into whole (wide) equation
    # grouping by tree_number will work on all the components of each equation in turn
    group_by(tree_number) %>%
    # summarise paste0 concatenates the rows into one (wide) reconstructed equation
    summarise(output = paste0(new_word, collapse = ''))
  
  #| transpose (flip) the array so that
  #| the equations are in columns rather than rows
  equations_in_rows <- equations_in_cols %>%
#    mutate(tree_number = paste0("tree_", tree_number)) %>%
    gather(key = var_name, value = value, 2:ncol(equations_in_cols)) %>% 
    spread(key = names(equations_in_cols)[1],value = 'value') %>%
    rename_with(~ paste0("tree_", .), -var_name) %>%
    select(-var_name)
  
  return(equations_in_rows)
} 
#usage:
# theRes <- sql_to_excel(trees_df_clas, iris)
# theRes["tree_1"]


# fit_to_excel ----
fit_to_excel <- function(trees_df, input_df, n_sf = 1, squishit = F) {
  
  equations_in_cols_a <- trees_df %>%
    # adapt
    # the kinds of things you find output from tidypredict_fit
    mutate(output = instruction) %>%
    mutate(output = str_replace_all(output, "ifelse", "if")) %>%
    mutate(output = str_replace_all(output, "==", "=")) %>%
    mutate(output = str_squish(output))
  
  # limit to a certain number of significant figures
  if(exists("n_sf")) {
    equations_in_cols_a <- equations_in_cols_a  %>%
      mutate(output = str_replace_all(output, "\\d+\\.\\d+", function(x) as.character(round(as.numeric(x), n_sf))))
  }
  
  # remove any unnecessary spaces that are only really there to aid the human eye
  if(squishit) {
    equations_in_cols_a <- equations_in_cols_a  %>%
      mutate(output = str_replace_all(output, " ", ""))
  }
  
  
  library(tidytext)
  # we are going to swap references to the column names in R
  # with column names in excel (e.g. Sepal.Width becomes A[ROW_NUM])
  # we're adding [ROW_NUM] because later we're going to have many rows of calcs
  # substitute A[ROW_NUM], with A[1], A[2] etc.
  # this dataframe has two columns,  the R column name and the A/B/C etc for excel
  # we'll subsitute the R ones with th excel cols references later
  replacements <- names(input_df) %>%
    tolower() %>%
    enframe(name = NULL, value = "word") %>%
    mutate(col_letter = paste0(LETTERS[row_number()], "[ROW_NUM]"))
  
  # ultimately we'll be having one equation per column
  # for now there's one equation per row
  # we're going to want to labels them rule_1...rule_n
  # (these will be the col titles) in excel)
  # AND ...
  # we want to substitute the original variable names in the equations
  # with the corresponding excel column references
  for(i in 1:nrow(equations_in_cols_a)) {
    row <- equations_in_cols_a[i,]$output
#    print(as.character(row))
    row <- stringi::stri_replace_all_fixed(row, replacements$word, replacements$col_letter, vectorize_all=FALSE)
#    print(row)
    equations_in_cols_a[i,]$output <- row
  } 
  
  equations_in_cols <- equations_in_cols_a %>%
    select(output) %>%
    mutate(tree_number = row_number(), .before = 1)
  #| transpose (flip) the array so that
  #| the equations are in columns rather than rows
  equations_in_rows <- equations_in_cols %>%
    select(output) %>%
    mutate(tree_number = row_number(), .before = 1) %>%
    gather(key = var_name, value = value, 2:ncol(equations_in_cols)) %>% 
    spread(key = names(equations_in_cols)[1],value = 'value') %>%
    rename_with(~ paste0("tree_", .), -var_name) %>%
    select(-var_name)
  
  return(equations_in_rows)
} 
