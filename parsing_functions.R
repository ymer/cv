# Take a position dataframe and the section id desired
# and prints the section to markdown. 
print_section <- function(position_data, section_id){
  position_data %>% 
    filter(section == section_id) %>% 
    arrange(desc(end)) %>% 
    mutate(id = 1:n()) %>% 
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description',
      values_drop_na = TRUE
    ) %>% 
    group_by(id) %>% 
    mutate(
      descriptions = list(description)
    ) %>% 
    ungroup() %>% 
    filter(description_num == 'description_1') %>% 
    mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue('{end} - {start}')
      ),
      description_bullets = map_chr(descriptions, ~paste('-', ., collapse = '\n')),
    ) %>% 
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
      glue_data(
        "### {title}",
        "\n\n",
        "{loc}",
        "\n\n",
        "{institution}",
        "\n\n",
        "{timeline}", 
        "\n\n",
        "{description_bullets}",
        "\n\n\n")
}

