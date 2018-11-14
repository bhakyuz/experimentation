# Lets shape a bit mtcars data
mtcars2 <- mtcars %>%
  dplyr::mutate(
    automobile = rownames(mtcars),
    brand = stringi::stri_replace_all(automobile, replacement = "", regex = " .*")
    )

set.seed(222)
# imaginary case
# a car manufacturer production units in milltions for each each brand 
mtcars.grouped <- mtcars2 %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(nb_of_models = dplyr::n_distinct(automobile) ) %>%
  dplyr::mutate(total_prodcution = sample(1:40, length(brand)))
mtcars.grouped
