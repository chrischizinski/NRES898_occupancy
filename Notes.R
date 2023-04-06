library(tidyverse)
library(osfr)

# install.packages("remotes")
# remotes::install_github("ropensci/osfr")


cr_project <- osfr::osf_retrieve_node("43PVJ")
osf_ls_files(cr_project)


osf_retrieve_file("5c2d277da7c0a7001b3d6310") %>%
  osf_ls_files() |> 
  as_tibble() |>
  rename(guid = id) -> osf_filez 


osf_filez$guid |> 
  purrr::walk(~osf_retrieve_file(.x) |> osf_download(path ="data/"))

osf_retrieve_file("5c2d36760e8efd0018cda55d") |> osf_download(path ="src/")

  

?osf_download
