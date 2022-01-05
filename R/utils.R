# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



ask <- function(...) {
  choices <- c("Yes", "No")
  cat(paste0(..., collapse = ""))
  utils::menu(choices) == which(choices == "Yes")
}

write_csv_output <- function(x, path, split = NULL) {

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(split)) {
    readr::write_csv(x, file = path, na = "")
    return(path)
  }

  if (!split %in% names(x)) {
    stop(split, " is not a column in the data frame", call. = FALSE)
  }

  x_list <- split(x, x[[split]])
  x_paths <- file.path(dirname(path), paste(names(x_list), basename(path), sep = "_"))

  purrr::walk2(x_list, x_paths, ~ {
    readr::write_csv(x = .x, file = .y, na = "")
  })
  x_paths
}
