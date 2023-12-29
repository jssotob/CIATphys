#' physiology_data
#'
#' @return A list with metadata and data
#' @export
#'
#' @examples
#' \dontrun{
#' physiology_data()
#' }
physiology_data <- function() {
  pacman::p_load(dplyr)

  # users


  users <- c("jssoto", "JRICAURTE")
  api_key <- c("a00ecc62-240b-4ab1-8c4f-6192b0adecb6", "790c1e21-308d-433d-a7dd-7cd40ce5a6da")
  api_secret <- c("579f865f55aba4de799b2fbce7ae566f", "9408f52d2519ced695a4955b53dd5313")

  users_lab <- sapply(users, function(x) paste0("\t", which(users == x), ". ", x, " \n")) %>%
    unname() %>%
    paste0(., collapse = " ")


  user_id <- suppressWarnings((readline(paste0("Select a FormShare user: ", "\n \n", users_lab))) %>%
                             as.numeric())


  while (!is.numeric(user_id) | is.na(user_id) | user_id < 0 | user_id > length(users)) {
    user_id <- (readline(paste0("Wrong answer. Try again. Select a FormShare user: ", "\n \n", users_lab))) %>%
      as.numeric()
  }

  # Create a new connection to FormShare
  cat(paste0("Logging in ", users[user_id],"\n"))
  my_connection <- FormShare::FormShare$new(
    server_url = "https://formshare.alliance.cgiar.org",
    user_id = users[user_id],
    api_key = api_key[user_id],
    api_secret = api_secret[user_id]
  )
  # Login
  my_connection$login()

  # Get the repositories that you have access
  my_repositories <- my_connection$get_repositories()

  # data to search for
  cat("Logged in!!! \n \n")
  unicos <- my_repositories$project_name %>% unique()

  if (any(unicos == "NA")) {
    unicos <- unicos[-which(unicos == "NA")]
  }

  repos <- sapply(unicos, function(x) paste0("\t", which(unicos == x), ". ", x, " \n")) %>%
    unname() %>%
    paste0(., collapse = " ")

  repo <- suppressWarnings((readline(paste0("Repository to get the data from: ", "\n \n", repos))) %>%
    as.numeric())

  while (!is.numeric(repo) | is.na(repo) | repo < 0 | repo > length(unicos)) {
    repo <- (readline(paste0("Wrong answer. Try again. Repository to get the data from: ", "\n \n", repos))) %>%
      as.numeric()
  }

  unicos <- my_repositories %>%
    filter(project_name == unicos[repo]) %>%
    .$form_name %>%
    unique()

  repos <- sapply(unicos, function(x) paste0("\t", which(unicos == x), ". ", x, " \n")) %>%
    unname() %>%
    paste0(., collapse = " ")

  repo <- suppressWarnings((readline(paste0("Select one dataset to get the data: ", "\n \n", repos))) %>%
    as.numeric())

  while (!is.numeric(repo) | is.na(repo) | repo < 0 | repo > length(unicos)) {
    repo <- (readline(paste0("Wrong answer. Try again. Select one dataset to get the data: ", "\n \n", repos))) %>%
      as.numeric()
  }


  index <- which(my_repositories$form_name == unicos[repo])


  # Execute a query

  tables <- my_connection$get_tables(my_repositories$form_schema[index])$table_name

  metadata <- my_connection$execute(my_repositories$form_schema[index], paste0("SELECT * FROM ", tables[1]))

  if (length(tables) == 2) {
    data <- my_connection$execute(my_repositories$form_schema[index], paste0("SELECT * FROM ", tables[2]))
  } else {
    data <- lapply(
      tables[2:length(tables)],
      function(x) my_connection$execute(my_repositories$form_schema[index], paste0("SELECT * FROM ", x))
    )
    for (i in 1:length(data)) {
      names(data)[i] <- tables[i + 1]
    }
    rm(i)
  }

  return(list(
    form_name = unicos[repo],
    metadata = metadata,
    data = data
  ))
}
