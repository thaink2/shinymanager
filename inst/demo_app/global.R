require(shiny)
require(dplyr)
# require(markdown) # rsconnect

# Initialisation de la base via un data.frame
# credentials <- data.frame(
#   user = c("shiny", "shinymanager"),
#   password = c("shiny", "shinymanager"),
#   # password will automatically be hashed
#   admin = c(FALSE, TRUE), # utilisateurs avec droits d'admin ?
#   stringsAsFactors = FALSE
# )
th2mageai::initialize_envs(working_mod = "dev")

credentials <- th2product::fetch_data_from_db(table = "th2coldorg") %>%
  dplyr::distinct(username, .keep_all = T) %>%
  dplyr::rename(user = username) %>%
dplyr::mutate(is_hashed_password = F)

print(credentials)

# Initialisation de la base de données
# create_db(
#   credentials_data = credentials,
#   sqlite_path = "database.sqlite", # elle sera crée
#   passphrase = "passphrase_wihtout_keyring"
# )

create_sql_db(
    credentials_data = credentials,
    config_path = "pg_template.yml"
  )
