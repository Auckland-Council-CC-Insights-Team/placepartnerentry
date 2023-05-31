#' Create An Access Token For The Google API
#'
#' @description This creates an access token for the Google API based on
#'   credentials passed via a JSON file.
#'
#'
#' @param json_file File path to a JSON file with the user's API credentials
#'
#' @return token
#' @noRd
create_token <- function(json_file){

my_app <- gargle::gargle_oauth_client_from_json(json_file)
scopes <- "https://www.googleapis.com/auth/forms.body"
token <-  gargle::credentials_user_oauth2(scopes, app = my_app)
return(token)

}

#' Create A Google Form
#'
#' @param json_file The json_file
#' @param title The title of the form
#'
#' @return resp
#' @export
create_form <- function(json_file,title){

form_create <- gargle::request_build(
  method = "POST",
  path = "v1/forms",
  token = create_token(json_file),
  base_url = "https://forms.googleapis.com",
  body = list(
    info = list(
      title = title
    )
  )
)

resp <- gargle::request_make(form_create)
return(resp)
}
