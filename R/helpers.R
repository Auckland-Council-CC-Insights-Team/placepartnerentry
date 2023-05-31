create_token <- function(json_file){

my_app <- gargle_oauth_client_from_json(json_file)
scopes <- "https://www.googleapis.com/auth/forms.body"
token <-  credentials_user_oauth2(scopes, app = my_app)
return(token)

}

create_form <- function(token,title){

form_create <- request_build(
  method = "POST",
  path = "v1/forms",
  token = token,
  base_url = "https://forms.googleapis.com",
  body = list(
    info = list(
      title = title
    )
  )
)

resp <- request_make(form_create)
return(resp)
}
