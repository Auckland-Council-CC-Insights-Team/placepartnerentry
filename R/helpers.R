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



#' Process A Response To Extract Form ID
#' @description This extract the form ID from the response.
#'
#' @param resp The response from the Google Forms API request.
#'
#' @return form_id
#' @export

process_response <- function(resp){
  out <- gargle::response_process(resp)
  form_id <- out[["formId"]]

  return(form_id)
}




#' Get Current Date, Month, And Year
#' @description This returns current date, month, and year as a list.
#' @return list(date, month, year)
#' @export


get_date <- function(){
  date <- Sys.Date()
  month <- format(date, "%B")
  year <- format(date, "%Y")

  return(list(date=date, month=month, year=year))
}




#' Get Form Description
#'
#' @return descrip
#' @export


get_form_descrip <- function(){
  current_date <- get_date()
  descrip <- paste("Please enter your participation data for the month of",
                   current_date$month, current_date$year)

  return(descrip)

}





#' Update Form Description
#' @param json_file The json_file
#' @param form_id The id of the form
#' @param items The list of items
#' @return form_update_resp
#' @export

update_form <- function(json_file, form_id){
  form_update <- list(
    requests = list(
      list(
        updateFormInfo = list(
          info = list(
            description = get_form_descrip()
          ),
          updateMask = "description"
        )
      )
    )
  )


  form_update_create <- gargle::request_build(
    method = "POST",
    path = paste0("v1/forms/", form_id, ":batchUpdate"),
    token = create_token(json_file),
    base_url = "https://forms.googleapis.com",
    body = form_update
  )


  form_update_resp <- gargle::request_make(form_update_create)

  return(form_update_resp)

}





#' Create Form Questions
#'
#' @param json_file The json file
#' @param form_id The id of the form
#' @param items The list of questions
#'
#' @return question_resp
#' @export

create_question <- function(json_file, form_id, items){
  question_resp_list <- list()
  for (i in 1:length(items)) {
    question_dat <- list(
      requests = list(
        list(
          createItem = list(
            item = items[[i]],
            location = list(
              index = i - 1
            )
          )
        )
      )
    )


    question_create <- gargle::request_build(
      method = "POST",
      path = paste0("v1/forms/", form_id, ":batchUpdate"),
      token = create_token(json_file),
      base_url = "https://forms.googleapis.com",
      body = question_dat
    )


    question_resp <- gargle::request_make(question_create)
    question_resp_list[[i]] <- question_resp

    Sys.sleep(1)

  }

  return(question_resp_list)
}






