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
  saveRDS(token, "token.rds")

  return(token)
}


#' Create A Google Form
#'
#' @param json_file The json_file
#' @param title The title of the form
#'
#' @return resp
#' @export
create_form <- function(json_file,title,token){
  form_create <- gargle::request_build(
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
    token = token,
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

create_question <- function(json_file, form_id, items, token){
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
      token = token,
      base_url = "https://forms.googleapis.com",
      body = question_dat
    )


    question_resp <- gargle::request_make(question_create)
    question_resp_list[[i]] <- question_resp

    Sys.sleep(1)

  }

  return(question_resp_list)
}




#' Generate Hash Key
#'
#' @param num_of_forms The number of Google forms
#'
#' @return dat
#' @export


generate_hash_key <- function(num_of_forms){

  input <- vector("list", num_of_forms)
  hash_key <- vector("list", num_of_forms)
  link <- vector("list", num_of_forms)


  orig_link <- "https://docs.google.com/forms/d/e/1FAIpQLScdr7BpeBbszlL1fogpYe4oQZ_nD4ao4Dbd-Sy3hIOQw9FhbA/viewform?usp=pp_url&entry.690249535=FOROFFICEUSEONLY"
  link2 <- substr(orig_link,1,nchar(orig_link)-16)


  for (i in 1:num_of_forms){
    input[[i]] <- paste(sample(c(0:9,letters,LETTERS),12,replace=T),collapse='')
    hash_key[[i]] <- digest(input[[i]], algo="sha256")
    link[[i]] <- paste0(link2,hash_key[[i]])
  }



  dat <- data.frame(
    "Hash_key" = unlist(hash_key),
    "Link" = unlist(link)
  )

  return(dat)
}
















