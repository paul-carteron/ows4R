#' GET_httr2
#' @description
#' Convert old GET methods from httr to httr2 syntax
#' 
#' @return nodeset obtain by `XML::xmlParse`
#' @noRD
#' 
#' @author Paul Carteron <carteronpaul@@gmail.com>

GET_httr2 = function(url, request, namedParams, mimeType){
  
  # param
  namedParams <- c(namedParams, request = request)
  
  # Part below can be removed because :
  # - "?" is automatically added with httr2 at the end of base url
  # - url is automatically encode
  
  # if(!endsWith(url,"?") && nzchar(params)) req <- paste0(req, "?")
  # if(regexpr("/cas?service=", url, fixed = T) > 0) params <- URLencode(params, reserved = TRUE)
  # req <- paste0(req, params)
  
  # headers
  headers <- private$headers
  if(!is.null(private$token)){
    headers <- c(headers, "Authorization" = paste(private$auth_scheme, private$token))
  }
  
  if (!is.null(mimeType)){
    headers <- c(headers, "content-type" = mimeType)
  }
  
  req <- request(url) |> 
    req_url_query(!!!namedParams) |> 
    req_headers(!!!headers) |> 
    req_options(!!!private$config)
  
  self$INFO(sprintf("Fetching %s", req$url))
  
  r <- NULL
  if(self$verbose.debug){
    r <- with_verbosity(req_perform(req), verbosity = 3)
  }else if(self$verbose.info){
    r <- req_perform(req) #|> req_progress() will be added in next httr2::version
  }else{
    r <- req_perform(req)
  }
  
  responseContent <- NULL
  if(is.null(mimeType)){
    responseContent <- resp_body_string(r, encoding = "UTF-8")
  }else{
    if(grepl("xml", mimeType) > 0){
      responseContent <- resp_body_xml(r) |> xmlParse()
    }else if (grepl("json", mimeType)){
      responseContent <- resp_body_json(r)
    }else{
      responseContent <- resp_body_string(r, encoding = "UTF-8")
    }
  }
  
  response <- list(request = request, requestHeaders = httr2::resp_headers(r),
                   status = httr2::resp_status(r), response = responseContent)
  return(response)
}
