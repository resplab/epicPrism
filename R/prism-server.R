get_my_name<-function()
{
  x<-getPackageName()
  return(x)
}


thisSession<-new.env()

thisSession$MODE_REQUIRE_API_KEY=TRUE;
thisSession$MODE_REQUIRE_SESSION=FALSE;
thisSession$MODE_REQUIRE_SESSION_DATA=FALSE;

thisSession$LONG_RUN_STATUS_READY<-0
thisSession$LONG_RUN_STATUS_DONE<-1
thisSession$LONG_RUN_STATUS_ERROR<- -1

thisSession$redis_connection_status <- 0  #0:not connected; 1:connected
thisSession$REDIS_ADDRESS = "srv-captain--persistent-redis"
thisSession$REDIS_PORT <- 3001

thisSession$MODEL_DESCRIPTION<-paste0("This is ",get_my_name()," - PRISM enabled!")
thisSession$MODEL_VERSION<-paste(packageVersion(get_my_name()))


#FOR ONE SHOT JSON CALL!
#Can authenticate the user either by API key or by a session_id. 
#' @export
gateway<-function(...)
{
  arguments=list(...)
  func<-arguments$func
  
  session_id<-arguments$session_id
  
  if(is.null(session_id)) session_id=""
  
  session_id<<-session_id
  
  arguments$func<-NULL
  arguments$api_key<-NULL
  arguments$session_id<-NULL
  
  
  if(length(arguments)==0) {
    out<-eval(parse(text=paste(func,"()")))
  }
  else {
    out<-do.call(func, args = arguments)
  }
  
  return(jsonlite::toJSON(out))
}




#' @export
gatewayasync<-function(...)
{
  token <- generate_token()
  
  arguments=list(...)
  func<-arguments$func
  
  session_id<-arguments$session_id
  
  if(is.null(session_id)) session_id=""
  
  session_id<<-session_id
  
  arguments$func<-NULL
  arguments$api_key<-NULL
  arguments$session_id<-NULL
  
  token <- generate_token()
  
  redisConnect(host = thisSession$REDIS_ADDRESS, port = thisSession$REDIS_PORT, password = Sys.getenv("REDIS_PASSWORD"))
  redisSet(paste0("AS:status:",token),"[READY]")
  redisSet(paste0("AS:status_time:",token), Sys.time())
  redisSet(paste0("AS:status_data:",token),
           list(model_name=get_my_name(),
                func=func,
                arguments=arguments
                )
          )
  redisClose()
  
  out <- list(token=token, error_code=0)

  return(jsonlite::toJSON(out))
}





#' @export
prism_model_run<-function(model_input=NULL)
{
  return(model_run(model_input))
}


#In API-based use without session ids this might seem a bit reduntant (it will not be required). But still good to check model availability
connect_to_model<-function(api_key="")
{
  model_name<-environmentName(environment(connect_to_model))
  out<-list(error_code=0,session_id="",version="",description="")
  
  
  if(thisSession$MODE_REQUIRE_SESSION)
  {
    session_id<-generate_session_id()
    out$session_id<-session_id
  }
  
  out$version<-thisSession$MODEL_VERSION
  out$description<-thisSession$MODEL_DESCRIPTION
  return(out)
}


generate_session_id<-function()
{
  id<-paste(c(sample(letters,1) , sample(c(letters,0:9),9,TRUE)),collapse="")
  return(id)
}



generate_token<-function()
{
  id<-paste(c(sample(letters,1) , sample(c(letters,0:9),9,TRUE)),collapse="")
  return(id)
}



#' @export
prism_get_output_structure<-function()
{
  out<-list(
    n_agents=prism_output(source="$n_agents", type = "numeric/scalar", group = "", title = "Number of simulated individuals", description = ""),
    cumul_time=prism_output(source="$cumul_time",type = "numeric/scalar", title = "cumulative time"),
    n_deaths=prism_output(source="$n_deaths",type = "numeric/scalar", title = "number of deaths"),
    n_COPD=prism_output(source="$n_COPD",type = "numeric/scalar", title = "Number of patients with COPD"),
    total_exac=prism_output(source="$total_exac",type = "numeric/vector", title = "Total number of exacerbations by severity"),
    total_exac_time=prism_output(source="$total_exac_time",type = "numeric/vector", title = "total_exac_time"),
    total_pack_years=prism_output(source="$total_pack_years",type = "numeric/scalar", title = "Total pack-years"),
    total_doctor_visit=prism_output(source="$total_doctor_visit",type = "numeric/vector", title = "Total doctor visits"),
    total_cost=prism_output(source="$total_cost",type = "numeric/scalar", title = "Total costs"),
    total_qaly=prism_output(source="$total_qaly",type = "numeric/scalar", title = "Total QALY")
  )
  return(out)
}



set_var<-function(variable,value)
{
  .GlobalEnv[[variable]]<-value
}


get_var<-function(variable)
{
  return(.GlobalEnv[[variable]])
}




prism_get_async_results <- function(token=NULL)
{
  if(is.null(token))
  {
    stop("Token was not provided.")
    return()
  }
  
  redisConnect(host = thisSession$REDIS_ADDRESS, port = thisSession$REDIS_PORT, password = "H1Uf5o6326n6C2276m727cU82O")
  
  status <- redisGet(paste0("AS:status:",token))
  if(is.null(status))
  {
    stop("Job with this token not found.")
  }
  status_data <- redisGet(paste0("AS:status_data:",token))
  
  redisClose()
  
  return(list(status=status,status_data=status_data))
}


