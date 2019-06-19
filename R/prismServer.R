#Last update: 2019.02.12
#Remember to run redis otherwise it will get stuck on redisConnect



get_my_name<-function()
{
  x<-getPackageName()
  return(x)
}



thisSession<-new.env()

thisSession$redis_connection_status<-0

thisSession$REDIS_ADDRESS="prism.resp.core.ubc.ca"
thisSession$REDIS_PORT <- 3001
 
thisSession$MODE_REQUIRE_API_KEY=TRUE;
thisSession$MODE_REQUIRE_SESSION=FALSE;
thisSession$MODE_REQUIRE_SESSION_DATA=FALSE;

thisSession$LONG_RUN_STATUS_READY<-0
thisSession$LONG_RUN_STATUS_DONE<-1
thisSession$LONG_RUN_STATUS_ERROR<- -1

thisSession$MODEL_DESCRIPTION<-paste0("This is ",get_my_name()," - PRISM enabled!")
thisSession$MODEL_VERSION<-paste(packageVersion(get_my_name()))

connect_redis_prism <- function (){
  if(thisSession$redis_connection_status==0)
  {
    rredis::redisConnect(host = thisSession$REDIS_ADDRESS, port = thisSession$REDIS_PORT, password = "H1Uf5o6326n6C2276m727cU82O")
    thisSession$redis_connection_status<-1
  }
}




#' @export
test<-function(...)
{
  return(get_my_name())
}




#FOR ONE SHOT JSON CALL!
#Can authenticate the user either by API key or by a session_id. 
#' @export
gateway<-function(...)
{
  arguments=list(...)
  
  func<-arguments$func
  
  session_id<-arguments$session_id
  api_key<-arguments$api_key
  
  if(is.null(session_id)) session_id=""
  if(is.null(api_key)) api_key=""
  
  if(func!="test") check_access(api_key,session_id,func)
  
  #try(
  #{
      set_redis_var(paste("RT:LastModelCall:",api_key,sep=""),get_my_name())
      set_redis_var(paste("RT:LastCallTime:",api_key,sep=""),Sys.time())
      rredis::redisIncr(paste("RT:CallCount:",api_key,sep=""))
      rredis::redisRPush(paste("RT:CallTimes:",api_key,sep=""),Sys.time())
  #})

  session_id<<-session_id
  
  arguments$func<-NULL
  arguments$api_key<-NULL
  arguments$session_id<-NULL
  
  if(!is.null(session_id)) restore_session(session_id)
  
  if(length(arguments)==0) 
  {out<-eval(parse(text=paste(func,"()")))}
  else 
  {out<-eval(parse(text=paste(func,substring(deparse(arguments),5))))}
  
  if(!is.null(session_id)) save_session(session_id)
  
  return(jsonlite::toJSON(out))
}





#' @export
prism_model_run<-function(model_input)
{
  return(model_run(model_input))
}






save_session<-function(session_id)
{
  if(!thisSession$MODE_REQUIRE_SESSION_DATA) return()
  connect_redis_prism()
  e<-new.env()
  for(nm in names(globalenv()))
  {
    if(typeof(globalenv()[[nm]])!='closure')
    {
      e[[nm]]<-globalenv()[[nm]]
    }
  }
  rredis::redisSet(paste(session_id,"env",sep=":"),e)
}






restore_session<-function(session_id)
{
  if(!thisSession$MODE_REQUIRE_SESSION_DATA) return()
  connect_redis_prism()
  e<-rredis::redisGet(paste(session_id,"env",sep=":"))
  for(nm in names(e))
  {
    if(typeof(e[[nm]])!='closure')
    {
      .GlobalEnv[[nm]]<-e[[nm]]
    }
  }
}






#In API-based use without session ids this might seem a bit reduntant (it will not be required). But still good to check model availability
connect_to_model<-function(api_key="")
{
  model_name<-environmentName(environment(connect_to_model))
  out<-list(error_code=0,session_id="",version="",description="")
  
  if(thisSession$MODE_REQUIRE_API_KEY)
  {
    if(is.null(api_key))
    {
      out$error_code<--1
      out$version<-"1234"
      out$description<-"Error: access to the model requires a valid API key."
      return(out)
    }
  }
  
  if(thisSession$MODE_REQUIRE_SESSION)
  {
    session_id<-generate_session_id()
    set_redis_var(session_id,value = model_name)
    out$session_id<-session_id
  }
  
  out$version<-thisSession$MODEL_VERSION
  out$description<-thisSession$MODEL_DESCRIPTION
  return(out)
}






disconnect_from_model<-function()
{
  if(!is.null(session_id) && session_id!="")
  {
    connect_redis_prism()
    keys<-rredis::redisKeys(pattern = paste(session_id,"*",sep=""))
    rredis::redisDelete(keys)
    #To prevent recording of this session environment by the calling gateway.
    thisSession$MODE_REQUIRE_SESSION_DATA<-FALSE
    return(TRUE)
  }
  else
  {
    warning("This was not a sessioned connection. Nothing to disconnet.")
    return(FALSE)
  }
}









#Checks if the submitted api_key has the privilge to access the model. 
#Currently only authenticates based on api_key
check_access<-function(api_key="", session_id="", func=NULL)
{
  if(thisSession$MODE_REQUIRE_API_KEY==FALSE) return(TRUE)
  if(api_key=="") stop("ERROR: API key is required.")
  
  #try({
    #set_redis_var(paste("MODEL_ACCESS/",get_my_name(),":",api_key,sep=""),"kooni")
    x<-get_redis_var(paste("MT:Access:",get_my_name(),":",api_key,sep=""))
    if(is.null(x)) stop("ERROR: Unauthorized access.")
    if(is.numeric(x))
    {
      if(x>0) return(TRUE)
      stop(paste("Access denied - code:",x))
    }
    stop(paste("Access denied:",x))
  #})
}







generate_session_id<-function()
{
  id<-paste(c(sample(letters,1) , sample(c(letters,0:9),9,TRUE)),collapse="")
  return(id)
}












#' @export
model_run.long<-function(input)
{
  if(is.null(session_id) || session_id=="")
    stop("Error: long run is not available as this is a session-less connection")
  
  key<-paste(session_id,"status",sep=":")
  
  if(get_redis_var(key))
  {
    #There is already a job for this session in the que!
    return(FALSE)
  }
  else
    set_redis_var(key,0)
  
  return(TRUE)
}









#' @export
prism_check_run_progress<-function()
{
  if(is.null(session_id) || session_id=="")
    stop("Error: long run is not available as this is a session-less connection")
  
  key<-paste(session_id,"status",sep=":")
  
  val<-get_redis_var(key)
  
  if(val)
  {
    return(val)
  }
  else
    return(FALSE)
}











#This function is called by model code in a different R process. sesssion_infor should be passed from one process to another.
#' @export
prism_set_run_progress<-function(value)
{
  if(is.null(session_id) || session_id=="")
    stop("Error: long run is not available as this is a session-less connection")
  
  key<-paste(session_id,"status",sep=":")
  
  set_redis_var(key,value)
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



####################Redis example

set_redis_var<-function(variable,value)
{
  #TODO: connect should not be in these functions as it will cause multiple connection attempts!
  connect_redis_prism()
  rredis::redisSet(variable,value)
  return(TRUE)
}


get_redis_var<-function(variable)
{
  connect_redis_prism()
  x<-rredis::redisGet(variable)
  return(x)
}



delete_redis_var<-function(variable)
{
  connect_redis_prism()
  rredis::redisDelete(variable)
}





set_var<-function(variable,value)
{
  .GlobalEnv[[variable]]<-value
}


get_var<-function(variable)
{
  return(.GlobalEnv[[variable]])
}
































#######################################LONG RUN functions###############
#This is just a placeholder. These functions should not be here!

#browses through rredis for all <session_id:staus,0> pairs and call model run
prism_patrol<-function()
  
  
  
  model_run.long<-function(session_id)
  {
    
  }
