model_run<-function(model_input)
{
  input<-unflatten_list(model_input)
  
  debug<-input$debug
  input$debug<-NULL
  
  settings<-epicR::get_default_settings()
  new_settings<-input$setting
  
  l<-length(new_settings)
  if(l>0)
    for(i in 1:l)
    {
      settings[names(new_settings[i])]<-new_settings[[i]]
    }
  
  input$settings<-NULL
  
  init_session(settings=settings)
  
  res<-epicR::run(input=input)
  
  output_ex<<-Cget_output_ex()
  
  plot(output_ex$n_alive_by_ctime_sex[,1],type='l',col='red')
  lines(output_ex$n_alive_by_ctime_sex[,1],type='l',col='green')
  
  data(cars)
  plot(cars)
  
  terminate_session()
  
  if(!is.null(debug$input_back) && debug$input_back==1)
  {
    input_back<-list()
    input_back$setting<-Cget_settings()
    input_back$input<-Cget_inputs()

    if(res==0)
      return(c(flatten_list(Cget_output()),Cget_output_ex(),flatten_list(input_back)))
    else
      return(c(list(result=FALSE),flatten_list(input_back)))
    
  }
  else
  {
    if(res==0)
      return(c(flatten_list(Cget_output()),Cget_output_ex()))
    else
      return(list(result=FALSE))
  }
}



#' @export
get_output_ex<-function()
{
  return(output_ex)
}



get_default_input<-function()
{
  model_input<-init_input()$values
  return(flatten_list(model_input))
}


get_output_structure<-function()
{
  
}



#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }
    
  for(i in 1:length(lst))
  {
    nm<-names(lst[i])
    
    message(nm)
    
    if(prefix!="")  nm<-paste(prefix,nm,sep=".")
    
    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}







#Gets a hierarchical named list and flattens it; updating names accordingly
unflatten_list<-function(lst)
{
  if(is.null(lst)) return(lst)
  out<-list()
  
  nms<-names(lst)
  
  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }
  
  return(out)
}