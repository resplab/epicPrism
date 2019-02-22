model_run<-function(model_input)
{
  res<-epicR::run(input=unflatten_list(model_input))
  return(toJSON(res))
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
  out<-list()
  for(i in 1:length(lst))
  {
    nm<-names(lst[i])
    
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
  out<-list()
  
  nms<-names(lst)
  
  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }
  
  return(out)
}