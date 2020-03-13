label_with_info <- function(label, tooltip_id) {
  
  x <- HTML(sprintf("<h1>%s <button id=\"%s\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button></h1>", 
                    label, tooltip_id))
  
  return(x)
  
}