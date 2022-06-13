label_with_info <- function(label, tooltip_id) {
  
  x <- HTML(sprintf("%s <button id=\"%s\" type=\"button\" class=\"btn btn-light btn-xs\"><i class=\"fa fa-info\"></i></button>", 
                    label, tooltip_id))
  
  return(x)
  
}