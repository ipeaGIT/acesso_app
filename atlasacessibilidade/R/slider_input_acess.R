

slider_input_acess <- function(label = "Escolha o tempo de viagem:", ...) {
  
  label_format <- h1(label)
  
  sliderInput(label = label_format, animate = animationOptions(interval = 2000), post = " min", ...)
  
  
  
}


slider_input_acess(inputId = "tempo_tp",
                   min = 30, max = 120,
                   step = 30, value = 30)
