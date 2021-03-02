
valueBox2 <- function (value, title, subtitle, icon = NULL, color = "aqua", 
                       width = 4, href = NULL) 
{
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon)) shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color), 
                    div(
                      class = "inner", 
                      tags$small(title), # adiciona o subtítulo
                      h3(value), 
                      p(subtitle)
                      ), 
                    if (!is.null(icon)) div(class = "icon-large", icon)
                    )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
    )
}