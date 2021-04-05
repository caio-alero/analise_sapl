
metric_ui = function(title, description, value) {
  column(6, style = 'padding: 0; height: -webkit-fill-available; height: -moz-available;',
         div(id = 'card',
             div(id = 'content', style = 'padding: 2px 10px 20px 10px;',
                 use_waitress(),
                 h4(title, style = 'color: #333333;'),
                 p(description, style = 'font-weight: 400; color: #607d8b;'),
                 h1(value, style = 'color: #03a9f4; font-size: 40px; text-align: center;')
             )
         )
  )
}
