string = require 'string'

-- monkeypatch a couple of functions into the `string` table

string.split = (text, splitter) ->
  -- Find all words that *don't* contain the splitter character.
  -- gmatch returns an iterator.
  text\gmatch "[^#{splitter}]+"
  
string.strong = (text) -> text\gsub '__(.-)__', "<strong>%1</strong>"
string.em = (text) -> text\gsub '_(.-)_', "<em>%1</em>"

{
  parse: (input) ->
    markup = {}
    in_list = false

    add_markup = (line) -> markup[#markup + 1] = line

    open_list = -> 
      if not in_list
        in_list = true
        add_markup '<ul>'

    close_list = ->
      if in_list then
        in_list = false
        add_markup '</ul>'

    for line in input\split "\n"
      line = line\strong!\em!

      text = line\match '^[*] (.*)'
      if text
        open_list!
        add_markup "<li>#{text}</li>"

      else
        close_list!
        
        header, text = line\match '^(#+) (.+)'
        if header and #header <= 6
          tag = "h#{#header}"
          add_markup "<#{tag}>#{text}</#{tag}>"

        else
          add_markup "<p>#{line}</p>"

    close_list!
    table.concat markup
}
