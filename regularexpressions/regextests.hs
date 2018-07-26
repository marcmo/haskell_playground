import Text.Regex


foo = matchRegex (mkRegex "sections/([a-z]*)[/a-z]*([a-z ]*\\.jpg$)") "./sections/bolivia/images/first shot.jpg"
