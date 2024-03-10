open TokenTypes

let tokenize input =
          let rec token_help input pos lst_of_words =
              let length = String.length input in 
              if pos >= length then lst_of_words
              else if Str.string_match (Str.regexp "L") input pos then
                     let value = Str.matched_string input in 
                     token_help input (pos + (String.length value)) ((lst_of_words) @ [Tok_Lambda])
              else if Str.string_match (Str.regexp "(") input pos then
                     let value = Str.matched_string input in
                     token_help input (pos + (String.length value)) ((lst_of_words) @ [Tok_LParen])
              else if Str.string_match (Str.regexp ")") input pos then
                     let value = Str.matched_string input in
                     token_help input (pos + (String.length value)) ((lst_of_words) @ [Tok_RParen])
              else if Str.string_match (Str.regexp "\\.") input pos then
                     let value = Str.matched_string input in
                     token_help input (pos + (String.length value)) ((lst_of_words) @ [Tok_Dot])
              else if Str.string_match (Str.regexp "[a-z]") input pos then
                    let value = Str.matched_string input in
                    token_help input (pos + (String.length value)) ((lst_of_words) @ [Tok_Var(value)])
              else if Str.string_match (Str.regexp " \\|\t\\|\n") input pos then
                     token_help input (pos + 1) lst_of_words
              else raise (Failure "tokenizing failed") in
          token_help input 0 []
