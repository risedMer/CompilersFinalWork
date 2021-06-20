module Parse

open System.IO
open FSharp.Text
open XMAbsyn
open Debug

let fromString (str : string) : program =
    let lexbuf = Lexing.LexBuffer<char>.FromString(str)
    try
        XMPar.Main XMLex.Token lexbuf
    with
        | exn -> let pos = lexbuf.EndPos
                 failwithf "%s near line %d, column %d\n"
                    (exn.Message) (pos.Line + 1) pos.Column

let token buf = 
    let res = XMLex.Token buf
    msg <|
            match res with
                | XMPar.EOF -> sprintf "%A\n" res
                | _         -> sprintf "%A, " res
    res

let fromFile (filename : string) =
    use reader = new StreamReader(filename)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    try 
      msg "\nToken:\n"
      
      //CPar.Main  语法分析主程序 
      let ast = XMPar.Main token lexbuf in
        msg "\nAST:\n";
        ast
    with 
      | exn -> let pos = lexbuf.EndPos 
               failwithf "%s in file %s near line %d, column %d\n" 
                  (exn.Message) filename (pos.Line+1) pos.Column