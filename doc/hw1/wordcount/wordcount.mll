{ 
module StringMap = Map.Make(String)
type token = EOF | Word of string 
}

rule token = parse
| eof {EOF}
| ['a'-'z' 'A'-'Z']+ as word { Word(word) }
| _ { token lexbuf }

{

let lexbuf = Lexing.from_channel stdin in
let wordcount =
    let wordlist = 
        let wordmap = 
            let rec next l = 
                match token lexbuf with
                 EOF -> l
                | Word(s) -> next(
                    if StringMap.mem s l then
                        StringMap.add s ((StringMap.find s l) + 1) l
                    else
                        StringMap.add s 1 l
                )
            in next StringMap.empty
        in StringMap.fold (fun k v n -> (v, k)::n) wordmap []
    in List.sort (fun (c1,_) (c2,_) -> Pervasives.compare c2 c1) wordlist
in List.iter (fun (v, k) -> print_int v; print_string " "; print_endline k) wordcount
}

