let rec remove_duplicates input = match input with
| [] -> []
| [_] -> input
| h1::((h2::_) as tl) -> if h1 = h2 then remove_duplicates tl else
    h1::remove_duplicates tl;;

remove_duplicates [1;1;1;3;4;1;1];;
