let rec many = fun str (out,n) ->
  if n == 0 then
    ()
  else
    (
      output_string out str; 
      many str (out,n - 1)
    )

let dateAndTime() =
  let ts = string_of_int in
  let ut = Unix.gmtime(Unix.time()) in
  let hour = ut.Unix.tm_hour in
  let hour' = if hour < 5 then (24 - (5 - hour)) else (hour - 5) in
  let min = ut.Unix.tm_min in
  let mins = if min < 10 then "0" ^ (ts min) else (ts min) in
  let time = (ts hour') ^ ":" ^ mins ^ " " in
  let dt1 = (ts (ut.Unix.tm_mon + 1)) ^ "." ^ (ts ut.Unix.tm_mday) in
  let dt2 = "." ^ (ts (ut.Unix.tm_year + 1900))
  in
    time ^ dt1 ^ dt2;;

let explode s =
  let n = String.length s in
  let rec aux k = if k = n then [] else s.[k] :: aux (k+1) in
    aux 0;;

let implode l =
  let s = String.create (List.length l) in
  let rec loop i = function
    | h::t -> s.[i] <- h; loop (succ i) t
    | [] -> s
  in loop 0 l

let whitespace : out_channel * int -> unit = many " ";;

let return(out) = output_string out "\n";;

let write(out,indent,outString) =
  (
    whitespace(out,indent);
    output_string  out outString
  );;

let writeln(out,indent,outString) =
  (
    whitespace(out,indent);
    output_string out outString;
    return(out)
  )

let makeFileName(fileName, newExtension) =
  let prefix = String.sub fileName 0 (String.rindex fileName '.') in
    prefix ^ "." ^ newExtension

let makeFile(fname, ext) =
  let fname' = makeFileName(fname, ext) in
  let _ = (if (Sys.file_exists fname') then
	     Sys.rename fname' (fname' ^ "~")
	   else ())
  in
    open_out fname';;

let interpreterName = "sfl"

let rec zip = function
([],[]) -> []
  | (x::xs, y::ys) -> (x,y)::zip(xs,ys)
  | _ -> raise(Failure "zip: attempting to zip lists of unequal length.");;

let rec unzip = function
[] -> ([],[])
  | (x,y)::xys -> let (xs,ys) = unzip xys in (x::xs,y::ys);;
