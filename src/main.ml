module Main =
  struct
    open Ast
    open Basis
    open Interpret
    open Core.Std
           
    let stBasis = Basis.Interpreter.staticBasis
    let dyBasis = Basis.Interpreter.dynamicBasis

    let decode_body uri_encoded =
      let kv_list = String.split ~on:'=' uri_encoded in
      match kv_list with
      | ["code"; body] ->
         Some (String.tr '+' ' ' body)
      | _ ->
         None

    let pct_decode_body pct_encoded =
      match pct_encoded with
      | Some body ->
         Some (Uri.pct_decode body)
      | None ->
         None
                    
    let rec make_context x y =
      match x, y with
      | [], [] -> []
      | (a,b) :: s, (c,d) :: t ->
         (a, (b, Some d)) :: (make_context s t)
      | _, _ -> raise (Failure "unable to make context\n")
                    
    let context = ref (make_context stBasis dyBasis)

    let interpret (source:string) : string =
      let ast = Interpret.ast_from_string source in
      match ast with
      | Some ast' ->
         let res = Interpret.interpret ast' context in
         (match res with
         | Interpret.Left (typ, v, ctx') ->
            String.concat ~sep:"" [Ast.toString v; ":"; Ast.toString typ; "\n"]
         | Interpret.Right s ->
            s)
      | None ->
         "Syntax Error: " ^ source

    open Lwt
    open Cohttp
    open Cohttp_lwt_unix
           
    let server =
      let callback _conn req body =
        body |> Cohttp_lwt_body.to_string
        >|= (fun body -> decode_body body |> pct_decode_body)
        >>= (fun body ->
             match body with
             | Some body ->
                Server.respond_string ~status:`OK ~body:(interpret body) ()
             | None ->
                Server.respond_string ~status:`Bad_request ~body:"Bad Http request...\n" ())
      in
      Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
                    
    let () = ignore (Lwt_main.run server)
  end;;
