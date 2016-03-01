module Main =
  struct
    open Ast
    open Basis
    open Interpret
    open Core.Std
           
    let st_basis = Basis.Interpreter.staticBasis
    let dy_basis = Basis.Interpreter.dynamicBasis

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
                    
    let context = ref (make_context st_basis dy_basis)

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

    let respond_with_status_and_body status body =
      Server.respond_string ~status:status ~body:body ()
                            
    let respond_success response =
      respond_with_status_and_body `OK response

    let respond_failure response =
      respond_with_status_and_body `Bad_request response
      
    let respond_with_compiled_result body =
      body |> Cohttp_lwt_body.to_string
      >|= (fun body -> decode_body body |> pct_decode_body)
      >>= (fun body ->
           match body with
           | Some body ->
              interpret body |> respond_success
           | None ->
              "Bad Http request...\n" |> respond_failure)

    let serveFile path =
      let path_substrs = String.split ~on:'/' path in
      let tail =
        match List.tl path_substrs with
        | Some lst -> lst
        | None -> []
      in
      let head =
        match List.hd path_substrs with
        | Some head' -> head'
        | None -> ""
      in
      let path_joined = String.concat ~sep:"/" tail in
      let path' = String.concat ~sep:"" [ head; path_joined ] in
      let substrs = String.split ~on:'.' path' in
      match substrs with
      | [ "" ] ->
         Server.respond_file ~fname:"home.html" ()
      | [ no_ext ] ->
         Server.respond_file ~fname:(String.concat ~sep:"" [ no_ext; ".html" ]) ()
      | _ as file_with_ext ->
         Server.respond_file ~fname:(String.concat ~sep:"." file_with_ext) ()
         
         
    let route req body =
      let path = req |> Request.uri |> Uri.path in
      let meth = req |> Request.meth in
      match meth with
      | `GET ->
         serveFile path
      | `POST ->
         (match path with
          | "/compile" ->
             respond_with_compiled_result body
          | _ as bad_path ->
             "Bad Http request:\t" ^ bad_path ^ "\n" |> respond_failure)
      | _ as bad_method ->
         "Bad Http request:\t" ^ (Code.string_of_method bad_method) ^ "\n"
         |> respond_failure
      
    let server =
      let callback _conn req body =
        route req body
      in
      Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
                    
    let () = ignore (Lwt_main.run server)
  end;;
