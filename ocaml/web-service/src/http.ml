open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
    (* Server.respond_string ~status:`OK ~body:"helloworld" () *)
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
      (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders: %s\nBody: %s\n"
         uri meth headers body))
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
    let tcp_mode = `TCP (`Port 8080) in
    let config = Server.make ~callback () in
      Server.create ~mode:tcp_mode config

let () = ignore (Lwt_main.run server)
