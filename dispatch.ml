open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (FS:KV_RO) (S:Cohttp_lwt.Server) = struct

  let start c fs http =

    let read_fs name =
      FS.size fs name
      >>= function
      | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
      | `Ok size ->
        FS.read fs name 0 (Int64.to_int size)
        >>= function
        | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
        | `Ok bufs -> return (Cstruct.copyv bufs)
    in

    (* Split a URI into a list of path segments *)
    let split_path uri =
      let path = Uri.path uri in
      let rec aux = function
        | [] | [""] -> []
        | hd::tl -> hd :: aux tl
      in
      List.filter (fun e -> e <> "")
        (aux (Re_str.(split_delim (regexp_string "/") path)))
    in

    (* dispatch non-file URLs *)
    let dispatcher segments = 
      (* tried fixing this with a stat of the file. 
       * after looking at the documenation, it appears
       * that there isn't a stat for the FS module. 
       * right now I've got it set up to check after 
       * the first failure if there is an index.html
       * file inside the directory otherwise fail. *) 
      let path = String.concat "/" segments in 
      try_lwt
        read_fs path 
        >>= fun body -> 
        S.respond_string ~status:`OK ~body () 
      with exn -> 
        try_lwt 
          read_fs (path ^ "/index.html") 
          >>= fun body -> 
          S.respond_string ~status:`OK ~body () 
        with exn -> S.respond_not_found () 
    in

    (* HTTP callback *)
    let callback conn_id request body =
      let uri = S.Request.uri request in
      dispatcher (split_path uri)
    in
    let conn_closed (_,conn_id) () =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in
    http { S.callback; conn_closed }

end
