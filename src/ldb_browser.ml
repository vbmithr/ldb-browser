open Base
open Cmdliner

let default_cmd =
  let doc = "Browse a LevelDB database." in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info ~doc "ldb-browser"

let with_db db ~f =
  let db = LevelDB.open_db db in
  try
    let ret = f db in
    LevelDB.close db ;
    ret
  with exn ->
    LevelDB.close db ;
    raise exn

let list db prefix =
  with_db db ~f:begin fun db ->
    LevelDB.iter_from begin fun k v ->
      match String.is_prefix k ~prefix with
      | false -> false
      | true ->
        Caml.Format.printf "%s ->@.  %s@."
          (String.chop_prefix_exn k ~prefix)
          (String.escaped v) ;
        true
    end db prefix
  end

let list =
  let doc = "List entries with some prefix." in
  let db =
    Arg.(required & (pos 0 (some dir) None) & info [] ~docv:"DIRECTORY") in
  let prefix =
    Arg.(required & (pos 1 (some string) None) & info [] ~docv:"PREFIX") in
  Term.(const list $ db $ prefix),
  Term.info ~doc "list"

let cmds = [
  list ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
