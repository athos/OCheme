open Util

let _ =
  let s = Stream.of_channel stdin in
  let env = Primitive.standard_env () in
  let rec loop () =
    begin
      try
        print_string "ocheme> ";
        flush stdout;
        let exp = Reader.read s in
        Printf.printf "%s\n" @@ Value.show @@ Primitive.eval exp env
      with
        Vm.Runtime_error msg ->
          Printf.printf "error: %s\n" msg
    end;
    loop()
  in loop ()
