let help () =
  String.concat ""
    (List.map
       (fun line -> line ^ "\n")
       [
         "Usage: mytool [OPTIONS] COMMAND";
         "";
         "Commands:";
         "  build    Build the project";
         "  test     Run the tests";
         "";
         "Options:";
         "  --help   Show this help";
       ])

let report ~rows = Printf.sprintf "processed %d rows\nstatus: ok" rows
let greet name = Printf.printf "Hello, %s!\n" name
