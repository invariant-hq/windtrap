(* [del]: the statement [e1] of [e1; e2] does not run. [()], an [assert]
   and the value of a sequence are no statement it drops. *)

let log = ref []

let push x =
  log := x :: !log;
  List.length !log

let loop n =
  for i = 1 to n do
    print_int i;
    print_newline ()
  done

let unit_first x =
  ();
  x

let checked x =
  assert (x > 0);
  x

let quiet x =
  (print_string "x" [@mutate off "the output is not checked"]);
  x

(* A statement that is a site already carries [del] besides. *)
let either a b =
  a || b;
  ()

(* A statement that runs when the module is initialized is no site. *)
let () =
  print_string "loaded";
  print_newline ()

(* A resource release is arid, also under [ignore] or as a [try] body. *)
let finish oc ic fd dir =
  output_string oc "done";
  close_out oc;
  close_in_noerr ic;
  (try Unix.close fd with Unix.Unix_error _ -> ());
  ignore (Unix.closedir dir);
  flush stdout
