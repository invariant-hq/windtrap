open Windtrap

let device =
  fixture (fun () ->
      match Sys.getenv_opt "GPU_DEVICE" with
      | Some name -> name
      | None -> skip ~reason:"GPU_DEVICE is not set" ())

let gpu =
  group "gpu"
    [
      test "the device has a name" (fun () -> is_true (device () <> ""));
      test "the device is not the CPU" (fun () ->
          is_false (String.equal (device ()) "cpu"));
    ]
