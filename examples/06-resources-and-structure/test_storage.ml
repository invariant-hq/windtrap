let () =
  exit
    (Windtrap.run "storage"
       [
         Db_tests.database;
         Server_tests.server;
         Gpu_tests.gpu;
         Process_tests.process;
       ])
