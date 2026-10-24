open Windtrap

let vault = abstract "v"
let pin = Gen.int_range 0 9999

let commands =
  [
    command "create" (Gen.unit @-> makes vault) Vault.create Vault.create;
    command "store"
      (vault ^-> Gen.pair pin pin @-> returns unit)
      (fun v (a, b) ->
        cover "the pins differ" (a <> b);
        Vault.store v a)
      (fun v (_, b) -> Vault.store v b);
    command "check" (vault ^-> pin @-> returns pass) Vault.check Vault.check;
    command "log" (vault ^-> returns (list string)) Vault.log Vault.log;
  ]

let vaults = group "Vault" [ stateful "logs nothing of a pin" commands ]
let () = exit (run "vault" [ vaults ])
