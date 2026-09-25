open Windtrap

let bread = Shop.item "bread" ~price:250 ~quantity:2
let milk = Shop.item "milk" ~price:120 ~quantity:1
let cart = [ bread; milk ]

let subtotal =
  group "subtotal"
    [
      test "is zero for an empty cart" (fun () ->
          equal int 0 (Shop.subtotal []));
      test "sums price times quantity" (fun () ->
          equal int 620 (Shop.subtotal cart));
    ]

let names =
  group "names"
    [
      test "keeps the order of the cart" (fun () ->
          equal ~__POS__ (list string) [ "bread"; "milk" ] (Shop.names cart));
    ]

let item = Testable.make ~pp:Shop.pp_item ~equal:( = )

let find =
  group "find"
    [
      test "returns the item of that name" (fun () ->
          equal ~__POS__ (option item) (Some milk) (Shop.find "milk" cart));
      test "returns None for an unknown name" (fun () ->
          equal ~__POS__ (option item) None (Shop.find "eggs" cart));
    ]

let by_name = Testable.contramap (fun (item : Shop.item) -> item.name) string

let remove =
  group "remove"
    [
      test "drops the item of that name" (fun () ->
          equal ~__POS__ (list by_name) [ milk ] (Shop.remove "bread" cart));
    ]

let with_tax =
  group "with_tax"
    [
      test "adds the rate to the price" (fun () ->
          equal ~__POS__ (float 1e-9) 744. (Shop.with_tax ~rate:0.2 620));
    ]

let receipt =
  group "receipt"
    [
      test "lists the items then the total" (fun () ->
          equal ~__POS__ text
            "bread         2 x  250\n\
             milk          1 x  120\n\
             total              620\n"
            (Shop.receipt cart));
    ]

let discount =
  group "discount"
    [
      test "is at most a tenth of the price" (fun () ->
          at_most ~__POS__ int ~than:62 (Shop.discount 620));
      test "is a multiple of 5 cents" (fun () ->
          satisfies ~__POS__ ~claim:"a multiple of 5" int
            (fun cents -> cents mod 5 = 0)
            (Shop.discount 620));
    ]

let parse_quantity =
  group "parse_quantity"
    [
      test "reads a number between spaces" (fun () ->
          let quantity =
            require_ok ~pp:(Testable.pp string) (Shop.parse_quantity " 3 ")
          in
          equal ~__POS__ int 3 quantity);
      test "reports the input it rejects" (fun () ->
          let message = require_error (Shop.parse_quantity "0") in
          equal ~__POS__ string "not a quantity: 0" message);
    ]

let label =
  group "label"
    [
      test "starts with the name" (fun () ->
          starts_with ~__POS__ ~affix:"bread" (Shop.label bread));
      test "shows the quantity then the price" (fun () ->
          in_order ~__POS__ ~subs:[ "2"; "250" ] (Shop.label bread));
    ]

let rates =
  group "rates"
    [
      test "no rate lowers a price" (fun () ->
          List.iter
            (fun rate ->
              let msg = Printf.sprintf "rate %g" rate in
              at_least ~__POS__ ~msg (float 1e-9) ~than:100.
                (Shop.with_tax ~rate 100))
            [ 0.; 0.055; 0.2 ]);
    ]

let validation =
  group "validation"
    [
      test "rejects a zero quantity" (fun () ->
          raises ~__POS__
            (Invalid_argument "Shop.item: quantity must be positive") (fun () ->
              Shop.item "eggs" ~price:30 ~quantity:0));
      test "rejects a negative quantity" (fun () ->
          raises_match ~__POS__ (Exn.invalid_arg ~substring:"quantity")
            (fun () -> Shop.item "eggs" ~price:30 ~quantity:(-1)));
    ]

let () =
  exit
    (run "shop"
       [
         subtotal;
         names;
         find;
         remove;
         with_tax;
         receipt;
         discount;
         parse_quantity;
         label;
         rates;
         validation;
       ])
