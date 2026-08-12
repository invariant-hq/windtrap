let is_alnum c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

let lower c = if c >= 'A' && c <= 'Z' then Char.chr (Char.code c + 32) else c

let slugify s =
  let buf = Buffer.create (String.length s) in
  let pending_sep = ref false in
  String.iter
    (fun c ->
      if is_alnum c then begin
        if !pending_sep && Buffer.length buf > 0 then Buffer.add_char buf '-';
        pending_sep := false;
        Buffer.add_char buf (lower c)
      end
      else pending_sep := true)
    s;
  Buffer.contents buf
