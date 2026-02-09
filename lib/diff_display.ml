(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Colorize unified diff lines while keeping the underlying text unchanged. *)
let colorize_unified_diff diff_text =
  let style_line line =
    if String.starts_with ~prefix:"@@ " line then Pp.styled_string `Cyan line
    else if String.starts_with ~prefix:"--- " line then
      Pp.styled_string `Bold line
    else if String.starts_with ~prefix:"+++ " line then
      Pp.styled_string `Bold line
    else if String.starts_with ~prefix:"- " line then Pp.styled_string `Red line
    else if String.starts_with ~prefix:"+ " line then
      Pp.styled_string `Green line
    else line
  in
  String.split_on_char '\n' diff_text
  |> List.map style_line |> String.concat "\n"
