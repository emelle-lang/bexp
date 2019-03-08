open Base
open Types

let render t =
  ignore (
      List.fold t.syntactic_forms ~init:0 ~f:(fun acc (Syntax syn) ->
          syn.syn_group#set_y (Float.of_int acc *. Block.col_height);
          let (_, cols) = Syntax.render syn in
          acc + cols + 1
        )
    )
