type 'a tsil =
  | Snoc of 'a tsil * 'a
  | Lin

let make_lsl2 init step =
  let rec loop t1 outs = function
    | [] -> outs
    | t2 as t1' :: ins' ->
       let outs' = Snoc (outs, step (t1, t2)) in
       loop t1' outs' ins'
  in
  function
  | [] -> None
  | t1 :: ins ->
     let outs = Snoc (Lin, init t1) in
     Some (loop t1 outs ins)

let bts_lsl2 =
  let init _ = `S
  and step = function
    | (`S, _) -> `S
    | (`HS, `S) -> `HS
    | (`HS, `HS) -> `Q
  in
  make_lsl2 init step

let _ =
  [[`S; `S; `S];
   [`HS; `S; `S];
   [`S; `HS; `S]]
  |> List.map bts_lsl2
