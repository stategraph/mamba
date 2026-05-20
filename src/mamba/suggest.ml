(* Damerau-Levenshtein with adjacent-transposition. Classic DP, O(n*m). *)
let distance a b =
  let la = String.length a and lb = String.length b in
  if la = 0 then lb
  else if lb = 0 then la
  else begin
    let d = Array.make_matrix (la + 1) (lb + 1) 0 in
    for i = 0 to la do d.(i).(0) <- i done;
    for j = 0 to lb do d.(0).(j) <- j done;
    for i = 1 to la do
      for j = 1 to lb do
        let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
        let m = min (d.(i - 1).(j) + 1) (d.(i).(j - 1) + 1) in
        let m = min m (d.(i - 1).(j - 1) + cost) in
        d.(i).(j) <-
          (if i > 1 && j > 1
              && a.[i - 1] = b.[j - 2]
              && a.[i - 2] = b.[j - 1]
           then min m (d.(i - 2).(j - 2) + 1)
           else m)
      done
    done;
    d.(la).(lb)
  end

let closest ?(max_distance = 3) needle candidates =
  candidates
  |> List.filter_map (fun c ->
       let d = distance needle c in
       if d <= max_distance then Some (d, c) else None)
  |> List.sort (fun (d1, _) (d2, _) -> compare d1 d2)
  |> List.map snd
