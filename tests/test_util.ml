let contains haystack needle =
  let lh = String.length haystack and ln = String.length needle in
  if ln = 0 then true
  else
    let rec loop i =
      if i + ln > lh then false
      else if String.sub haystack i ln = needle then true
      else loop (i + 1)
    in
    loop 0

let must_contain label out needle =
  Alcotest.(check bool)
    (label ^ ": contains " ^ needle) true (contains out needle)

let must_omit label out needle =
  Alcotest.(check bool)
    (label ^ ": does NOT contain " ^ needle) false (contains out needle)
