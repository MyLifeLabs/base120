let print_demo_head () =
  Printf.printf "Base120       Decimal       Hexadecimal\n"

let print_demo_line n =
  Printf.printf "%-13s %-13i %s%x\n"
    (Base120.to_string n)
    n
    (if n > 0 then "" else "-")
    (abs n)

let demo () =
  print_demo_head ();
  let random repeat maxi =
    for i = 1 to repeat do
      let n = Int64.to_int (Random.int64 (Int64.of_int maxi)) in
      print_demo_line n
    done
  in
  print_demo_line 123_456_790;
  print_demo_line (-978_654_321);
  random 10 1_000;
  random 10 1_000_000;
  random 10 1_000_000_000;
  random 10 0xffffffffff;
  for i = 1 to 10 do
    print_demo_line (138_392 + i)
  done


let () = demo ()
