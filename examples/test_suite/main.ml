let print_byte b =
  let hex n =
    if n < 10 then
      String.make 1 (Char.chr (Char.code '0' + n))
    else
      String.make 1 (Char.chr (Char.code 'A' + n - 10))
  in
    print_string (hex (b lsr 4)) ;
    print_string (hex (b land 0xF))
;;
let unhex s =
  let s' = String.make (String.length s / 2) ' ' in
    for i = 0 to String.length s' - 1 do
      let hc = function
	| '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
	| '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'A' -> 10 | 'B' -> 11
	| 'C' -> 12 | 'D' -> 13 | 'E' ->  14 | 'F' -> 15
      in
	s'.[i] <- Char.chr ((hc s.[i * 2] lsl 4) + hc s.[i * 2 + 1])
    done ; s'
;;
let print_data d =
  let s = Marshal.to_string d [] in
  for i = 0 to String.length s - 1 do
    print_byte (Char.code s.[i])
  done ;
  print_newline ()
;;

module TestInt32 = struct
  open Int32

  let a = 42l
  let b = 12341234l
  let c = -2l
  let d = of_int 42
  let e = of_int 13

  let _ =
    print_endline "--- Int32 ---" ;
    print_endline (to_string a) ;
    print_endline (to_string b) ;
    print_endline (to_string c) ;
    print_endline (to_string d) ;
    print_endline (to_string e) ;
    print_endline (to_string (add a c)) ;
    print_endline (to_string (neg b)) ;
    print_endline (to_string (sub d e)) ;
    print_endline (to_string (sub e d)) ;
    print_endline (to_string (rem b a)) ;
    print_endline (to_string (rem c a)) ;
    print_endline (to_string (rem b c)) ;
    print_endline (to_string (mul b a)) ;
    print_endline (to_string (div b a)) ;
    print_endline (to_string max_int) ;
    print_endline (to_string (add max_int 1l)) ;
    print_data b
end

module TestNativeint = struct
  open Nativeint

  let a = of_int32 42l
  let b = of_int32 12341234l
  let c = of_int32 (-2l)
  let d = of_int 42
  let e = of_int 13

  let _ =
    print_endline "--- Nativeint ---" ;
    print_endline (to_string a) ;
    print_endline (to_string b) ;
    print_endline (to_string c) ;
    print_endline (to_string d) ;
    print_endline (to_string e) ;
    print_endline (to_string (add a c)) ;
    print_endline (to_string (neg b)) ;
    print_endline (to_string (sub d e)) ;
    print_endline (to_string (sub e d)) ;
    print_endline (to_string (rem b a)) ;
    print_endline (to_string (rem c a)) ;
    print_endline (to_string (rem b c)) ;
    print_endline (to_string (mul b a)) ;
    print_endline (to_string (div b a)) ;
    print_endline (to_string max_int) ;
    print_endline (to_string (add max_int (of_int 1))) ;
    print_data b
end

module TestInt64 = struct
  open Int64

  let a = 42L
  let b = 123412341234L
  let c = -2L
  let d = of_int 42
  let e = of_int 13

  let _ =
    print_endline "--- Int64 ---" ;
    print_endline (to_string a) ;
    print_endline (to_string b) ;
    print_endline (to_string c) ;
    print_endline (to_string d) ;
    print_endline (to_string e) ;
    print_endline (to_string (add a c)) ;
    print_endline (to_string (neg b)) ;
    print_endline (to_string (sub d e)) ;
    print_endline (to_string (sub e d)) ;
    print_endline (to_string (rem b a)) ;
    print_endline (to_string (rem c a)) ;
    print_endline (to_string (rem b c)) ;
    print_endline (to_string (mul b a)) ;
    print_endline (to_string (div b a)) ;
    print_endline (to_string max_int) ;
    print_endline (to_string (add max_int 1L)) ;
    print_data b ;
    print_data (b,b) ;
    print_data (c,d) ;
    print_data (c,d) ;
    print_data (c,c) ;
    List.iter (fun s ->
		 let (x,y) = Marshal.from_string (unhex s) 0 in
		   print_endline "-------" ;
		   print_endline (to_string x) ;
		   print_endline (to_string y))
      [ "8495A6BE00000019000000030000000B00000009A0125F6A00FFFFFFFFFFFFFFFE125F6A00000000000000002A" ;
	"8495A6BE0000000F000000020000000700000006A0125F6A00FFFFFFFFFFFFFFFE0401" ;
	"8495A6BE0000000F000000020000000700000006A0125F6A000000001CBBF2E1F20401"]
end
