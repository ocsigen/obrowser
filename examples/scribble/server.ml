let table = Hashtbl.create 20 and mutex = Mutex.create ()
let with_clients f =
  Mutex.lock mutex ;
  try let r = f table in Mutex.unlock mutex ; r
  with e -> Mutex.unlock mutex ; raise e
      
let timeout = 20.

let client id =
  with_clients
    (fun clients ->
       let alive = ref true in
	 Hashtbl.replace clients id ([], alive) ;
	 let rec check_alive () =
	   if not !alive then (
	     Hashtbl.remove clients id
	   ) else (
	     alive := false ;
	     Thread.delay timeout ;
	     check_alive ()
	   ) in ignore (Thread.create check_alive ()))

let flush id =
  with_clients
    (fun clients ->
       try
	 let r, alive = Hashtbl.find clients id in
	   alive := true ; Hashtbl.replace clients id ([], alive) ; r
       with Not_found -> client id ; [])
    
let dispatch msg =
  with_clients
    (fun clients ->
       Hashtbl.iter (fun k (d, alive) -> Hashtbl.replace clients k (msg :: d, alive))  clients)
    
let establish_server server_fun sockaddr =
   let domain = Unix.domain_of_sockaddr sockaddr in
   let sock = Unix.socket domain Unix.SOCK_STREAM 0 
   in Unix.bind sock sockaddr ;
     Sys.set_signal Sys.sigint (Sys.Signal_handle
				  (fun _ -> print_endline "Killed.";
				     Unix.shutdown sock Unix.SHUTDOWN_ALL ;
				     Unix.close sock ;
				     exit 0)) ;
     Unix.listen sock 3;
     while true do
       let (s, caller) = Unix.accept sock in
       let inchan = Unix.in_channel_of_descr s in
       let outchan = Unix.out_channel_of_descr s in
	 ignore (Thread.create (fun () ->
				  (try server_fun inchan outchan caller with e -> print_endline (Printexc.to_string e)) ;
				  Pervasives.flush outchan ;
				  Unix.shutdown s Unix.SHUTDOWN_ALL ; Unix.close s) ())
     done
       
let input_line_http inchan =
  let rec input acc s =
    try
      match input_char inchan with
	| '\r' -> ignore (input_char inchan) ; (acc, s)
	| c -> input (c :: acc) (succ s)
    with End_of_file -> if s > 0 then (acc, s) else raise End_of_file
  in
  let chars, s = input [] 0 in
  let line = String.create s in
  let rec fill n l =
    match l with
      | hd :: tl ->
	  fill (pred n) tl ;
	  line.[n] <- hd
      | _ -> ()
  in
    fill (pred s) chars ; line

let rec server_fun inchan outchan addr' =
  let addr = Unix.string_of_inet_addr (match addr' with | Unix.ADDR_UNIX _ -> assert false | Unix.ADDR_INET (ip,_) -> ip) in
  let request = input_line_http inchan in
    match Str.split (Str.regexp "[ ]+") request with
      | ["POST" ; _ ; "HTTP/1.1"|"HTTP/1.0"|"HTTP/0.9" as http] ->
	  Pervasives.flush stdout ;
	  let body =
	    let rec eat_headers () =
	      if input_line_http inchan = "" then get_body [] else eat_headers ()
	    and get_body acc =
	      try
		let line = input_line_http inchan in
		  if line = "" then List.rev acc
		  else get_body (line :: acc)
	      with End_of_file -> List.rev acc
	    in eat_headers ()
	  in
	    List.iter dispatch body ;
	    Pervasives.flush stdout ;
	    Printf.fprintf outchan "%s 200OK\n\n" http
      | ["GET" ; reqt ; "HTTP/1.1"|"HTTP/1.0"|"HTTP/0.9" as http] ->
	  let req' = String.sub reqt 1 (String.length reqt - 1) in
	  let prefix, req =
	    let pos = ref 0 in
	      while (!pos < String.length req'
		     && req'.[!pos] <> '/') do incr pos done ;
	      if !pos = String.length req' then
		(req', "")
	      else
		(String.sub req' 0 !pos,
		 String.sub req'
		   (!pos + 1)
		   (String.length req' - !pos - 1))
	  in
	    begin
	      match prefix with
		| "register" ->
		    client (addr ^ "::" ^ req) ;
		    Printf.fprintf outchan "%s 200OK\n\n" http
		| "poll" ->
		    List.iter (Printf.fprintf outchan "%s\n")
		      ((http ^ " 200 OK") :: "" :: flush (addr ^ "::" ^ req))
		| _ ->
		    try
		      List.iter
			(Printf.fprintf outchan "%s\n")
			((http ^ " 200 OK")
			 :: ""
			 :: (let f = open_in prefix in
			     let rec read () =
			       try let l = input_line f in l :: read () with _ -> []
			     in
			     let r = read () in close_in f ; r ))
		    with
		      | e ->
			  Printf.printf "Error: %s\n" (Printexc.to_string e) ;
			  output_string outchan "HTTP/1.0 404 Not Found\n\n404 Not Found"
	    end
      | _ -> failwith "PAF !"	  
	  
let _ = establish_server server_fun (Unix.ADDR_INET (Unix.inet_addr_any, (int_of_string Sys.argv.(1))))
