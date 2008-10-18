open Js

type relief = Top | Bot | Flat
    
type box_config =
    { x:int; y:int; w:int; h:int; bw:int; mutable r:relief ;
      b1_col : Graphics.color ;
      b2_col : Graphics.color ;
      b_col : Graphics.color}
      
let draw_box_outline bcf col = 
  Graphics.set_color col ;
  Graphics.draw_rect bcf.x bcf.y bcf.w  bcf.h
    
let draw_box bcf = 
  let x1 = bcf.x and y1 = bcf.y in
  let x2 = x1+bcf.w and y2 = y1+bcf.h in 
  let ix1 = x1+bcf.bw
  and ix2 = x2-bcf.bw 
  and iy1 = y1+bcf.bw
  and iy2 = y2-bcf.bw in 
  let border1 g =
    Graphics.set_color g;
    Graphics.fill_poly 
      [| (x1,y1);(ix1,iy1);(ix2,iy1);(ix2,iy2);(x2,y2);(x2,y1) |] 
  in
  let border2 g = 
    Graphics.set_color g;
    Graphics.fill_poly 
      [| (x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2) |]
  in
    Graphics.set_color bcf.b_col;
    ( match bcf.r with
          Top  -> 
            Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1) ;
            border1 bcf.b1_col ; 
            border2 bcf.b2_col
	| Bot  -> 
            Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1) ;
            border1 bcf.b2_col ; 
            border2 bcf.b1_col
	| Flat -> 
            Graphics.fill_rect x1 y1 bcf.w bcf.h ) ;
    draw_box_outline bcf Graphics.black
      
let erase_box bcf = 
  Graphics.set_color bcf.b_col ; 
  Graphics.fill_rect
    (bcf.x+bcf.bw) (bcf.y+bcf.bw) 
    (bcf.w-(2*bcf.bw)) (bcf.h-(2*bcf.bw))

type position = Left | Center | Right

let draw_string_in_box pos str bcf col = 
  let (w, h) = Graphics.text_size str in
  let ty = bcf.y + (bcf.h-h)/2 in 
    ( match pos with 
          Center -> Graphics.moveto (bcf.x + (bcf.w-w)/2) ty 
        | Right  -> let tx = bcf.x + bcf.w - w - bcf.bw - 1 in 
            Graphics.moveto tx ty 
        | Left   -> let tx = bcf.x + bcf.bw + 1 in Graphics.moveto tx ty  ) ;
    Graphics.set_color col ;
    Graphics.draw_string str

let set_grey x = Graphics.rgb x x x

let grey1 = set_grey 100
and grey2 = set_grey 170
and grey3= set_grey 240

type config = { 
  nbcols  : int ;
  nbrows : int ; 
  nbmines : int }

let default_config = { nbcols=10; nbrows=10; nbmines=15 } 

type cell = { 
  mutable mined : bool ;
  mutable seen : bool ; 
  mutable flag : bool ; 
  mutable nbm : int     
} 


type board = cell array array  

let iter_on_cell cf f = 
  for i=0 to cf.nbcols-1 do for j=0 to cf.nbrows-1 do f (i,j) done done 

let random_list_mines lc m = 
  let cell_list = ref [] 
  in while (List.length !cell_list) < m do 
      let n = Random.int lc in
        if not (List.mem n !cell_list) then cell_list := n :: !cell_list 
    done ;
    !cell_list 

let generate_seed () =
  let t = Sys.time () in           
  let n = int_of_float (t*.1000.0) 
  in Random.init(n mod 100000)            


let valid cf (i,j) = i>=0 && i<cf.nbcols && j>=0 && j<cf.nbrows  

let neighbours cf (x,y) =
  let ngb = [x-1,y-1; x-1,y; x-1,y+1; x,y-1; x,y+1; x+1,y-1; x+1,y; x+1,y+1]
  in List.filter (valid cf) ngb 


let initialize_board cf = 
  let cell_init () = { mined=false; seen=false; flag=false; nbm=0 } in 
  let copy_cell_init b (i,j) = b.(i).(j) <- cell_init() in
  let set_mined b n = b.(n / cf.nbrows).(n mod cf.nbrows).mined <- true
  in
  let count_mined_adj b (i,j) =
    let x = ref 0 in
    let inc_if_mined (i,j) = if b.(i).(j).mined then incr x 
    in List.iter inc_if_mined (neighbours cf (i,j)) ;
      !x
  in
  let set_count b (i,j) =
    if not b.(i).(j).mined 
    then b.(i).(j).nbm <- count_mined_adj b (i,j)
  in
  let list_mined = random_list_mines (cf.nbcols*cf.nbrows) cf.nbmines in 
  let board = Array.make_matrix cf.nbcols cf.nbrows (cell_init ()) 
  in iter_on_cell cf (copy_cell_init board) ;
    List.iter (set_mined board) list_mined ;
    iter_on_cell cf (set_count board) ;
    board 

let cells_to_see bd cf (i,j) = 
  let visited = Array.make_matrix cf.nbcols cf.nbrows false in 
  let rec relevant = function 
      [] -> ([],[])
    | ((x,y) as c)::l -> 
        let cell=bd.(x).(y)
        in if cell.mined || cell.flag || cell.seen || visited.(x).(y) 
          then relevant l
          else let (l1,l2) = relevant l 
          in visited.(x).(y) <- true ;
            if cell.nbm=0 then (l1,c::l2) else (c::l1,l2)
  in 
  let rec cells_to_see_rec = function 
      [] -> []  
    | ((x,y) as c)::l -> 
        if bd.(x).(y).nbm<>0 then c :: (cells_to_see_rec l)
        else let (l1,l2) = relevant (neighbours cf c)
        in  (c :: l1)  @  (cells_to_see_rec (l2 @ l))
  in visited.(i).(j) <- true ;
    cells_to_see_rec [(i,j)]  

let b0 = 3 
let l1 = 15 
let l2 = l1 
let l4 = 20 + 2*b0 
let l3 = l4*default_config.nbcols + 2*b0 
let l5 = 40 + 2*b0 


let h1 = l1 
let h2 = 30 
let h3 = l5+20 + 2*b0 
let h4 = h2 
let h5 = 20 + 2*b0 
let h6 = l5 + 2*b0 


type window_config = {
  cf : config ;            
  g_bcf : box_config ;   
  f_bcf : box_config ; 
  m_bcf : box_config ;
  m1_bcf : box_config ;
  m2_bcf : box_config ; 
  s_bcf : box_config ; 
  mutable cc_bcf : box_config ;
  cell : int*int -> (int*int) ;
  coor : int*int -> (int*int) 
} 

let make_box x y w h bw r =
  { x=x; y=y; w=w; h=h; bw=bw; r=r; b1_col=grey1; b2_col=grey3; b_col=grey2 } 
let make_wcf cf = 
  let wcols =  b0 + cf.nbcols*l4 + b0 
  and hrows =  b0 + cf.nbrows*h5 + b0  in
  let g_bcf =  let gw = (b0 + l1 + wcols + l2 + b0) 
               and gh = (b0 + h1 + hrows + h2 + h3 + h4 + b0)
  in make_box 0 0 gw gh b0 Top
  and f_bcf = make_box l1 h1 wcols hrows b0 Bot  in
  let m_bcf = make_box  ((g_bcf.w - l3) / 2) (b0+h1+hrows+h2) l3 h3 b0 Bot  in 
  let m1_bcf = make_box (m_bcf.x + b0) (b0 + h1 + hrows + h2)
    ((l3-l5)/2-(2*b0)) (h3-(2*b0)) 5 Flat  in
  let s_bcf = make_box (m1_bcf.x + m1_bcf.w) 
    (m1_bcf.y + (h3-h6) / 2) l5 h6 b0 Top  in 
  let m2_bcf = make_box (s_bcf.x + s_bcf.w) 
    m1_bcf.y m1_bcf.w m1_bcf.h 5 Flat  in
  let cc_bcf = make_box 0 0 l4 h5 b0 Top
  in { cf = cf; 
       g_bcf = g_bcf; f_bcf=f_bcf; m_bcf=m_bcf; m1_bcf=m1_bcf; 
       s_bcf=s_bcf; m2_bcf=m2_bcf; cc_bcf = cc_bcf;
       cell = (fun (i,j) -> ( l1+b0+l4*i , h1+b0+h5*j)) ;
       coor = (fun (x,y) -> ( (x-l1)/l4 , (y-h1)/h5 )) } 

let close_ccell wcf i j =
  let x,y = wcf.cell (i,j) 
  in wcf.cc_bcf <- {wcf.cc_bcf with x=x; y=y; r=Top} 

let open_ccell wcf i j =
  let x,y = wcf.cell (i,j) 
  in wcf.cc_bcf <- {wcf.cc_bcf with x=x; y=y; r=Flat}  

let draw_closed_cc wcf i j = 
  close_ccell wcf i j;
  draw_box wcf.cc_bcf 

let draw_num_cc wcf i j n =
  open_ccell wcf i j ;
  draw_box wcf.cc_bcf ;
  if n<>0 then draw_string_in_box Center (string_of_int n) 
    wcf.cc_bcf Graphics.white 

let draw_bomb_cc wcf i j = 
  open_ccell wcf i j ;
  let cc = wcf.cc_bcf 
  in draw_box wcf.cc_bcf ;
    Graphics.set_color Graphics.black ; 
    Graphics.fill_circle (cc.x+cc.w/2) (cc.y+cc.h/2) (cc.h/3) 

let draw_flag_cc wcf i j =
  close_ccell wcf i j ;
  draw_box wcf.cc_bcf ;
  draw_string_in_box Center "!" wcf.cc_bcf Graphics.blue 

let draw_cross_cc wcf i j =
  let x,y = wcf.cell (i,j) 
  and w,h = wcf.cc_bcf.w, wcf.cc_bcf.h in
  let a=x+w/4 and b=x+3*w/4 
	      and c=y+h/4 and d=y+3*h/4 
  in Graphics.set_color Graphics.red ;
    Graphics.set_line_width 3 ;
    Graphics.moveto a d ; Graphics.lineto b c ; 
    Graphics.moveto a c ; Graphics.lineto b d ;
    Graphics.set_line_width 1 

let draw_cell wcf bd i j = 
  let cell = bd.(i).(j) 
  in match (cell.flag, cell.seen , cell.mined ) with 
      (true,_,_)  -> draw_flag_cc wcf i j
    | (_,false,_) -> draw_closed_cc wcf i j
    | (_,_,true)  -> draw_bomb_cc wcf i j
    | _           -> draw_num_cc wcf i j cell.nbm  

let draw_cell_end wcf bd i j = 
  let cell = bd.(i).(j) 
  in match (cell.flag, cell.mined ) with 
      (true,true) -> draw_flag_cc wcf i j
    | (true,false) -> draw_num_cc wcf i j cell.nbm; draw_cross_cc wcf i j
    | (false,true) -> draw_bomb_cc wcf i j
    | (false,false) -> draw_num_cc wcf i j cell.nbm 

let draw_flag_switch wcf on =  
  if on  then wcf.s_bcf.r <- Bot  else wcf.s_bcf.r <- Top ;
  draw_box wcf.s_bcf ;
  if on  then draw_string_in_box Center "ON" wcf.s_bcf Graphics.red
  else draw_string_in_box Center "OFF" wcf.s_bcf Graphics.blue 

let draw_mark_title wcf =
  let m = "Marquage" in 
  let w,h = Graphics.text_size m in
  let x = (wcf.g_bcf.w-w)/2 
  and y0 = wcf.m_bcf.y+wcf.m_bcf.h in
  let y = y0+(wcf.g_bcf.h-(y0+h))/2 
  in Graphics.moveto x y ;
    Graphics.draw_string m  

let print_score wcf nbcto nbfc =
  erase_box wcf.m1_bcf ;
  draw_string_in_box Center (string_of_int nbcto) wcf.m1_bcf Graphics.blue ;
  erase_box wcf.m2_bcf ;
  draw_string_in_box Center (string_of_int (wcf.cf.nbmines-nbfc)) wcf.m2_bcf 
    ( if nbfc>wcf.cf.nbmines then Graphics.red else Graphics.blue ) 

let draw_field_initial wcf = 
  draw_closed_cc wcf 0 0 ;
  let cc = wcf.cc_bcf  in 
  let bitmap = draw_box cc ; Graphics.get_image cc.x cc.y cc.w cc.h  in
  let draw_bitmap (i,j) = let x,y=wcf.cell (i,j) 
  in Graphics.draw_image bitmap x y 
  in iter_on_cell wcf.cf draw_bitmap  

let draw_field_end wcf bd = 
  iter_on_cell wcf.cf (fun (i,j) -> draw_cell_end wcf bd i j)  

let open_wcf wcf = 
  Node.append
    (get_element_by_id "body")
    (Graphics.open_graph wcf.g_bcf.w wcf.g_bcf.h) ;
  draw_box wcf.g_bcf ;
  draw_box wcf.m_bcf ;
  draw_flag_switch wcf false ;
  draw_box wcf.f_bcf ;
  draw_field_initial wcf ;
  draw_mark_title wcf ;
  print_score wcf ((wcf.cf.nbrows*wcf.cf.nbcols)-wcf.cf.nbmines) 0 

type clickon = Out | Cell of (int*int) | SelectBox  

let locate_click wcf st1 st2 =
  let clickon_of st = 
    let x = st.Graphics.mouse_x and y = st.Graphics.mouse_y
    in if x>=wcf.s_bcf.x && x<=wcf.s_bcf.x+wcf.s_bcf.w && 
        y>=wcf.s_bcf.y && y<=wcf.s_bcf.y+wcf.s_bcf.h  
      then SelectBox
      else let (x2,y2) = wcf.coor (x,y)
      in if x2>=0 && x2<wcf.cf.nbcols && y2>=0 && y2<wcf.cf.nbrows
      then Cell (x2,y2) else Out
  in 
  let r1=clickon_of st1 and r2=clickon_of st2
  in if r1=r2 then r1 else Out 

type demin_cf = 
    { wcf : window_config; bd : cell array array;
      mutable nb_marked_cells : int; 
      mutable nb_hidden_cells : int;  
      mutable flag_switch_on : bool } 

exception Fin

let loop d f_init f_key f_mouse f_end = 
  try
    f_init ();
    while true do 
      let st = Graphics.wait_next_event 
        [Graphics.Button_down;Graphics.Key_pressed]  
      in if st.Graphics.keypressed  then f_key st.Graphics.key
        else let st2 = Graphics.wait_next_event [Graphics.Button_up] 
        in f_mouse (locate_click d.wcf st st2)
    done
  with Fin -> f_end ()

let d_init d () = open_wcf d.wcf          
let d_end  () = Graphics.close_graph()
let d_key c = if c='q' || c='Q' then raise Fin

let mark_cell d i j =
  if d.bd.(i).(j).flag 
  then ( d.nb_marked_cells <- d.nb_marked_cells -1; 
         d.bd.(i).(j).flag <- false )
  else ( d.nb_marked_cells <- d.nb_marked_cells +1 ; 
         d.bd.(i).(j).flag <- true ) ;
  draw_cell d.wcf d.bd i j ;
  print_score d.wcf d.nb_hidden_cells d.nb_marked_cells

let ending d str = 
  draw_field_end d.wcf d.bd ;
  erase_box  d.wcf.s_bcf ;
  draw_string_in_box Center str d.wcf.s_bcf Graphics.black;
  ignore(Graphics.wait_next_event 
           [Graphics.Button_down;Graphics.Key_pressed]);
  raise Fin

let reveal d i j = 
  let reveal_cell (i,j) = 
    d.bd.(i).(j).seen <- true ; 
    draw_cell d.wcf d.bd i j ;
    d.nb_hidden_cells <- d.nb_hidden_cells -1 
  in 
    List.iter reveal_cell (cells_to_see d.bd d.wcf.cf (i,j)) ;
    print_score d.wcf d.nb_hidden_cells d.nb_marked_cells ;
    if d.nb_hidden_cells = 0 then ending d "GAGNE"

let d_mouse d click = match click with 
    Cell (i,j) -> 
      if d.bd.(i).(j).seen then ()
      else if d.flag_switch_on then mark_cell d i j 
      else if d.bd.(i).(j).flag then ()
      else if d.bd.(i).(j).mined then ending d "PERDU"
      else reveal d i j
  | SelectBox -> 
      d.flag_switch_on <- not d.flag_switch_on;
      draw_flag_switch d.wcf d.flag_switch_on
  | Out -> () 

let create_demin nb_c nb_r nb_m = 
  let nbc = max default_config.nbcols nb_c 
  and nbr = max default_config.nbrows nb_r in 
  let nbm = min (nbc*nbr) (max 1 nb_m) in
  let cf = { nbcols=nbc ; nbrows=nbr ; nbmines=nbm } in 
    generate_seed () ;
    let wcf = make_wcf cf in
      {  wcf = wcf ;
	 bd = initialize_board wcf.cf;
	 nb_marked_cells = 0; 
	 nb_hidden_cells = cf.nbrows*cf.nbcols-cf.nbmines;
	 flag_switch_on = false } 

let go nbc nbr nbm = 
  let d = create_demin nbc nbr nbm in
    loop d (d_init d) d_key (d_mouse d) (d_end)

let _ = go 20 20 15
