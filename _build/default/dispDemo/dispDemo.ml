open Images
open Rgba32
open Graphics

let () = Graphics.open_graph " 1000x600";;

draw_rect 500 200 300 200;
(*captured pieces*)
draw_rect 500 5 300 200;
(*log*)
draw_rect 5 500 150 75;
(*timer*)
draw_rect 825 500 150 75;

(*menu*)
moveto 30 530;
draw_string "Timer goes here";

moveto 580 300;
draw_string "Captured pieces go here";

moveto 580 100;
draw_string "Log goes here";

moveto 850 530;
draw_string "Menu goes here"

(*library doesnt seem to support transparency in pngs, so map function
  from unused color to transparent*)
let make_transp a = if a = 0x656654 then transp else a

let make_transp_image a = Array.map make_transp a

let bdt =
  []
  |> Png.load "assets/Chess_bdt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let blt =
  []
  |> Png.load "assets/Chess_blt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let kdt =
  []
  |> Png.load "assets/Chess_kdt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let klt =
  []
  |> Png.load "assets/Chess_klt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let ndt =
  []
  |> Png.load "assets/Chess_ndt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let nlt =
  []
  |> Png.load "assets/Chess_nlt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let pdt =
  []
  |> Png.load "assets/Chess_pdt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let plt =
  []
  |> Png.load "assets/Chess_plt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let qdt =
  []
  |> Png.load "assets/Chess_qdt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let qlt =
  []
  |> Png.load "assets/Chess_qlt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let rdt =
  []
  |> Png.load "assets/Chess_rdt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let rlt =
  []
  |> Png.load "assets/Chess_rlt60.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let no_piece =
  []
  |> Png.load "assets/no_piece.png"
  |> Graphic_image.of_image |> dump_image
  |> Array.map make_transp_image
  |> make_image

let board = [] |> Png.load "assets/board.png" |> Graphic_image.of_image

(*static starting board*)
let current_board =
  [|
    rlt;
    nlt;
    blt;
    qlt;
    klt;
    blt;
    nlt;
    rlt;
    plt;
    plt;
    plt;
    plt;
    plt;
    plt;
    plt;
    plt;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    no_piece;
    pdt;
    pdt;
    pdt;
    pdt;
    pdt;
    pdt;
    pdt;
    pdt;
    rdt;
    ndt;
    bdt;
    qdt;
    kdt;
    bdt;
    ndt;
    rdt;
  |]

(*x coordinates*)
let x_order =
  [|
    5;
    55;
    105;
    155;
    205;
    255;
    305;
    355;
    5;
    55;
    105;
    155;
    205;
    255;
    305;
    355;
    5;
    55;
    105;
    155;
    205;
    255;
    305;
    355;
    5;
    55;
    105;
    155;
    205;
    255;
    305;
    355;
    5;
    55;
    105;
    155;
    205;
    255;
    305;
    355;
    5;
    55;
    105;
    155;
    205;
    255;
    305;
    355;
    5;
    55;
    105;
    155;
    205;
    255;
    305;
    355;
    5;
    55;
    105;
    155;
    205;
    255;
    305;
    355;
  |]

(*y coordinates*)
let y_order =
  [|
    5;
    5;
    5;
    5;
    5;
    5;
    5;
    5;
    55;
    55;
    55;
    55;
    55;
    55;
    55;
    55;
    105;
    105;
    105;
    105;
    105;
    105;
    105;
    105;
    155;
    155;
    155;
    155;
    155;
    155;
    155;
    155;
    205;
    205;
    205;
    205;
    205;
    205;
    205;
    205;
    255;
    255;
    255;
    255;
    255;
    255;
    255;
    255;
    305;
    305;
    305;
    305;
    305;
    305;
    305;
    305;
    355;
    355;
    355;
    355;
    355;
    355;
    355;
    355;
  |]

(*draw view for player on light side*)
let draw_dt current_board =
  Graphics.draw_image board 5 5;
  for i = 0 to 63 do
    let a = Array.get current_board (63 - i) in
    let x = Array.get x_order i in
    let y = Array.get y_order i in
    Graphics.draw_image a x y
  done

(*draw view for player on dark side*)
let draw_lt current_board =
  Graphics.draw_image board 5 5;
  for i = 0 to 63 do
    let a = Array.get current_board i in
    let x = Array.get x_order i in
    let y = Array.get y_order i in
    Graphics.draw_image a x y
  done
;;

while true do
  if button_down () then draw_dt current_board
  else draw_lt current_board
done
