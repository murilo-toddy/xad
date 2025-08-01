type piece =
    | King
    | Queen
    | Rook
    | Knight
    | Bishop
    | Pawn
;;
type color = Black | White;;
type game_piece = piece * color;;
type board = game_piece option list list

let rows = 8;;
let cols = 8;;

let pawn_row cols = List.init cols (fun _ -> Pawn);;
let piece_row = [Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook];;
let rec empty_board (rows: int) (cols: int) =
    let rec inner board row col =
        match row, col with
        (* end of iteration *)
        | 1, 0 -> board
        (* end of col, add a new row *)
        | _, 0 -> inner ([] :: board) (row - 1) cols
        (* normal case, add None to last list *)
        | _, _ ->
            match board with
            | [] -> inner [[None]] row (col - 1)
            | last :: rest -> 
                let newboard = (None :: last) :: rest in
                inner newboard row (col - 1)
    in
    inner [[]] rows cols
;;

let starting_board rows cols =
    let all_pieces c = List.map (List.map (fun p -> Some (p, c))) [pawn_row cols; piece_row] in
    let white = all_pieces White in
    let black = all_pieces Black in
    List.rev white @ empty_board 4 8 @ black
;;

let piece_repr (p: game_piece option) =
    let repr = function
      | King -> "k"
      | Queen -> "q"
      | Rook -> "r"
      | Knight -> "h"
      | Bishop -> "b"
      | Pawn -> "p"
    in
    match p with
    | None -> " "
    | Some (p, c) -> 
        let str = repr p in
        match c with
        | White -> String.uppercase_ascii str
        | Black -> str
;;

let print_board board = 
    let print_col_edges () = print_string (String.make (2 * rows + 1) '-') in
    let print_row_edges () = print_string "|" in
    let newline () = print_endline "" in
        print_col_edges (); newline ();
        List.iter (fun row ->
            let reprs = List.map piece_repr row in
            print_row_edges ();
            print_string (String.concat " " reprs);
            print_row_edges ();
            newline ()
        ) board;
        print_col_edges (); newline ()
;;

(* assumes move is valid *)
let move (from_x, from_y) (to_x, to_y) (board: board) =
    let set_piece (pos_x, pos_y) piece board =
        let update_nth ~value ~index l =
            let rec aux idx = function
                | [] -> []
                | head :: tail ->
                        if idx = index then value :: tail
                        else head :: aux (idx + 1) tail
            in
            aux 0 l
        in
        let target_row = update_nth ~index:pos_y ~value:piece (List.nth board pos_x) in
        update_nth ~index:pos_x ~value:target_row board
    in
    let current_piece = List.nth (List.nth board from_x) from_y in
    let partial = set_piece (from_x, from_y) None board in
    let new_board = set_piece (to_x, to_y) current_piece partial in
    new_board
;;

let () =
    let board = starting_board rows cols in
    print_board (move (1, 2) (5, 5) board)
;;
