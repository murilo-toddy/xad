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

let string_of_piece (p: game_piece option) =
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

let string_of_color = function
    | White -> "White"
    | Black -> "Black"
;;

let valid_coordinates (row, col) = row >= 0 && row < rows && col >= 0 && col < cols;;

let piece_at row col board =
    if valid_coordinates (row, col)
    then List.nth (List.nth board row) col
    else None
;;

let print_board_with_moves board moves =
    let print_col_edges () = print_string ("  " ^ String.make (2 * rows + 1) '-') in
    let print_row_edges () = print_string "|" in
    let newline () = print_endline "" in
    print_col_edges (); newline ();
    let rec print_row row =
        if row >= 8 
        then ()
        else
            let rec print_cell col =
                if col >= 8
                then ()
                else
                    print_string (string_of_piece (piece_at row col board));
                    print_cell (col + 1)
            in
            Printf.printf "%d " row;
            print_row_edges ();
            print_row_edges ();
            newline ();
            print_row (row + 1);
            print_cell 0
    in
    print_row 0;
    print_col_edges (); newline ();
    print_endline "   0 1 2 3 4 5 6 7";
;;

let print_board board = print_board_with_moves board [];;

let print_game board player =
    print_endline "";
    print_endline ("Next player: " ^ (string_of_color player));
    print_board board;
;;

let next_player = function
    | White -> Black
    | Black -> White
;;

let valid_moves row col piece color board: (int * int) list =
    (* 
      TODO: handle cases
      - king currently in check
      - moving a piece would put king in check
    *)
    let contains_friend (row, col) =
       match piece_at row col board with
       | Some (_, c) when c = color -> false
       | _ -> true
    in
    let moves = match piece with
    | King ->
        let moves = [
            (row + 1, col + 1);
            (row - 1, col + 1);
            (row + 1, col - 1);
            (row - 1, col - 1);
        ] in
        List.filter contains_friend moves
    | Rook ->
        let rec navigate (row, col) (dir_row, dir_col) board moves =
            let (next_row, next_col) = (row + dir_row, col + dir_col) in
            if valid_coordinates (next_row, next_col) then
                match piece_at next_row next_col board with
                | Some (p, c) when c = color -> moves
                | Some (p, c) when c != color -> (next_row, next_col) :: moves
                | _ ->
                    let moves = (next_row, next_col) :: moves in
                    navigate (next_row, next_col) (dir_row, dir_col) board moves
            else moves
        in
        navigate (row, col) (1, 0) board []
            @ navigate (row, col) (-1, 0) board []
            @ navigate (row, col) (0, 1) board []
            @ navigate (row, col) (0, -1) board []
    | Knight ->
        let moves = [
            (row + 1, col + 2);
            (row + 2, col + 1);
            (row - 1, col + 2);
            (row - 2, col + 1);
            (row + 1, col - 2);
            (row + 2, col - 1);
            (row - 1, col - 2);
            (row - 2, col - 1);
        ] in
        List.filter contains_friend moves
    | Pawn ->
        (* TODO: en passant *)
        let direction = if color = White then 1 else -1 in
        let moves = match piece_at (row + direction) col board with
        | Some (p, _) -> []
        | _ -> [(row + direction, col)] 
        in
        let moves = match (color, row) with
        | White, 1
        | Black, 6 -> (row + 2 * direction, col) :: moves
        | _ -> moves
        in
        let capture (at_row, at_col) =
            match piece_at at_row at_col board with
            | Some (_, c) when c != color -> [(at_row, at_col)]
            | _ -> []
        in
        let moves = moves 
            @ capture (row + direction, col + 1) 
            @ capture (row + direction, col - 1) 
        in
        moves
    | _ -> []
    in
    List.filter valid_coordinates moves

let print_valid_moves color (row, col) moves =
    Printf.printf "Valid moves for %s at (%d, %d): " (string_of_color color) row col;
    List.iter (fun (row, col) -> Printf.printf "(%d, %d) " row col) moves;
    print_endline "";
;;

let is_move_valid (from_row, from_col) (to_row, to_col) (player: color) (board: board) =
    match piece_at from_row from_col board with
    | None -> false
    | Some (_, color) when player != color -> false
    | Some (piece, color) ->
        let moves = valid_moves from_row from_col piece color board in
        print_valid_moves player (from_row, from_col) moves;
        List.exists (fun (pos_row, pos_col) -> 
            (pos_row, pos_col) = (to_row, to_col)
        ) moves
;;

let move (from_row, from_col) (to_row, to_col) (board: board) =
    let set_piece (pos_row, pos_col) piece board =
        let update_nth ~value ~index l =
            let rec aux idx = function
                | [] -> []
                | head :: tail ->
                    if idx = index then value :: tail
                    else head :: aux (idx + 1) tail
            in
            aux 0 l
        in
        let target_row = update_nth ~index:pos_col ~value:piece (List.nth board pos_row) in
        update_nth ~index:pos_row ~value:target_row board
    in
    let current_piece = List.nth (List.nth board from_row) from_col in
    let partial = set_piece (from_row, from_col) None board in
    let new_board = set_piece (to_row, to_col) current_piece partial in
    new_board
;;

let move_if_valid (from_row, from_col) (to_row, to_col) (player: color) (board: board) =
    if is_move_valid (from_row, from_col) (to_row, to_col) player board
    then move (from_row, from_col) (to_row, to_col) board, next_player player
    else (
        Printf.printf
            "ERROR: Invalid move from (%d, %d) to (%d, %d) for %s\n"
            from_row from_col to_row to_col (string_of_color player)
        ;
        board, player
    )
;;

let () =
    let board = starting_board rows cols in
    let player = Black in
    let board = move (6, 0) (7, 7) board in
    let board, player = move_if_valid (7, 0) (5, 0) player board in
    let player = Black in
    let board, player = move_if_valid (5, 0) (5, 2) player board in
    print_game board player
;;
