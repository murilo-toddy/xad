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
type coordinate = int * int;;

type state =
    | Running
    | Draw
    | Win of color
;;

let rows = 8;;
let cols = 8;;

let explode (s: string) =
    let rec exp idx l = if idx < 0 then l else exp (idx - 1) (s.[idx] :: l) in
    exp (String.length s - 1) []
;;

let coord_of_string s =
    if String.length s != 2
    then None
    else match explode s with
    (* chess notation shows col first *)
    | [col_char; row_char] ->
        let int_from_ref ~ref ~value = int_of_char value - int_of_char ref in
        Some (int_from_ref ~ref:'1' ~value:row_char, int_from_ref ~ref:'a' ~value:col_char)
    | _ -> None
;;

let string_of_coord (row, col) =
    let string_from_ref ~ref ~value =
        String.make 1 (char_of_int (value + int_of_char ref))
    in
    string_from_ref ~value:col ~ref:'a' ^ string_from_ref ~value:row ~ref:'1'
;;

let piece_of_char char =
    let lower_char = Char.lowercase_ascii char in
    let color = if char = lower_char then Black else White in
    let piece = match lower_char with
    | 'k' -> King
    | 'q' -> Queen
    | 'r' -> Rook
    | 'b' -> Bishop
    | 'n' -> Knight
    | 'p' -> Pawn
    | _ -> assert false
    in
    (piece, color)
;;

let board_of_fen (fen: string) =
    let prepend board sub =
        match board with
        | [] -> [sub]
        | hd :: tl -> (sub @ hd) :: tl
    in
    let rec inner (board: board) (fen: char list) =
        match fen with
        | [] -> List.map List.rev board
        | ch :: fen ->
            match ch with
            (* end of current row *)
            | '/' -> inner ([] :: board) fen
            (* prepend N spaces *)
            | '0'..'9' -> 
                let size = int_of_char ch - int_of_char '0' in
                let newboard = prepend board @@ List.init size (fun x -> None) in
                inner newboard fen
            | _ -> inner (piece_of_char ch |> Option.some |> (fun p -> [p]) |> prepend board) fen
    in
    inner [[]] @@ explode fen
;;


let string_of_piece (p: game_piece option) =
    let repr = function
      | King -> "k"
      | Queen -> "q"
      | Rook -> "r"
      | Knight -> "n"
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

let piece_at row col (board: board) =
    if valid_coordinates (row, col)
    then List.nth (List.nth board row) col
    else None
;;

(* linear lookup, returns first find *)
let search_piece (piece: piece) (color: color) (board: board) =
    let rec search_row row =
        let rec search_col col =
            if col >= cols
            then search_row (row + 1)
            else
                match piece_at row col board with
                | Some (p, c) when p = piece && c = color -> Some (row, col)
                | _ -> search_col (col + 1)
        in
        if row >= rows
        then None
        else search_col 0
    in
    search_row 0
;;

let search_king color board =
    match search_piece King color board with
    | Some pos -> pos
    | None -> assert false
;;

(* TODO: this function is pretty bad *)
let print_board_with_moves board moves =
    let colored = "\027[41m" in
    let reset = "\027[0m" in
    let print_col_edges () = print_string ("  " ^ String.make (2 * rows + 1) '-') in
    let print_row_edges () = print_string (reset ^ "|") in
    let newline () = print_endline "" in
    print_col_edges ();
    newline ();
    for row = 0 to (rows - 1) do
        Printf.printf "%d " @@ rows - row;
        print_row_edges ();
        let rec to_row_str acc col =
            if col = cols
            then acc
            else
                (* TODO: clean this up, reversing so that A1 is in the bottom left *)
                let piece = string_of_piece (piece_at (rows - row - 1) col board) in
                let colored_piece =
                    if List.exists (
                        fun (x, y) -> (x, y) = (rows - row - 1, col)
                    ) moves
                    then colored ^ piece
                    else reset ^ piece in
                to_row_str (acc @ [colored_piece]) (col + 1)
        in
        print_string (String.concat " " (to_row_str [] 0));
        print_row_edges ();
        newline ();
    done;
    print_col_edges ();
    newline ();
    let row_coords = "a b c d e f g h" in
    print_endline ("   " ^ row_coords);
;;

let print_board board = print_board_with_moves board [];;

let print_game board player =
    print_endline "";
    string_of_color player |> String.cat "Next player: " |> print_endline;
    print_board board;
;;

let next_player = function
    | White -> Black
    | Black -> White
;;

let valid_moves row col board: (int * int) list =
    (* 
      TODO: handle cases
      - king currently in check
      - moving a piece would put king in check
    *)
    match piece_at row col board with
    | None -> []
    | Some (piece, color) ->
        let doesnt_contain_friend (row, col) =
           match piece_at row col board with
           | Some (_, c) when c = color -> false
           | _ -> true
        in
        (* goes out in specific direction until other piece is found *)
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
        let navigate_rows row col board =
            navigate (row, col) (1, 0) board []
                @ navigate (row, col) (-1, 0) board []
                @ navigate (row, col) (0, 1) board []
                @ navigate (row, col) (0, -1) board []
        in
        let navigate_diags row col board =
            navigate (row, col) (1, 1) board []
                @ navigate (row, col) (1, -1) board []
                @ navigate (row, col) (-1, 1) board []
                @ navigate (row, col) (-1, -1) board []
        in
        let moves = match piece with
        | King ->
            let moves = [
                (row + 1, col + 1);
                (row - 1, col + 1);
                (row + 1, col - 1);
                (row - 1, col - 1);
            ] in
            List.filter doesnt_contain_friend moves
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
            List.filter doesnt_contain_friend moves
        | Rook -> navigate_rows row col board
        | Bishop -> navigate_diags row col board
        | Queen ->
            navigate_rows row col board
                @ navigate_diags row col board
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
        in
        List.filter valid_coordinates moves

(* TODO(perf): update to set *)
let all_moves_for color board =
    let rec search_row row acc =
        let rec search_col col acc =
            if col >= cols
            then search_row (row + 1) acc
            else
                match piece_at row col board with
                | Some (p, c) when c != color -> search_col (col + 1) (acc @ (valid_moves row col board))
                | _ -> search_col (col + 1) acc
        in
        if row >= rows
        then acc
        else search_col 0 acc
    in
    search_row 0 []
;;

let is_check board color =
    let king_pos = search_king color board in
    let moves_for_opponent = all_moves_for (next_player color) board in
    List.exists (fun pos -> pos = king_pos) moves_for_opponent
;;

let print_valid_moves color (row, col) moves board =
    Printf.printf "Valid moves for %s at %s: " (string_of_color color) (string_of_coord (row, col));
    List.iter (fun (row, col) -> Printf.printf "%s " (string_of_coord (row, col))) moves;
    print_endline "";
    print_board_with_moves board moves;
    print_endline "";
;;

let is_move_valid (from_row, from_col) (to_row, to_col) (player: color) (board: board) =
    match piece_at from_row from_col board with
    | None -> false
    | Some (_, color) when player != color -> false
    | Some (piece, color) ->
        let moves = valid_moves from_row from_col board in
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

(* check if player's king is in check after move *)
let move_if_valid from_coord to_coord (player: color) (board: board) =
    let new_board, next_player = move from_coord to_coord board, next_player player in
    if is_move_valid from_coord to_coord player board &&
        not (is_check board player)
    then new_board, next_player
    else (
        Printf.printf
            "ERROR: Invalid move from %s to %s for %s\n"
                (string_of_coord from_coord)
                (string_of_coord to_coord)
                (string_of_color player)
        ;
        board, player
    )
;;

let check_game_over player board =
    Running
;;

let play_turn (from_row, from_col) (to_row, to_col) player board =
    let board, next_player = move_if_valid (from_row, from_col) (to_row, to_col) player board in
    let new_state = check_game_over player board in
    (new_state, board, next_player)
;;

(*
  Engine
  - print board state
  - get next play
  - validate next play
  - make move
  - check end condition
*)
let play_game board =
    let read_move () =
        let rec read_coord () =
            print_string "select a piece: ";
            let line = String.trim (read_line ()) in
            match coord_of_string line with
            | Some coord -> coord
            | None ->
                print_endline "invalid input";
                read_coord ()
        in
        read_coord ()
    in
    let player = White in
    let state = Running in
    let rec game_loop (state: state) (board: board) (player: color) =
        print_game board player;
        let (from_row, from_col) = read_move () in
        let moves = valid_moves from_row from_col board in
        print_valid_moves player (from_row, from_col) moves board;
        let (to_row, to_col) = read_move () in
        let state, board, next_player = play_turn (from_row, from_col) (to_row, to_col) player board in
        match state with
        | Running -> game_loop state board next_player
        | Draw ->
            print_string "game over: draw\n";
            exit 0;
        | Win color ->
            Printf.printf "game over: winner %s\n" (string_of_color color);
            exit 0;
    in
    game_loop state board player
;;

let starting_position = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";;

let () = play_game @@ board_of_fen starting_position;;
