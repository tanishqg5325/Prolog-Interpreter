open Lexer;;
open Parser;;
open Interpreter;;

if Array.length Sys.argv < 2 then begin
  print_string "Input file not provided.\nExiting...\n";
  exit 0;
end;;

if Array.length Sys.argv > 2 then begin
  print_string "Too many arguments.\nExiting...\n";
  exit 0;
end;;

let fstream = open_in Sys.argv.(1);;
let init_prog = Parser.program Lexer.read (Lexing.from_channel fstream);;
let _ = checkProgram init_prog;;
let prog = modifyInitialProg init_prog 1;;

print_string "Program loaded successfully\n";;

try
  while(true) do
    print_string "?- ";
    let line = read_line() in
    if line = "halt." then exit 0
    else try
      let g = Parser.goal Lexer.read (Lexing.from_string line) in
      match (interpret_goal prog g) with
          (true, _) -> print_string "true.\n"
        | (false, _) -> print_string "false.\n"
    with e -> Printf.printf "%s\n" (Printexc.to_string e)
  done

with _ -> print_string "\n% halt\n"
