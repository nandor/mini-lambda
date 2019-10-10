(* Compiler Construction - Minimal Lambda Language
 *
 * Entry point to the compiler: parses arguments and runs the
 * code generation pipeline. In case of an error, a diagnostic
 * is emitted pointing to a location in the source code.
 *)

let in_chan = ref stdin
let out_chan = ref stdout
let backend = ref Backend_x86_64.compile

let usage = "usage: " ^ Sys.argv.(0) ^ "[-o out] [-x86_64] input"

let speclist =
  [ ( "-x86_64"
    , Arg.Unit (fun () -> backend := Backend_x86_64.compile)
    , ": x86_64 target"
    )
  ;  ( "-armv7"
    , Arg.Unit (fun () -> backend := Backend_armv7.compile)
    , ": armv7 target"
    )
  ; ( "-o"
    , Arg.String (fun s -> out_chan := open_out s)
    , ": output stream"
    )
  ]

let () =
  Arg.parse speclist (fun s -> in_chan := open_in s) usage;
  let lexbuf = Lexing.from_channel !in_chan in
  try
    let ast = Parser.program Lexer.token lexbuf in
    let typed_ast = Typing.check ast in
    let ir = Ir_lowering.lower typed_ast in
    !backend ir !out_chan
  with
  | Lexer.Error(lnum, cnum, chr) ->
    Printf.eprintf "(%d:%d) lexer error: invalid character '%c'\n"
      lnum
      cnum
      chr;
    exit 1
  | Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.eprintf "(%d:%d) parser error: invalid token '%s'\n"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
      (Lexing.lexeme lexbuf);
    exit 1
  | Typing.Error(pos, msg) ->
    Printf.eprintf "(%d, %d) type error: %s\n"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
      msg;
    exit 1

