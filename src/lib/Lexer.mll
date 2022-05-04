{
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
 open Parser

let parse_hex h =
    String.map (fun c -> if c = '\\' then '0' else c) h
      |> int_of_string |> Char.chr

let keywords =
  [
    (* common backslashed characters *)
    "\\n",  CHAR '\n'       ;
    "\\t",  CHAR '\t'       ;
    "\\a",  CHAR '\x07'     ; (* bell character *)
    "\\e",  CHAR '\x2B'     ; (* escape character *)
    "\\f",  CHAR '\x0C'     ; (* formfeed character *)
    "\\r",  CHAR '\x0D'     ; (* carriage return character *)
    "\\v",  CHAR '\x0B'     ; (* vertical tab character *)
    "\\0",  CHAR '\x00'     ; (* empty character *)
    (* meta sequences *)
    "\\d",  BSL_LOWD        ; (* backslash lower case d *)
    "\\D",  BSL_UPD         ; (* backslash upper case D *)
    "\\s",  BSL_LOWS        ;
    "\\S",  BSL_UPS         ;
    "\\w",  BSL_LOWW        ;
    "\\W",  BSL_UPW         ;
    "\\b",  BSL_LOWB        ;
    "\\B",  BSL_UPB         ;
    "\\A",  BSL_UPA         ;
    "\\Z",  BSL_UPZ         ;
    "\\u",  BSL_LOWU        ;
  ]

let kwd_table = Hashtbl.create (List.length keywords)

let _ =
  List.iter (fun (a,b) -> Hashtbl.add kwd_table a b) keywords

let remove_fst_char str =
  if String.length str = 0 then str
  else String.sub str 1 ((String.length str) - 1)

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
}

rule token = parse
| '*' { STAR    }
| '+' { PLUS    }
| '|' { PIPE    }
| '(' { LPAR    }
| ')' { RPAR    }
| '.' { DOT     }
| '?' { QMARK   }
| '[' { SQLPAR  }
| ']' { SQRPAR  }
| '^' { CIRCACC }
| '-' { MINUS   }
| '{' { LBRACK  }
| '}' { RBRACK  }
| ':' { COLUMN  }
| '<' { LE      }
| '>' { GT      }
| 'P' { P       }
| '(' '?' ('m' | 'i' | 's')* ')' { MODIFIER }
| '{' (['0'-'9']+ as d )'}'      { REPEAT (int_of_string d, int_of_string d)}
| '{' (['0'-'9']+ as d ) ',' '}' { UNBOUNDEDREPEAT (int_of_string d) }
| '{' (['0'-'9']+ as d1 ) ',' (['0'-'9']+ as d2) '}'
    { REPEAT (int_of_string d1, int_of_string d2 ) }
| '$'     { DOLLAR }
| '/'     { SLASH  }
(* special characters that have a meaning if after the backslash *)
| '\\' [
      'n' 't' 'a' 'e' 'f' 'r' 'v' '0'
      'd' 'D' 's' 'S' 'w' 'W' 'b' 'B' 'A' 'Z' 'u'
    ] as id { Hashtbl.find kwd_table id }
(* hex numbers *)
| '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] as hex { CHAR (parse_hex hex) }
(* backreferences *)
| '\\' ((['0'-'9'] | ['0'-'9']['0'-'9']) as n) { BACKREFERENCE (int_of_string n) }
(* charcters that after the backslash, they are just listerals *)
| '\\' ([^
    'n' 't' 'a' 'e' 'f' 'r' 'v' '0'
    'd' 'D' 's' 'S' 'w' 'W' 'b' 'B' 'A' 'Z' 'u'
    'x'
    '0'-'9'
    ] as c) { CHAR c } (* observe that this includes the keywords *)
(* any character but the backslash can be a single character *)
| [^ '\\'] as c { CHAR c      }
(* end of file: can optionally have flags at the end. *)
| (('/' ('g'|'s'|'a'|'m'|'i'|'x'|'U'|'H'|'R'|'P'|'O')*)? as flags) eof { EOF (remove_fst_char flags)    }
| eof { EOF ""      }
