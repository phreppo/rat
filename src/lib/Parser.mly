%{ open ParserRe %}

// Tokens that end the regex.
%token <string> EOF

// Token with value attached.
%token <char>     CHAR
%token <int*int>  REPEAT
%token <int>      UNBOUNDEDREPEAT
%token <int>      BACKREFERENCE

%token EPS
%token MODIFIER // Inline modifiers such as: (?smi)

%token STAR       "*"
%token PLUS       "+"
%token PIPE       "|"
%token LPAR       "("
%token RPAR       ")"
%token DOT        "."
%token QMARK      "?"
%token SQLPAR     "["
%token SQRPAR     "]"
%token CIRCACC    "^"
%token MINUS      "-"
%token LBRACK     "{"
%token RBRACK     "}"
%token DOLLAR     "$"
%token COLUMN     ":"
%token LE         "<"
%token GT         ">"
%token P          "P"
%token SLASH      "/"

// Backslashed character with particular meaning.
%token BSL_LOWD    "\\d"
%token BSL_UPD     "\\D"
%token BSL_LOWS    "\\s"
%token BSL_UPS     "\\S"
%token BSL_LOWW    "\\w"
%token BSL_UPW     "\\W"
%token BSL_LOWB    "\\b"
%token BSL_UPB     "\\B"
%token BSL_UPA     "\\A"
%token BSL_UPZ     "\\Z"
%token BSL_LOWU    "\\u"

%start main
%type < ParserRe.t > main

%%

main:
| r = top { r }

top:
| "/"? r = regex flags =      EOF  { (r, flags) }

regex:
| EOF        { eps }
| r = choice { r   }

choice:
| a = choice "|" b = concat { choice a b }
| a = choice "|"            { choice a eps }
|            "|" b = concat { choice eps b }
| r = concat                { r }

concat:
| a = concat b = repetition { concat a b }
| r = repetition { r }

repetition:
| r = repetition "*" { star r }
| r = repetition "+" { plus r }
| r = repetition "?" { choice r eps } // ? is greedy: first it tries to match
| r = repetition lb_ub = REPEAT { let from, until = lb_ub in repeat_from_until r from until }
| r = repetition lb = UNBOUNDEDREPEAT { concat (repeat r lb) (star r) }
| r = atom { r }

atom:
| "(" r = regex ")"                       { r }
| "(" "?" ":"               r = regex ")" { r } // Non-capturing group.
| "(" "?"     "<" CHAR+ ">" r = regex ")" { r } // Capturing group with identifier.
| "(" "?" "P" "<" CHAR+ ">" r = regex ")" { r } // Capturing group with identifier.
| "(" ")"                                 { eps }
| r = char_class                          { r }
// meta sequences
| "."   { any_char          }
| "\\d" { any_digit         }
| "\\D" { any_non_digit     }
| "\\s" { any_space         }
| "\\S" { any_non_space     }
| "\\w" { any_word_char     }
| "\\W" { any_non_word_char }
// other basic atoms
| EPS               { eps             }
| c = char_in_atom  { c               }
| i = BACKREFERENCE { backreference i }
| "\\u"             { utf8            }
| "$"               { dollar }
| "\\b"             { word_boundary }
| "\\Z"             { end_of_string }
// The following productions returning epsilon are sound, but might make arise false positives.
| MODIFIER      { eps    } // TODO: support this properly.
| "\\B"         { eps    }
| "^"           { eps    }
| "\\A"         { eps    } // Same as ^.

char_in_atom:
| c = CHAR  { from_char  c  }
// minus here has no particular meaning: only inside [] is used for ranges.
| "-" { from_char '-' }
| "{" { from_char '{' }
| "}" { from_char '}' }
| ":" { from_char ':' }
| "<" { from_char '<' }
| ">" { from_char '>' }
| "P" { from_char 'P' }
| "/" { from_char '/' }

// TODO: solve the conflicts. For now it looks like it's working fine. The
// conflicts are due to the fact the ^ can appear as first character of the
// characters set, but also inside. Example: [^^] is the  set of characters that
// are not ^^.
char_class:
| "[" "^" cs = char_list "]" { Charset.complement cs |> ch }
| "["     cs = char_list "]" { ch cs }

// A sequence of characters, represented as a set.
char_list:
| c  = element_in_char_list               { c }
| cs = char_list c = element_in_char_list { Charset.union c cs }

// In character sets most of the characters keep their literal meaning: [*]
// means literally the character set {*}, and not the kleene star.
// Some modifiers are still available, for example \d.
element_in_char_list:
| "\\d" { Charset.digits         }
| "\\D" { Charset.non_digits     }
| "\\s" { Charset.spaces         }
| "\\S" { Charset.non_spaces     }
| "\\w" { Charset.word_chars     }
| "\\W" { Charset.non_word_chars }
| "\\b" { Charset.singleton '\x08'   } (* \b is backspace character inside [] *)
| "\\B" { Charset.singleton 'B'      } (* \B is 'B' inside [] *)
(* TOFIX: now regexes like [\d-w] are accepted, but they should not *)
(* This production introduces a conflict, because the minus can appear in the
   left and in the right hand side of the minus, but with different meanig.
   For example, [--0] matches the characters in {-, ., /, 0}, and not just
   {-, 0}. *)
| c1 = char_in_class "-" c2 = char_in_class { Charset.range c1 c2 }
| c1 = char_in_class "-"                    { Charset.singleton c1 |> Charset.add '-'}
| c  = char_in_class                        { Charset.singleton c }

// the special chars (as * and |) have literal meaning inside of [].
char_in_class:
| c = CHAR {  c  }
| "*"      { '*' }
| "+"      { '+' }
| "|"      { '|' }
| "("      { '(' }
| ")"      { ')' }
| "."      { '.' }
| "?"      { '?' }
| "["      { '[' }
| "^"      { '^' }
| "-"      { '-' }
| "{"      { '{' }
| "}"      { '}' }
| "$"      { '$' }
| ":"      { ':' }
| "<"      { '<' }
| ">"      { '>' }
| "P"      { 'P' }
| "/"      { '/' }
