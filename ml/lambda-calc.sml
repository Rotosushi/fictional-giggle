(*
  A standard ML implementation of a lambda calculus interpreter
  as described in "The Working Programmers Guide To ML"

  the interpreter follows the usual internal structure:

    lex -> parse -> reduce -> print result -> lex ...

*)



(*
  Here we lay out the structure of the lexical
  analysis datatype.

  we break up the input text into tokens of either
  Identifiers or Keywords. storing the matched text with the token.
  the book doesn't give a treatment of natural numbers,
  but they would certainly get a new keyword in the lexer.

  any lexer will also provide a function named scan which
  takes some string as input and returns a list of tokens.

*)
signature LEXER =
  sig
  datatype token = ID of string | KEY of string | NUM of int
  val scan : string -> token list
end;

(*
  we give the keywords of the language as a
  structure to the lexer, which allows the lexer
  to be much more flexible to extension.

  alphas defines a list of keywords which
  consist of alphabet characters like:
    "if" and "let"

  symbols defines a list of key symbols like:
    "(" and ")"
*)
signature KEYWORD =
  sig
  val alphas  : string list
  and symbols : string list
end;


functor Lexer (Keyword: KEYWORD): LEXER =
  struct
  datatype token = ID  of string
                 | KEY of string
                 | NUM of int;

  (* is x a member of l? (some list of strings)*)
  fun member (x: string, l) = List.exists (fn y => x=y) l;

  (*
  if a is a member of the keywords then return that keyword
  else it's an identifier.
  *)
  fun alphaTok a =
    if member(a, Keyword.alphas) then KEY(a) else ID(a);


  (*
  symbolic
    this function scans a symbolic keyword
    "+" or "-" or "(" or ")" or ";" etc

    this is a straightforward recursive implementation.
    Substring.getc returns none is the string is empty
    so we return the passed char as a key, paired with
    the empty input, because we had to have predicted
    a symbolic keyword to call symbolic and sy contains
    that character. (in this particular state)
    if we read some char it's bound to c the rest of the substring
    is bound to ss1.

    the conditional is deciding between two paths,
    either, the string sy exists in the keywords,
    then we construct the token with the sy string,
    and return the valid token, paired to the rest of the input.
    else we append our new symbol char and see if that is a valid
    symbol keyword.

    this function is serviceable if your intention is
    to treat multichar operators in the parser
    (saying '+' && '=' instead of "+=")
    because the function will always match the operator
    "+" over another operator "+="; given how it is currently
    written.
  *)
fun symbolic (sy, ss) =
  case Substring.getc ss of
      NONE          => (KEY sy, ss)
    | SOME (c, ss1) =>
      if member(sy, Keyword.symbols)
        orelse not (Char.isPunct c)
      then (KEY sy, ss)
      else symbolic (sy ^ String.str c, ss1);


fun numeric (ns, ss) =
  case Substring.getc ss of
      NONE          => (NUM o Int.fromString ns, ss)
    | SOME (c, ss1) =>
      if Char.isDigit c
      then numeric (ns ^ String.str c, ss1)
      else (NUM o Int.fromString ns, ss1);


(*
  scanning
  this is the function which drives the scanner
  breaking up the input sequence into a sequence of tokens.

  this is the straightforward functional implementation,
  which lexes the whole input string in a single pass and
  constructs a list of tokens for the parser to consume.
*)
fun scanning (toks, ss) =
  case Substring.getc ss of
      NONE      => rev toks
    | SOME      =>
      if Char.isAlpha c
      then (* identifier or keyword *)
        let val (id, ss2) = Substring.splitl Char isAlphaNum ss
            val tok       = alphaTok (Substring.string id)
        in scanning (tok::toks, ss2);
        end
      else if Char.isPunct c
      then (* symbolic keyword *)
        let val (tok, ss2) = symbolic (String.str c, ss1)
        in scanning (tok::toks, ss2);
        end
      else if Char.isDigit c
      then (* integer literal *)
        let val (tok, ss2) = numeric (String.str c, ss1)
        in scanning (tok::toks, ss2);
        end
      else (* eat whitespace *)
        scanning (toks, Substring.dropl (not o Char.isGraph) ss);

fun scan input = scanning([], Substring.all input);
end;


(*----------------------------------------------------------------*)

(*
Parsing primitives.
the trivial parsers recognize the basic
informational units of the program,
the Identifier, the nubers,
and the keywords.

then we have combinators to
build up parsers of the sentances in the
grammar. the basic combinators are:
alternates, sequences, and repetition.
(notice djikstra's three)

*)

(*
declare the fixity of our
parser combinator operators
the exact numbers are arbitrary
they just express the ordering.
*)
infix 6 $--;
infix 5 --;
infix 3 >>;
infix 0 ||;


(* the type signature of our functional parser combinators *)
signature PARSER =
  sig
  exception SyntaxErr of string
  type token
  val id      : token list -> string * token list
  val $       : string -> token list -> string * token list
  val empty   : 'a -> 'b list * 'a
  val ||      : ('a -> 'b) * ('a -> 'b) -> 'a -> 'b
  val !!      : ('a -> 'b * 'c) -> 'a -> 'b * 'c
  val --      : ('a -> 'b * 'c) * ('c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e
  val $--     : string * (token list -> 'a * 'b) -> token list -> 'a * 'b
  val >>      : ('a -> 'b * 'c) * ('b -> 'd) -> 'a -> 'd * 'c
  val repeat  : ('a -> 'b * 'a) -> 'a -> 'b list * 'a
  val infixes :
    (token list -> 'a * token list) * (string -> int) *
    (string -> 'a -> 'a -> 'a) -> token list -> 'a * token list
  val reader : (token list -> 'a * 'b list) -> string -> 'a
end


(* the implementation of the parser combinators *)
functor Parser (Lex: LEXER) : PARSER =
  struct
  type token = Lex.token;

  exception SyntaxErr of string;

  (*
    this primitive combinator matches the Id token
    and returns the Token paired with the rest.
  *)
  fun id (Lex.ID a :: toks) = (a, toks)
   |  id toks               = raise SyntaxErr "Identifier expected!";
(*
  this primitive combinator matches keywords
  in the language and returns the keyword paired
  with the the rest.
*)
  fun $a (Lex.KEY b :: toks) = if a = b
                               then (a, toks)
                               else raise SyntaxErr a
  |   $a _                   = raise SyntaxErr "Keyword expected";

  fun empty toks = ([], toks);

  fun (ph1 || ph2) toks = ph1 toks handle SyntaxErr _ => ph2 toks;

  fun !! ph toks = ph toks handle SyntaxErr msg =>
                           raise Fail ("Syntax error: " ^ msg);

  fun (ph1 -- ph2) toks =
    let val (x, toks2) = ph1 toks
        val (y, toks3) = ph2 toks2
    in ((x, y), toks3)
    end;

  fun (ph >> f) toks =
    let val (x, toks2) = ph toks
    in (f x, toks2)
    end;

  fun (a $-- ph) = ($a -- !!ph >> #2);

  fun repeat ph toks = (ph -- repeat ph >> (op::) || empty ) toks;

  (* this looks like the classic operator precedence parser algorithm *)
  fun infixes (ph, prec_of, apply) =
    let fun over k toks = next k (ph toks)
        and next k (x, Lex.Key(a)::toks) =
          if prec_of a < k
          then (x, Lex.Key a :: toks)
          else next k ((over (prec_of a) >> apply a x) toks)
        |  next k (x, toks) = (x, toks)
    in over 0
    end;


  fun reader ph a =
    (case ph (Lex.scan a) of
          (x, []) => x
        | (_, _::_) => raise SyntaxErr "Extra characters in phrase"
    );

  end;
