{
module Parser where

import Model
}

%name parseProgram
%tokentype { Token }

%token
  "->"              { ArrowToken }
  "."               { DotToken }
  ","               { CommaToken }
  "go"              { GoToken }
  "take"            { TakeToken }
  "mark"            { MarkToken }
  "nothing"         { NothingToken }
  "turn"            { TurnToken }
  "case"            { CaseToken }
  "of"              { OfToken }
  "end"             { EndToken }
  "left"            { LeftToken }
  "right"           { RightToken }
  "front"           { FrontToken }
  ";"               { SemiColonToken }
  "Empty"           { EmptyToken }
  "Lambda"          { LambdaToken }
  "Debris"          { DebrisToken }
  "Asteroid"        { AsteroidToken }
  "Boundary"        { BoundaryToken }
  "_"               { UnderscoreToken }
  Ident             { IdentifierToken $$ }

%%

Program : Rules { Program $1 }

Rules : Rules_ { reverse $1 }

Rules_ :
  {- empty -} { [] }
  | Rules_ Rule { $2 : $1 }
  
Rule : Ident "->" Commands "." { Rule $1 $3 }

Commands : Commands_ { reverse $1 }

Commands_ :
  {- empty -} { [] }
  | Command { [$1] }
  | Commands_ "," Command { $3 : $1 }

Command : 
    "go" { GoCommand }
  | "take" { TakeCommand }
  | "mark" { MarkCommand }
  | "nothing" { NothingCommand }
  | "turn" Direction { TurnCommand $2 }
  | "case" Direction "of" Alts "end" { CaseCommand $2 $4 }
  | Ident { FunctionCall $1 }

Direction :
    "left" { LeftDirection }
  | "right" { RightDirection }
  | "front" { FrontDirection }

Alts : Alts_ { reverse $1 }

Alts_ :
  {- empty -} { [] }
  | Alt { [$1] }
  | Alts_ ";" Alt { $3 : $1 }

Alt : Pattern "->" Commands { Alt $1 $3 }

Pattern : 
    "Empty" { EmptyPattern }
  | "Lambda" { LambdaPattern }
  | "Debris" { DebrisPattern }
  | "Asteroid" { AsteroidPattern }
  | "Boundary" { BoundaryPattern }
  | "_" { UnderscorePattern }

{

happyError _ = error "parse error"

}
