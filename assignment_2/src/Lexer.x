{
module Lexer where

import Model
}

%wrapper "basic"

tokens :-
  $white+           ;
  "--".*            ;
  "->"              { const ArrowToken }
  "."               { const DotToken }
  ","               { const CommaToken }
  "go"              { const GoToken }
  "take"            { const TakeToken }
  "mark"            { const MarkToken }
  "nothing"         { const NothingToken }
  "turn"            { const TurnToken }
  "case"            { const CaseToken }
  "of"              { const OfToken }
  "end"             { const EndToken }
  "left"            { const LeftToken }
  "right"           { const RightToken }
  "front"           { const FrontToken }
  ";"               { const SemiColonToken }
  "Empty"           { const EmptyToken }
  "Lambda"          { const LambdaToken }
  "Debris"          { const DebrisToken }
  "Asteroid"        { const AsteroidToken }
  "Boundary"        { const BoundaryToken }
  "_"               { const UnderscoreToken }
  [0-9a-zA-Z\+\-]+  { IdentifierToken }
