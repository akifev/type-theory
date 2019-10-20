{
module Parser where
import Lexer
import Grammar
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    open                                    { TSym '(' }
    close                                   { TSym ')' }
    lambda                                  { TSym '\\' }
    point                                   { TSym '.' }
    var                                     { TVar $$ }

%%

Expression:
    Space lambda var point Expression       { Lambda (Just $1) $3 $5 }
    | lambda var point Expression           { Lambda Nothing $2 $4 }
    | Space                                 { Expression $1 }

Space:
    Space Atom                              { Space $1 $2 }
    |   Atom                                { SpaceAtom $1 }

Atom:
    open Expression close                   { Atom $2 }
    |   var                                 { Var $1 }

{
parseError _ = error "Parse error"
}