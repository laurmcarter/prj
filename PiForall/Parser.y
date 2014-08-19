
{
module Main where
}

%name PiForall
%tokentype { Token }
%error { parseError }

%monad { E } { (>>=) } { return }

%token
    let         { Let _ $$ }
    in          { In  _ $$ }

{

data E a = Ok a | Failed AlexPosn String

instance Functor E where
  fmap f (Ok a) = Ok $ f a
  fmap f failed = failed

}
