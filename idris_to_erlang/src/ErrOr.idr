module ErrOr

%default total

-------------------------------------------------------------------------------
-- [Definitions]

||| An error type for convenience. The error can either contain an error message or some value
||| given some type of that value.
|||
||| For when the need of a proof is absent.
public export
data Error : Type where
  StdErr : String -> Error

||| A Maybe/Either convenience type for Error.
public export
data ErrorOr : Type -> Type where
  Err : Error -> ErrorOr ty
  Just : (x : ty) -> ErrorOr ty

-------------------------------------------------------------------------------
-- [Implementations] 

public export
implementation Functor ErrorOr where
  map f (Err err) = Err err
  map f (Just x) = Just (f x)

public export
implementation Applicative ErrorOr where
  pure x = Just x
  (Err err) <*> _ = Err err
  (Just f) <*> (Err err) = Err err
  (Just f) <*> (Just x) = Just (f x)

public export
implementation Monad ErrorOr where
  (Err err) >>= _ = Err err
  (Just x) >>= f = f x

public export
Show Error where
  show (StdErr err) = err

public export
implementation Show c => Show (ErrorOr c) where
  show (Err err) = show err
  show (Just x)  = show x


public export
error : String -> ErrorOr c
error msg = Err (StdErr msg)
