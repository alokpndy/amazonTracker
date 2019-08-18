{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ViewPatterns #-}
{-# language DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-} 
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# language TypeFamilies #-}

module Serve () where

import Data.Foldable 
import Data.Maybe 
import Data.Aeson 
import Servant
import GHC.Generics
import GHC.TypeLits
import Data.Traversable
import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Monoid 
import Data.Typeable
import Data.Function ((&))

import Data.Kind
import Data.Char
import Control.Monad.Writer
import Data.Aeson (Value (..), (.=), object)
import Data.Text (Text, pack)
import Data.Typeable
import qualified GHC.TypeLits as Err
import Data.Vector (fromList)


                    

-- * In Swift we can Generic Where Clauses
{-
func allItemsMatch<C1: Container, C2: Container>
    (_ someContainer: C1, _ anotherContainer: C2) -> Bool
    where C1.Item == C2.Item, C1.Item: Equatable {
-}
type family Where t cs :: Constraint where
  Where _ '[] = () 
  Where t (c ': cs) = (c t, Where t cs)


-- * forall t. is analogous to <T> in Swift
-- Also (t : Equatable) is analogous to Haskell's (Eq t)
anyD :: forall t. Where t [Show, Eq] => t -> String
anyD t = show t 

-- * Equality Constraint 
type Let = (~)
doSome :: Let m Maybe => m Int -> m Int -> Int
doSome m1 m2 = 2

data (a :: k1) :<< (b :: k2)
infixr 5 :<<
  
class Container c where
  type Item c :: Type
  format :: String -> Proxy c -> Item c 
  
instance KnownSymbol text => Container (text :: Symbol) where
  type Item text = String
  format s _ = s <> symbolVal (Proxy @text) 

instance (Container a, KnownSymbol text)
    => Container ((text :: Symbol) :<< a ) where
  type Item (text :<< a) = Item a
  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a) 

instance (Container a, Show param)
    => Container ((param :: Type) :<< a ) where
  type Item (param  :<< a ) = param -> Item a
  format s _ param = format (s <> show param) (Proxy @a) 

instance {-# OVERLAPPING #-} Container a 
    => Container (String :<< a ) where
  type Item (String  :<< a ) = String -> Item a
  format s _ param = format (s <> param) (Proxy @a) 



data Actor = E | K   deriving (Eq, Show)

data SActor (b:: Actor) where
  SE :: SActor 'E
  SK :: SActor 'K

fromSActor :: SActor b -> Actor
fromSActor SE = E
fromSActor SK = K

-- toSActor :: forall (b :: Actor). Actor -> SActor b -- * Not Possible SKolem
data SomeSActor where
  SomeSActor :: SActor b -> SomeSActor

elimSomeActor :: (forall (b :: Actor). SActor b -> r) -> SomeSActor -> r
elimSomeActor f (SomeSActor b) = f b

toSActor :: Actor -> SomeSActor
toSActor E = SomeSActor SE
toSActor K = SomeSActor SK



class Generica a where
  type Repa (a :: Type) :: Type -> Type
  from :: a -> Repa a x
  to :: Repa a x -> a
{-
Rep Bool :: * -> *
= D1
    ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
    (C1 ('MetaCons "False" 'PrefixI 'False) U1
     :+: C1 ('MetaCons "True" 'PrefixI 'False) U1)

~~~~~> = ..(  U1) :+: ..(U1)
              ()   |     ()    -- True | False

-}
{-

{ "title": "Person"
, "type": "object"
, "properties":
    { "name" : { "type": "string" }
    , "age"  : { "type": "integer"}
    , "phone": { "type": "string" }
    , "permissions":
        { "type": "array", "items": { "type": "boolean" }}
    }
, "required": ["name", "age", "permission"]
}

-}

class GSchema (a :: Type -> Type) where
   gschema :: Writer [Text] Value

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b

emitRequired :: forall nm. KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm

type family ToJSONType (a :: Type) :: Symbol where
  ToJSONType Int = "integer"
  ToJSONType Integer  = "integer"
  ToJSONType Float = "number"
  ToJSONType Double = "number"
  ToJSONType String = "string"
  ToJSONType Bool = "boolean"
  ToJSONType [a] = "array"
  ToJSONType a = TypeName a 


type family RepName (x :: Type -> Type) :: Symbol where
  RepName (D1 ('MetaData nm _ _ _) _) = nm
type family TypeName (t :: Type) :: Symbol where
  TypeName t = RepName (Rep t)

makeTypeObj :: forall a. KnownSymbol (ToJSONType a) => Value
makeTypeObj = object [ "type" .=
                       String (pack . symbolVal $ Proxy @(ToJSONType a)) ]

makePropertyObj :: forall name. (KnownSymbol name) => Value -> Value
makePropertyObj v = object [ pack (symbolVal $ Proxy @name) .= v ]

instance (KnownSymbol nm, KnownSymbol (ToJSONType a))
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3)
                    (K1 _4 a)) where
   gschema = do
        emitRequired @nm
        pure . makePropertyObj @nm
             $ makeTypeObj @a
   {-# INLINE gschema #-}

instance (GSchema f, GSchema g) => GSchema (f :*: g) where
  gschema =
    mergeObjects <$> gschema @f
       <*> gschema @g
  {-# INLINE gschema #-}




instance GSchema a => GSchema (M1 C _1 a) where
  gschema = gschema @a
  {-# INLINE gschema #-}


instance (GSchema a, KnownSymbol nm)
    => GSchema (M1 D ('MetaData nm _1 _2 _3) a) where
  gschema = do
    sch <- gschema @a
    pure $ object
      [ "title" .= (String . pack . symbolVal $ Proxy @nm)
      , "type" .= String "object"
      , "properties" .= sch
      ] 
  {-# INLINE gschema #-}


schema :: forall a. (GSchema (Rep a), Generic a) => Value
schema =
  let (v, reqs) = runWriter $ gschema @(Rep a)
    in mergeObjects v $ object [ "required" .=
       Array (fromList $ String <$> reqs) ]
{-# INLINE schema #-}

data Person = Person { name :: String, age :: Integer } deriving (Generic) 
