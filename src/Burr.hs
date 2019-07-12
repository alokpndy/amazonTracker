{-# LANGUAGE InstanceSigs #-}



module  Burr where


import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Monad.State
import Control.Monad.Reader 
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Cont




main :: IO () 
main = getLine >>=(\x -> putStr x   >> getLine >>= (\y -> putStr (x++y))) 

-- fmap f ma == ma >>= return . f 

newtype Red r a = Red { runRed :: r -> a }

instance Functor (Red r) where
 -- fmap :: (a -> b) -> Red r a -> Red r b
  fmap f ra = Red $  f . (runRed ra)  
  
added :: Red Integer Integer
added  = Red $  (+1)    

subtr :: Red Integer Integer
subtr = Red $ (*2)

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName
  , dogsAddress :: Address } deriving  Show


  
instance  Applicative (Red r) where
  pure :: a -> Red r a 
  pure a = Red $ \r -> a

  (<*>) :: Red r (a -> b) -> Red r a -> Red r b
  (<*>) (Red (rab)) ra = Red $  (\x -> rab x ((runRed ra) x) )
  

instance Monad (Red r) where
  return :: a -> Red r a
  return a = Red $ \r -> a

  (>>=) :: Red r a -> (a -> Red r b) -> Red r b
  (>>=) (Red ra) ara = Red $ \x -> (runRed (ara (ra x))) x 




newtype St s a = St { runST :: s -> (a , s) }

instance Functor (St s) where
  fmap :: (a -> b) -> St s a -> St s b
  fmap ab (St sa) = St $ \x ->  let (a1, s1) =  (sa x) in
                                  (ab a1, s1) 

instance Applicative (St s) where
  pure :: a -> St s a
  pure a = St $ \x -> (a, x)

  (<*>) :: St s (a -> b) -> St s a -> St s b
  (<*>) (St sab) sa = St $ \x -> let (a1,s1) = ((runST sa) x) in
                                   (let (fab, s) = sab x in
                                      (fab a1, s) )
                                   
                                
                                  

stO :: St String String
stO = St $ \s -> (s, s)

mo = pure (+1) :: St Integer (Integer -> Integer)
lo = (pure 10 :: St Integer Integer)

mo' = pure (+1) :: State Integer (Integer -> Integer)
lo' = (pure 10 :: State Integer Integer)


instance Monad (St s) where
  return :: a -> St s a 
  return a = St $ \s -> (a,s)

  (>>=) :: St s a -> (a -> St s b) -> St s b
  (>>=) (St sa) aSb = St $ \s1 -> let (a1,s2) =  (sa s1) in
                                     ((runST (aSb a1)) s2)
                                       


lo'' = runState ((>>=) (return 1 :: State Integer Integer) (return . (+1))) 1000  --- (2,1000)
lo''' = runST ((>>=) (return 1 :: St Integer Integer) (return . (+1))) 1000 -- (2,1000)



-- NobT
newtype NobT m a = NobT { runNobT ::  m (Maybe a) }

instance Functor m => Functor (NobT m) where
  fmap :: (a -> b) -> NobT m a -> NobT m b
  fmap f (NobT ma) = NobT $ (fmap . fmap) f ma

instance Applicative m => Applicative (NobT m) where
  pure :: a -> NobT m a
  pure a = NobT $ (pure . pure) a

  (<*>) :: NobT m (a -> b) -> NobT m a -> NobT m b
  (<*>) (NobT mab) (NobT ma) = NobT $ liftA2 (<*>) mab  ma -- (<*>) <$> mab <*> ma
  
 
instance Monad m => Monad (NobT m) where
  return = pure

  (>>=) :: NobT m a -> (a -> NobT m b) -> NobT m b
 -- (>>=) (NobT ma) aNam = NobT $ (ma >>= \x ->  let (Just mb) = (fmap (runNobT . aNam) x) in mb )
  (>>=) (NobT ma) aNam = NobT $ do
                             v <- ma
                             case v of
                               Nothing -> return Nothing
                               Just y -> runNobT (aNam y) 


newtype AnyT e m a = AnyT { runAnyT :: m (Either e a) }

instance Functor m => Functor (AnyT e m) where
  fmap :: (a -> b) -> AnyT e m a -> AnyT e m b
  fmap f (AnyT ema) = AnyT $ (fmap . fmap) f ema 

instance Applicative m => Applicative (AnyT e m) where
  pure :: a -> AnyT e m a
  pure a = AnyT $ (pure . pure) a

  (<*>) :: AnyT e  m (a -> b) -> AnyT e m a -> AnyT e m b
  (<*>) (AnyT emab) (AnyT ema) = AnyT $ liftA2 (<*>) emab  ema -- (<*>) <$> mab <*> ma
