{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Monad.Except
import Control.Monad.Trans.Class
import Control.Monad.Except
import GHC.Generics



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


newtype RedT r m a = RedT { runRedT :: r -> m a }

instance Functor m => Functor (RedT r m) where
  fmap :: (a -> b) -> RedT r m a -> RedT r m b
  fmap f (RedT rma) = RedT $ \r -> fmap f (rma r)
  
instance (Applicative m) => Applicative (RedT r m) where
  pure :: a -> RedT r m a
  pure a = RedT $ \r -> pure a

  (<*>) :: RedT r m (a -> b) -> RedT r m a -> RedT r m b
  (<*>) (RedT rmab) (RedT rma) = RedT $ \r -> (rmab r) <*> (rma r)

instance Monad m => Monad (RedT r m) where
  return = pure

  (>>=) :: RedT r m a -> (a -> RedT r m b) -> RedT r m b 
  (>>=) (RedT rma) (aRmb) = RedT $ \r -> do
                                 a <- rma r
                                 b <- runRedT (aRmb a) r 
                                 return b

r1 :: RedT Integer Maybe Integer
r1 = return 100

rf1 :: Integer -> RedT Integer Maybe Integer
rf1 i = return  (i + 1000)

rf2 :: Integer -> RedT Integer Maybe Integer
rf2 i = return  (i * 1000)


newtype StT s m a = StT { runStT :: s -> m (a, s) }


instance (Functor m, Monad m) => Functor (StT s m) where
  fmap :: (a -> b) -> StT s m a -> StT s m b
  fmap f k = StT $ \s -> do
                            ~(a1,s2) <- runStT k s
                            return (f a1, s2) 
                                
                                   
instance (Applicative m, Monad m) => Applicative (StT s m) where
  pure :: a -> StT s m a
  pure a = StT $ \s -> pure (a, s)

  (<*>) :: StT s m (a -> b) -> StT s m a -> StT s m b
  (<*>) (StT smab) sma = StT $ \s -> do
                               ~(f, s1) <- smab s
                               (a, s2)  <- (runStT sma s1)
                               return (f a, s2) 


instance Monad m => Monad (StT s m) where
  return :: a -> StT s m a
  return a = StT $ \s -> return (a, s)
  
  (>>=) :: StT s m a -> (a -> StT s m b) -> StT s m b
  (>>=) (StT k) f = StT $ \s -> do
                     ~(a, s')  <-  k s
                     runStT (f a) s'  
                           
                                    

                       
newtype ActionError  = ActionError String  deriving (Show)
newtype ActionEnv = ActionEnv Integer  deriving (Show)
newtype ScottyResponse = ScottyResponse Bool deriving (Show)


                      {-
newtype ActionT e m a =
  ActionT { runAM :: ExceptT (ActionError e)
                             (ReaderT ActionEnv (StateT ScottyResponse m))   -- m  ~ Reader r m  a       m ~ StateT s m a 
                             a } deriving (Functor, Applicative)
-}
--mopo :: ExceptT e m a ---- runExceptT :: m (EIther e a )
bloom :: ExceptT ActionError (ReaderT ActionError (StateT ScottyResponse Maybe))  Integer
bloom = 
       ExceptT $
              ReaderT  (\x ->
                 ( StateT (\y ->
                            Just (Left x,y)) ))



type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data User' f = User
  { pName :: HKD f String
  , pAge  :: HKD f Int
  } deriving (Generic)

