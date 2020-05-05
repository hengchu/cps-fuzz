module HFunctor where

newtype K a b = K a
  deriving (Show, Eq, Ord, Functor)

newtype DeriveHXFunctor    h     f a = DeriveHXFunctor    (h f a)
newtype DeriveHInjectTrans h j l r a = DeriveHInjectTrans (h r a)

-- | This is right-associative so we can pattern match on the first type
-- parameter in typeclass instances.
infixr 6 :+:

infixr 9 :.:

type h :.: j = HComp h j

newtype HComp h j r a where
  HComp :: h (j r) a -> HComp h j r a

data
  (f :: (* -> *) -> * -> *)
    :+: (g :: (* -> *) -> * -> *) :: (* -> *) -> * -> * where
  Inl :: f r a -> (f :+: g) r a
  Inr :: g r a -> (f :+: g) r a

unK :: K a b -> a
unK (K a) = a

instance Semigroup m => Semigroup (K m a) where
  (K a) <> (K b) = K (a <> b)

instance Monoid m => Monoid (K m a) where
  mempty = K mempty

data HMaybe f a where
  HJust :: f a -> HMaybe f a
  HNothing :: HMaybe f a
  deriving (HXFunctor) via (DeriveHXFunctor HMaybe)

-- | Take the fixpoint of a functor-functor.
data HXFix (h :: (* -> *) -> * -> *) (f :: * -> *) (a :: *) where
  HXFix :: h (HXFix h f) a -> HXFix h f a
  Place :: f a -> HXFix h f a

data HFix (h :: (* -> *) -> * -> *) (a :: *) where
  HFix :: h (HFix h) a -> HFix h a

-- | Inject one HXFunctor into another.
class
  HInject
    (h :: (* -> *) -> * -> *)
    (j :: (* -> *) -> * -> *) where
  hinject' :: h r a -> j r a
  hproject' :: j r a -> Maybe (h r a)

class HFunctor (h :: (* -> *) -> * -> *) where
  hmap ::
    (forall a. f a -> g a) ->
    (forall a. h f a -> h g a)

class HFunctor h => HFoldable h where
  hfoldMap ::
    Monoid m =>
    (forall a. f a -> m) ->
    (forall a. h f a -> m)

class HFoldable h => HTraversable h where
  htraverse ::
    Applicative m =>
    (forall a. f a -> m (g a)) ->
    (forall a. h f a -> m (h g a))

class HXFunctor (h :: (* -> *) -> * -> *) where
  hxmap ::
    (forall a. f a -> g a) ->
    (forall a. g a -> f a) ->
    (forall a. h f a -> h g a)

instance HFunctor h => HXFunctor (DeriveHXFunctor h) where
  hxmap f _ (DeriveHXFunctor term) = DeriveHXFunctor $ hmap f term

infixr 6 `sumAlg`

-- | Takes 2 algebras and lift them to an algebra on the sum type.
sumAlg ::
  (forall a. h f a -> f a) ->
  (forall a. j f a -> f a) ->
  (forall a. (h :+: j) f a -> f a)
sumAlg alg1 _ (Inl a) = alg1 a
sumAlg _ alg2 (Inr a) = alg2 a

sumAlgConst ::
  HInject h j =>
  (forall a. h f a -> f a) ->
  (forall a. f a) ->
  (forall a. j f a -> f a)
sumAlgConst alg _ (hproject' -> Just term) = alg term
sumAlgConst _   c _                        = c

-- | Lift an algebra through fully polymorphic injection.
sumAlgMonoid ::
  (forall a. Monoid (f a), HInject h j) =>
  (forall a. h f a -> f a) ->
  (forall a. j f a -> f a)
sumAlgMonoid alg (hproject' -> Just term) = alg term
sumAlgMonoid _   _                        = mempty

xwrap :: h (HXFix h f) a -> HXFix h f a
xwrap = HXFix

wrap :: h (HFix h) a -> HFix h a
wrap = HFix

xplace :: f a -> HXFix h f a
xplace = Place

-- | Relax a fixpoint into a fixpoint potentially with holes in it. This allows
-- us to fold over the structure with catamorphism.
relax :: HFunctor h => HFix h a -> HXFix h f a
relax (HFix term) = HXFix . (hmap relax) $ term

-- | Catamorphism over a functor-functor. But wait a second, can't we just
-- instantiate `f` with some monad? I guess that's OK... If `a` is a monadic
-- type, then we just have layers of uncomposed monads. The result will not be a
-- monad transformer stack, as the binds of `f` do not propagate into `a`, but
-- that's OK maybe?
hxcata ::
  forall h f.
  HXFunctor h =>
  (forall a. h f a -> f a) ->
  (forall a. HXFix h f a -> f a)
hxcata _ (Place term) = term
hxcata alg (HXFix term) = alg . go $ term
  where
    go = hxmap (hxcata alg) xplace

hcata ::
  forall h f.
  HFunctor h =>
  (forall a. h f a -> f a) ->
  (forall a. HXFix h f a -> f a)
hcata _   (Place term) = term
hcata alg (HXFix term) = alg . go $ term
  where
    go = hmap (hcata alg)

hcata' ::
  forall h f.
  HFunctor h =>
  (forall a. h f a -> f a) ->
  (forall a. HFix h a -> f a)
hcata' alg (HFix term) =
  alg . hmap (hcata' alg) $ term

hcataM ::
  forall h f m.
  (HTraversable h, Monad m) =>
  (forall a. h f a -> m (f a)) ->
  (forall a. HXFix h f a -> m (f a))
hcataM _    (Place term) = pure term
hcataM algM (HXFix term) = (algM =<<) . htraverse (hcataM algM) $ term

hcataM' ::
  forall h f m.
  (HTraversable h, Monad m) =>
  (forall a. h f a -> m (f a)) ->
  (forall a. HFix h a -> m (f a))
hcataM' algM (HFix term) = (algM =<<) . htraverse (hcataM' algM) $ term

-- | Lifting functor-functors through injection.
inject ::
  forall h j f a.
  (HXFunctor h, HInject h j) =>
  (HXFix h (HXFix j f) a) ->
  HXFix j f a
inject = hxcata (xwrap . hinject')

inject' ::
  forall h j a.
  (HFunctor h, HInject h j) =>
  HFix h a ->
  HFix j a
inject' = hcata' (wrap . hinject')

hproject ::
  forall h j a.
  (HFunctor j,
   HTraversable h,
   HInject h j) =>
  HFix j a ->
  Maybe (HFix h a)
hproject =
  hcataM' (fmap wrap . unMaybeHomM)
  . hcata' (wrap . prj . hproject')
  where
    prj :: forall h r a. Maybe (h r a) -> (HMaybe :.: h) r a
    prj (Just a) = HComp (HJust a)
    prj Nothing  = HComp HNothing

unMaybeHomM :: (HMaybe :.: h) f a -> Maybe (h f a)
unMaybeHomM (HComp (HJust a)) = Just a
unMaybeHomM (HComp HNothing)  = Nothing


-- ##########################################
-- # General higher-order functor instances #
-- ##########################################

instance HFunctor HMaybe where
  hmap f =
    \case
      HJust a -> HJust (f a)
      HNothing -> HNothing

instance HFoldable HMaybe where
  hfoldMap f =
    \case
      HJust a -> f a
      HNothing -> mempty

instance HTraversable HMaybe where
  htraverse f =
    \case
      HJust a -> HJust <$> f a
      HNothing -> pure HNothing

instance HFunctor h => HFunctor (HXFix h) where
  hmap f =
    \case
      HXFix term -> HXFix (hmap (hmap f) term)
      Place term -> Place (f term)

instance HFoldable h => HFoldable (HXFix h) where
  hfoldMap = hfoldMap'
    where
      hfoldMap' ::
        forall m f. Monoid m => (forall a. f a -> m) -> (forall a. HXFix h f a -> m)
      hfoldMap' f (Place a) = f a
      hfoldMap' f (HXFix a) = hfoldMap (hfoldMap' f) a

instance HTraversable h => HTraversable (HXFix h) where
  htraverse = htraverse'
    where
      htraverse' ::
        forall m f g.
        Applicative m =>
        (forall a. f a -> m (g a)) ->
        (forall a. HXFix h f a -> m (HXFix h g a))
      htraverse' f (Place a) = Place <$> f a
      htraverse' f (HXFix a) = HXFix <$> htraverse (htraverse' f) a

instance (HFunctor f, HFunctor g) => HFunctor (f :.: g) where
  hmap f =
    \case
      HComp a -> HComp (hmap (hmap f) a)

instance (HFoldable f, HFoldable g) => HFoldable (f :.: g) where
  hfoldMap f =
    \case
      HComp a -> hfoldMap (hfoldMap f) a

instance (HTraversable f, HTraversable g) => HTraversable (f :.: g) where
  htraverse f =
    \case
      HComp a -> HComp <$> htraverse (htraverse f) a

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g) where
  hmap f =
    \case
      Inl left -> Inl $ hmap f left
      Inr right -> Inr $ hmap f right

instance (HFoldable f, HFoldable g) => HFoldable (f :+: g) where
  hfoldMap f =
    \case
      Inl left -> hfoldMap f left
      Inr right -> hfoldMap f right

instance (HTraversable f, HTraversable g) => HTraversable (f :+: g) where
  htraverse f =
    \case
      Inl left -> Inl <$> htraverse f left
      Inr right -> Inr <$> htraverse f right

instance (HInject h j, HInject j l) => HInject (DeriveHInjectTrans h j l) l where
  hinject' (DeriveHInjectTrans a) = hinject' @j @l . hinject' @h @j $ a
  hproject' a =
    hproject' @j @l a >>= hproject' @h @j >>= (return . DeriveHInjectTrans)
