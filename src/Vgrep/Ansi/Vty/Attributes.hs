{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vgrep.Ansi.Vty.Attributes
  ( Attr ()
  , combineStyles
  ) where

import Data.Bits               ((.|.))
import Graphics.Vty.Attributes (Attr (..), MaybeDefault (..), defAttr)

-- | vty-6 removed the 'Semigroup' and 'Monoid' instances for 'Attr' and
-- 'MaybeDefault' because they were misbehaved for general use. vgrep relies on
-- them to combine and reset ANSI formattings, so they are reinstated here with
-- the original vty-5 semantics.
instance Eq v => Semigroup (MaybeDefault v) where
    Default     <> Default     = Default
    Default     <> KeepCurrent = Default
    Default     <> SetTo v     = SetTo v
    KeepCurrent <> Default     = Default
    KeepCurrent <> KeepCurrent = KeepCurrent
    KeepCurrent <> SetTo v     = SetTo v
    SetTo _v    <> Default     = Default
    SetTo v     <> KeepCurrent = SetTo v
    SetTo _     <> SetTo v     = SetTo v

instance Eq v => Monoid (MaybeDefault v) where
    mempty = KeepCurrent

instance Semigroup Attr where
    attr0 <> attr1 =
        Attr ( attrStyle attr0     <> attrStyle attr1 )
             ( attrForeColor attr0 <> attrForeColor attr1 )
             ( attrBackColor attr0 <> attrBackColor attr1 )
             ( attrURL attr0       <> attrURL attr1 )

instance Monoid Attr where
    mempty = Attr mempty mempty mempty mempty

-- | Combines two 'Attr's. This differs from 'mappend' from the 'Monoid'
-- instance of 'Attr' in that 'Vty.Style's are combined rather than
-- overwritten.
combineStyles :: Attr -> Attr -> Attr
combineStyles l r = defAttr
    { attrStyle = case (attrStyle l, attrStyle r) of
        (SetTo l', SetTo r') -> SetTo (l' .|. r')
        (l', r')             -> l' <> r'
    , attrForeColor = attrForeColor l <> attrForeColor r
    , attrBackColor = attrBackColor l <> attrBackColor r
    }
