module Vgrep.Ansi.Vty.Attributes
  ( Attr ()
  , combineAttr
  , combineStyles
  ) where

import Data.Bits               ((.|.))
import Graphics.Vty.Attributes (Attr (..), MaybeDefault (..), defAttr)

-- | Combines two 'Attr's so that the right one overrides the left where it sets
-- something, and 'currentAttr' acts as the identity. vty-6 removed the
-- 'Semigroup' instance with these (vty-5) semantics; vgrep relies on them to
-- accumulate and reset ANSI formatting.
combineAttr :: Attr -> Attr -> Attr
combineAttr attr0 attr1 =
    Attr ( combineMaybeDefault (attrStyle attr0)     (attrStyle attr1) )
         ( combineMaybeDefault (attrForeColor attr0) (attrForeColor attr1) )
         ( combineMaybeDefault (attrBackColor attr0) (attrBackColor attr1) )
         ( combineMaybeDefault (attrURL attr0)       (attrURL attr1) )

combineMaybeDefault :: Eq v => MaybeDefault v -> MaybeDefault v -> MaybeDefault v
combineMaybeDefault l r = case (l, r) of
    (Default,     Default)     -> Default
    (Default,     KeepCurrent) -> Default
    (Default,     SetTo v)     -> SetTo v
    (KeepCurrent, Default)     -> Default
    (KeepCurrent, KeepCurrent) -> KeepCurrent
    (KeepCurrent, SetTo v)     -> SetTo v
    (SetTo _v,    Default)     -> Default
    (SetTo v,     KeepCurrent) -> SetTo v
    (SetTo _,     SetTo v)     -> SetTo v

-- | Combines two 'Attr's. This differs from 'combineAttr' in that 'Vty.Style's
-- are combined rather than overwritten.
combineStyles :: Attr -> Attr -> Attr
combineStyles l r = defAttr
    { attrStyle = case (attrStyle l, attrStyle r) of
        (SetTo l', SetTo r') -> SetTo (l' .|. r')
        (l', r')             -> combineMaybeDefault l' r'
    , attrForeColor = combineMaybeDefault (attrForeColor l) (attrForeColor r)
    , attrBackColor = combineMaybeDefault (attrBackColor l) (attrBackColor r)
    }
