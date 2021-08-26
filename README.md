# via-field

A GHC plugin that provides a proof of concept for [@Iceland_jack's Via Fields
idea](https://www.reddit.com/r/haskell/comments/pbq9rn/via_fields_finer_granularity_in_deriving/).

You can use it with the GHC flag `-fplugin=ViaFields`.

## Examples

```haskell
-- | Coodinates ordered right-to-left, top-to-bottom
data XY = XY { x :: Int via Down Int, y :: Int }
  deriving stock (Eq, Ord)

-- | A custom monoid with type variables
data T a b = T (Int via Sum Int) String a (b via Product b)
  deriving stock (Eq, Generic)
  deriving (Semigroup, Monoid) via Generically (T a b)
```

## Supported GHC Versions

Currently only GHC 9.2.1-rc1 is supported but support for older version should
be coming soon.

## Implementation

The plugin works by use the via type in the data declaration and replacing the
constructor with a pattern that uses the non-via type, mangling the original
names in the process. For example,

```haskell
data XY = XY { x :: Int via Down Int, y :: Int }
  deriving stock (Eq, Ord)
```

gets desugared to

```haskell
data XY = _via-fields$XY { _via-fields$x :: Down Int, _via-fields$y :: Int }
  deriving stock (Eq, Ord)
  
pattern XY :: Int -> Int -> XY
pattern XY { x = var0, y = var1 } =
  _via-fields$XY (ViaFields.Util.Coerced var0) var1
```

where `ViaFields.Util.Coerced` is a pattern synonym that applies
`Data.Coerce.coerce`.

## Limitations

Because of the way the plugin works derived `Show` instances are a complete
mess.
