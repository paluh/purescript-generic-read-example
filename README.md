I was asked about an example of `purescript-generics` usage:

How to create `genericRead` function which can parse `String` and create value of type `a` if we assume that given datatype has all constructors of kind `*` (so it constructors don't take any parameters)?

This repo is my answer to this question. As you can see the meat of the answer is pattern matching on signature value (`SigProd _ cs`) and usage of `fromSpine` function on constructed `GenericSpine` value (I mean `SProd` call). Nothing really complicated (rest of the code cleanups constructors' names and makes string comparison):

```purescript
  genericRead :: forall a. Generic a => String -> Proxy a -> Maybe a
  genericRead s p =
    case (toSignature p) of
      (SigProd _ cs) -> oneOf (map step cs)
      _ -> Nothing
   where
    step constructor = do
      -- leave only last part of constructor ie. Main.Foo -> Foo
      let fullConstructorName = constructor.sigConstructor
      constructorName <- last <<< split "." $ fullConstructorName
      if constructorName == s
        -- this example only works for construcotrs of kind `*`
        then fromSpine $ SProd fullConstructorName []
        else Nothing
```

I'm leaving this simple example as it can help you grasp basic idea behind `purescript-generics`.

You can find full example in [src/Main.purs](src/Main.purs).
