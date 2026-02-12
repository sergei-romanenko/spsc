
instance : Monad List where
  pure x := [ x ]
  bind xs f := List.flatten ( Functor.map f xs )

instance : Alternative List where
  failure := @List.nil
  orElse l l' := List.append l (l' ())
