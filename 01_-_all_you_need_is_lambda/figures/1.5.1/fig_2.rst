..
  figure 2

1. (λx.xy)z

   We apply the lambda to the argument z.

2. (λ[x ∶= z].xy)

   Since x is the bound variable, all instances of x in the body of the
   function will be replaced with z. The head will be eliminated,
   and we replace any x in the body with a z.

3. zy

   The head has been applied away, and there are no more heads
   or bound variables. Since we know nothing about z or y, we can
   reduce this no further.

..
  end figure 2
