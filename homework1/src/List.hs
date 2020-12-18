module List where


data List a = Nil | Cons a (List a)

append :: a -> List a -> List a
append = Cons

mConcat :: List a -> List a -> List a 
mConcat list Nil = list 
mConcat Nil list = list 
mConcat list (Cons elem list1) = mConcat (append elem list) list1

sConcat :: List a -> List a -> List a
sConcat (Cons a list_a) (Cons b list_b) = mConcat (append b (Cons a list_a)) list_b
sConcat _ _ = Nil
