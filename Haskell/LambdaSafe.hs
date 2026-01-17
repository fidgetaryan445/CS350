data Variable a = VChar a | Cons a (Variable a) deriving Eq
--here Cons has no relation to lists . Con is just a method to crate multi-char variables as one for ex to represnt xy Cons 'x' (VChar 'y') will be used .
--variable was initialy defined as such with some different ways of reducing and renaming in mind. the current defination is an overengineered one . 
--use of string could have solved the problem as well 

 
data L a = Var (Variable a) | LTerm (L a) (L a) | Lambda (Variable a) (L a) deriving Eq

instance Show a => Show (Variable a) where
  show (VChar a1) = show [a1]
  show (Cons a1 a2) = show (a1 : f a2)
    where
      f (VChar x) = [x]
      f (Cons l v) = l : f v

instance Show a => Show (L a) where 
  show (Var a1) = show a1
  show (Lambda a1 a2) = "\\" ++ show a1 ++ "." ++ show a2
  show (LTerm a1 a2) = f a1 ++ f a2
    where 
      f (Var x) = show x
      f (Lambda x1 x2) = "(" ++ show (Lambda x1 x2) ++ ")"
      f (LTerm x1 x2) = show (LTerm x1 x2)


--representations 
{-
(a) Lambda (VChar 'c') (LTerm (Var (VChar 'c')) (Var (VChar 'c')))
(b) Lambda (VChar 'c') (LTerm (Var (VChar 'c')) (Var (VChar 'd')))
(c) LTerm (Lambda (VChar 'x') (LTerm (Var (VChar 'x')) (Var (VChar 'y')))) (Lambda (VChar 'y') (LTerm (Var (VChar 'x')) (Var (VChar 'y'))))
(d) LTerm (Lambda (VChar 'y') (Var (VChar 'x'))) (Var (VChar 'y'))
(e) LTerm (Lambda (VChar 'y') (Var (VChar 'x'))) (Var (VChar 'a'))

-}

create 0 x y = y 
create n x y = create (n-1) x (x:y)


--update y i n  =  [if idx == i then x+n else x | (x,idx) <- zip y [0..]  ]
update x (y:ys) 0 n  = update ((y+n):x) ys (-1) n 
update x  []    _ _  = reverse x 
update x (y:ys) i n  = update (y:x) ys (i-1) n 



freq  = []
occ   = []
char_diff a b  = (fromEnum a) - (fromEnum b) 

new_Var a 0 = a
new_Var a n = a ++ [toEnum (48+n)] 

check_occ a occ i
			| i== length(occ)  = occ ++ [a] 
			| occ !! i == a    = occ
			| otherwise        = check_occ a occ (i+1) 

consis occ freq 
			| length(occ)==length(freq) = freq
			| otherwise                 = reverse(create (length(occ)-length(freq)) 0 (reverse freq))

find_id a occ i 
			| i == length(occ) = -1
			| occ!!i == a = i 
			| otherwise   = find_id a occ (i+1)

var_to_str (VChar a) str = a:str
var_to_str (Cons a b) str= var_to_str b (a:str)

str_to_varh [] var  = var 
str_to_varh (x:xs) var = str_to_varh xs (Cons x var)

str_to_varf (x:xs) = str_to_varh xs (VChar x)

str_to_var  x = str_to_varf (reverse x)

rename (Var a) occ freq =
  let str = var_to_str a []
      idx = find_id str occ 0
  in if idx == -1
        then (Var a , occ , freq)
        else (Var(str_to_var (new_Var str (freq !! idx))),occ,freq)

rename (Lambda a b) occ freq =
  let str   = var_to_str a []
      occ'  = check_occ str occ 0
      freq' = consis occ' freq
      idx   = find_id str occ' 0
      fren  = update [] freq' idx 1
      newA  = str_to_var (new_Var str (fren !! idx))
      (newB,occr,freqr) = rename b occ' fren
	  
  in ((Lambda newA newB),occr,freqr)

rename (LTerm a b) occ freq = 
		let (newA,occ',freq') = rename a occ freq
		    (newB,occ'',freq'') = rename b occ' freq' 
		in ((LTerm newA newB ) , occ'' , freq'')
	

rename_single (Var a) occ freq = rename (Var a) occ freq
rename_single (Lambda a b) occ freq = 
 let str   = var_to_str a []
     idx   = find_id str occ 0
     fren  = update [] freq idx 1
     newA  
			|idx /= -1 = str_to_var (new_Var str (fren !! idx))
			|otherwise = a
     (newB,occr,freqr) = rename b occ fren
	  
  in ((Lambda newA newB),occr,freqr)
rename_single (LTerm a b) occ freq = rename (LTerm a b) occ freq 

	
example :: L Char
example = LTerm
    (Lambda (VChar 'x') (Lambda (VChar 'y') (Var (VChar 'x'))))
    (Lambda (VChar 'x') (Lambda (VChar 'y') (Var (Cons 'y' (VChar 'x')))))

freevars (Var a) xs 
			|notElem (var_to_str a "") xs  =  (var_to_str a ""):xs
			|otherwise = xs
freevars (LTerm x1 x2) xs  = (freevars x1 xs) ++ (freevars x2 xs)
freevars (Lambda x1 x2) xs = filter (/= var_to_str x1 "") (freevars x2 xs)


subst x n (Var a) = if a == x then n else (Var a)
subst x n (LTerm x1 x2) = LTerm (subst x n x1) (subst x n x2) 
subst x n (Lambda x1 x2) 
			| x == x1  = Lambda x1 x2
			| notElem (var_to_str x1 "") (freevars n []) = Lambda x1 (subst x n x2) 
			| otherwise = 
				let  exp = (Lambda x1 x2) 
				     (exp',occ,freq)  = rename_single exp [var_to_str x1 ""] [0]
				in subst x n exp'
					 
beta_reduce (Var a) = Var a
beta_reduce (Lambda a b) = Lambda a (beta_reduce b)
beta_reduce (LTerm (Lambda a b ) x1) = beta_reduce (subst a x1 b)
--beta_reduce (LTerm x1 x2) = LTerm (beta_reduce x1) x2
beta_reduce (LTerm x1 x2) =
    let
        go expr expr' = if expr == expr' then expr else go expr' (beta_reduce expr')
        x1' = beta_reduce x1
        expr' = LTerm x1' x2
        expr = LTerm x1 x2
        res = go expr expr'
    in res

				
