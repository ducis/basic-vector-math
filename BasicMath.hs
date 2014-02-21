{-# LANGUAGE ScopedTypeVariables,BangPatterns,RankNTypes,FlexibleInstances,MultiParamTypeClasses,PostfixOperators,NoMonomorphismRestriction #-}

module BasicMath where
import Control.Monad

-- 全！角！符！号！
(·)::Num a=>(a,a,a)->(a,a,a)->a
(·) = dot
dot::Num a=>(a,a,a)->(a,a,a)->a
dot (x,y,z) (x',y',z') = x*x'+y*y'+z*z'

(×)::Num a=>(a,a,a)->(a,a,a)->(a,a,a)
(×) = cross
cross::Num a=>(a,a,a)->(a,a,a)->(a,a,a)
cross (x,y,z) (x',y',z') = (y*z'-z*y',z*x'-x*z',x*y'-y*x')

(－)::Num a=>(a,a,a)->(a,a,a)->(a,a,a)
(x,y,z)－(x',y',z')=(x-x',y-y',z-z')
(＋)::Num a=>(a,a,a)->(a,a,a)->(a,a,a)
(x,y,z)＋(x',y',z')=(x+x',y+y',z+z')

-- 中！文！
点::Num a=>(a,a,a)->(a,a,a)->a
点=(·)
叉::Num a=>(a,a,a)->(a,a,a)->(a,a,a)
叉=(×)
(+.+)::Num a=>(a,a,a)->(a,a,a)->(a,a,a)
(+.+) = (＋)
(-.-)::Num a=>(a,a,a)->(a,a,a)->(a,a,a)
(-.-) = (－)
(>.<)::Num a=>(a,a,a)->(a,a,a)->(a,a,a)
(>.<) = (×)
lSq v = v·v

class Bei_able a b c where
        倍::a->b->c
instance Num a=>Bei_able a a a where
        x `倍` y = x*y
instance Num a=>Bei_able a (a,a,a) (a,a,a) where
        x `倍` (y0,y1,y2) = (x*y0,x*y1,x*y2)


class Jian_able a b c where
        减::a->b->c
instance Num a=>Jian_able a a a where
        x `减` y = x-y
instance Num a=>Jian_able (a,a,a) (a,a,a) (a,a,a) where
        x `减` y = x－y


class Jia_able a b c where
        加::a->b->c
instance Jia_able R R R where
        x `加` y = x+y
instance Jia_able R3 R3 R3 where
        x `加` y = x＋y
--instance Num a=>Jia_able a a a where
--      x `加` y = x+y
--instance Num a=>Jia_able (a,a,a) (a,a,a) (a,a,a) where
--      x `加` y = x＋y


class Fu_able a b where
        负::a->b
instance Num a=>Fu_able a a where
        负 x = (-x)
instance Num a=>Fu_able (a,a,a) (a,a,a) where
        负 (x,y,z) = (-x,-y,-z)


{-instance (Num a,Num b,Num c)=>Num (a,b,c) where
        (x,y,z)-(x',y',z')=(x-x',y-y',z-z')
        (x,y,z)+(x',y',z')=(x+x',y+y',z+z')-}

cpl::[a]->[b]->[(a,b)]
cpl=liftM2 (,) --cartesian product of lists

type T3 a = (a,a,a)

type T2 a = (a,a)

map3::(a->b)->T3 a->T3 b
map3 f (x,y,z) = (f x,f y,f z)
pam3::T3 (a->b)->a->T3 b
pam3 (x,y,z) i = (x i,y i,z i)
--map2::(a->b)->T2 a->T2 b
map2 f (x,y) = (f x,f y)
zipWith2::(a->b->c)->T2 a->T2 b->T2 c
zipWith2 f (x,y) (x',y') = (f x x', f y y')
rd3::(a->a->a)->T3 a->a
rd3 f (x,y,z) = x `f` y `f` z

(/</)::(Num a,Ord a)=>(a,a)->(a,a)->Bool
x /</ y = (n*d')<(n'*d) where
        ((n,d),(n',d')) = map2 fracFix (x,y)
(/>/)::(Num a,Ord a)=>(a,a)->(a,a)->Bool
x />/ y = y /</ x
(/<=/)::(Num a,Ord a)=>(a,a)->(a,a)->Bool
x /<=/ y = not $ x />/ y
(/>=/)::(Num a,Ord a)=>(a,a)->(a,a)->Bool
x />=/ y = not $ x /</ y

sigdiff::(Num a,Ord a)=>(a,a)->(a,a)->a
sigdiff x y = (n*d')-(n'*d) where
        ((n,d),(n',d')) = map2 fracFix (x,y)

fracFix::(Num a,Ord a)=>(a,a)->(a,a)
fracFix (n,d) = if d<0 then (-n,-d) else (n,d)

(．) x  = fst x
(：) x  = snd x
首 = fst
次 = snd


把 x f = f x

连 f g = g.f

class Linear v s where
        lerp::v->v->s->v

type R=Double
type I=Int
type R3=(R,R,R)
type I2=(I,I)
type I3=(I,I,I)
type R2=(R,R)
type R33=(R3,R3,R3) -- column major

instance Linear R3 R where
        lerp l r x = (((x `倍` r)::R3) `加` (((1-x)`倍`l)::R3))::R3
