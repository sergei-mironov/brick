structure P = Prelude
structure L = List
val foldl = @@L.foldl
val rev = @@L.rev
val sort = @@L.sort

type point = (int * int)

fun gtx p1 p2 = p1.1 > p2.1
fun gty p1 p2 = p1.2 > p2.2

fun foldlWhile [a:::Type] [s:::Type] (f:s->a->(bool*s)) (s:s) (l:list a) : s =
    let
        fun foldlWhile' acc ls =
            case ls of
              | [] => acc
              | x :: ls =>
                case f acc x of
                  | (False,acc') => acc'
                  | (True,acc') => foldlWhile' acc' ls
    in
        foldlWhile' s l
    end
  
fun ifoldl [a:::Type] [s:::Type] (f:int -> s -> a -> s) (fst':int) (lst':int) (s:s) (len:int, l:list a) : s =
  let
    val fst = min fst' lst'
    val lst = max fst' lst'
  in
    if fst > len-1 || lst < 0 then s else
      (foldlWhile (fn (i,s) a => 
        if (i < fst) then
          (True, (i+1, s))
        else (* i >= fst *)
          (if (i <= lst) then
            (True, (i+1, f i s a))
           else
            (False, (i,s)))
      ) (0,s) l).2
  end

fun ifoldll [a:::Type] [s:::Type] (f: point -> s -> a -> s) (fst:point) (lst:point) (s:s) ((w,h), ll:list (list a)) : s =
  ifoldl (fn y s l =>
    ifoldl (fn x s a =>
      f (x,y) s a
    ) fst.1 lst.1 s (w,l)
  ) fst.2 lst.2 s (h,ll)

fun abs (i:int) : int = if i>=0 then 1 else -1

(*
 ____        _
|  _ \  __ _| |_ __ _ 
| | | |/ _` | __/ _` |
| |_| | (_| | || (_| |
|____/ \__,_|\__\__,_|

*)

datatype player = A | B

fun next p = case p of |B=>A |A=>B

datatype move = Move of (player * point) 

(* fun moves (g:list point) : list point = *)


fun valid ((xp,yp):point) (l:list point) =
  L.foldl (fn (x,y) v => v && (xp > x || yp > y)) True l
  
fun contour (l:list point) : list point =
  L.foldl (fn (x,y) res =>
    case valid (x,y) res of
      |True => (x,y)::res
      |False => res) [] l

fun zip_reverse [a:::Type] [b:::Type] (la:list a) (lb:list b) : list (a*b) =
  (List.foldl (fn a (lb, res) => case lb of
    |b :: lb => (lb, (a,b) :: res)
    |[] => ([],res)) (lb,[]) la).2


fun hline ((x1,y):point) ((x2,_):point) = 
  case x1 = x2 of
    |True => (x1,y) :: []
    |False => (x1,y) :: (hline (x1 + (abs (x2-x1)), y) (x2,y))

fun rect ((x1,y1):point) ((x2,y2):point) : list (list point) =
  case y2 = y1 of
    |True => (hline (x1,y1) (x2,y1)) :: []
    |False => (hline (x1,y1) (x2,y1)) :: (rect (x1,y1+(abs (y2-y1))) (x2,y2))
 

type width = int
type height = int

datatype game = Game of (width*height*list point)

fun contouredX (Game (_,_,ms)) = sort gtx (contour ms)
fun contouredY (Game (_,_,ms)) = sort gty (contour ms)
fun intervalsY g =
  let
    val (Game (w,h,ms)) = g
    val mc = contouredY g
  in
    ((w-1,0) :: (List.mp (fn (x,y) => (w-1,y+1)) mc))
      `zip_reverse`
    ((List.mp (fn (x,y) => (x+1,y)) mc) `List.append` ((0,h-1)::[]))
  end

type rect a = list (list a)

fun movesY [a:::Type] (g:game) (r:rect a) : list a =
  let
    val intrs = intervalsY g
    val (Game (w,h,ms)) = g
  in
    foldl(fn (p1,p2) s =>
      ifoldll (fn pt s a => a :: s) p1 p2 s ((w,h),r)
      ) [] intrs
  end

(*
 ____                _
|  _ \ ___ _ __   __| | ___ _ __
| |_) / _ \ '_ \ / _` |/ _ \ '__|
|  _ <  __/ | | | (_| |  __/ |
|_| \_\___|_| |_|\__,_|\___|_|

*)

fun hlineX (p1:point) (p2:point) : transaction (xtable * list (source bool)) = 
  let
    fun cell (x,y) =
      s <- source True;
      return (
        <xml>
        <td dynStyle={
          b <- signal s;
          return (if b then
            STYLE "width:50px; height:50px; overflow:hidden; display:inline-block; white-space:nowrap; background:blue"
          else
            STYLE "width:50px; height:50px; overflow:hidden; display:inline-block; white-space:nowrap; background:white")}>
            </td>
        </xml>, s)

    fun hlineX' ((x1,y):point) ((x2,_):point) =
      case x1 = x2 of
        |True => 
          (x,s) <- cell (x1,y);
          return (x,s :: [])
        |False =>
          (x,s) <- cell (x1,y);
          (xx,l) <- hlineX' (x1 + (abs (x2-x1)), y) (x2,y);
          return (<xml>{xx}{x}</xml>, s :: l)
  in
    p <- hlineX' p1 p2;
    return (<xml><tr>{p.1}</tr></xml>, p.2)
  end

fun rectX (p1:point) (p2:point) : transaction (xbody * list (list (source bool))) =
  let
    fun rectX' (p1:point) (p2:point) : transaction (xtable * list (list (source bool))) =
      let
        val (x1,y1) = p1
        val (x2,y2) = p2
      in
      case eq y1 y2 of
        |True => 
          p <- hlineX (x1,y1) (x2,y1);
          return (<xml>{p.1}</xml>, p.2 :: [])
        |False =>
          p <- hlineX (x1,y1) (x2,y1);
          pp <- rectX' (x1,y1+(abs (y2-y1))) (x2,y2);
          return (<xml>{p.1}{pp.1}</xml>, p.2 :: pp.2)
      end
  in
    p <- rectX' p1 p2;
    return (<xml><table>{p.1}</table></xml>, p.2)
  end


(* val moves = List.rev ((1,1) :: (3,3) :: (5,8) :: (7,9) :: []) *)

(* fun choose (m:list point) = *)
(*   let *)
(*     fun choose' m = *)
(*       ... *)
(*   in *)
(*     choose' (0, *)
(*   end *)

val ms = L.rev ((1,1) :: (2,3) :: (3,3) :: (1,7) :: (8,2) :: [])
(* val ms = L.rev ((0,0) :: []) *)
val w = 13
val h = 11
val g = Game (w,h, ms)

fun main {} : transaction page =
  (x,ll) <- rectX (0,0) (12,10);
  P.forM_ (movesY g ll) (fn s => set s False);
  return
  <xml><head/>
  <body>
    {[contour ms]} <br/>
    {[contouredY g]} <br/>
    {[intervalsY g]} <br/>

    {x}
    <button value="Check" onclick={fn _ => 
      ifoldll (fn _ m s => m ; set s False) (2,2) (4,6) (return {}) ((w,h),ll)
    }/>
  </body>
  </xml>

