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

fun ifoldl2 [a:::Type] [s:::Type] (f: point -> s -> a -> s) (fst:point) (lst:point) (s:s) ((w,h), ll:list (list a)) : s =
  ifoldl (fn y s l =>
    ifoldl (fn x s a =>
      f (x,y) s a
    ) fst.1 lst.1 s (w,l)
  ) fst.2 lst.2 s (h,ll)

fun abs (i:int) : int = if i>=0 then 1 else -1

fun ifor [s:::Type] (f: int -> s -> s) (fst':int) (lst':int) (s:s) : s =
  let
    val fst = min fst' lst'
    val lst = max fst' lst'

    fun ifor' s fst =
        if (lst > fst) then
          ifor' (f fst s) (fst+1)
        else
          s
  in
    ifor' s fst
  end

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
  L.foldl (fn (x,y) v => v && (xp < x || yp < y)) True l
  
fun contour (l:list point) : list point =
  L.foldl (fn (x,y) res => (x,y) :: L.filter (fn (x2,y2) => x2 < x || y2 < y) res) [] l

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

type rect a = list (list a)

fun gfoldl [a:::Type] [s:::Type] (f: point -> s -> a -> s) (g:game) (s:s) (r:rect a) : s =
  let
    val (Game (w,h,ms)) = g
    val mc = sort gty (contour ms)
  in
     (foldlWhile (fn (y,s,mc,w') l =>
      case mc of
        | [] => (True, (y+1,ifoldl (fn x s a => f (x,y) s a) 0 w' s (w,l),[],w'))
        | p :: mc' =>
          let
            val (w'',mc'') = (if y = p.2 then (p.1-1,mc') else (w',mc))
          in
            (True, (y+1,ifoldl (fn x s a => f (x,y) s a) 0 w'' s (w,l),mc'',w''))
          end
     ) (0,s,mc,w-1) r).2
  end

datatype result = Win | Loose

fun gfor [s:::Type] (f : point -> s -> s) (g:game) (s:s) : s =
  let
    val (Game (w,h,ms)) = g
    val mc = sort gty (contour ms)
  in
     (ifor (fn y (s,mc,w') =>
      case mc of
        | [] => (ifor (fn x s => f (x,y) s) 0 w' s, [], w')
        | p :: mc' =>
          let
            val (w'',mc'') = (if y = p.2 then (p.1-1,mc') else (w',mc))
          in
            (ifor (fn x s => f (x,y) s) 0 w'' s, mc'',w'')
          end
     ) 0 (h-1) (s,mc,w-1)).1
  end
  

(* fun calc (g:game) : option result = *)
(*   let *)
(*     val (Game (w,h,ms) = g *)
(*   in *)
(*     if w = 0 && h = 0 then *)
(*       Some Win *)
(*     else *)
(*       let *)
(*         val ms' = gfoldl () g [] *)
(*       in *)
(*       end *)
(*   end *)
  



(*
 ____                _
|  _ \ ___ _ __   __| | ___ _ __
| |_) / _ \ '_ \ / _` |/ _ \ '__|
|  _ <  __/ | | | (_| |  __/ |
|_| \_\___|_| |_|\__,_|\___|_|

*)

(* type cell = (source bool * source string) *)

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
          return (<xml>{x}{xx}</xml>, s :: l)
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
          return (<xml>{pp.1}{p.1}</xml>, p.2 :: pp.2)
      end
  in
    p <- rectX' p1 p2;
    return (<xml><table>{p.1}</table></xml>, p.2)
  end

val ms = ((11,2) :: (3,10) :: (9,5) :: (8,6) :: [])
val w = 13
val h = 11
val g = Game (w,h, ms)

fun main {} : transaction page =
  (x,ll) <- rectX (0,0) (12,10);
  return
  <xml><head/>
  <body>
    {[ms]} <br/>
    {[contour ms]} <br/>
    {[sort gty (contour ms)]} <br/>
    {x}
    <button value="Check" onclick={fn _ => 
      gfoldl (fn _ m a => m ; set a False) g (return {}) ll
    }/>
  </body>
  </xml>

