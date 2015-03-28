structure P = Prelude
structure L = List
val foldl = @@L.foldl
val rev = @@L.rev
val sort = @@L.sort

type point = (int * int)

fun gtx p1 p2 = p1.1 > p2.1
fun gty p1 p2 = p1.2 > p2.2
(* fun lty p1 p2 = p1.2 < p2.2 *)

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

(* Iterate throw [fst..lst], assume that fst <= lst *)
fun ifor' [s:::Type] (f: int -> s -> s) (fst:int) (lst:int) (s:s) : s =
  if (lst >= fst) then
    ifor' f (fst+1) lst (f fst s)
  else
    s

fun ifor [s:::Type] (f: int -> s -> s) (fst':int) (lst':int) (s:s) : s =
  let
    val fst = min fst' lst'
    val lst = max fst' lst'
  in
    ifor' f fst lst s
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
  
(* fun contour (l:list point) : list point = *)
(*   L.foldl (fn (x,y) res => (x,y) :: L.filter (fn (x2,y2) => x2 < x || y2 < y) res) [] l *)

fun contour (l:list point) : list point =
  L.foldl (fn p res => if valid p res then p :: res else res) [] l

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

fun gmap' [s:::Type] (fy : int -> s -> s)  (fx : point -> s -> s) (s:s) (g:game) : s =
  let
    val (Game (w,h,ms)) = g
    val mc = sort gty (contour ms)
  in
     (ifor' (fn y (s,mc,w') =>
      let
        val s = if y<>0 then fy y s else s
      in
      case mc of
        | [] => (ifor' (fn x s => fx (x,y) s) 0 w' s, [], w')
        | p :: mc' =>
          let
            val (w'',mc'') = (if y = p.2 then (p.1-1,mc') else (w',mc))
          in
            (ifor' (fn x s => fx (x,y) s) 0 w'' s, mc'',w'')
          end
      end
     ) 0 (h-1) (s,mc,w-1)).1
  end

fun gmap [s:::Type] (f : point -> s -> s) (s:s) (g:game) : s =
  gmap' (fn _ s => s) f s g

fun gfoldl [a:::Type] [s:::Type] (f: point -> s -> a -> s) (g:game) (s:s) (r:rect a) : s =
  (gmap' (fn y (r,s) => (case r of |[] => [] |(r :: rs) => rs,s)) (fn p (r,s) =>
    (case r of
      |[] => (r,s)
      |l :: ls =>
        (case l of
          |[] => (r,s)
          |a::as => (as :: ls, f p s a)))) (r,s) g).2

val tms = (0,1)::(1,0)::[]

val tg = Game (10,10,tms)

val tms2 = gmap (fn p s => p::s) [] tg
  
datatype result = Win | Loose

val show_result : show result = mkShow (fn r => case r of |Win=>"Win" |Loose =>"Loose")

(*
fun gwfold [s] (f: point -> result -> s -> s) (s:s) (g:game) : s =
  let
    val (Game (w,h,ms)) = g
  in
    if w = 0 && h = 0 then
      (f (-1,-1) Win s)
    else
        gmap (fn p s => ... ) s g
      let
        val ms' = 
      in
        ms'
      end
  end
*)

fun gres (p:point) (g:game) : result =
  let
    val (Game (w,h,ms)) = g

    val g' = if valid p ms then
               Game (w,h, p::ms)
             else
               error <xml>Bug: invalid move {[p]} have moves {[ms]}</xml>
  in
    if p.1 > 0 || p.2 > 0 then
      gmap (fn p' st =>
        case gres p' g' of
          |Win => Loose
          |Loose => st
      ) Win g'
    else
      Loose
  end



(*
 ____                _
|  _ \ ___ _ __   __| | ___ _ __
| |_) / _ \ '_ \ / _` |/ _ \ '__|
|  _ <  __/ | | | (_| |  __/ |
|_| \_\___|_| |_|\__,_|\___|_|

*)

type cell = (source bool * source string)

fun hlineX (p1:point) (p2:point) : transaction (xtable * list cell) = 
  let
    fun cell (x,y) =
      s1 <- source True;
      s2 <- source "";
      return (
        <xml>
        <td dynStyle={
          b <- signal s1;
          return (if b then
            STYLE "width:50px; height:50px; overflow:hidden; display:inline-block; white-space:nowrap; background:blue"
          else
            STYLE "width:50px; height:50px; overflow:hidden; display:inline-block; white-space:nowrap; background:#eeeeee")}>
         <dyn signal={v <-signal s2; return (cdata v)}/>
         </td>
        </xml>, (s1,s2))

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

type grect a = (list (list a))

fun rectX (p1:point) (p2:point) : transaction (xbody * grect cell) =
  let
    fun rectX' (p1:point) (p2:point) : transaction (xtable * grect cell) =
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

(* too long to implement *)
(* type gzipper a = (list (list a) * list a * list a * list (list a)) *)

fun gnav [a:::Type] (p:point) (g:grect a) : a = 
  let
    fromopt (L.nth (fromopt (L.nth g p.2)) p.1)
  where
    fun fromopt [a] (o:option a) : a = case o of |None => error <xml>gnav error</xml> |Some x => x
  end


val ms = ((1,2) :: (2,1) :: (3,3) :: [])
(* val ms = ((0,1) :: (1,0) :: (11,2) ::[]) *)
val w = 5
val h = 5
val g = Game (w,h, ms)

fun main {} : transaction page =
  (x,ll) <- rectX (0,0) (w-1,h-1);
  return
  <xml><head/>
  <body onload={
      gfoldl (fn p m a =>
        m ; if valid p ms then set a.1 False else return {}
      ) g (return {}) ll
    }>
    {[sort gty (contour ms)]} <br/>
    {[gmap (fn p x => p :: x) [] g]} <br/>
    (* {[tms2]} <br/> *)
    (* {[sort gty (contour ms)]} <br/> *)
    {x}
    <button value="Check1" onclick={fn _ => 
      gfoldl (fn _ m a => m ; set a.1 False) g (return {}) ll
    }/><br/>
    <button value="Check2" onclick={fn _ => 
      gmap (fn p m =>
        let val c = gnav p ll in
        m ; set c.1 False ; set c.2 (show p) end) (return {}) g
    }/><br/>
    <button value="Check3" onclick={fn _ => 
      gfoldl (fn p m a =>
        let
          val x = gres p g
        in
          m ; set a.2 (show x)
        end
      ) g (return {}) ll
    }/><br/>
  </body>
  </xml>

