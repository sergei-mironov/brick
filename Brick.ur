
type point = (int * int)

datatype player = A | B

fun next p = case p of |B=>A |A=>B

datatype move = Move of (player * point) 

datatype game = Game of list move


fun valid ((xp,yp):point) (l:list point) =
  List.foldl (fn (x,y) v => v && (xp > x || yp > y)) True l
  
fun compress (l:list point) : list point =
  List.foldl (fn (x,y) res =>
    case valid (x,y) res of
      |True => (x,y)::res
      |False => res) [] l

fun zip_reverse [a:::Type] [b:::Type] (la:list a) (lb:list b) : list (a*b) =
  (List.foldl (fn a (lb, res) => case lb of
    | b :: lb => (lb, (a,b) :: res)
    | [] => ([],res)) (lb,[]) la).2


fun abs (i:int) : int = if i>=0 then 1 else -1

fun hline ((x1,y):point) ((x2,_):point) = 
  case x1 = x2 of
    |True => (x1,y) :: []
    |False => (x1,y) :: (hline (x1 + (abs (x2-x1)), y) (x2,y))

fun rect ((x1,y1):point) ((x2,y2):point) : list (list point) =
  case y2 = y1 of
    |True => (hline (x1,y1) (x2,y1)) :: []
    |False => (hline (x1,y1) (x2,y1)) :: (rect (x1,y1+(abs (y2-y1))) (x2,y2))




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
          return (<xml>{p.1}{pp.1}</xml>, p.2 :: pp.2)
      end
  in
    p <- rectX' p1 p2;
    return (<xml><table>{p.1}</table></xml>, p.2)
  end
  
fun main {} : transaction page =
  (x,l) <- rectX (1,1) (12,10);
  return <xml><head/><body>{x}</body></xml>

