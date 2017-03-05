(*---------------------------------------------------------------------------
   Copyright (c) 2017 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module type Vector = sig
  type 'a t
  val length : 'a t -> int
  val set : 'a t -> int -> 'a -> 'a t
  val get : 'a t -> int -> 'a
  val insert : 'a t -> int -> 'a -> 'a t
  val delete : 'a t -> int -> 'a t
end

module type Mergeable_vector = sig
  include Vector
  type 'a patch
  val diff : 'a t -> 'a t -> 'a patch
  val apply : 'a t -> 'a patch -> 'a t
  val merge : resolve:('a -> 'a -> 'a) -> ancestor:'a t -> 'a t -> 'a t -> 'a t
end

module Make (V : Vector) : Mergeable_vector = struct

  type 'a edit =
    | Ins of int * 'a
    | Del of int * 'a
    | Rep of int * 'a * 'a

  type 'a patch = 'a edit list

  let diff xs ys =
    let cache = Array.init (V.length xs+1)
      (fun _ -> Array.make (V.length ys+1) None)
    in
    let rec loop i j =
      let cache_i = Array.unsafe_get cache i in
      let min3 x y z =
        let m' (a,al) (b,bl) = if a < b then (a,al) else (b,bl) in
        m' (m' x y) z
      in
      match Array.unsafe_get cache_i j with
      | Some v -> v
      | None ->
          let res =
            begin match i,j with
            | 0,0 -> (0, [])
            | 0, j ->
                let d,e = loop 0 (j-1) in
                (d+1, (Ins (i,V.get ys (j-1))::e))
            | i, 0 ->
                let d,e = loop (i-1) 0 in
                (d+1, (Del(i-1,V.get xs (i-1))::e))
            | _ ->
                let xsim1 = V.get xs (i-1) in
                let ysim1 = V.get ys (j-1) in
                let d,e = loop (i-1) j in
                let r1 = (d+1, Del (i-1,xsim1)::e) in
                let d,e = loop i (j-1) in
                let r2 = (d+1, Ins (i,ysim1)::e) in
                let d,e = loop (i-1) (j-1) in
                let r3 =
                  if xsim1 = ysim1 then d,e
                  else (d+1, (Rep (i-1, xsim1, ysim1)::e))
                in
                min3 r1 r2 r3
            end
          in
          Array.unsafe_set cache_i j (Some res);
          res
    in
    let d,e = loop (V.length xs) (V.length ys) in
    List.rev e

  let index = function
    | Ins (i,_) -> i
    | Del (i,_) -> i
    | Rep (i,_,_) -> i

  let shift_edit o = function
    | Ins(i,x) -> Ins(i+o,x)
    | Del(i,x) -> Del(i+o,x)
    | Rep(i,x,x') -> Rep(i+o,x,x')

  let rec shift_patch acc o = function
    | [] -> List.rev acc
    | e::tl -> shift_patch (shift_edit o e::acc) o tl

  let offset = function
    | Ins _ -> 1
    | Del _ -> -1
    | Rep _ -> 0

  let transform ~resolve p q =
    let cons2 (x,y) (xs,ys) = (x::xs, y::ys) in
    let rec go xs a ys b =
      match xs, a, ys, b with
      | [], _, [], _ -> ([], [])
      | xs, a, [], _ -> (shift_patch [] a xs, [])
      | [], _, ys, b -> ([], shift_patch [] b ys)
      | x::xs, a, y::ys, b ->
          if index x < index y then
            let p',q' = go xs a (y::ys) (b + offset x) in
            (shift_edit a x::p',q')
          else if index x > index y then
            let p',q' = go (x::xs) (a + offset y) ys b in
            (p',shift_edit b y::q')
          else begin
            match x,y with
            | _ when x = y -> go xs (a + offset y) ys (b + offset x)
            | Ins (i,nx), Ins (_, ny) ->
                let n = resolve nx ny in
                cons2 (Rep (i+a,ny,n), Rep (i+b,nx,n)) (go xs (a + offset y) ys (b + offset x))
            | Rep (i, _, nx), Rep (_, _, ny) ->
                let n = resolve nx ny in
                cons2 (Rep (i + a, ny, n), Rep (i + b, nx, n)) (go xs a ys b)
            | Ins _, _ ->
                let p',q' = go xs a (y::ys) (b + offset x) in
                (shift_edit a x::p',q')
            | _, Ins _ ->
                let p',q' = go (x::xs) (a + offset y) ys b in
                (p', shift_edit b y::q')
            | Rep (i,_,nx), Del _ ->
                let p',q' = go xs (a + offset y) ys b in
                (p', Del (i+b, nx)::q')
            | Del _, Rep (i, _, ny) ->
                let p',q' = go xs a ys (b + offset x) in
                (Del (i+a,ny)::p',q')
            | Del _, Del _ -> go xs (a + offset y) ys (b + offset x)
          end
    in
    go p 0 q 0

  let rec apply off s = function
    | [] -> s
    | Ins(pos,c)::tl ->
        let s' = V.insert s (pos+off) c in
        apply (off + 1) s' tl
    | Rep(pos,x,x')::tl ->
        let s' = V.set s (pos + off) x' in
        apply off s' tl
    | Del(pos,x)::tl ->
        let s' = V.delete s (pos + off) in
        apply (off - 1) s' tl

  let apply s =
    try apply 0 s
    with Invalid_argument _ ->
      raise (Invalid_argument "incompatible patch")

  let merge ~resolve ~ancestor l r =
    let p = diff ancestor l in
    let q = diff ancestor r in
    let _,q' = transform ~resolve p q in
    apply l q'

  include V
end


(*---------------------------------------------------------------------------
   Copyright (c) 2017 KC Sivaramakrishnan

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
