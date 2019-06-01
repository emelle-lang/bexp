(* Copyright (C) 2019 Types Logics Cats.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

(** This is a reimplementation of the Doubly_linked interface from Jane Street's
    Core_kernel library. No code from Core_kernel's Doubly_linked module was
    used when writing this file. *)

open Base

type 'a elt = {
    a : 'a;
    mutable prev : 'a elt option;
    mutable next : 'a elt option;
  }

type 'a t = {
    mutable front : 'a elt option;
    mutable back : 'a elt option;
  }

let create () =
  { front = None; back = None }

let iter ~f t =
  let rec loop = function
    | None -> ()
    | Some elt ->
       f elt.a;
       loop elt.next
  in loop t.front

let fold ~f ~init t =
  let rec loop acc = function
    | None -> acc
    | Some elt -> loop (f acc elt.a) elt.next
  in loop init t.front

let insert_first t a =
  match t.front with
  | None ->
     let elt = { a; prev = None; next = None } in
     t.front <- Some elt;
     t.back <- t.front;
     elt
  | (Some head) as next ->
     let elt = { a; prev = None; next } in
     t.front <- Some elt;
     head.prev <- t.front;
     elt

let remove t elt =
  begin match elt.prev with
  | None ->
     t.front <- elt.next
  | Some prev ->
     prev.next <- elt.next
  end;
  begin match elt.next with
  | None ->
     t.back <- elt.prev
  | Some next ->
     next.prev <- elt.prev
  end

module Elt = struct
  type 'a t = 'a elt
end
