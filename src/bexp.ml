(* Copyright (C) 2019 TheAspiringHacker.

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)
include Types

let create ?x ?y ~width ~height hole =
  let workspace = Workspace.create ?x ?y ~width ~height hole in
  { workspace
  ; hole }

module Block = Block
module Hole = Hole
module Palette = Palette
module Syntax = Syntax
module Toolbox = Toolbox
module Widget = Widget
module Workspace = Workspace
