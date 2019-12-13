(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open External
open Gfx
open System.Text
open Engine

type CharInfo with
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    static member internal path = pixel.filled Color.Black
    member this.isWall = this = pixel.wall

let W = 30
let H = 30

type cell = { 
    x0 : int  
    y0 : int 
    visited : bool 
    char : CharInfo
} 

type direction = Up | Right | Down | Left


type state = {
    maze: sprite
}

// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
type maze (w, h) as this =    
    inherit image (w ,h)

    let i = new image (w, h) 

    let changecell (d : direction) (c : cell) = 
        match d with 
        | Up-> {x0 = c.x0 ; y0 = (c.y0 - 2) ; visited = c.visited ; char = c.char}
        | Right -> {x0 = (c.x0 + 2) ; y0 = c.y0 ; visited = c.visited ; char = c.char}
        | Down -> {x0 = c.x0 ; y0 = (c.y0 + 2) ; visited = c.visited ; char = c.char}
        | Left -> {x0 = (c.x0 - 2) ; y0 = c.y0 ; visited = c.visited ; char = c.char}
    
    let is_visited c = c.visited
    
    let remove_wall d c = 
        let newcell = changecell d c 
        {x0 = newcell.x0 ; y0 = newcell.y0 ; visited = newcell.visited ; char = CharInfo.path}

    // TODO: do not forget to call the generation function in your object initializer
    do this.generate

    // TODO: start with implementing the generation
    member private __.generate =
        i.draw_rectangle (0, 0, w, h, CharInfo.wall)

let main () =
    let engine = new engine (W, H)

    let maze1 = new maze (W, H)

    let spr1 = engine.create_and_register_sprite (maze1,0 ,0, 1)

    let update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        st, key.KeyChar = 'q'

    let st0 = {
        maze = spr1  
        }

    engine.loop_on_key update st0



  
  
