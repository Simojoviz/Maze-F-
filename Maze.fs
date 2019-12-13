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

type direzione = Su | Destra | Giu | Sinistra

let changecell (d : direzione) (c : cell) = 
    match d with 
    |Su -> {x0 = c.x0 ; y0 = (c.y0 - 2) ; visited = c.visited ; char = c.char}
    |Destra -> {x0 = (c.x0 + 2) ; y0 = c.y0 ; visited = c.visited ; char = c.char}
    |Giu -> {x0 = c.x0 ; y0 = (c.y0 + 2) ; visited = c.visited ; char = c.char}
    |Sinistra -> {x0 = (c.x0 - 2) ; y0 = c.y0 ; visited = c.visited ; char = c.char}

let is_visited c = c.visited

let remove_wall d c = 
    let newcell = changecell d c 
    {x0 = newcell.x0 ; y0 = newcell.y0 ; visited = newcell.visited ; char = CharInfo.path}

// TASK 1: implement the maze type
type maze (w, h) =
    
    
    
    

    member private __.generate = ()
  
  
