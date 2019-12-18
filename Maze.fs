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

let W = 31
let H = 31

type direction = Up | Right | Down | Left

// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
type maze (w, h) as this =  
    
    let mazestruc = Array2D.create w h false

    let removeWall (x1, y1) (x2, y2)=
        let Xcellwall = (min x1 x2) + 1
        let Ycellwall = (min y1 y2) + 1
        mazestruc.[Xcellwall, Ycellwall] <- true

    let rndDir l = 
        let i = rnd_int 0 3
        List.item i l

    let isValidCell (x,y) = x>=1 && x<=(w-1) && y>=1 && y<=(h-1)

    // TODO: do not forget to call the generation function in your object initializer
    do this.generate

    // TODO: start with implementing the generation
    member private __.generate = 
        let generateMaze =
            mazestruc.[1 , 1] <- true
            let rec loop m =
                    let mutable X = 1
                    let mutable Y = 1
                    let mutable move = []
                    let dir = rndDir [Up ; Right ; Down ; Left]
                    match dir with 
                    |Up when isValidCell (X , Y - 2) -> mazestruc.[X , Y-2] <- true
                                                        removeWall (X,Y) (X,Y-2)
                                                        Y <- (Y-2)
                                                        move <- Up :: move
                                                        loop mazestruc
                    |Right when isValidCell (X + 2 , Y) -> mazestruc.[X+2 , Y] <- true
                                                           removeWall (X,Y) (X+2,Y)
                                                           X <- (X+2)
                                                           move <- Right :: move
                                                           loop mazestruc
                    |Down when isValidCell (X , Y + 2) -> mazestruc.[X , Y+2] <- true
                                                          removeWall (X,Y) (X,Y+2)
                                                          Y <- (Y+2)
                                                          move <- Down :: move
                                                          loop mazestruc
                    |Left when isValidCell (X - 2 , Y) -> mazestruc.[X-2 , Y] <- true
                                                          removeWall (X,Y) (X-2,Y)
                                                          X <- (X-2)
                                                          move <- Left :: move
                                                          loop mazestruc
                    |_ -> if isValidCell (X , Y - 2) || isValidCell (X + 2 , Y) || isValidCell (X , Y + 2) || isValidCell (X - 2 , Y) then loop m
                          else if move = [] then ()                           
                               else match List.head move with
                                    |Up -> Y <- (Y+2)
                                           move <- List.tail move
                                           loop mazestruc
                                    |Right -> X <- (X-2)
                                              move <- List.tail move
                                              loop mazestruc
                                    |Down -> Y <- (Y-2)
                                             move <- List.tail move
                                             loop mazestruc
                                    |Left ->  X <- (X+2)
                                              move <- List.tail move
                                              loop mazestruc
            loop mazestruc
        generateMaze   
        

let main () = ()
   



  
  
