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

type state = {
    maze: sprite
    }

// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
type maze (w, h) as this =  
    
    let mazestruct = new image (w, h, Array.create (w*h) CharInfo.wall) 

    let removeWall (x1, y1) (x2, y2)=
        let Xcellwall = (min x1 x2) + 1
        let Ycellwall = (min y1 y2) + 1
        mazestruct.[Xcellwall, Ycellwall] <- CharInfo.path

    let rndDir l = 
        let i = rnd_int 0 3
        List.item i l

    let isValidCell (x,y) = x>=1 && x<=(w-1) && y>=1 && y<=(h-1)    

        
    
    // TODO: do not forget to call the generation function in your object initializer
    do this.generate

    // TODO: start with implementing the generation
    member private __.generate =
        let geneerateMaze =
            mazestruct.[1 , 1] <- CharInfo.path
            let rec loop m X Y move =
                    let dir = rndDir [Up ; Right ; Down ; Left]
                    match dir with 
                    |Up when isValidCell (X , Y - 2) -> mazestruct.[X , Y-2] <- CharInfo.path
                                                        removeWall (X,Y) (X,Y-2)
                                                        loop mazestruct X (Y-2) (Up::move)
                    
                    |Right when isValidCell (X + 2 , Y) -> mazestruct.[X+2 , Y] <- CharInfo.path
                                                           removeWall (X,Y) (X+2,Y)                                                           
                                                           loop mazestruct (X+2) Y (Right::move)
                    
                    |Down when isValidCell (X , Y + 2) -> mazestruct.[X , Y+2] <- CharInfo.path
                                                          removeWall (X,Y) (X,Y+2)                                                          
                                                          loop mazestruct X (Y+2) (Down::move)
                    
                    |Left when isValidCell (X - 2 , Y) -> mazestruct.[X-2 , Y] <- CharInfo.path
                                                          removeWall (X,Y) (X-2,Y)                                                          
                                                          loop mazestruct (X-2) Y (Left::move)
                    |_ -> if isValidCell (X , Y - 2) || isValidCell (X + 2 , Y) || isValidCell (X , Y + 2) || isValidCell (X - 2 , Y) then loop m X Y move
                          else if move = [] then ()                           
                               else match List.head move with
                                    |Up -> loop mazestruct X (Y+2) (List.tail move)
                                    
                                    |Right -> loop mazestruct (X-2) Y (List.tail move)
                                    
                                    |Down -> loop mazestruct X (Y-2) (List.tail move)

                                    |Left ->  loop mazestruct (X+2) Y (List.tail move)
            loop mazestruct 1 1 []   
        geneerateMaze

    member val finalMaze = mazestruct

let main () = 
    let engine = new engine (W, H)

    let maze = new maze (W,H)

    let spr1 = engine.create_and_register_sprite (maze.finalMaze, 0, 0, 0 )

    let update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        st, key.KeyChar = 'q'

    let st0 = {
        maze = spr1  
        }

    engine.loop_on_key update st0
   



  
  
